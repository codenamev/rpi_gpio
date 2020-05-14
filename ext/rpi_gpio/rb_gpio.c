/*
Original code by Ben Croston modified for Ruby by Nick Lowery
(github.com/clockvapor)
Copyright (c) 2014-2016 Nick Lowery

Copyright (c) 2013-2016 Ben Croston

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include "rb_gpio.h"

extern VALUE m_GPIO;
int gpio_warnings = 1;

struct rb_callback
{
  unsigned int gpio;
  VALUE rb_cb;
  unsigned long long lastcall;
  unsigned int bouncetime;
  struct rb_callback *next;
};
static struct rb_callback *rb_callbacks = NULL;

VALUE _extract_channels(VALUE channel_or_list)
{
    VALUE channel_list;

    // parse arguments
    if (RB_TYPE_P(channel_or_list, T_ARRAY) == 1) {
        channel_list = channel_or_list;
    } else {
        channel_list = rb_ary_new();
        rb_ary_push(channel_list, channel_or_list);
    }

    return channel_list;
}

void define_gpio_module_stuff(void)
{
    int i;

    rb_define_module_function(m_GPIO, "setup", GPIO_setup, 2);
    rb_define_module_function(m_GPIO, "clean_up", GPIO_clean_up, -1);
    rb_define_module_function(m_GPIO, "reset", GPIO_reset, 0);
    rb_define_module_function(m_GPIO, "set_numbering", GPIO_set_numbering, 1);
    rb_define_module_function(m_GPIO, "set_high", GPIO_set_high, 1);
    rb_define_module_function(m_GPIO, "set_low", GPIO_set_low, 1);
    rb_define_module_function(m_GPIO, "high?", GPIO_test_high, 1);
    rb_define_module_function(m_GPIO, "low?", GPIO_test_low, 1);
    rb_define_module_function(m_GPIO, "detect_event", GPIO_detect_event, 2);
    rb_define_module_function(m_GPIO, "remove_event_detection", GPIO_remove_event_detect, 1);
    rb_define_module_function(m_GPIO, "event_detected?", GPIO_event_detected, 1);
    rb_define_module_function(m_GPIO, "set_warnings", GPIO_set_warnings, 1);

    for (i = 0; i < 54; i++) {
        gpio_direction[i] = -1;
    }

    // detect board revision and set up accordingly
    if (get_rpi_info(&rpiinfo)) {
        rb_raise(rb_eRuntimeError, "this gem can only be run on a Raspberry Pi");
        setup_error = 1;
        return;
    } else if (rpiinfo.p1_revision == 1) {
        pin_to_gpio = &pin_to_gpio_rev1;
    } else if (rpiinfo.p1_revision == 2) {
        pin_to_gpio = &pin_to_gpio_rev2;
    } else { // assume model B+ or A+
        pin_to_gpio = &pin_to_gpio_rev3;
    }
}

int mmap_gpio_mem(void)
{
    int result;

    if (module_setup) {
        return 0;
    }

    result = setup();
    if (result == SETUP_DEVMEM_FAIL) {
        rb_raise(rb_eRuntimeError, "no access to /dev/mem; try running as root");
        return 1;
    } else if (result == SETUP_MALLOC_FAIL)  {
        rb_raise(rb_eNoMemError, "out of memory");
        return 2;
    } else if (result == SETUP_MMAP_FAIL)  {
        rb_raise(rb_eRuntimeError, "mmap of GPIO registers failed");
        return 3;
    } else if (result == SETUP_CPUINFO_FAIL) {
        rb_raise(rb_eRuntimeError, "unable to open /proc/cpuinfo");
        return 4;
    } else if (result == SETUP_NOT_RPI_FAIL) {
        rb_raise(rb_eRuntimeError, "not running on a RPi");
        return 5;
    } else { // result == SETUP_OK
        module_setup = 1;
        return 0;
    }
}

int is_gpio_initialized(unsigned int gpio)
{
    if (gpio_direction[gpio] != INPUT && gpio_direction[gpio] != OUTPUT) {
        rb_raise(rb_eRuntimeError,
            "you must setup the GPIO channel first with "
            "RPi::GPIO.setup CHANNEL, :as => :input or "
            "RPi::GPIO.setup CHANNEL, :as => :output");
        return 0;
    }

    return 1;
}

int is_gpio_output(unsigned int gpio)
{
    if (gpio_direction[gpio] != OUTPUT) {
        if (gpio_direction[gpio] != INPUT) {
            rb_raise(rb_eRuntimeError,
                "you must setup the GPIO channel first with "
                "RPi::GPIO.setup CHANNEL, :as => :input or "
                "RPi::GPIO.setup CHANNEL, :as => :output");
            return 0;
        }

        rb_raise(rb_eRuntimeError, "GPIO channel not setup as output");
        return 0;
    }

    return 1;
}

int is_gpio_input(unsigned int gpio)
{
    if (gpio_direction[gpio] != INPUT) {
        if (gpio_direction[gpio] != OUTPUT) {
            rb_raise(rb_eRuntimeError,
                "you must setup the GPIO channel first with "
                "RPi::GPIO.setup CHANNEL, :as => :input or "
                "RPi::GPIO.setup CHANNEL, :as => :output");
            return 0;
        }

        rb_raise(rb_eRuntimeError, "GPIO channel not setup as input");
        return 0;
    }

    return 1;
}

int is_rpi(void)
{
    if (setup_error) {
        rb_raise(rb_eRuntimeError, "this gem can only be run on a Raspberry Pi");
        return 0;
    }

    return 1;
}

// RPi::GPIO.clean_up(channel=nil)
// clean up everything by default; otherwise, clean up given channel
VALUE GPIO_clean_up(int argc, VALUE *argv, VALUE self)
{
    int i;
    int found = 0;
    int channel = -666; // lol, quite a flag
    unsigned int gpio;

    if (argc == 1) {
        channel = NUM2INT(argv[0]);
    } else if (argc > 1) {
        rb_raise(rb_eArgError, "wrong number of arguments; 0 for all pins, 1 for a specific pin");
        return Qnil;
    }

    if (channel != -666 && get_gpio_number(channel, &gpio)) {
        return Qnil;
    }

    if (module_setup && !setup_error) {
        if (channel == -666) {
            // clean up any /sys/class exports
            event_cleanup_all();

            // set everything back to input
            for (i = 0; i < 54; i++) {
                if (gpio_direction[i] != -1) {
                    setup_gpio(i, INPUT, PUD_OFF);
                    gpio_direction[i] = -1;
                    found = 1;
                }
            }
        } else {
            // clean up any /sys/class exports
            event_cleanup(gpio);

            // set everything back to input
            if (gpio_direction[gpio] != -1) {
                setup_gpio(gpio, INPUT, PUD_OFF);
                gpio_direction[gpio] = -1;
                found = 1;
            }
        }
    }

    // check if any channels set up - if not warn about misuse of GPIO.clean_up()
    if (!found && gpio_warnings) {
        rb_warn("no channels have been set up yet; nothing to clean up");
    }

    return Qnil;
}

// RPi::GPIO.reset
//
// cleans up all pins, unsets numbering mode, enables warnings.
VALUE GPIO_reset(VALUE self)
{
    GPIO_clean_up(0, NULL, self);
    gpio_mode = MODE_UNKNOWN;
    GPIO_set_warnings(self, Qtrue);
    return Qnil;
}

// RPi::GPIO.setup(channel, hash(:as => {:input, :output}, :pull => {:off,
// :down, :up}(default :off), :initialize => {:high, :low}))
//
// sets up a channel as either input or output with an optional pull-down or
// pull-up resistor and an optional initialize state
VALUE GPIO_setup(VALUE self, VALUE channel, VALUE hash)
{
    unsigned int gpio;
    int chan = -1;
    VALUE channel_list = _extract_channels(channel);
    int chan_count = RARRAY_LEN(channel_list);
    const char *direction_str = NULL;
    int direction;
    VALUE pud_val = Qnil;
    const char *pud_str = NULL;
    int pud = PUD_OFF;
    int func;

    VALUE initialize_val = Qnil;
    const char *initialize_str = NULL;
    int initialize = HIGH;

    // func to set up channel stored in channel variable
    int setup_one(void) {
        if (get_gpio_number(chan, &gpio)) {
            return 0;
        }

        // warn if the channel is already in use (not from this program).
        func = gpio_function(gpio);
        if (gpio_warnings &&
            ((func != 0 && func != 1) ||
            (gpio_direction[gpio] == -1 && func == 1)))
        {
            rb_warn("this channel is already in use... continuing anyway. use RPi::GPIO.set_warnings(false) to "
                "disable warnings");
        }

        if (gpio_warnings) {
            if (rpiinfo.p1_revision == 0) { // compute module - do nothing
            } else if ((rpiinfo.p1_revision == 1 &&
                    (gpio == 0 || gpio == 1)) ||
                    (gpio == 2 || gpio == 3)) {
                if (pud == PUD_UP || pud == PUD_DOWN) {
                    rb_warn("a physical pull up resistor is fitted on this channel");
                }
            }
        }

        if (direction == OUTPUT && (initialize == LOW || initialize == HIGH)) {
            output_gpio(gpio, initialize);
        }
        setup_gpio(gpio, direction, pud);
        gpio_direction[gpio] = direction;
        return 1;
    }

    // pin direction
    direction_str = rb_id2name(rb_to_id(rb_hash_aref(hash, ID2SYM(rb_intern("as")))));
    if (strcmp("input", direction_str) == 0) {
        direction = INPUT;
    } else if (strcmp("output", direction_str) == 0) {
        direction = OUTPUT;
    } else {
        rb_raise(rb_eArgError, "invalid pin direction; must be :input or :output");
    }

    // pull up, down, or off
    pud_val = rb_hash_aref(hash, ID2SYM(rb_intern("pull")));
    if (pud_val != Qnil) {
        if (direction == OUTPUT) {
            rb_raise(rb_eArgError, "output pin cannot use pull argument");
            return Qnil;
        }

        pud_str = rb_id2name(rb_to_id(pud_val));
        if (strcmp("down", pud_str) == 0) {
            pud = PUD_DOWN;
        } else if (strcmp("up", pud_str) == 0) {
            pud = PUD_UP;
        } else if (strcmp("off", pud_str) == 0) {
            pud = PUD_OFF;
        } else {
            rb_raise(rb_eArgError, "invalid pin pull direction; must be :up, :down, or :off");
            return Qnil;
        }
    } else {
        pud = PUD_OFF;
    }
    // initialize high or low
    initialize_val = rb_hash_aref(hash, ID2SYM(rb_intern("initialize")));
    if (initialize_val != Qnil) {
        if (direction == INPUT) {
            rb_raise(rb_eArgError, "input pins cannot use initial argument");
            return Qnil;
        }

        initialize_str = rb_id2name(rb_to_id(initialize_val));
        if (strcmp("high", initialize_str) == 0) {
            initialize = HIGH;
        } else if (strcmp("low", initialize_str) == 0) {
            initialize = LOW;
        } else {
            rb_raise(rb_eArgError, "invalid pin initialize state; must be :high or :low");
            return Qnil;
        }
    } else {
        initialize = HIGH;
    }

    if (!is_rpi() || mmap_gpio_mem()) {
        return Qnil;
    }

    if (direction != INPUT && direction != OUTPUT) {
        rb_raise(rb_eArgError, "invalid direction");
        return Qnil;
    }

    if (direction == OUTPUT) {
        pud = PUD_OFF;
    }

    for (int i = 0; i < chan_count; i++) {
      chan = NUM2INT(rb_ary_entry(channel_list, i));
      if (!setup_one()) {
        return Qnil;
      }
    }

    return self;
}

// RPi::GPIO.set_numbering(mode)
VALUE GPIO_set_numbering(VALUE self, VALUE mode)
{
    int new_mode;
    const char *mode_str = NULL;

    if (TYPE(mode) == T_SYMBOL) {
        mode_str = rb_id2name(rb_to_id(mode));
    } else {
        mode_str = RSTRING_PTR(mode);
    }

    if (strcmp(mode_str, "board") == 0) {
        new_mode = BOARD;
    } else if (strcmp(mode_str, "bcm") == 0) {
        new_mode = BCM;
    } else {
        rb_raise(rb_eArgError, "invalid numbering mode; must be :board or :bcm");
    }

    if (!is_rpi()) {
        return Qnil;
    }

    if (new_mode != BOARD && new_mode != BCM) {
        rb_raise(rb_eArgError, "invalid mode");
        return Qnil;
    }

    if (rpiinfo.p1_revision == 0 && new_mode == BOARD) {
        rb_raise(rb_eRuntimeError, ":board numbering system not applicable on compute module");
        return Qnil;
    }

    gpio_mode = new_mode;
    return self;
}

static void run_rb_callbacks(unsigned int gpio)
{
  struct rb_callback *cb = rb_callbacks;
  struct timeval tv_timenow;
  unsigned long long timenow;
  VALUE result;
  int exception;

  if (gpio_warnings)
    rb_warn("running callbacks...");

  while (cb != NULL)
  {
    if (cb->gpio == gpio) {
      gettimeofday(&tv_timenow, NULL);
      timenow = tv_timenow.tv_sec*1E6 + tv_timenow.tv_usec;
      if (cb->bouncetime == 0 || timenow - cb->lastcall > cb->bouncetime*1001 || cb->lastcall == 0 || cb->lastcall > timenow) {
        // run callback
        rb_funcall(cb->rb_cb, rb_intern("call"), 0);
        //result = rb_protect(rb_wrapped_callback, cb->rb_cb, &exception);
        rb_warn("Callback performed");
        if (exception) {
          //fprintf(stderr, "Callback failed\n");
          //rb_jump_tag(exception);
          rb_raise(rb_eRuntimeError, "callback failed");
        } else {
          rb_warn("Callback success");
          rb_iter_break();
        }
      }
      cb->lastcall = timenow;
    }
    cb = cb->next;
  }
}

static int add_rb_callback(unsigned int gpio, unsigned int bouncetime, VALUE cb_func)
{
  struct rb_callback *new_rb_cb;
  struct rb_callback *cb = rb_callbacks;

  // add callback to rb_callbacks list
  new_rb_cb = malloc(sizeof(struct rb_callback));
  if (new_rb_cb == 0)
  {
    rb_raise(rb_eNoMemError, "unable to add callback for event detection");
    return -1;
  }

  new_rb_cb->rb_cb = cb_func;
  new_rb_cb->gpio = gpio;
  new_rb_cb->lastcall = 0;
  new_rb_cb->bouncetime = bouncetime;
  new_rb_cb->next = NULL;
  if (rb_callbacks == NULL) {
    rb_callbacks = new_rb_cb;
  } else {
    // add to end of list
    while (cb->next != NULL)
      cb = cb->next;
    cb->next = new_rb_cb;
  }
  rb_warn("Adding edge callback from add_rb_callback...");
  add_edge_callback(gpio, run_rb_callbacks);
  if (gpio_warnings)
    rb_warn("added callback");
  return 0;
}

// RPi::GPIO.detect_event(channel, hash(:edge => {:rising, :falling, :both},
// :bouncetime => Integer(default -666))
// :callback => rb_block_proc(), &callback)
//
// sets up a listener with callback on a channel for the given edge with an optional
// bouncetime
VALUE GPIO_detect_event(VALUE self, VALUE channel, VALUE hash)
{
  unsigned int gpio;
  int edge, result;
  int bouncetime = 0;
  const char *edge_str = NULL;
  VALUE edge_val = Qnil;
  VALUE bounce_val = Qnil;
  VALUE callback = Qnil;

  if (rb_block_given_p()) {
    callback = rb_block_proc();
  } else {
    callback = rb_hash_aref(hash, ID2SYM(rb_intern("callback")));
  }

  if (NIL_P(callback)) {
    rb_raise(rb_eArgError, "either a block or callback kwarg is required");
    return Qnil;
  }

  if (get_gpio_number(NUM2INT(channel), &gpio))
    return Qnil;

  // check channel is set up as an input
  if (!is_gpio_input(gpio)) {
    rb_raise(rb_eRuntimeError, "you must setup() the GPIO channel as an input first");
    return Qnil;
  }

  // is edge valid value
  edge_val = rb_hash_aref(hash, ID2SYM(rb_intern("edge")));
  if (edge_val != Qnil) {
    edge_str = rb_id2name(rb_to_id(edge_val));
    if (strcmp("rising", edge_str) == 0) {
      edge = RISING_EDGE;
    } else if (strcmp("falling", edge_str) == 0) {
      edge = FALLING_EDGE;
    } else if (strcmp("both", edge_str) == 0) {
      edge = BOTH_EDGE;
    } else {
      rb_raise(rb_eArgError, "invalid edge; must be :rising, :falling, or :both");
      return Qnil;
    }
  } else {
    rb_raise(rb_eArgError, "you must specify an :edge; must be :rising, :falling, or :both");
  }

  if (check_gpio_priv()) {
    rb_raise(rb_eArgError, "GPIO not priv yo...");
    return Qnil;
  }

  bounce_val = rb_hash_aref(hash, ID2SYM(rb_intern("bouncetime")));
  if (FIXNUM_P(bounce_val)) {
    rb_warn("Updated bounce");
    bouncetime = NUM2INT(bounce_val);
  }

  // starts a thread
  if ((result = add_edge_detect(gpio, edge, bounce_val)) != 0) {
    if (result == 1) {
      rb_raise(rb_eRuntimeError, "edge detection already enabled for this GPIO channel");
      return Qnil;
    } else {
      rb_raise(rb_eRuntimeError, "failed to add edge detection");
      return Qnil;
    }
  }

  if (callback != Qnil && add_rb_callback(gpio, bouncetime, callback) != 0) {
    rb_warn("callback failed to add");
    return Qnil;
  }

  if (gpio_warnings)
    rb_warn("callback added...");

  return self;
}

// RPi::GPIO.remove_event_detection(gpio)
VALUE GPIO_remove_event_detect(VALUE self, VALUE channel)
{
  unsigned int gpio;
  int chan = -1;
  VALUE channel_list = _extract_channels(channel);
  int chan_count = RARRAY_LEN(channel_list);
  struct rb_callback *cb = rb_callbacks;
  struct rb_callback *temp;
  struct rb_callback *prev = NULL;

  if (chan_count < 1) {
    rb_raise(rb_eArgError, "a channel is required");
    return Qnil;
  }

  for (int i = 0; i < chan_count; i++) {
    chan = NUM2INT(rb_ary_entry(channel_list, i));
    // ignore unregistered gpio channels
    if (get_gpio_number(chan, &gpio))
      continue;

    // remove all ruby callbacks for gpio
    while (cb != NULL)
    {
      if (cb->gpio == gpio)
      {
        if (prev == NULL)
          rb_callbacks = cb->next;
        else
          prev->next = cb->next;
        temp = cb;
        cb = cb->next;
        free(temp);
      } else {
        prev = cb;
        cb = cb->next;
      }
    }

    remove_edge_detect(gpio);
  }

  return self;
}

// RPi::GPIO.event_detected(channel)
VALUE GPIO_event_detected(VALUE self, VALUE channel)
{
  unsigned int gpio;

  if (get_gpio_number(NUM2INT(channel), &gpio))
    return Qnil;

  if (event_detected(gpio))
    return Qtrue;
  else
    return Qfalse;
}

// RPi::GPIO.set_high(channel)
VALUE GPIO_set_high(VALUE self, VALUE channel)
{
    unsigned int gpio;
    int chan = -1;
    VALUE channel_list = _extract_channels(channel);
    int chan_count = RARRAY_LEN(channel_list);

    for (int i = 0; i < chan_count; i++) {
      chan = NUM2INT(rb_ary_entry(channel_list, i));
      if (get_gpio_number(chan, &gpio) || !is_gpio_output(gpio) || check_gpio_priv()) {
        return Qnil;
      } else {
        output_gpio(gpio, 1);
      }
    }

    return self;
}

// RPi::GPIO.set_low(channel)
VALUE GPIO_set_low(VALUE self, VALUE channel)
{
  unsigned int gpio;
  int chan = -1;
  VALUE channel_list = _extract_channels(channel);
  int chan_count = RARRAY_LEN(channel_list);

  for (int i = 0; i < chan_count; i++) {
    chan = NUM2INT(rb_ary_entry(channel_list, i));
    if (get_gpio_number(chan, &gpio) || !is_gpio_output(gpio) || check_gpio_priv()) {
      return Qnil;
    } else {
      output_gpio(gpio, 0);
    }
  }

  return self;
}

// RPi::GPIO.high?(channel)
VALUE GPIO_test_high(VALUE self, VALUE channel)
{
    unsigned int gpio;

    if (get_gpio_number(NUM2INT(channel), &gpio) || !is_gpio_initialized(gpio) || check_gpio_priv()) {
        return Qnil;
    }

    return input_gpio(gpio) ? Qtrue : Qfalse;
}

// RPi::GPIO.low?(channel)
VALUE GPIO_test_low(VALUE self, VALUE channel)
{
    return GPIO_test_high(self, channel) ? Qfalse : Qtrue;
}

// RPi::GPIO.set_warnings(state)
VALUE GPIO_set_warnings(VALUE self, VALUE setting)
{
    if (!is_rpi()) {
        return Qnil;
    }

    gpio_warnings = RTEST(setting);
    return self;
}
