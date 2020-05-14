require "spec_helper"

# This test suite assumes the following circuit is connected:
# GND_PIN = 6
# LED_PIN = 12 (with resistor to 0v)
# SWITCH_PIN = 18 (with 0.1 uF capacitor around switch) to 0v
# LOOP_IN = 16 connected with 1K resistor to LOOP_OUT
# LOOP_OUT = 22

describe RPi::GPIO do
  before :each do
    RPi::GPIO.set_warnings false
    RPi::GPIO.reset
    RPi::GPIO.set_warnings false
  end

  after :each do
    RPi::GPIO.set_warnings false
    RPi::GPIO.reset
  end

  describe ".detect_event" do
    let(:channel) { 18 }
    let(:options) {{ edge: :rising, callback: Proc.new {} }}

    subject { RPi::GPIO.detect_event channel, options }

    before do
      RPi::GPIO.set_numbering :board
      RPi::GPIO.setup 18, as: :input
    end

    it "doesn't raise an error when valid arguments are provided" do
      expect { subject }.to_not raise_error
    end

    context "with no callback provided" do
      it "raises an error" do
        options.delete(:callback)
        expect { subject }.to raise_error(ArgumentError)
      end

      context "but block provided" do
        it "doesn't raise an error" do
          options.delete(:callback)
          expect {
            RPi::GPIO.detect_event(channel, options) do
              puts "Detected."
            end
          }.to_not raise_error
        end
      end
    end

    context "with a callback provided" do
      context "with invalid edge value" do
        it "raises an error" do
          options[:edge] = :sharp
          expect { subject }.to raise_error(ArgumentError)
        end
      end

      context "with rising edge" do
        it "doesn't raise an error" do
          options[:edge] = :rising
          expect { subject }.to_not raise_error
        end
      end

      context "with falling edge" do
        it "doesn't raise an error" do
          options[:edge] = :falling
          expect { subject }.to_not raise_error
        end
      end

      context "with both rising and falling edge" do
        it "doesn't raise an error" do
          options[:edge] = :both
          expect { subject }.to_not raise_error
        end
      end

      context "with a valid bounce value" do
        it "doesn't raise an error" do
        end
      end

      context "with an invalid bounce" do
        it "raises an error" do
        end
      end

      context "with a callback provided" do
        it "calls the provided callback" do
        end
      end
    end
  end
end
