(in-package #:trading)

(defun midpoint (high low)
  (/ (+ high low) 2))

(defun units-for-limit-trade (price side stop)
  (format t "~d"
          (floor
           (* (side-factor side)
              (/ (position-size) (- stop price))))))

(defun units-for-trade (instrument side stop)
  (let
      ((price
        (cond
          ((string= side "sell") (price-of instrument "ask"))
          ((string= side "buy") (price-of instrument "bid")))))
    (units-for-limit-trade price side stop)))

(defparameter *instrument-hours*
  '(:copper "18:00-17:15"
    :natgas "18:00-17:15"
    :loonie ""
    :yen ""
    :sugar "03:00-13:00"
    :silver ""
    :soybeans "20:00-08:45, 9:30-15:15"))

(defun trading-hours (&optional instrument)
  (cond ((null instrument) *instrument-hours*)
        (t (getf *instrument-hours* instrument))))

(defclass candle ()
  ((start-datetime
    :initarg :start-datetime)
   (ask-high
    :initarg :ask-high
    :reader ask-high)
   (bid-high
    :initarg :bid-high
    :reader bid-high)
   (ask-low
    :initarg :ask-low
    :reader ask-low)
   (bid-low
    :initarg :bid-low
    :reader bid-low)
   (ask-open
    :initarg :ask-open
    :reader ask-open)
   (bid-open
    :initarg :bid-open
    :reader bid-open)
   (ask-close
    :initarg :ask-close
    :reader ask-close)
   (bid-close
    :initarg :bid-close
    :reader bid-close)))
