;;;; trading.asd

(asdf:defsystem #:trading
  :description "Trading stuff on the OANDA platform"
  :author "Will Langstroth <will@langstroth.com>"
  :license "MIT"
  :serial t
  :depends-on (#:jonathan
               #:drakma
               #:dexador
               #:chronograph)
  :components ((:file "package")
               (:file "oanda")
               (:file "backtest")
               (:file "trading")))
