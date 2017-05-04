(in-package #:trading)

(load "secrets")

;; Either you do the following, or you have to (flexi-streams:octets-to-string)
;; anything that comes from (drakma:http-request)
(setq drakma:*text-content-types*
      (cons '("application" . "json")
            drakma:*text-content-types*))

(defvar *base-url* "https://api-fxtrade.oanda.com/v1/")
(defvar *account-url*
  (concatenate 'string *base-url* "accounts/" *default-account*))
(defvar *trades-url*
  (concatenate 'string *account-url* "/trades"))

;; *bearer* is in the secrets file
(defun auth-bearer ()
  (format nil "Bearer ~a" *bearer*))

(defun price-url-of (instrument)
  (concatenate 'string *base-url* "prices?instruments=" instrument))

(defun oanda-request (url)
  (drakma:http-request
   url
   :additional-headers `(("Authorization" . ,(auth-bearer)))))

(defun price-of (instrument &optional (bid-or-ask :|bid|))
  (getf (prices instrument) bid-or-ask))

(defun prices (instrument)
  (first
   (getf
    (jonathan:parse
     (oanda-request
      (price-url-of instrument)))
    :|prices|)))

(defun trades ()
  (getf
   (jonathan:parse
    (oanda-request *trades-url*))
   :|trades|))

(defun side-factor (side)
  (cond ((string= side "sell") -1)
               (t 1)))

(defun euro-price (&optional (bid-or-ask :|bid|))
  (price-of "EUR_USD" bid-or-ask))

(defun show-trade (trade-plist)
  (let*
      ((name (getf trade-plist :|instrument|))
       (units (getf trade-plist :|units|))
       (trade-price (getf trade-plist :|price|))
       (side (getf trade-plist :|side|))
       (trade-time (subseq (getf trade-plist :|time|) 0 16))
       (price
        (cond
          ((string= side "sell") (price-of name :|ask|))
          (t (price-of name :|bid|))))
       (adjustment-factor
        (cond
          ((string= (subseq name 0 3) "USD") price)
          ((search "_EUR" name) (/ 1 (euro-price)))
          (t 1)))
       (price-diff (/ (* (side-factor side)
                         (- (* price units)
                            (* trade-price units)))
                      adjustment-factor))
       (bought-sold
        (cond ((string= side "sell") "↓")
              (t "↑")))
       (pl-string (format nil "~$" price-diff)))
    (format t "~%~a ~a ~5@a ~10a @ ~7@a | P/L: ~7@a ~%"
            trade-time
            bought-sold
            units name
            trade-price
            pl-string)))

(defun show-trades ()
  (format t "~a~%" (chronograph:iso-now))
  (loop for trade in (trades)
     do (show-trade trade)))

(defun account ()
  (jonathan:parse
   (oanda-request *account-url*)))

(defun account-summary ()
  nil)

(defun balance ()
  (getf (account) :|balance|))

(defparameter *position-factor* 0.03)

(defun position-size (&optional (size *position-factor*))
  (* (balance) size))

(defun profit-loss ()
  (getf (account) :|unrealizedPl|))

(defun show-pl ()
  (format t "~%P/L: ~$" (profit-loss)))

(defun trade-status ()
  (show-trades)
  (show-pl))

(defun instrument-interest (instruments)
  (oanda-request (price-url-of instruments)))
