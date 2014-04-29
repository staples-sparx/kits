(ns kits.parse-test
  (:require  [kits.parse :as parse] )
  (:use      [expectations] )
  (:import   [java.lang.Math] )
)

(set! *warn-on-reflection* false)

; AWTAWT TODO: move to kits/testing
(defn error-ratio
  "Compute the error ratio for two floating-point values."
  [ val1 val2 ]
  (let [  dbl1         (double val1)
          dbl2         (double val2)
          abs-delta    (Math/abs (- dbl1 dbl2))
          max-abs-val  (Math/max (double (Math/abs dbl1))
                                 (double (Math/abs dbl2)) )
          error-ratio  (/ abs-delta max-abs-val) ]
    error-ratio 
  ))

(expect 15                     (parse/str->byte "15"))
(expect -5                     (parse/str->byte "-5"))
(expect NumberFormatException  (parse/str->byte "999"))
(expect NumberFormatException  (parse/str->byte " "))

(expect 15                     (parse/str->short "15"))
(expect -5                     (parse/str->short "-5"))
(expect 999                    (parse/str->short "999"))
(expect NumberFormatException  (parse/str->short "99999"))
(expect NumberFormatException  (parse/str->short" "))

(expect 15                     (parse/str->int "15"))
(expect -5                     (parse/str->int "-5"))
(expect 99999                  (parse/str->int "99999"))
(expect NumberFormatException  (parse/str->int "9876543210"))
(expect NumberFormatException  (parse/str->int " "))

(expect 15                     (parse/str->long "15"))
(expect -5                     (parse/str->long "-5"))
(expect 99999                  (parse/str->long "99999"))
(expect 9876543210             (parse/str->long "9876543210"))
(expect NumberFormatException  (parse/str->long "98765432109876543210"))
(expect NumberFormatException  (parse/str->long " "))

(expect 15.0                   (parse/str->float "15"))
(expect -5.0                   (parse/str->float "-5"))
(expect NumberFormatException  (parse/str->float " "))
(expect 0.5                    (parse/str->float "0.5"))
(expect #(< % 1e-6)  (error-ratio (parse/str->float "0.1") 0.1 ))
(expect #(< % 1e-6)  (error-ratio (parse/str->float "3.141592654") 3.141592654 ))

(expect 15.0                   (parse/str->double "15"))
(expect -5.0                   (parse/str->double "-5"))
(expect NumberFormatException  (parse/str->double " "))
(expect 0.5                    (parse/str->double "0.5"))
(expect #(< % 1e-9)  (error-ratio (parse/str->double "0.1") (double (/ 1 10) )))
(expect #(< % 1e-9)  (error-ratio (parse/str->double "3.141592654") Math/PI   ))


