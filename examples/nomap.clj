;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns nomap (:use [embang emit runtime]))

(defquery
 nomap
 (let
  [foo
   (fn
    foo
    []
    (+ (sample (normal 1 0.1)) (if (sample (flip 0.5)) (foo) 0.0)))]
  (predict '(foo) (foo))))

