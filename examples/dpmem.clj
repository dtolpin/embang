;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns dpmem (:use [embang emit runtime]))

(defm
 sample-stick-index
 [breaking-rule index]
 (if
  (sample (flip (breaking-rule index)))
  index
  (sample-stick-index breaking-rule (+ index 1))))

(defm
 make-sethuraman-stick-picking-procedure
 [concentration]
 (let
  [V (mem (fn V [x] (sample (beta 1.0 concentration))))]
  (fn [] (sample-stick-index V 1))))

(defm
 DPmem
 [concentration base]
 (let
  [get-value-from-cache-or-sample
   (mem
    (fn
     get-value-from-cache-or-sample
     [args stick-index]
     (apply base args)))]
  (let
   [get-stick-picking-procedure-from-cache
    (mem
     (fn
      get-stick-picking-procedure-from-cache
      [args]
      (make-sethuraman-stick-picking-procedure concentration)))]
   (fn
    [& varargs]
    (do
     (let
      [index ((get-stick-picking-procedure-from-cache varargs))]
      (get-value-from-cache-or-sample varargs index)))))))

