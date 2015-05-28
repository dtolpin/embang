;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns branching (:use [embang emit runtime]))
(defquery
 branching
 "simple branching"
 (let
  [fib
   (fn
    fib
    [n]
    (cond (= n 0) 0 (= n 1) 1 :else (+ (fib (- n 1)) (fib (- n 2)))))]
  (let
   [r (sample (poisson 4))]
   (let
    [l (if (< 4 r) 6 (+ (fib (* 3 r)) (sample (poisson 4))))]
    (observe (poisson l) 6)
    (predict 'r r)))))
(defquery
 optimized
 "simple branching with some basic manual optimizations"
 (let
  [poisson-source (poisson 4)]
  (let
   [fib
    (fn
     fib
     [n]
     (cond (= n 0) 0 (= n 1) 1 :else (+ (fib (- n 1)) (fib (- n 2)))))]
   (let
    [r (sample poisson-source)]
    (let
     [l (if (< 4 r) 6 (+ (fib (* 3 r)) (sample poisson-source)))]
     (observe (poisson l) 6)
     (predict 'r r))))))
