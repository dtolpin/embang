;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns aircraft (:use [embang emit runtime]))
(defquery
 aircraft
 (let
  [num-aircraft (+ 1 (sample (poisson 5)))]
  (let
   [aircraft-info
    (mem
     (fn
      aircraft-info
      [aircraft-id]
      (let
       [position
        (sample (normal 2.0 5.0))
        num-blips
        (sample (discrete (list 0.1 0.4 0.5)))
        blips
        (map
         (fn [i] (list aircraft-id i (sample (normal position 1.0))))
         (range num-blips))]
       (list position blips))))]
   (let
    [all-blips
     (reduce
      (fn
       [acc aircraft-id]
       (concat (second (aircraft-info aircraft-id)) acc))
      (repeat 3 '(0 0 0))
      (range num-aircraft))]
    (observe (normal (count all-blips) 1) 3)
    (observe (normal (nth (nth all-blips 0) 2) 1) 1.0)
    (observe (normal (nth (nth all-blips 1) 2) 1) 2.0)
    (observe (normal (nth (nth all-blips 2) 2) 1) 3.0)
    (predict 'num-aircraft num-aircraft)
    (let
     [positions
      (map
       (fn [aircraft-id] (first (aircraft-info aircraft-id)))
       (range num-aircraft))]
     (predict 'positions positions))))))
