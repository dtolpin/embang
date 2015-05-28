;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns rim (:use [embang emit runtime]))

(defquery
 rim
 (let
  [insert_nth
   (fn
    insert_nth
    [item n coll]
    (if
     (empty? coll)
     (list item)
     ((fn
       loop
       [item i fore next back]
       (cond
        (= i n)
        (concat fore (list item next) back)
        (empty? back)
        (concat fore (list next item))
        :else
        (recur
         item
         (inc i)
         (concat fore (list next))
         (first back)
         (rest back))))
      item
      0
      (list)
      (first coll)
      (rest coll))))]
  (let
   [shuffle
    (fn
     shuffle
     [orig]
     ((fn
       loop
       [n shuffled next remains]
       (if
        (empty? remains)
        (insert_nth next n shuffled)
        (recur
         (sample (uniform-discrete 0 (+ 2 (count shuffled))))
         (insert_nth next n shuffled)
         (first remains)
         (rest remains))))
      0
      (list)
      (first orig)
      (rest orig)))]
   (let
    [pair_with_each
     (fn
      pair_with_each
      [item col]
      ((fn
        loop
        [litem ritem remains collected]
        (if
         (empty? remains)
         (conj collected (list litem ritem))
         (recur
          litem
          (first remains)
          (rest remains)
          (conj collected (list litem ritem)))))
       item
       (first col)
       (rest col)
       []))]
    (let
     [all_pairwise_prefs
      (fn
       all_pairwise_prefs
       [ranking]
       ((fn
         loop
         [item remains results]
         (if
          (= 0 (count remains))
          results
          (recur
           (first remains)
           (rest remains)
           (concat results (pair_with_each item remains)))))
        (first ranking)
        (rest ranking)
        []))]
     (let
      [preference_sample
       (fn
        preference_sample
        [ranking]
        (do
         (let
          [pairs (all_pairwise_prefs ranking)]
          (let
           [N (count pairs)]
           (let [n (sample (uniform-discrete 0 N))] (nth pairs n))))))]
      (let
       [random_order (shuffle (list "a" "b" "c" "d"))]
       (observe
        (flip
         (if
          (= '("a" "b") (preference_sample random_order))
          0.99
          0.01))
        true)
       (predict 'random_order random_order))))))))

(defquery
 cat
 (let
  [alphabet (list (list "a" "b") (list "b" "c") (list "a" "c"))]
  (let
   [probs (sample (dirichlet (repeat (count alphabet) 1)))]
   (let
    [get_categorical_pairs
     (fn
      get_categorical_pairs
      [alphabet probs]
      (if
       (empty? probs)
       (list)
       (conj
        (get_categorical_pairs (rest alphabet) (rest probs))
        (list (first alphabet) (first probs)))))]
    (let
     [my_dist (get_categorical_pairs alphabet probs)]
     (observe (categorical my_dist) (list "a" "b"))
     (predict 'my_dist my_dist))))))

