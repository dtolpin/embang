;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns
 lr-iris
 (:require [clojure.core.matrix :refer [dot]])
 (:use [embang runtime emit] iris-data))

(def
 iris-data-setosa
 (filter (fn* [p1__1#] (= (last p1__1#) :setosa)) iris-data))

(def
 iris-data-not-setosa
 (filter (fn* [p1__2#] (not= (last p1__2#) :setosa)) iris-data))

(assert (= (last (first iris-data-setosa)) :setosa))

(assert (not= (last (first iris-data-not-setosa)) :setosa))

(def test-setosa (rand-nth iris-data-setosa))

(def test-not-setosa (rand-nth iris-data-not-setosa))

(assert (= (last test-setosa) :setosa))

(assert (not= (last test-not-setosa) :setosa))

(with-primitive-procedures
 [dot]
 (defquery
   lr-iris
   (let
     [features (fn features [record] (cons 1 (butlast record)))]
     (let
       [iris-data
        (filter
          (fn
            [record]
            (not (or (= record test-setosa) (= record test-not-setosa))))
          iris-data)]
       (let
         [sigma (sqrt (sample (gamma 1 1)))]
         (let
           [b (repeatedly 5 (fn [] (sample (normal 0.0 sigma))))]
           (let
             [z (fn z [x] (/ 1.0 (+ 1.0 (exp (* -1.0 (dot b x))))))]
             (reduce
               (fn
                 [_ record]
                 (observe
                   (flip (z (features record)))
                   (= (last record) iris-setosa)))
               ()
               iris-data)
             (let
               [is-setosa (fn is-setosa [x] (sample (flip (z x))))]
               (predict
                 '(is-setosa (features test-setosa))
                 (is-setosa (features test-setosa)))
               (predict
                 '(is-setosa (features test-not-setosa))
                 (is-setosa (features test-not-setosa)))))))))))
