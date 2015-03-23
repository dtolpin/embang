(ns embang.transform-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use embang.transform))

(def tx-tag-it {:type :tag-it :tags :all})
(defmethod transform :tag-it [code tag tx] [[tag code] tx])

(def tx-some-tags {:tags #{:list} :type :some-tags})
(defmethod transform :some-tags [code tag tx] [[tag code] tx])

(def tx-by-tag {:type :by-tag})
(defmethod transform [:atom :by-tag]
  [code tag tx] [[tag code] tx])
(defmethod transform [:application :by-tag]
  [code tag tx] [[tag code] tx])

(def tx-count-lists {:tags #{:list} :type :count-lists :count 0})
(defmethod transform :count-lists [code tag tx] [[tag code] (update-in tx [:count] inc)])
