(ns embang.json
  (:require [clojure.data.json :as json])
  (:require [embang core state]))

(defn doquery
  "wraps do embang.core/doquery"
  [& args]
  (let [samples (ref (apply embang.core/doquery args))]
    (reify
      java.util.Iterator
      (hasNext [this] (some? (seq @samples)))
      (next [this]
        (-> (dosync 
              (let [[sample & more] @samples]
                (ref-set samples more)
                sample))
            (select-keys [:embang.state/log-weight
                          :embang.state/predicts])
            (json/write-str)))
      (remove [this] (throw (UnsupportedOperationException.))))))
