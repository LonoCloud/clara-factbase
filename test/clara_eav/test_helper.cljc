(ns clara-eav.test-helper
  (:require
    #?@(:clj [[clojure.spec.test.alpha :as st]]
        :cljs [[cljs.spec.test.alpha :as st]])))

(defn spec-fixture [f]
  (st/instrument)
  (f)
  (st/unstrument))

(defmulti strip-ids (fn [s] (type s)))
(defmethod strip-ids clojure.lang.PersistentArrayMap
  [entity] (dissoc entity :eav/eid))
(defmethod strip-ids :default
  [collection] (map strip-ids collection))

(defn set= [& vectors] (apply = (map set vectors)))
