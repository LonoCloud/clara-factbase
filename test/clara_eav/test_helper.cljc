(ns clara-eav.test-helper
  (:require [expound.alpha :as expound]
    #?@(:clj [[clojure.spec.alpha :as s]]
        :cljs [[cljs.spec.alpha :as s]])
    #?@(:clj [[clojure.spec.test.alpha :as st]]
        :cljs [[cljs.spec.test.alpha :as st]])))

(defn spec-fixture [f]
  (st/instrument)
  (binding [#_#_s/*explain-out* expound/printer]
    (f))
  (st/unstrument))

(defmulti strip-ids (fn [s] (type s)))
(defmethod strip-ids #?(:clj clojure.lang.PersistentArrayMap
                        :cljs cljs.core.PersistentArrayMap)
  [entity] (dissoc entity :eav/eid :db/id))
(defmethod strip-ids :default
  [collection] (map strip-ids collection))

(defn set= [& vectors] (apply = (map set vectors)))
