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
  [entity]
  (with-meta
    (dissoc entity :eav/eid :db/id)
    {:db/id (if (:db/id entity)
              (:db/id entity)
              (:eav/eid entity))}))
(defmethod strip-ids :default
  [collection] (map strip-ids collection))

;; (defn strip-ids
;;   [entities]
;;   (for [entity entities]
;;     (with-meta
;;       (dissoc entity :eav/eid :db/id)
;;       {:db/id (if (:db/id entity)
;;                 (:db/id entity)
;;                 (:eav/eid entity))})))

(defn set= [& vectors] (apply = (map set vectors)))
