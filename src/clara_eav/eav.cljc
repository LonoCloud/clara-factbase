(ns clara-eav.eav
  "This namespace is about defining EAVs and converting to EAVs from various
  representations (mainly from an entity map)."
  (:require
    #?(:clj [clojure.spec.alpha :as s]
       :cljs [cljs.spec.alpha :as s]))
  #?(:clj (:import (clojure.lang Indexed))
     :cljs (:require-macros [clara.rules :as rules])))

(defrecord EAV [e a v]
  ;; allows vector destructuring
  #?(:clj Indexed :cljs IIndexed)
  (#?(:clj nth :cljs -nth) [_ i]
    (case i, 0 e, 1 a, 2 v,
          #?(:clj (throw (IndexOutOfBoundsException.))
             :cljs (vector-index-out-of-bounds i 3))))
  (#?(:clj nth :cljs -nth) [_ i default]
    (case i, 0 e, 1 a, 2 v, default)))

(defn fact-type-fn
  "Clara-Rules `fact-type-fn` function for EAVs used in session creation.
  The attribute `:a` of an EAV it's used as it's type."
  [fact]
  (let [t (type fact)]
    (if (= EAV t)
      (:a fact)
      t)))

(defn ancestors-fn
  "Clara-Rules `ancestors-fn` function for EAVs used in session creation.
  The `EAV` record type matches all eavs, the :eav/all type does the same."
  [type]
  (if (keyword? type)
    [EAV :eav/all]
    (ancestors type)))

(s/def ::e #(or (string? %)
                (keyword? %)
                (int? %)
                (uuid? %)))
(s/def ::a keyword?)
(s/def ::v some?)

