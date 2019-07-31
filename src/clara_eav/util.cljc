(ns clara-eav.util
  (:require #?@(:clj [[clojure.spec.alpha :as s]]
                :cljs [[cljs.spec.alpha :as s]])))

(defn assert-spec [spec v]
  (if (s/valid? spec v)
    v
    (throw (ex-info (s/explain-str spec v)
                    {:explain-data (s/explain-data spec v)}))))
