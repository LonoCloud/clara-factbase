(ns clara-eav.util
  (:require #?@(:clj [[clojure.spec.alpha :as s]]
                :cljs [[cljs.spec.alpha :as s]])))

(defn assert-spec [spec v]
  (if (s/valid? spec v)
    v
    (throw (ex-info (s/explain-str spec v)
                    {:explain-data (s/explain-data spec v)}))))
-
(defn bagwise-mutual-diff
  "For each item in bag-b, remove the same number of occurences of that item in
  bag-a and vice versa. Return a vector of the resulting bag-a and bag-b.

  Example:
  (bagwise-mutual-diff [:a :a :b] [:a :b :c :c])
  => [[:a] [:c :c]]"
  ([bag-a bag-b] (bagwise-mutual-diff hash bag-a bag-b))
  ([keyfn bag-a bag-b]
   (loop [bag-a' []
          bag-b' []
          bag-a (sort-by keyfn bag-a)
          bag-b (sort-by keyfn bag-b)]
     (if (or (empty? bag-a) (empty? bag-b))
       [(into bag-a' bag-a) (into bag-b' bag-b)]
       (let [[A & BAG-A] bag-a
             [B & BAG-B] bag-b
             res (compare (keyfn A) (keyfn B))]
         (cond
           (neg? res)  (recur (conj bag-a' A) bag-b' BAG-A bag-b)
           (zero? res) (recur bag-a' bag-b' BAG-A BAG-B)
           (pos? res)  (recur bag-a' (conj bag-b' B) bag-a BAG-B)))))))
