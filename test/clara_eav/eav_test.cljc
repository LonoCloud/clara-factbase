(ns clara-eav.eav-test
  (:require [clara-eav.test-helper :as test-helper]
    #?@(:clj [[clojure.test :refer [deftest testing is are use-fixtures]]
              [clara-eav.eav :as eav]]
        :cljs [[cljs.test :refer-macros [deftest testing is are use-fixtures]]
               [clara-eav.eav :as eav :refer [EAV]]]))
  #?(:clj (:import
            (clara_eav.eav EAV)
            (clojure.lang Associative))))

(use-fixtures :once test-helper/spec-fixture)

(deftest eav-test
  (testing "Constructing and destructuring a eav."
    (let [[e a v] (eav/->EAV 1 :todo/done false)]
      (is (= [1 :todo/done false] [e a v])))))

(defrecord MyRecord [x])

(deftest fact-type-fn-test
  (testing "Fact type of eav."
    (let [d (eav/->EAV 1 :todo/done false)]
      (is (= :todo/done (#'eav/fact-type-fn d)))))
  (testing "Fact type of non-eav."
    (let [r (->MyRecord 1)]
      (is (= MyRecord (#'eav/fact-type-fn r))))))

(deftest ancestors-fn-test
  (testing "Ancestors of keyword."
    (is (= [EAV :eav/all] (#'eav/ancestors-fn :db/id))))
  #?(:clj (testing "Ancestors of my record."
            (is (some #{Associative}
                      (#'eav/ancestors-fn MyRecord))))))
