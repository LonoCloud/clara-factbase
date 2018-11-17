(ns clara-eav.store-test
  (:require [clara-eav.eav :as eav]
            [clara-eav.store :as store]
            [clara-eav.test-helper :as test-helper]
    #?(:clj [clojure.test :refer [deftest testing is are use-fixtures]]
       :cljs [cljs.test :refer-macros [deftest testing is are use-fixtures]]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(use-fixtures :once test-helper/spec-fixture)

(deftest state-test
  (testing "Store cleaned after transaction computation"
    (let [store {:max-eid 0
                 :eav-index {}
                 :options {}}
          store-tx (assoc store
                     :insertables []
                     :retractables []
                     :tempids {}
                     :options {})]
      (is (= store (store/state store-tx))))))

(deftest tempid?-test
  (testing "Are tempids"
    (are [x] (#'store/tempid? x)
      "my-id"
      -7))
  (testing "Are not tempids"
    (are [x] (not (#'store/tempid? x))
      :global
      10
      #uuid"8ed62381-c3ef-4174-ae8d-87789416bf65")))

(def store
  {:max-eid 1
   :options store/default-options
   :eav-index {0 {:todo/text "Buy eggs"}
               1 {:todo/tag :not-cheese}}})

(def eavs
  [(eav/->EAV "todo1-id" :todo/text "Buy milk")
   (eav/->EAV "todo1-id" :todo/tag :milk)
   (eav/->EAV 0 :todo/text "Buy eggs")
   (eav/->EAV 0 :todo/tag :eggs)
   (eav/->EAV -3 :todo/text "Buy ham")
   (eav/->EAV -3 :todo/tag :ham)
   (eav/->EAV 1 :todo/text "Buy cheese")
   (eav/->EAV 1 :todo/tag :cheese)
   (eav/->EAV :do :eav/transient "something")
   (eav/->EAV -9 :eav/transient "something")])

(def eav-index'
  [#:todo{:tag :eggs
          :text "Buy eggs"}
   #:todo{:tag :cheese
          :text "Buy cheese"}
   #:todo{:tag :milk
          :text "Buy milk"}
   #:todo{:tag :ham
          :text "Buy ham"}])

(deftest -eavs-test
  (testing "Entity removed from store, others are left alone"
    (are [s s'] (test-helper/set= s (-> s' store/dump-entity-maps test-helper/strip-ids))
      [#:todo{:text "Buy eggs"}]
      (store/-eavs store [(eav/->EAV 1 :todo/tag :not-cheese)]))))

(deftest +eavs-test
  (testing "Entities added to store, entities updated in store, tempids to eids"
    (are [s s'] (= s (-> s' store/dump-entity-maps test-helper/strip-ids))
      eav-index' (store/+eavs store eavs))))

