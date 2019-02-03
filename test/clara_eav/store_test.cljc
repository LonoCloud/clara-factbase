(ns clara-eav.store-test
  (:require [clara-eav.eav :as eav]
            [clara-eav.store :as store]
            [clara-eav.test-helper :as th]
    #?(:clj [clojure.test :refer [deftest testing is are use-fixtures]]
       :cljs [cljs.test :refer-macros [deftest testing is are use-fixtures]]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(use-fixtures :once th/spec-fixture)

(deftest state-test
  (testing "Store cleaned after transaction computation"
    (let [store (store/init {})
          store-tx (assoc store
                     :insertables []
                     :retractables []
                     :tempids {})]
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
  (merge store/default-store
         {:max-eid 1
          :eav-index {0 {:todo/text "Buy eggs"}
                      1 {:todo/tag :not-cheese}}}))

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
    (are [s s'] (th/set= s (-> s' store/dump-entity-maps th/strip-ids))
      [#:todo{:text "Buy eggs"}]
      (store/-eavs store [(eav/->EAV 1 :todo/tag :not-cheese)]))))

(deftest +eavs-test
  (testing "Entities added to store, entities updated in store, tempids to eids"
    (are [s s'] (= s (-> s' store/dump-entity-maps th/strip-ids))
      eav-index' (store/+eavs store eavs))))

(defn gen-schema [pre-schema]
  (map (fn [[id cd vt & extra]]
         (merge {:ident id :cardinality cd :valueType vt}
                (apply hash-map extra)))
       pre-schema))

(def basic-schema
  (gen-schema
   [[:schema/foo :cardinality/one any?]]))

(def full-schema
  (gen-schema
   [[:schema/foo   :cardinality/one any?]
    [:schema/name  :cardinality/one string?]
    [:schema/age   :cardinality/one int?]
    [:schema/boss  :cardinality/one :valueType/ref]
    [:schema/child :cardinality/many :valueType/ref]
    [:schema/boss  :cardinality/one :valueType/ref :unique :unique/identity]
    [:schema/ssn   :cardinality/one any? :unique :unique/identity]
    [:schema/id    :cardinality/one any? :unique :unique/identity]
    [:schema/parking-space :cardinality/one any? :unique :unique/value]]))

;; Store with basic-schema
(def store-basic-schema
  (store/init
   {:schema basic-schema}))

;; Store with full-schema and schema enforcement
(def store-enforce-schema
  (store/init
   {:schema-mode :enforce
    :schema full-schema }))

;; Store with full-schema and schema,tx-overwrite enforcement
(def store-enforce-schema-overwrite
  (store/init
   {:schema-mode :enforce
    :tx-overwrite-mode :enforce
    :schema full-schema }))

(def foo      #:schema{:db/id 1    :foo 1})
(def bar      #:schema{:db/id 2    :bar 2})
(def tina     #:schema{:db/id -1   :name "Tina" :ssn 123 :id 777 :parking-space 1})
(def tina-age #:schema{:db/id "t1" :age 73 :ssn 123})
(def tina-kid1 #:schema {:db/id -1 :child -3})
(def tina-kid2 #:schema {:db/id -1 :child -7})
(def tina-boss #:schema {:db/id -1 :boss -6})
(def bob      #:schema{:db/id -3   :name "Bob" :ssn 345 :parking-space 1})
(def bob-foo  #:schema{:db/id 88   :ssn 345 :foo 7})
(def bob-age  #:schema{:db/id 88   :age 77})
(def arlan    #:schema{:db/id -4   :name "Arlan" :ssn 456 :id 888 :parking-space 2})
(def mal      #:schema{:db/id -5   :name "Mal" :ssn 123 :id 888 :parking-space 9})
(def may      #:schema{:db/id -6   :name "May" :ssn 234 :parking-space 10})
(def franz    #:schema{:db/id -7   :name "Franz" :ssn 345 :parking-space 11})

(defn upsert [store tx]
  (store/+eavs store (eav/eav-seq tx)))

(deftest schema-tests
  (testing "no-enforcement violation"
    (upsert store-basic-schema [foo bar]))
  (testing "schema enforcement"
    (upsert store-enforce-schema [foo]))
  (testing "schema enforcement violation"
    (try
      (upsert store-enforce-schema [foo bar])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :no-schema)
                    (eav/eav-seq [bar]))))))
  (testing "simple value non-collision"
    (let [store (upsert store-enforce-schema [tina arlan])]
      (is true)))
  (testing "simple value collision"
    (try
      (upsert store-enforce-schema [tina bob])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :value-av-e-set)
                    {[:schema/parking-space 1] #{1 2}})))))
  (testing "simple ident upsert"
    (let [store (upsert store-enforce-schema [tina tina-age])]
      (is (= (store/dump-entity-maps store))
          [#:schema{:name "Tina", :ssn 123, :id 777, :parking-space 1, :age 73, :db/id 1}])))
  (testing "simple ident tx-overwrite (ignore)"
    (let [store (upsert store-enforce-schema [tina arlan mal])]
      (is true)))
  (testing "simple ident tx-overwrite (enforce)"
    (try
      (upsert store-enforce-schema-overwrite [tina arlan mal])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :tx-overwrites)
                    [[[1 :schema/name] [(eav/->EAV 1 :schema/name "Tina")
                                        (eav/->EAV 1 :schema/name "Arlan")
                                        (eav/->EAV 1 :schema/name "Mal")]]
                     [[1 :schema/ssn] [(eav/->EAV 1 :schema/ssn 123)
                                       (eav/->EAV 1 :schema/ssn 456)
                                       (eav/->EAV 1 :schema/ssn 123)]]
                     [[1 :schema/id] [(eav/->EAV 1 :schema/id 777)
                                      (eav/->EAV 1 :schema/id 888)
                                      (eav/->EAV 1 :schema/id 888)]]
                     [[1 :schema/parking-space] [(eav/->EAV 1 :schema/parking-space 1)
                                                 (eav/->EAV 1 :schema/parking-space 2)
                                                 (eav/->EAV 1 :schema/parking-space 9)]]])))))
  (testing "simple ident committed collision"
    (let [store (upsert store-enforce-schema [tina tina-age arlan ])]
      (try
        (upsert (store/state store) [mal])
        (is false)
        (catch #?(:clj Exception :cljs js/Error) e
               (is true)))))
  (testing "in transaction upsert"
    (let [store (upsert store-enforce-schema [bob bob-foo bob-age])]
      (is (= (store/dump-entity-maps store)
             [{:schema/name "Bob",
               :schema/ssn 345,
               :schema/parking-space 1,
               :schema/foo 7,
               :schema/age 77,
               :db/id 88}]))))
  (testing "simple ref test"
    (let [store (upsert store-enforce-schema-overwrite
                       [tina tina-boss
                        arlan franz may])]
      (= (store/dump-entity-maps store)
         [#:schema{:name "Tina", :ssn 123, :id 777, :parking-space 1, :boss 3, :db/id 2}
          #:schema{:name "Arlan", :ssn 456, :id 888, :parking-space 2, :db/id 1}
          #:schema{:name "Franz", :ssn 345, :parking-space 11, :db/id 4}
          #:schema{:name "May", :ssn 234, :parking-space 10, :db/id 3}])))
  #_ ;TODO add card/many
  (testing "card/many ref test"
    (let [store (upsert store-enforce-schema-overwrite
                       [tina tina-kid1 tina-kid2
                        arlan franz may])]
      (prn :ref2 (store/dump-entity-maps store))))
  (testing ""
    (is true)))
