(ns clara-eav.rules-test
  (:require [clara-eav.test-helper :as th]
            [clara-eav.eav :as eav]
            [clara-eav.store :as store]
            #?@(:clj [[clara.rules :as rules]
                      [clara-eav.rules :as eav.rules]
                      [clojure.test :refer [deftest testing is are use-fixtures]]]
                :cljs [[clara.rules :as rules :include-macros true]
                       [clara-eav.rules :as eav.rules :include-macros true]
                       [cljs.test :refer-macros [deftest testing is are use-fixtures]]])))

(use-fixtures :once th/spec-fixture)

;; Entity maps

(def new1 #:todo{:db/id :new :text "..." :done false})
(def milk1 #:todo{:db/id :milk :text "Buy milk" :done false})
(def eggs1 #:todo{:db/id :eggs :text "Buy eggs" :done true})
(def flakes #:todo{:text "Buy flakes" :done false})

(def new2 (assoc new1 :todo/text "!!!"))
(def milk2 (assoc milk1 :todo/text "Buy milk2"))
(def eggs2 (assoc eggs1 :todo/text "Buy two eggs"))
(def ham #:todo{:text "Buy ham" :done false})
(def cookie-a #:todo{:db/id :task-c-a :text "Buy cookie a" :done false})
(def cookie-b #:todo{:db/id :task-c-b :text "Buy cookie b" :done false})

(def toast5 #:todo{:db/id "toast-tempid" :text "Buy toast" :done true})
(def jam5 #:todo{:db/id -7 :text "Buy jam" :done true})

;; Rules

(eav.rules/defrule milk-and-flakes-r
  [[_ :todo/text "Buy milk"]]
  =>
  (eav.rules/upsert-unconditional! flakes))

(eav.rules/defrule milk2-and-cookies-r
  [[_ :todo/text "Buy milk2"]]
  =>
  (eav.rules/upsert! [cookie-a cookie-b]))

(eav.rules/defrule remove-r
  [[::remove :eav/transient ?e]]
  [?eav <- [?e ?a ?v]]
  =>
  (eav.rules/retract! ?eav))

;; Queries

(eav.rules/defquery todo-q [:?e]
  [?todo <- eav.rules/entity :from [[?e]]])

(eav.rules/defquery todos-q []
  [[?e :todo/text]]
  [?todos <- eav.rules/entities :from [[?e]]])

(eav.rules/defquery transients-q []
  [?transient <- :eav/transient])

(eav.rules/defquery age-q [:?name]
  [[?e :schema/name ?name]]
  [[?e :schema/age ?age]])

;; Session

(eav.rules/defsession session
  'clara-eav.rules-test)

;; Helpers

(defn todo
  [session ?e]
  (-> (rules/query session todo-q :?e ?e)
      first
      :?todo))

(defn todos
  [session]
  (->> (rules/query session todos-q)
       (mapcat :?todos)))

(defn transients
  [session]
  (->> (rules/query session transients-q)
       (map :?transient)))

(defn upsert
  [session tx]
  (-> (eav.rules/upsert session tx)
      (rules/fire-rules)))

(defn retract
  [session tx]
  (-> (eav.rules/retract session tx)
      (rules/fire-rules)))



;; Tests

(defn test-session
  [expectation session]
  (are [x y] (th/set= x y)
    expectation (-> (todos session) th/strip-ids)
    expectation (-> (:store session) store/dump-entity-maps th/strip-ids)))


(deftest upsert-retract-accumulate-test
  (testing "Upsert, retract, accumulate entities"
    (let [

          ;; - upsert via call
          ;; - upsert-unconditional! via milk-and-flakes-r rule
          ;; - entity accumulator via todo-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session1 (upsert session [new1 milk1 eggs1])
          eav-index (-> [new1 milk1 eggs1 flakes] th/strip-ids)
          _ (are [x y] (th/set= x y)
              new1 (todo session1 :new))
          _ (test-session eav-index session1)

          ;; - upsert via call
          ;; - upsert! via milk2-and-cookies-r rule
          ;; - entity accumulator via todo-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session2 (upsert session1 [new2 milk2 ham eggs2])
          eav-index (-> [new2 milk2 eggs2 flakes ham cookie-a cookie-b] th/strip-ids)
          _ (are [x y] (th/set= x y)
              new2 (todo session2 :new))
          _ (test-session eav-index session2)

          ;; - retract via call
          ;; - entity accumulator via todo-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session3 (retract session2 [new2 eggs2])
          eav-index (-> [flakes milk2 ham cookie-a cookie-b] th/strip-ids)
          _ (are [x y] (th/set= x y)
              nil (todo session3 :new))
          _ (test-session eav-index session3)

          ;; - upsert transient via call
          ;; - retract! via remove-r rule
          ;; - eav binding via transients-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session4 (upsert session3 [[::remove :eav/transient :task-c-a]
                                     [::remove :eav/transient :task-c-b]])
          eav-index (-> [flakes milk2 ham] th/strip-ids)
          _ (are [x y] (th/set= x y)
              [] (transients session4))
          _ (test-session eav-index session4)

          ;; - upsert via call
          ;; - tempids (string and negative int) resolution
          ;; - eav binding via transients-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session5 (upsert session4 [toast5 jam5])
          eav-index (-> [flakes milk2 ham jam5 toast5] th/strip-ids)
          _ (are [x y] (th/set= x y)
              [] (transients session5))
          _ (test-session eav-index session5)])))

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

;; Session with basic-schema
(eav.rules/defsession* session-basic-schema
  {:schema basic-schema}
  'clara-eav.rules-test)

;; Session with full-schema and schema enforcement
(eav.rules/defsession* session-enforce-schema
  {:schema-mode :enforce
   :schema full-schema }
  'clara-eav.rules-test)

;; Session with full-schema and schema,tx-overwrite enforcement
(eav.rules/defsession* session-enforce-schema-overwrite
  {:schema-mode :enforce
   :tx-overwrite-mode :enforce
   :schema full-schema }
  'clara-eav.rules-test)

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

(deftest schema-tests
  (testing "no-enforcement violation"
    (upsert session-basic-schema [foo bar]))
  (testing "schema enforcement"
    (upsert session-enforce-schema [foo]))
  (testing "schema enforcement violation"
    (try
      (upsert session-enforce-schema [foo bar])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :no-schema)
                    (eav/eav-seq [bar]))))))
  (testing "simple value non-collision"
    (let [sess (upsert session-enforce-schema [tina arlan])]
      (is true)))
  (testing "simple value collision"
    (try
      (upsert session-enforce-schema [tina bob])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :value-av-e-set)
                    {[:schema/parking-space 1] #{1 2}})))))
  (testing "simple ident upsert"
    (let [sess (upsert session-enforce-schema [tina tina-age arlan])]
      (is (= [73] (map :?age (rules/query sess age-q :?name "Tina"))))))
  (testing "simple ident tx-overwrite (ignore)"
    (let [sess (upsert session-enforce-schema [tina arlan mal])]
      (is true)))
  (testing "simple ident tx-overwrite (enforce)"
    (try
      (upsert session-enforce-schema-overwrite [tina arlan mal])
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
    (try
      (let [sess (upsert session-enforce-schema [tina tina-age arlan ])
            sess2 (upsert sess [mal])]
        (is false))
      (catch #?(:clj Exception :cljs js/Error) e
             (is true))))
  (testing "in transaction upsert"
    (let [sess (upsert session-enforce-schema [bob bob-foo bob-age])]
      (is (= (store/dump-entity-maps (:store sess))
             [{:schema/name "Bob",
               :schema/ssn 345,
               :schema/parking-space 1,
               :schema/foo 7,
               :schema/age 77,
               :db/id 88}]))))
  (testing "simple ref test"
    (let [sess (upsert session-enforce-schema-overwrite
                       [tina tina-boss
                        arlan franz may])]
      (= (store/dump-entity-maps (:store sess))
         [#:schema{:name "Tina", :ssn 123, :id 777, :parking-space 1, :boss 3, :db/id 2}
          #:schema{:name "Arlan", :ssn 456, :id 888, :parking-space 2, :db/id 1}
          #:schema{:name "Franz", :ssn 345, :parking-space 11, :db/id 4}
          #:schema{:name "May", :ssn 234, :parking-space 10, :db/id 3}])))
  #_ ;TODO add card/many
  (testing "card/many ref test"
    (let [sess (upsert session-enforce-schema-overwrite
                       [tina tina-kid1 tina-kid2
                        arlan franz may])]
      (prn :ref2 (store/dump-entity-maps (:store sess)))))
  (testing ""
    (is true)))
