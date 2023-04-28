(ns clara-eav.store-test
  (:require [clara-eav.eav :as eav]
            [clara-eav.store :as store]
            [clara-eav.test-helper :as test-helper]
            [clojure.set :as set]
            [medley.core :as medley]
            #?@(:clj [[clojure.spec.alpha :as s]
                      [clojure.test :refer [deftest testing is are use-fixtures]]]
                :cljs [[cljs.spec.alpha :as s]
                       [cljs.test :refer-macros [deftest testing is are use-fixtures]]])))

(use-fixtures :once test-helper/spec-fixture)

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

(def eav-record (eav/->EAV 1 :todo/done true))
(def eav-vector [1 :todo/done true])

(defn rand-uuid []
  (str (medley/random-uuid)))

(deftest eav-spec-test
  (testing "Are eav records."
    (are [x y] (= x (s/valid? ::store/eav y))
         true eav-record
         true eav-vector
         true (assoc eav-vector 0 (rand-uuid))
         false (assoc eav-record :a "not-a-keyword")
         false (assoc eav-vector 1 "not-a-keyword")
         false (conj eav-vector "4th element")
         false 1)))

(def new-todo
  {:todo/text "Buy milk"
   :todo/done false})

(def saved-todo
  {:eav/eid 10
   :todo/text "Buy eggs"
   :todo/done true})

(def saved-todo-eavs
  #{(eav/->EAV 10 :todo/text "Buy eggs")
    (eav/->EAV 10 :todo/done true)})

(deftest entity->eav-seq-test
  (testing "Entity map to EAVs with tempids"
    (let [eav-seq (#'store/entity->eav-seq {} new-todo)
          eids (map (comp second first) eav-seq)]
      (is (= 2 (count eav-seq)))
      (is (every? string? eids))
      (is (apply = eids))))
  (testing "Entity map to EAVs with eids"
    (is (= saved-todo-eavs (set (#'store/entity->eav-seq {} saved-todo))))))

(def list1 (list eav-record))
(def list2 (list eav-record eav-record))

(defn eid-of [eavs a' v']
  (let [=av (fn [[e a v]]
              (when (and (= a a') (= v v'))
                e))
        eid (some =av eavs)]
    (if eid
      eid
      (throw (ex-info (str "EID for " a' " and " v' " not found") {})))))

(deftest eav-seq-test
  (testing "Converts EAVs and lists of EAVs to a eav sequence"
    (are [x y] (= x (store/eav-seq y))
      list1 eav-record
      list1 eav-vector
      list1 list1
      list1 (list eav-vector)
      list2 list2
      list2 (list eav-vector eav-vector)
      list2 (list eav-vector eav-record)
      list2 (list eav-record eav-vector)))
  (testing "Converts a new entity map to a eav sequence"
    (let [eav-seq (store/eav-seq new-todo)
          eid1 (eid-of eav-seq :todo/text "Buy milk")
          eid2 (eid-of eav-seq :todo/done false)]
      (is (= 2 (count eav-seq)))
      (is (= eid1 eid2))
      (is (string? eid1))
      (is (string? eid2))))
  (testing "Converts a saved entity map to a eav sequence"
    (let [eav-seq (store/eav-seq saved-todo)]
      (is (= saved-todo-eavs
             (set eav-seq)))))
  (testing "Converts a list of entity maps to a EAVs sequence"
    (let [eav-seq (store/eav-seq (list new-todo saved-todo))
          eid1 (eid-of eav-seq :todo/text "Buy milk")
          eid2 (eid-of eav-seq :todo/done false)]
      (is (= 4 (count eav-seq)))
      (is (= eid1 eid2))
      (is (string? eid1))
      (is (string? eid2))
      (is (set/subset? saved-todo-eavs (set eav-seq))))))

(def store
  (merge store/default-store
         {:max-eid 1
          :tempids {}
          :attrs {}
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
    (are [s s'] (test-helper/set= s (-> s' store/dump-entity-maps test-helper/strip-ids))
      [#:todo{:text "Buy eggs"}]
      (store/-eavs store [(eav/->EAV 1 :todo/tag :not-cheese)]))))

(deftest +eavs-test
  (testing "Entities added to store, entities updated in store, tempids to eids"
    (are [s s'] (= s (-> s' store/dump-entity-maps test-helper/strip-ids))
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

(def standard-schema
  (gen-schema
   [[:schema/any :cardinality/one any?]
    [:schema/any-many :cardinality/many any?]
    [:schema/uniq-val :cardinality/one any? :unique :unique/value]
    [:schema/uniq-ident :cardinality/one any? :unique :unique/identity]
    [:schema/ref-one :cardinality/one :valueType/ref]
    [:schema/ref-many :cardinality/many :valueType/ref]]))

;; Store with basic-schema
(def store-basic-schema
  (store/init
   {:schema basic-schema}))

;; Store with full-schema and schema enforcement
(def store-enforce-schema
  (store/init
   {:schema-mode :enforce
    :tx-conflicts-mode :enforce
    :multi-retract-submode :enforce
    :schema full-schema}))

;; Store with full-schema and schema,tx-conflicts enforcement
(def store-enforce-schema-overwrite
  (store/init
   {:schema-mode :enforce
    :tx-conflicts-mode :enforce
    :schema full-schema}))

(def store-standard-enforce
  (store/init
   {:schema-mode :enforce
    :tx-conflicts-mode :enforce
    :schema standard-schema}))

(def foo      #:schema{:eav/eid 1    :foo 1})
(def bar      #:schema{:eav/eid 2    :bar 2})
(def tina     #:schema{:eav/eid -1   :name "Tina" :ssn 123 :id 777 :parking-space 1})
(def tina-age #:schema{:eav/eid "t1" :age 73 :ssn 123})
(def tina-kid1 #:schema {:eav/eid -1 :child -3})
(def tina-kid2 #:schema {:eav/eid -1 :child -7})
(def tina-boss #:schema {:eav/eid -1 :boss -6})
(def bob      #:schema{:eav/eid -3   :name "Bob" :ssn 345 :parking-space 1})
(def bob-foo  #:schema{:eav/eid 88   :ssn 345 :foo 7})
(def bob-age  #:schema{:eav/eid 88   :age 77})
(def arlan    #:schema{:eav/eid -4   :name "Arlan" :ssn 456 :id 888 :parking-space 2})
(def mal      #:schema{:eav/eid -5   :name "Mal" :ssn 123 :id 888 :parking-space 9})
(def may      #:schema{:eav/eid -6   :name "May" :ssn 234 :parking-space 10})
(def franz    #:schema{:eav/eid -7   :name "Franz" :ssn 345 :parking-space 11})

(defn store-upsert [store tx]
  (store/+eavs store (store/eav-seq (:attrs store) tx)))

(defn store-retract [store tx]
  (store/-eavs store (store/eav-seq (:attrs store) tx)))

(defn store-transact [store tx]
  (store/transact* store (store/tx-seq (:attrs store) tx)))

(deftest schema-tests
  (testing "no-enforcement violation"
    (store-upsert store-basic-schema [foo bar]))
  (testing "schema enforcement"
    (store-upsert store-enforce-schema [foo]))
  (testing "schema enforcement violation"
    (try
      (store-transact store-enforce-schema [foo bar])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :no-schema)
                    (store/eav-seq (:attrs store-enforce-schema) [bar]))))))
  (testing "simple value non-collision"
    (let [store (store-upsert store-enforce-schema [tina arlan])]
      (is true)))
  (testing "simple value collision"
    (try
      (store-transact store-enforce-schema [tina bob])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (-> e ex-data :value-av-e-set)
                    {[:schema/parking-space 1] #{1 2}})))))
  (testing "simple ident upsert"
    (let [store (store-transact store-enforce-schema [tina tina-age])]
      (is (= (store/dump-entity-maps store)
             [#:schema{:name "Tina", :ssn 123, :id 777, :parking-space 1, :age 73, :eav/eid 1}]))))
  (testing "non-colliding-tempids"
    (let [store (store-transact store-standard-enforce [#:schema{:eav/eid 0 :any 0}
                                                        #:schema{:eav/eid 1 :any 1}
                                                        #:schema{:eav/eid 2 :any 2}
                                                        #:schema{:eav/eid 3 :any 3}
                                                        #:schema{:eav/eid 5 :any 5}])
          store (store-transact store [#:schema{:eav/eid -1 :any 4}])]
      (is (= (set (store/dump-entity-maps store))
             (set [#:schema{:eav/eid 0 :any 0}
                   #:schema{:eav/eid 1 :any 1}
                   #:schema{:eav/eid 2 :any 2}
                   #:schema{:eav/eid 3 :any 3}
                   #:schema{:eav/eid 4 :any 4}
                   #:schema{:eav/eid 5 :any 5}])))))
  (testing "simple ident retract"
    (let [store (store-transact store-standard-enforce [#:schema{:eav/eid 1 :uniq-ident 4}])
          store (store-transact store [[:db/retract 1 :schema/uniq-ident 4]])]
      (is (= (store/dump-entity-maps store)
             []))))
  (testing "simple ident retract 2"
    (let [store (store-transact store-standard-enforce [#:schema{:eav/eid 1
                                                                 :uniq-ident 1
                                                                 :any 1}])
          store (store-transact store [[:db/retract 1 :schema/uniq-ident 1]])
          store (store-transact store [#:schema{:eav/eid -1 :uniq-ident 1}])]
      (is (= (store/dump-entity-maps store)
             [#:schema{:eav/eid 1 :any 1}
              #:schema{:eav/eid 2 :uniq-ident 1}]))))
  (testing "simple ident tx-conflicts (ignore)"
    (let [store (store-upsert store-enforce-schema [tina arlan mal])]
      (is true)))
  (testing "card/one tx collision (enforce)"
    (try
      (store-transact store-enforce-schema-overwrite
              [[:db/add 1 :schema/foo "SAME"]
               [:db/add 1 :schema/foo "DIFFERENT"]])
      (is false "FAILURE: exception expected")
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (->> e ex-data :tx-conflicts set)
                    (set [[[:db/add 1 :schema/foo] [["SAME"] ["DIFFERENT"]]]]))))))
  (testing "upsert ident tx-conflicts (enforce)"
    (try
      (store-transact store-enforce-schema-overwrite [tina arlan mal])
      (is false)
      (catch #?(:clj Exception :cljs js/Error) e
             (is (= (->> e ex-data :tx-conflicts set)
                    (set [[[:db/add 1 :schema/name] [["Tina"] ["Arlan"] ["Mal"]]]
                          [[:db/add 1 :schema/id] [[777] [888] [888]]]
                          [[:db/add 1 :schema/ssn] [[123] [456] [123]]]
                          [[:db/add 1 :schema/parking-space] [[1] [2] [9]]]]))))))
  (testing "simple ident committed collision"
    (let [store (store-transact store-enforce-schema [tina tina-age arlan])]
      (try
        (store-transact (store/state store) [mal])
        (is (= :result :exception))
        (catch #?(:clj Exception :cljs js/Error) e
          (is true)))))
  (testing "in transaction upsert"
    (let [store (store-transact store-enforce-schema [bob bob-foo bob-age])]
      (is (= (store/dump-entity-maps store)
             [{:schema/name "Bob",
               :schema/ssn 345,
               :schema/parking-space 1,
               :schema/foo 7,
               :schema/age 77,
               :eav/eid 88}]))))
  (testing "simple ref test"
    (let [store (store-upsert store-enforce-schema-overwrite
                       [tina tina-boss
                        arlan franz may])]
      (= (store/dump-entity-maps store)
         [#:schema{:name "Tina", :ssn 123, :id 777, :parking-space 1, :boss 3, :eav/eid 2}
          #:schema{:name "Arlan", :ssn 456, :id 888, :parking-space 2, :eav/eid 1}
          #:schema{:name "Franz", :ssn 345, :parking-space 11, :eav/eid 4}
          #:schema{:name "May", :ssn 234, :parking-space 10, :eav/eid 3}])))
  (testing "simple card/many test"
    (let [store (store-upsert store-standard-enforce [#:schema{:eav/eid 1 :any-many [1]}])
          store (store-upsert store [#:schema{:eav/eid 1 :any-many [2]}])]
      (is (= (store/dump-entity-maps store)
             [#:schema{:eav/eid 1 :any-many #{1 2}}])))))
  ;; TODO card/many ref test

;; TODO: Datomic resolves to DB:
;; [[1 :schema/name "Karel Novak"]
;;  [2 :schema/name "Jan Novak"]]
#_
[[[:db/add -100 :schema/name "Karel Novak"]]
 [[:db/retract -100 :schema/name "Jan Novak"]
  [:db/add -20 :schema/name "Jan Novak"]
  [:db/add -100 :schema/name "Karel Novak"]]]

;; TODO: Datomic resolves to DB:
;; [[1 :schema/name "Jane Doe"]]
#_
[[[:db/add -20 :schema/name "Jane Doe"]
  [:db/add -100 :schema/name "Jane Doe"]]]

;; NOTE Two retracts of temp-ids with same uniq/ident AV resolve to single newid

;; TODO: Datomic resolves to DB:
#_
[{:schema/name "Max Otto von Stierlitz"} {:schema/name "Jane Doe"}]
#_
[[[:db/add -100 :schema/name "Max Otto von Stierlitz"]]
 [[:db/add -100 :schema/name "Jane Doe"]
  [:db/retract -100 :schema/name "Max Otto von Stierlitz"]
  [:db/add -100 :schema/name "Jane Doe"]]]

;; TODO: Datomic resolves to DB:
#_
({:schema/name "Maksim"} {:schema/name "Jan Novak"})
#_
[[[:db/add -100 :schema/name "Maksim"]
  [:db/add -20 :schema/name "Jan Novak"]]
 [[:db/retract -10 :schema/name "Jan Novak"]
  [:db/add -10 :schema/name "Maksim"]]]


;; TODO Datomic throws exception:
;; ":db.error/datoms-conflict Two datoms in the same transaction conflict
;;  {:d1 [17592186045431 :schema/name \"Erika Mustermann\" 13194139534326 true],
;;   :d2 [17592186045431 :schema/name \"John Doe\" 13194139534326 true]}"
#_
[[[:db/add -100 :schema/name "Erika Mustermann"]]
 [[:db/add -100 :schema/name "Erika Mustermann"]
  [:db/add -20 :schema/name "John Doe"]
  [:db/retract -100 :schema/name "John Doe"]]]
;; XXX BUUUUUUUUUTTTTT ... this works and poduces (NOTE order difference):
#_
[{:schema/name "John Doe"} {:schema/name "Erika Mustermann"}]
#_
[[[:db/add -100 :schema/name "Erika Mustermann"]]
 [[:db/add -20 :schema/name "John Doe"]
  [:db/retract -100 :schema/name "John Doe"]
  [:db/add -100 :schema/name "Erika Mustermann"]]]


;; TODO Datomic throws exception:
#_
{:d1 [17592186045431 :schema/name "John Doe" 13194139534326 true],
 :d2 [17592186045431 :schema/name "Erika Mustermann" 13194139534326 true],
 :db/error :db.error/datoms-conflict}
#_
[[[:db/add -100 :schema/name "Erika Mustermann"]]
 [[:db/add -20 :schema/name "John Doe"]
  [:db/add -100 :schema/name "Erika Mustermann"]
  [:db/retract -100 :schema/name "John Doe"]]]


;; NOTE this works for all
#_
[[[:db/add -100 :schema/name "Erika Mustermann"]]
 [[:db/retract -100 :schema/name "John Doe"]
  [:db/add -20 :schema/name "John Doe"]
  [:db/add -100 :schema/name "Erika Mustermann"]]]

;; NOTE: clara ends up with James while datomic has Maksim and James
#_
[[[:db/add -100 :schema/name "Maksim"]]
 [[:db/add -100 :schema/name "James John Jones"]
  [:db/retract -10 :schema/name "Maksim"]
  [:db/add -10 :schema/name "James John Jones"]]]

;; NOTE: Other cases to test
;;({:res {:clara [({:schema/name "X"}) ({:schema/name "O"})],
;;        :datomic [({:schema/name "X"}) ({:schema/name "X"} {:schema/name "O"})],
;;        :store [({:schema/name "X"}) ({:schema/name "O"})]},
;;  :tx (([:db/add -1 :schema/name "X"])
;;       ([:db/retract -1 :schema/name "X"]
;;        [:db/add -2 :schema/name "O"]
;;        [:db/add -1 :schema/name "O"]))}
;;
;; {:res {:clara [({:schema/name "X"}) ({:schema/name "O"})],
;;        :datomic [({:schema/name "X"}) ({:schema/name "X"} {:schema/name "O"})],
;;        :store [({:schema/name "X"}) ({:schema/name "O"})]},
;;  :tx (([:db/add -1 :schema/name "X"])
;;       ([:db/add -2 :schema/name "O"]
;;        [:db/retract -1 :schema/name "X"]
;;        [:db/add -1 :schema/name "O"]))}
;; {:res {:clara [({:schema/name "X"}) ({:schema/name "O"})],
;;        :datomic [({:schema/name "X"}) ({:schema/name "X"} {:schema/name "O"})],
;;        :store [({:schema/name "X"}) ({:schema/name "O"})]},
;;  :tx (([:db/add -1 :schema/name "X"])
;;       ([:db/add -2 :schema/name "O"]
;;        [:db/add -1 :schema/name "O"]
;;        [:db/retract -1 :schema/name "X"]))}
;;
;; {:res {:clara [({:schema/name "X"}) ({:schema/name "O"})],
;;        :datomic [({:schema/name "X"}) ({:schema/name "X"} {:schema/name "O"})],
;;        :store [({:schema/name "X"}) ({:schema/name "O"})]},
;;  :tx (([:db/add -1 :schema/name "X"])
;;       ([:db/retract -2 :schema/name "X"]
;;        [:db/add -1 :schema/name "O"]
;;        [:db/add -2 :schema/name "O"]))}
;; {:res {:clara [({:schema/name "X"}) ({:schema/name "O"})],
;;        :datomic [({:schema/name "X"}) ({:schema/name "X"} {:schema/name "O"})],
;;        :store [({:schema/name "X"}) ({:schema/name "O"})]},
;;  :tx (([:db/add -1 :schema/name "X"])
;;       ([:db/add -1 :schema/name "O"]
;;        [:db/retract -2 :schema/name "X"]
;;        [:db/add -2 :schema/name "O"]))}
;; {:res {:clara [({:schema/name "X"}) ({:schema/name "O"})],
;;        :datomic [({:schema/name "X"}) ({:schema/name "X"} {:schema/name "O"})],
;;        :store [({:schema/name "X"}) ({:schema/name "O"})]},
;;  :tx (([:db/add -1 :schema/name "X"])
;;       ([:db/add -1 :schema/name "O"]
;;        [:db/add -2 :schema/name "O"]
;;        [:db/retract -2 :schema/name "X"]))})
;;
;;----
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/retract -2 :schema/name "X"]
;;       [:db/add -1 :schema/name "O"]
;;       [:db/add -2 :schema/name "O"]))
;;
;;-> ({:schema/name "X"}, {:schema/name "O"})
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/add -1 :schema/name "O"]
;;       [:db/retract -2 :schema/name "X"]
;;       [:db/add -2 :schema/name "O"]))
;;
;;-> ({:schema/name "X"}, {:schema/name "O"})
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/add -1 :schema/name "O"]
;;       [:db/add -2 :schema/name "O"]
;;       [:db/retract -2 :schema/name "X"]))
;;->
;;Execution error (Exceptions$IllegalArgumentExceptionInfo) at datomic.error/argd (error.clj:77).
;;:db.error/datoms-conflict Two datoms in the same transaction conflict
;;{:d1 [17592186045463 :schema/name "O" 13194139534358 true], :d2 [17592186045461 :schema/name "O" 13194139534358 true]}
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/retract -2 :schema/name "X"]
;;       [:db/add -2 :schema/name "O"]
;;       [:db/add -1 :schema/name "O"]))
;;
;;-> ({:schema/name "O"})
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/add -2 :schema/name "O"]
;;       [:db/retract -2 :schema/name "X"]
;;       [:db/add -1 :schema/name "O"]))
;;->
;;Execution error (Exceptions$IllegalArgumentExceptionInfo) at datomic.error/argd (error.clj:77).
;;:db.error/datoms-conflict Two datoms in the same transaction conflict
;;{:d1 [17592186045470 :schema/name "O" 13194139534367 true], :d2 [17592186045472 :schema/name "O" 13194139534367 true]}
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/add -2 :schema/name "O"]
;;       [:db/add -1 :schema/name "O"]
;;       [:db/retract -2 :schema/name "X"]))
;;->
;;Execution error (Exceptions$IllegalArgumentExceptionInfo) at datomic.error/argd (error.clj:77).
;;:db.error/datoms-conflict Two datoms in the same transaction conflict
;;{:d1 [17592186045474 :schema/name "O" 13194139534371 true], :d2 [17592186045476 :schema/name "O" 13194139534371 true]}
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/retract -2 :schema/name "X"]
;;       [:db/add -1 :schema/name "O"]))
;;
;;-> ({:schema/name "O"})
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/retract -2 :schema/name "X"]
;;       [:db/add -2 :schema/name "O"]))
;;
;;-> ({:schema/name "O"})
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/add -1 :schema/name "O"]
;;       [:db/retract -2 :schema/name "X"]))
;;
;;-> ({:schema/name "O"})
;;
;;:txs (([:db/add -1 :schema/name "X"])
;;      ([:db/add -2 :schema/name "O"]
;;       [:db/retract -2 :schema/name "X"]))
;;
;;-> ({:schema/name "O"})
;;
;;----
;;
;;   [[[:db/retract -2 :schema/name "X"]
;;     [:db/add -1 :schema/name "O"]
;;     [:db/add -2 :schema/name "O"]]
;;    [[:db/add -1 :schema/name "O"]
;;     [:db/retract -2 :schema/name "X"]
;;     [:db/add -2 :schema/name "O"]]
;;    [[:db/add -1 :schema/name "O"]
;;     [:db/add -2 :schema/name "O"]
;;     [:db/retract -2 :schema/name "X"]]
;;    [[:db/retract -2 :schema/name "X"]
;;     [:db/add -2 :schema/name "O"]
;;     [:db/add -1 :schema/name "O"]]
;;    [[:db/add -2 :schema/name "O"]
;;     [:db/retract -2 :schema/name "X"]
;;     [:db/add -1 :schema/name "O"]]
;;    [[:db/add -2 :schema/name "O"]
;;     [:db/add -1 :schema/name "O"]
;;     [:db/retract -2 :schema/name "X"]]
;;    [[:db/retract -2 :schema/name "X"]
;;     [:db/add -1 :schema/name "O"]]
;;    [[:db/retract -2 :schema/name "X"]
;;     [:db/add -2 :schema/name "O"]]
;;    [[:db/add -1 :schema/name "O"]
;;     [:db/retract -2 :schema/name "X"]]
;;    [[:db/add -2 :schema/name "O"]
;;     [:db/retract -2 :schema/name "X"]]]


;; clara-eav.main=> (-> res :shrunk :smallest first)
;; [[[:db/add -100 :schema/name "Karel Novak"]] [[:db/add -100 :schema/name "Maksim"]] [[:db/add -2 :schema/name "Maksim"] [:db/retract -2 :schema/name "Karel Novak"]]]
;; clara-eav.main=> (-> res2 :shrunk :smallest first)
;; [[[:db/add -20 :schema/name "John Doe"] [:db/retract -2 :schema/name "John Doe"] [:db/retract -20 :schema/name "Maksim"] [:db/add -2 :schema/name "Maksim"]]]
;; clara-eav.main=> (-> res3 :shrunk :smallest first)
;; [[[:db/add -100 :schema/name "Jane Doe"]] [[:db/add -100 :schema/name "Jack Ryan"] [:db/add -20 :schema/name "Jack Ryan"] [:db/retract -100 :schema/name "Jane Doe"]] [[:db/add -100 :schema/name "Anamika"] [:db/add -20 :schema/name "Anamika"] [:db/retract -20 :schema/name "Jane Doe"]]]
;; clara-eav.main=> (-> res4 :shrunk :smallest first)
;; [[[:db/retract -10 :schema/name "Erika Mustermann"] [:db/add -20 :schema/name "Erika Mustermann"] [:db/add -100 :schema/name "Jack Ryan"] [:db/add -10 :schema/name "Jack Ryan"]]]

;; clara-eav.main=> (-> res :shrunk :smallest first)
;; [[[:db/add -100 :schema/name "John Doe"]] [[:db/retract -100 :schema/name "Anamika"] [:db/add -10 :schema/name "John Doe"] [:db/retract -10 :schema/name "Anamika"]]]


;; clara-eav.main=> (-> res :shrunk :smallest first)
;; [[[:db/add -100 :schema/name "Jack Ryan"] [:db/add -20 :schema/name "Max Otto von Stierlitz"]] [[:db/retract -10 :schema/name "Jack Ryan"] [:db/add -10 :schema/name "Max Otto von Stierlitz"]]]

;; ;;clara-eav.main=> (-> res :shrunk :smallest first)
;; [[[:db/add -100 :schema/name "Jack Ryan"]] [[:db/retract -20 :schema/name "Maksim"] [:db/add -100 :schema/name "Maksim"] [:db/retract -100 :schema/name "Jack Ryan"]]]
;;
;; ;;clara-eav.main=> (-> res :shrunk :smallest first)
;; [[[:db/add 16000 :schema/name "Karel Novak"] [:db/add 18000 :schema/name "Jane Doe"]] [[:db/add -100 :schema/name "Jane Doe"] [:db/add 18000 :schema/name "Karel Novak"] [:db/add 16000 :schema/name "Maksim"]]]
