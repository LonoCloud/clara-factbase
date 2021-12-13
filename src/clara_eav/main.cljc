(ns clara-eav.main
  "Property-test Clara EAV"
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators :as accumulators]
            [clara-eav.eav :as eav]
            [clara-eav.session :as session]
            [clara-eav.store :as store]
            [clara-eav.rules :as eav.rules]
            [clara-eav.util :as util]

            [expound.alpha :as expound] ;; needed by test-helper
            [clara-eav.test-helper :as th]
            [datascript.core :as d]
            [differ.core :as differ]
            [clojure.pprint :refer [pprint]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as tgen]
            [clojure.test.check.properties :as tprop]
    #?@(:clj [[clojure.spec.alpha :as s]
              [clojure.spec.gen.alpha :as sgen]
              [clojure.spec.test.alpha :as stest]
              [clara-eav.dsl :as dsl]]
        :cljs [[cljs.spec.alpha :as s]
               [cljs.spec.gen.alpha :as sgen]
               [clojure.spec.test.alpha :as stest]]))
  #?(:cljs (:require-macros
             [clara.rules :as rules])))

;;
;; From test/clara_eav/store_test.cljc
;;
(defn gen-schema [pre-schema]
  (map (fn [[id cd vt & extra]]
         (merge {:ident id :cardinality cd :valueType vt}
                (apply hash-map extra)))
       pre-schema))

(defn gen-datomic-schema [pre-schema]
  (map (fn [[id cd vt & extra]]
         (merge #:db{:ident id :cardinality cd :valueType vt}
                (apply hash-map extra)))
       pre-schema))

(def clara-schema
  (gen-schema
    [[:schema/name  :cardinality/one  string?]
     [:schema/age   :cardinality/one  int?]
     [:schema/aka   :cardinality/many string?]]))

(def datomic-schema
  (gen-datomic-schema
   [[:schema/name  :db.cardinality/one  :db.type/string]
    [:schema/age   :db.cardinality/one  :db.type/long]
    [:schema/aka   :db.cardinality/many :db.type/string]]))

(def ds-schema
  {:schema/name {:db/cardinality :db.cardinality/one}
   :schema/age  {:db/cardinality :db.cardinality/one}
   :schema/aka  {:db/cardinality :db.cardinality/many}})

;;(defn upsert [store tx]
;;  (store/+eavs store (store/eav-seq (:attrs store) tx)))
;;
;;(defn retract [store tx]
;;  (store/-eavs store (store/eav-seq (:attrs store) tx)))
;;
;;;; -------------
;;
;;;; From: https://stackoverflow.com/a/43722784/471795
;;(defn map->nsmap
;;  "Namespace a map"
;;  [m n]
;;  (reduce-kv (fn [acc k v]
;;               (let [new-kw (if (and (keyword? k)
;;                                     (not (qualified-keyword? k)))
;;                              (keyword (str n) (name k))
;;                              k) ]
;;                 (assoc acc new-kw v)))
;;             {} m))
;;
;;;; -------------
;;
;;(defn clara-eav->tuple
;;  [eav]
;;  ((juxt :e #(keyword (name (:a %))) :v) eav))
;;
;;(defn clara-store->tuples
;;  [store]
;;  (map clara-eav->tuple
;;       (mapcat #(#'store/entity->eav-seq (:attrs store) %)
;;               (store/dump-entity-maps store))))
;;
;;(defn ds-db->tuples
;;  [db]
;;  (map #(vec (take 3 %)) (seq db)))
;;
;;(defn apply-both [c-db d-db txes]
;;  (let [#_ #_ c-db (reduce
;;               (fn [db tx]
;;                 (if-let [rid (get tx :db.fn/retractEntity)]
;;                   (let [ntx (assoc (dissoc tx :db.fn/retractEntity)
;;                                    :db/id rid)]
;;                     (prn :ntx ntx :foo (map->nsmap ntx "schema"))
;;                     (retract db (map->nsmap ntx "schema")))
;;                   (upsert db (map->nsmap tx "schema"))))
;;               c-db
;;               txes)
;;        d-db (reduce
;;               (fn [db tx]
;;                 (d/db-with db [tx]))
;;               d-db
;;               txes)]
;;    [c-db d-db]))
;;
;;(defn test1 []
;;  (let [c-db (store/init {:schema-mode :enforce
;;                          :schema clara-schema})
;;        d-db (d/empty-db ds-schema)
;;        [c-db d-db]
;;        ,,, (apply-both c-db d-db
;;                        [{:db/id -1
;;                          :name  "Maksim"
;;                          :age   45
;;                          :aka   ["Max Otto von Stierlitz", "Jack Ryan"]}
;;                         {:db/id -2
;;                          :name  "John Doe"
;;                          :aka   ["James John Jones"]}
;;                         [:db.fn/retractEntity 2]
;;                         #_{:db.fnz/retractEntity 2
;;                            }])]
;;    [(set (clara-store->tuples c-db))
;;     (set (ds-db->tuples d-db))]))

(defn- eav-map->entity [attrs {:keys [e a v]}]
  ;; TODO THROW with helpful message if store/*store* is not bound
  ;; TODO pass in attrs?
  (let [attrs (-> @store/*store* :attrs)]
    {:db/id [e] a (if (= :cardinality/many (-> a attrs :cardinality))
                     #{v}
                     [v])}))

(defn- entity-grouping [attrs args]
  (for [[e V] args
        :let [V' (apply merge-with into (map (partial eav-map->entity attrs) V))
              ms (doall ;; <- force execution to ensure dynamic var
                  (map (fn [[k v]]
                         (if (vector? v)
                           (if (apply = v)
                             [k  (first v)]
                             (throw (ex-info "Repeated :cardinality/one values"
                                             {:e e :a k :v v})))
                           [k v]))
                       V'))]]
    (into {} ms)))

(defn schementity [attrs]
  (accumulators/grouping-by :e (partial #'entity-grouping attrs)))

(eav.rules/defquery entities []
  [?entities <- (schementity {}) :from [[?e]]])

(eav.rules/defsession* c-session
  {:schema-mode :enforce
   :schema clara-schema}
  'clara-eav.main)

(defn clara-entities [session]
  (binding [store/*store* (atom (:store session))]
    (doall (mapcat :?entities (rules/query session entities)))))

(defn datascript-entities [db]
  (d/q '[:find [(pull ?e [*]) ...] :where [?e]] db))

(defn- card-many->set
  [attrs entity]
  (into {} (for [[k v] entity]
             (if (= :cardinality/many (-> k attrs :cardinality))
               [k (set v)]
               [k v]))))

(defn normalize-entities
  [schema entities]
  (for [entity entities]
    (card-many->set (store/schema->attrs schema) entity)))

(defn compare-states
  [clara-schema clara-session dsdb]
  (let [norm (partial normalize-entities clara-schema)]
    {:clara      (set (-> (clara-entities clara-session) th/strip-ids))
     :store      (set (-> (:store clara-session) store/dump-entity-maps th/strip-ids norm))
     :datascript (set (-> dsdb datascript-entities th/strip-ids norm))}))

(defn run-tx [mode tx]
  #_(clojure.pprint/pprint tx)
  (let [norm (partial normalize-entities clara-schema)
        c-session (try (-> (eav.rules/upsert c-session tx)
                           (rules/fire-rules))
                       (catch Exception e
                         (println e)
                         :exception))
        ds-db (try (-> (d/empty-db ds-schema)
                       (d/db-with (map (fn [{:keys [e a v]}] [:db/add e a v]) tx)))
                   (catch Exception e
                     (println e)
                     :exception))]
    #_(compare-states clara-schema c-session ds-db)
    (merge
     {:clara :exception :store :exception :datascript :exception}
     (when (not= :exception c-session)
       {:clara      (set (-> (clara-entities c-session) th/strip-ids))
        :store      (set (-> (:store c-session) store/dump-entity-maps th/strip-ids norm))})
     (when (not= :exception ds-db)
       {:datascript (set (-> ds-db datascript-entities th/strip-ids norm))}))))

(defn test2 []
  (run-tx [#:schema{:db/id -1
                    :name  "Maksim"
                    :age   45
                    :aka   ["Max Otto von Stierlitz", "Jack Ryan"]}
           #:schema{:db/id -2
                    :name  "John Doe"
                    :aka   ["James John Jones"]}]))

;; Datascript exception on ID 0
(def IDS
  [-100 -20 -10 -2 -1
   11000 12000 13000 14000 15000 16000 17000 18000 19000 ])

(def AGES
  [1 2 3 4 5 6 7 8 9 10 11 20 50 100 101])

(def NAMES
  ["Maksim"
   "Max Otto von Stierlitz"
   "Jack Ryan"
   "John Doe"
   "Jane Doe"
   "James John Jones"
   "Jan Novak"
   "Karel Novak"
   "Joe Farnarkle"
   "Erika Mustermann"
   "Anamika"
   "Mario Rossi"])

(s/def ::tx-stream (s/coll-of ::tx-event))
(s/def ::tx-event (s/cat :mode ::tx-mode :tx ::tx))
(s/def ::tx-mode #{:insert :retract})
(s/def ::tx (s/coll-of ::tx-statement))
(s/def :schema/name #{"Maksim"
                      "Max Otto von Stierlitz"
                      "Jack Ryan"
                      "John Doe"
                      "Jane Doe"
                      "James John Jones"
                      "Jan Novak"
                      "Karel Novak"
                      "Joe Farnarkle"
                      "Erika Mustermann"
                      "Anamika"
                      "Mario Rossi"})
(s/def :schema/age #{1 2 3 4 5 6 7 8 9 10 11 20 50 100 101})
(s/def :schema/aka (s/coll-of :schema/name :kind set?))
(s/def ::tx-statement (s/keys :req [:db/id]
                              :opt [:schema/name :schema/age :schema/aka]))

#_(def tx-gen (s/gen ::tx-stream {:db/id #(s/gen #{-100 -20 -10 -2 -1
                                                 11000 12000 13000 14000 15000 16000 17000 18000 19000})}))

(def tx-gen (s/gen ::tx-stream {::eav/e #(s/gen (set IDS))}))

(defn create-gen
  []
  (tgen/vector
   (tgen/tuple
    (tgen/frequency [[40 (tgen/return :insert)]
                     [60 (tgen/return :retract)]])
    (tgen/fmap
     #(store/eav-seq clara-schema %)
     (tgen/vector
      (tgen/fmap
       (fn [[id name? name age? age aka? aka]]
         (merge
          {:db/id (nth (cycle IDS) id)}
          (when name?
            {:schema/name (nth (cycle NAMES) name)})
          (when age?
            {:schema/age (nth (cycle AGES) age)})
          (when aka?
            {:schema/aka (set (map #(nth (cycle NAMES) %) aka))})))
       (tgen/tuple
        tgen/nat
        tgen/boolean tgen/nat
        tgen/boolean tgen/nat
        tgen/boolean (tgen/vector tgen/nat 0 (count NAMES)))))))))

(defn test3 []
  (let [gen (create-gen)
        samp (tgen/sample gen 11)
        _ (pprint samp)
        res (run-tx :assert samp)]
    (if (apply = (vals res))
      (println "SUCCESS identical")
      (do
        (println "FAILURE")
        (println "clara/store diff:")
        (let [[added removed] (differ/diff (:clara res) (:store res))]
          (pprint {:store added
                   :clara removed}))
        (println "clara/datascript diff:")
        (let [[added removed] (differ/diff (:clara res) (:datascript res))]
          (pprint {:datascript added
                   :clara removed}))
        (println "-----------------------")
        res))))

(def bad-001
  [{:db/id 1001}
   {:db/id -10}
   {:db/id -9, :schema/name "Jane Doe"}
   {:db/id 1003, :schema/aka #{"Jack Ryan"}}])

(def bad-002
  [[-100, :schema/name, "Maksim"] [-100, :schema/age, 1] [-100, :schema/aka, "Maksim"] [-20, :schema/aka, "Max Otto von Stierlitz"] [-20, :schema/aka, "Maksim"] [-20, :schema/aka, "Jack Ryan"] [-20, :schema/aka, "Max Otto von Stierlitz"] [-100, :schema/age, 5] [-100, :schema/aka, "Jack Ryan"] [-100, :schema/aka, "John Doe"] [-100, :schema/aka, "Max Otto von Stierlitz"] [-100, :schema/aka, "Jane Doe"] [-100, :schema/aka, "Maksim"] [-20, :schema/age, 4] [-2, :schema/name, "Jane Doe"] [1003, :schema/age, 7] [1003, :schema/aka, "Maksim"] [-1, :schema/name, "Joe Farnarkle"] [-1, :schema/age, 3] [-2, :schema/aka, "James John Jones"] [-2, :schema/aka, "Joe Farnarkle"] [-2, :schema/aka, "Erika Mustermann"] [1004, :schema/name, "Karel Novak"] [1004, :schema/age, 3] [1004, :schema/aka, "James John Jones"] [1004, :schema/aka, "Joe Farnarkle"] [1004, :schema/aka, "John Doe"] [1004, :schema/aka, "Jan Novak"] [1004, :schema/aka, "Max Otto von Stierlitz"] [1004, :schema/aka, "Erika Mustermann"] [1004, :schema/aka, "Jane Doe"]])

(defn test-case [mode samp]
  (let [samp (store/eav-seq samp)
        _ (pprint samp)
        res (run-tx mode samp)]
    (if (apply = (vals res))
      (println "SUCCESS identical")
      (do
        (println "FAILURE")
        (println "clara/store diff:")
        (let [[added removed] (differ/diff (:clara res) (:store res))]
          (pprint {:store added
                   :clara removed}))
        (println "clara/datascript diff:")
        (let [[added removed] (differ/diff (:clara res) (:datascript res))]
          (pprint {:datascript added
                   :clara removed}))
        (println "-----------------------")
        res))))

(defn test4 [num-tests & qc-opts]
  (let [prop (tprop/for-all
              [[mode samp] (s/gen ::tx-stream) #_(create-gen)]
              (let [res (run-tx mode samp)]
                #_#_
                (prn :samp samp)
                (prn :res-vals (vals res))
                (and (not= :exception (-> res :clara))
                     (apply = (vals res)))))]
    (apply quick-check num-tests prop qc-opts)))

(defn -main [& argv]
  (pprint (test2))
  (apply = (vals (test2))))
