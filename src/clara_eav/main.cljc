(ns clara-eav.main
  "Property-test Clara EAV"
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine] ;; implicitly needed
            [clara.rules.accumulators :as accumulators]
            [clara-eav.eav :as eav]
            [clara-eav.session :as session]
            [clara-eav.store :as store]

            [clara-eav.rules :as eav.rules]
            [clara-eav.util :as util]

            [expound.alpha :as expound] ;; needed by test-helper
            [clara-eav.test-helper :as th]
            [datascript.core :as ds]
            [datomic.api :as d]
            [differ.core :as differ]
            [zprint.core :refer [zprint]]
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
;; Data for generation
;;
;; Datascript exception on ID 0
(def TEMP-IDS [-100 -20 -10 -2 -1])
;; Datascript needs these spread out or else there are collisions
(def REAL-IDS [11000 12000 13000 14000 15000 16000 17000 18000 19000])
(def ALL-IDS (into TEMP-IDS REAL-IDS))

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
    [[:schema/name  :cardinality/one  string? :unique :unique/identity]
     [:schema/age   :cardinality/one  int?]
     [:schema/email :cardinality/many string? :unique :unique/identity]
     [:schema/aka   :cardinality/many string?]]))

(def d-schema
  (gen-datomic-schema
   [[:schema/name  :db.cardinality/one  :db.type/string :db/unique :db.unique/identity]
    [:schema/age   :db.cardinality/one  :db.type/long]
    [:schema/email   :db.cardinality/many  :db.type/string :db/unique :db.unique/identity]
    [:schema/idlink   :db.cardinality/one  :db.type/ref :db/unique :db.unique/identity]
    [:schema/midlink   :db.cardinality/many  :db.type/ref :db/unique :db.unique/identity]
    [:schema/link   :db.cardinality/one  :db.type/ref]
    [:schema/mlink   :db.cardinality/many  :db.type/ref]
    [:schema/aka   :db.cardinality/many :db.type/string]]))

(def ds-schema
  {:schema/name {:db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   :schema/age  {:db/cardinality :db.cardinality/one}
   :schema/aka  {:db/cardinality :db.cardinality/many}})

;; Initialize datastores/DBs

(eav.rules/defsession* c-session-like-datascript
  (assoc store/datascript-default-options :schema clara-schema)
  'clara-eav.main)

(eav.rules/defsession* c-session-like-datomic
  (assoc store/datomic-default-options :schema clara-schema)
  'clara-eav.main)

(def d-uri "datomic:mem://gentest")

(def d-conn
  (do
    (d/delete-database d-uri)
    (d/create-database d-uri)
    (let [d-conn (d/connect d-uri)]
      @(d/transact d-conn d-schema)
      d-conn)))

(def ID->DATOMIC-ID
  (let [tx-fn #(vector %1 %2 :db/doc "TEMPORARY")
        tx (d/transact d-conn (map #(tx-fn :db/add %)
                                   (repeatedly (count REAL-IDS)
                                               #(d/tempid :db.part/user))))
        eids (-> @tx :tempids vals)]
    (d/transact d-conn (map #(tx-fn :db/retract %) eids))
    (zipmap (concat TEMP-IDS REAL-IDS)
            (map #(datomic.db.DbId. :db.part/user %) (concat TEMP-IDS eids)))))

;;;;

(defn- eav-map->entity [attrs {:keys [e a v]}]
  ;; TODO THROW with helpful message if store/*store* is not bound
  ;; TODO pass in attrs?
  (let [attrs (if (empty? attrs) (-> @store/*store* :attrs) attrs)]
    {:eav/eid [e] a (if (= :cardinality/many (-> a attrs :cardinality))
                     #{v}
                     [v])}))

(defn- entity-grouping [attrs args]
  ;;(prn :args args) ;; TODO: why does this appear on ns load
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
  (prn :I-appear-on-reload-but-why)
  (accumulators/grouping-by :e (partial #'entity-grouping attrs)))

(eav.rules/defquery entities []
  [?entities <- (schementity {}) :from [[?e]]])

(defn clara-entities [session]
  (binding [store/*store* (atom (:store session))]
    (doall (mapcat :?entities (rules/query session entities)))))

(defn datascript-entities [db]
  (let [xs (ds/q '[:find [(pull ?e [*]) ...] :where [?e]] db)]
    xs))

(defn datomic-entities [db]
  (let [eavs (for [a (map :db/ident d-schema)
                   [e v] (d/q '[:find ?e ?v
                                :in $ ?a
                                :where [?e ?a ?v]] db a)]
               {:e e :a a :v v})]
    (prn :datomic-entities eavs)
    (entity-grouping (store/schema->attrs clara-schema)
                   (group-by :e eavs))))

(defn- card-many->set
  [attrs entity]
  (into {} (for [[k v] entity]
             (if (= :cardinality/many (-> k attrs :cardinality))
               [k (set v)]
               [k v]))))

(defn normalize-entities
  [entities]
  (for [entity entities]
    (card-many->set (store/schema->attrs clara-schema) entity)))
(defn normalize-entity-maps
  [entity-maps]
  (->> entity-maps th/strip-ids normalize-entities (sort-by hash)))

(defn run-txs [txs oracles]
  (assert (not (and (:datascript oracles) (:datomic oracles)))
          "Simultaneous datascript and datomic oracles not yet supported")
  (loop [c-session (if (:datomic oracles)
                     c-session-like-datomic
                     c-session-like-datascript)
         ds-db (when (:datascript oracles) (ds/empty-db ds-schema))
         d-db (when (:datomic oracles) (d/db d-conn))
         [tx & txs] txs]
    ;(prn :tx tx)
    (let [c-session (try (-> (eav.rules/transact c-session tx)
                             (rules/fire-rules))
                         (catch Exception e {:exception e :tx tx}))
          ds-db (when (:datascript oracles)
                  (try (ds/db-with ds-db tx)
                       (catch Exception e {:exception e :tx tx})))
          d-tx (map (fn [[A e a v]] [A (get ID->DATOMIC-ID e) a v]) tx)
          ;;_ (prn :d-tx d-tx)
          d-db (when (:datomic oracles)
                 (try (:db-after (d/with d-db d-tx))
                      (catch Exception e {:exception e :tx tx})))]
      (if (and (seq tx) (not (some :exception [c-session ds-db d-db])))
       (recur c-session ds-db d-db txs)
       (merge
        (if (:exception c-session)
         {:clara c-session
          :store c-session}
         {:clara (->> c-session clara-entities th/strip-ids (sort-by hash))
          :store (->> c-session :store store/dump-entity-maps normalize-entity-maps)})
        (when (:datascript oracles)
         (if (:exception ds-db)
          {:datascript ds-db}
          {:datascript (->> ds-db datascript-entities normalize-entity-maps)}))
        (when (:datomic oracles)
         (if (:exception d-db)
          (do (prn :d-exception) {:datomic d-db})
          {:datomic (->> d-db datomic-entities normalize-entity-maps)})))))))


(defn zzprint [prefix & args]
  (let [s (with-out-str (apply zprint args))]
    (print (clojure.string/replace s #"(?m)^" prefix))))

(defn exception-elide [result]
  (into {} (for [[k v] result]
             [k (if (and (map? v) (contains? v :exception))
                  (assoc v :exception :ELIDED)
                  v)])))

(defn check-txs [txs oracles]
  (println "----- using txs:")
  (zzprint "    " txs)
  (let [full-res (run-txs txs oracles)
        res (exception-elide full-res)]
    (println "----- res:")
    (zzprint "    " res)
    (if (apply = (vals res))
      (do
       (println "SUCCESS (identical)")
       res)
      (do
        (println "FAILURE")
        (println "clara/store diff:")
        (let [[added removed] (differ/diff (:clara res) (:store res))]
          (zzprint "    " {:store added
                           :clara removed}))
        (when (:datascript oracles)
          (println "clara/datascript diff:")
          (let [[added removed] (differ/diff (:clara res) (:datascript res))]
            (zzprint "    " {:datascript added
                             :clara removed})))
        (when (:datomic oracles)
          (println "clara/datomic diff:")
          (let [[added removed] (differ/diff (:clara res) (:datomic res))]
            (zzprint "    " {:datomic added
                             :clara removed})))
        (println "-----")
        full-res))))

(defn entity->tx
  [action entity]
  (map #(into [action] ((juxt :e :a :v) %))
        (store/entity->eav-seq
         (store/schema->attrs clara-schema)
         entity)))


(defn sanity-check-1 [action oracle]
  (let [items [#:schema{:eav/eid -1
                        :name  "Maksim"
                        :age   45}]
        tx (mapcat (partial entity->tx action) items)
        _ (prn :tx tx)
        res (check-txs [tx] #{oracle})]
    (println "full res:")
    (zzprint "    " res)
    (apply = (vals (exception-elide res)))))

(defn sanity-check-2 [action oracle]
  (let [items [#:schema{:eav/eid -1
                        :name  "Maksim"
                        :age   45
                        :aka   ["Max Otto von Stierlitz", "Jack Ryan"]}
               #:schema{:eav/eid -2
                        :name  "John Doe"
                        :aka   ["James John Jones"]}]
        tx (mapcat (partial entity->tx action) items)
        res (check-txs [tx] #{oracle})]
    (println "full res:")
    (zzprint "    " res)
    (apply = (vals (exception-elide res)))))

(defn sanity-check-3 [oracle]
  (let [tx [[:db/add -20 :schema/age 1]
            [:db/add -20 :schema/age 2]]
        res (check-txs [tx] #{oracle})]
    (println "full res:")
    (zzprint "    " res)
    (apply = (vals (exception-elide res)))))

(defn id-remap-fn
  [tmps-in-retract? action id]
  (if tmps-in-retract?
    (nth (cycle ALL-IDS) id)
    (condp = action
      :db/add (nth (cycle ALL-IDS) id)
      :db/retract (nth (cycle REAL-IDS) id))))

(defn create-gen-txs
  [id-fn]
  (tgen/not-empty
   (tgen/vector ; Transactions
     (tgen/fmap #(vec (apply concat %)) ; Transaction
      (tgen/vector
       (tgen/fmap ; Action group
        (fn [[action id name? name age? age aka? aka]]
          (entity->tx
           action
           (merge
            {:eav/eid (id-fn action id)}
            (when name?
              {:schema/name (nth (cycle NAMES) name)})
            (when age?
              {:schema/age (nth (cycle AGES) age)})
            (when aka?
              {:schema/aka (set (map #(nth (cycle NAMES) %) aka))}))))
        (tgen/tuple
         (tgen/frequency [[40 (tgen/return :db/add)]
                          [60 (tgen/return :db/retract)]]) ;; add/retract
         tgen/nat ;; id
         tgen/boolean tgen/nat ;; name
         tgen/boolean tgen/nat ;; age
         tgen/boolean (tgen/vector tgen/nat 0 (count NAMES))))))))) ;; aka

(def txs-prop-datascript
 (tprop/for-all
  [txs (create-gen-txs (partial id-remap-fn false))]
  (let [res (run-txs txs #{:datascript})]
    (apply = (vals (exception-elide res))))))

(def txs-prop-datomic
  (tprop/for-all
   [txs (create-gen-txs (partial id-remap-fn true))]
   (let [res (run-txs txs #{:datomic})]
     (apply = (vals (exception-elide res))))))

(defn do-gen-datascript [size]
  (quick-check size txs-prop-datascript))

(defn do-gen-datomic [size]
 (quick-check size txs-prop-datomic))

;; (do (def res (do-gen-datascript 30)) (zprint res))
;; (do (def res (do-gen-datomic 30)) (zprint res))
;; (zprint (check-txs (-> res :shrunk :smallest (nth 0)) #{:datomic}))

#_(defn -main [& argv]
    (do-gen-dataomici 20))
