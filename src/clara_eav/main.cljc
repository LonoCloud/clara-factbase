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

(def d-schema
  (gen-datomic-schema
   [[:schema/name  :db.cardinality/one  :db.type/string]
    [:schema/age   :db.cardinality/one  :db.type/long]
    [:schema/aka   :db.cardinality/many :db.type/string]]))

(def ds-schema
  {:schema/name {:db/cardinality :db.cardinality/one}
   :schema/age  {:db/cardinality :db.cardinality/one}
   :schema/aka  {:db/cardinality :db.cardinality/many}})

(eav.rules/defsession* c-session
  {:schema-mode :enforce
   :tx-overwrite-mode :enforce
   :schema clara-schema}
  'clara-eav.main)

(def d-uri "datomic:mem://gentest")

(defn- eav-map->entity [attrs {:keys [e a v]}]
  ;; TODO THROW with helpful message if store/*store* is not bound
  ;; TODO pass in attrs?
  (let [attrs (if (empty? attrs) (-> @store/*store* :attrs) attrs)]
    {:eav/eid [e] a (if (= :cardinality/many (-> a attrs :cardinality))
                     #{v}
                     [v])}))

(defn- entity-grouping [attrs args]
  (prn :args args)
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

(defn clara-entities [session]
  (binding [store/*store* (atom (:store session))]
    (doall (mapcat :?entities (rules/query session entities)))))

(defn datascript-entities [db]
  (let [xs (ds/q '[:find [(pull ?e [*]) ...] :where [?e]] db)]
    (prn :ds-xs xs)
    xs))

(defn datomic-entities [db]
  (let [eavs (for [a (keys ds-schema)
                   [e v] (d/q '[:find ?e ?v
                                :in $ ?a
                                :where [?e ?a ?v]] db a)]
               {:e e :a a :v v})]
    (entity-grouping (store/schema->attrs clara-schema)
                   (group-by :e eavs)))

  #_(d/datoms db {:index :eavt})

  #_(let [xs (for [{:keys [e a v]} (d/seek-datoms db :eavt)]
               [e a v])]
      (prn :count-xs (count xs))
      (prn :xs xs)
      []))

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

(def d-conn
  (do
    (d/delete-database d-uri)
    (d/create-database d-uri)
    (let [d-conn (d/connect d-uri)]
      @(d/transact d-conn d-schema)
      d-conn)))

(def num-eids 10)

(def REAL-IDS
  (let [tx-fn #(vector %1 %2 :db/doc "TEMPORARY")
        tx (d/transact d-conn (map #(tx-fn :db/add %)
                                   (repeatedly num-eids #(d/tempid :db.part/user))))
        eids (-> @tx :tempids vals)]
   (d/transact d-conn (map #(tx-fn :db/retract %) eids))
   eids))

(defn run-tx [tx]
  (let [norm-fn (partial normalize-entities clara-schema)]
    (loop [c-session c-session
           ds-db (ds/empty-db ds-schema)
           d-db (d/db d-conn)
           [[action items] & tx] tx]
      (let [eavs (store/eav-seq clara-schema items)
            [ds-cmd c-fn] (condp = action
                            :insert [:db/add eav.rules/upsert]
                            :retract [:db/retract eav.rules/retract])
            ds-tx (map (fn [{:keys [e a v]}] [ds-cmd (int (mod e Integer/MAX_VALUE)) a v]) eavs)
            d-tx (map (fn [{:keys [e a v]}] [ds-cmd (datomic.db.DbId. :db.part/user e) a v]) eavs)
            ;;_ (prn :eavs eavs)
            c-session (try (-> (c-fn c-session eavs)
                               (rules/fire-rules))
                           (catch Exception e
                             (println e)
                             :exception))
            ds-db (try (ds/db-with ds-db ds-tx)
                       (catch Exception e
                         (println e)
                         :exception))
            _ (prn :d-tx d-tx)
            d-db (:db-after (try (d/with d-db d-tx)
                                 (catch Exception e
                                   (println e)
                                   {:db-after :exception})))]
        (if (seq tx)
          (recur c-session ds-db d-db tx)
          (merge
           {:clara {:exception-on eavs}
            :store {:exception-on eavs}
            :datomic {:exception-on eavs}
            :datascript {:exception-on eavs}}
           (when (not= :exception c-session)
             {:clara      (set (-> (clara-entities c-session) th/strip-ids))
              :store      (set (-> (:store c-session) store/dump-entity-maps th/strip-ids norm-fn))})
           (when (not= :exception d-db)
             {:datomic (set (-> d-db datomic-entities th/strip-ids norm-fn))})
           (when (not= :exception ds-db)
             {:datascript (set (-> ds-db datascript-entities th/strip-ids norm-fn))})))))))

(defn zzprint [prefix & args]
  (let [s (with-out-str (apply zprint args))]
    (print (clojure.string/replace s #"(?m)^" prefix))))

(defn check-tx [tx]
  (println "----- using tx:")
  (zzprint "    " tx)
  (let [res (run-tx tx)]
    (println "----- full res:")
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
        (println "clara/datascript diff:")
        (let [[added removed] (differ/diff (:clara res) (:datascript res))]
          (zzprint "    " {:datascript added
                           :clara removed}))
        (println "clara/datomic diff:")
        (let [[added removed] (differ/diff (:clara res) (:datomic res))]
          (zzprint "    " {:datomic added
                           :clara removed}))
        (println "-----")
        res))))

(defn sanity-check-1 [action]
  (let [items [#:schema{:eav/eid -1
                        :name  "Maksim"
                        :age   45}]
        res (check-tx [[action items]])]
    (apply = (vals res))))

(defn sanity-check-2 [action]
  (let [items [#:schema{:eav/eid -1
                        :name  "Maksim"
                        :age   45
                        :aka   ["Max Otto von Stierlitz", "Jack Ryan"]}
               #:schema{:eav/eid -2
                        :name  "John Doe"
                        :aka   ["James John Jones"]}]
        res (check-tx [[action items]])]
    (apply = (vals res))))


;; Datascript exception on ID 0
(def TEMP-IDS [-100 -20 -10 -2 -1])
;(def TEMP-IDS ["-100" "-20" "-10" "-2" "-1"])
;(def REAL-IDS [11000 12000 13000 14000 15000 16000 17000 18000 19000])
(def IDS (into TEMP-IDS REAL-IDS))

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

(defn create-gen-tx
  []
  (tgen/not-empty
   (tgen/vector
    (tgen/let [action (tgen/frequency [[40 (tgen/return :insert)]
                                       [60 (tgen/return :retract)]])]
      (tgen/tuple
       (tgen/return action)
       (tgen/vector
        (tgen/fmap
         (fn [[id name? name age? age aka? aka]]
           (merge
            (condp = action
              :insert {:eav/eid (nth (cycle IDS) id)}
              :retract {:eav/eid (nth (cycle REAL-IDS) id)})
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
          tgen/boolean (tgen/vector tgen/nat 0 (count NAMES))))))))))

(def tx-prop
  (tprop/for-all
   [tx (create-gen-tx)]
   (let [res (run-tx tx)]
     (apply = (vals res)))))

(defn do-gen-test [size]
  (quick-check size tx-prop))

(def bad-001
  [{:eav/eid 1001}
   {:eav/eid -10}
   {:eav/eid -9, :schema/name "Jane Doe"}
   {:eav/eid 1003, :schema/aka #{"Jack Ryan"}}])

(def bad-002
  [[-100, :schema/name, "Maksim"] [-100, :schema/age, 1] [-100, :schema/aka, "Maksim"] [-20, :schema/aka, "Max Otto von Stierlitz"] [-20, :schema/aka, "Maksim"] [-20, :schema/aka, "Jack Ryan"] [-20, :schema/aka, "Max Otto von Stierlitz"] [-100, :schema/age, 5] [-100, :schema/aka, "Jack Ryan"] [-100, :schema/aka, "John Doe"] [-100, :schema/aka, "Max Otto von Stierlitz"] [-100, :schema/aka, "Jane Doe"] [-100, :schema/aka, "Maksim"] [-20, :schema/age, 4] [-2, :schema/name, "Jane Doe"] [1003, :schema/age, 7] [1003, :schema/aka, "Maksim"] [-1, :schema/name, "Joe Farnarkle"] [-1, :schema/age, 3] [-2, :schema/aka, "James John Jones"] [-2, :schema/aka, "Joe Farnarkle"] [-2, :schema/aka, "Erika Mustermann"] [1004, :schema/name, "Karel Novak"] [1004, :schema/age, 3] [1004, :schema/aka, "James John Jones"] [1004, :schema/aka, "Joe Farnarkle"] [1004, :schema/aka, "John Doe"] [1004, :schema/aka, "Jan Novak"] [1004, :schema/aka, "Max Otto von Stierlitz"] [1004, :schema/aka, "Erika Mustermann"] [1004, :schema/aka, "Jane Doe"]])

#_(defn -main [& argv]
    (zprint (test2))
    (apply = (vals (test2))))
