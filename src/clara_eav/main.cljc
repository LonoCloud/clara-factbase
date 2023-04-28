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
            [clojure.set :as set]
            [clojure.walk :refer [prewalk]]
            [datascript.core :as ds]
            [datomic.api :as d]
            [differ.core :as differ]
            [zprint.core :refer [zprint]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as tgen]
            [clojure.test.check.properties :as tprop]

            [clojure.math.combinatorics :as combo]
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

(def datomic-test-cases
  [[[:db/retract -2 :schema/name "X"]
    [:db/add -1 :schema/name "O"]
    [:db/add -2 :schema/name "O"]]
   [[:db/add -1 :schema/name "O"]
    [:db/retract -2 :schema/name "X"]
    [:db/add -2 :schema/name "O"]]
   [[:db/add -1 :schema/name "O"]
    [:db/add -2 :schema/name "O"]
    [:db/retract -2 :schema/name "X"]]
   [[:db/retract -2 :schema/name "X"]
    [:db/add -2 :schema/name "O"]
    [:db/add -1 :schema/name "O"]]
   [[:db/add -2 :schema/name "O"]
    [:db/retract -2 :schema/name "X"]
    [:db/add -1 :schema/name "O"]]
   [[:db/add -2 :schema/name "O"]
    [:db/add -1 :schema/name "O"]
    [:db/retract -2 :schema/name "X"]]
   [[:db/retract -2 :schema/name "X"]
    [:db/add -1 :schema/name "O"]]
   [[:db/retract -2 :schema/name "X"]
    [:db/add -2 :schema/name "O"]]
   [[:db/add -1 :schema/name "O"]
    [:db/retract -2 :schema/name "X"]]
   [[:db/add -2 :schema/name "O"]
    [:db/retract -2 :schema/name "X"]]])

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

(eav.rules/defsession* clara-session-like-datascript
  (assoc store/datascript-default-options :schema clara-schema)
  'clara-eav.main)

(eav.rules/defsession* clara-session-like-datomic
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

(def DATOMIC-ID->ID
  (reduce (fn [m [k v]] (-> m (assoc v k) (assoc (:idx v) k)))
          {} ID->DATOMIC-ID))

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
                             ;; TODO: datascript mode triggers this
                             ;;(throw (ex-info "Repeated :cardinality/one values"
                             ;;                {:e e :a k :v v})))
                             ;;[k  (str "ERROR: Repeated :cardinality/one values: " v)])
                             [k  v])
                           [k v]))
                       V'))]]
    (into {} ms)))

(defn schementity [attrs]
  (prn :I-appear-on-reload-but-why)
  (accumulators/grouping-by :e (partial #'entity-grouping attrs)))

(eav.rules/defquery entities []
  [?entities <- (schementity {}) :from [[?e]]])


(defn- card-many->set
  [attrs entity]
  (with-meta
    (into {} (for [[k v] entity]
               (if (= :cardinality/many (-> k attrs :cardinality))
                 [k (set v)]
                 [k v])))
    (meta entity)))

(defn- normalize-entities
  [entities]
  (for [entity entities]
    (card-many->set (store/schema->attrs clara-schema) entity)))

(defn normalize-entity-maps
  [entity-maps]
  (->> entity-maps th/strip-ids normalize-entities (sort-by hash)))

(defn clara-entities* [session]
  (binding [store/*store* (atom (:store session))]
    (doall (mapcat :?entities (rules/query session entities)))))

(defn clara-entities [session]
   (->> session clara-entities* th/strip-ids (sort-by hash) seq))

(defn store-entities [session]
  (->> session :store store/dump-entity-maps normalize-entity-maps seq))

(defn datascript-entities* [db]
  (ds/q '[:find [(pull ?e [*]) ...] :where [?e]] db))

(defn datascript-entities [db]
  (->> db datascript-entities* normalize-entity-maps seq))

(defn datomic-entities* [db]
  (let [eavs (for [a (map :db/ident d-schema)
                   [e v] (d/q '[:find ?e ?v
                                :in $ ?a
                                :where [?e ?a ?v]] db a)]
               {:e e :a a :v v})]
    ;;(prn :datomic-entities eavs)
    (entity-grouping (store/schema->attrs clara-schema)
                   (group-by :e eavs))))

(defn datomic-entities [db]
  (->> db datomic-entities* normalize-entity-maps seq))

(defn- run-clara-tx [c-sess tx]
  (let [c-sess (try (-> (eav.rules/transact (with-meta c-sess {}) tx)
                        (rules/fire-rules))
                    (catch Exception e
                      (with-meta c-sess {:exception e :tx tx})))]
    (vary-meta c-sess merge {:entities (clara-entities c-sess)})))

(defn- run-clara-store-tx [c-sess tx]
  ;; clara session but with entities for the store
  (vary-meta c-sess merge {:entities (store-entities c-sess)}))

(defn- run-datascript-tx [ds-db tx]
  (let [ds-db (try (ds/db-with (with-meta ds-db {}) tx)
                   (catch Exception e
                     (with-meta ds-db {:exception e :tx tx})))]
    (vary-meta ds-db merge {:entities (datascript-entities ds-db)})))

(defn- run-datomic-tx [d-db tx]
 (let [d-tx (map (fn [[A e a v]] [A (get ID->DATOMIC-ID e) a v]) tx)
       d-db (try (:db-after (d/with (with-meta d-db {}) d-tx))
                 (catch Exception e
                   (with-meta d-db {:exception e :tx tx})))]
   (vary-meta d-db merge {:entities (datomic-entities d-db)})))

(defn- run-datomic-ordered-tx [d-db tx]
 (let [d-tx (sort-by (comp {:db/retract 1 :db/add 2} first)
              (map (fn [[A e a v]] [A (get ID->DATOMIC-ID e) a v]) tx))
       d-db (try (:db-after (d/with (with-meta d-db {}) d-tx))
                 (catch Exception e
                   (with-meta d-db {:exception e :tx tx})))]
   (vary-meta d-db merge {:entities (datomic-entities d-db)})))

#_
(defn- datomic-behavior? [d-db tx]
  (let [entities (:entities (meta d-db))
        ;; TODO XXX NOTE: only handling :schema/name statically, not all unique/ident attributes
        ident-attrs #{:schema/name}

        temp-retract-ident-in-db?
        , (fn [db? [A e a v]]
            (and (= :db/retract A)
                 (db? [a v])))

        temp-add-diff-ident-not-db?
        , (fn [db? [re ra rv] [A e a v]]
            (and (= :db/add A)
                 (neg? e) ; pre-transformed tempid
                 (= re e)
                 (= ra a)
                 (not= rv v)
                 (not (db? [a v]))))

        diff-temp-add-diff-ident-not-db?
        , (fn [[de da dv] [A e a v]]
            (and (= :db/add A)
                 (neg? e) ; pre-transformed tempid
                 (not= de e)
                 (= da a)
                 (= dv v)))]
    (or (some #(= % #{true false})
              (for [[av eavs] (->> tx
                                   (keep (fn [[A e a v]]
                                           (when (ident-attrs a)
                                                [e a v])))
                                   (group-by (fn [[e a v]] [a v])))]
                (set (map (comp store/tempid? first) eavs))))
        (some identity
              (for [[attr db-ident-avs] (->> (mapcat seq entities)
                                             (filter (fn [[a v]] (ident-attrs a)))
                                             (group-by first))
                    :let [db? (set db-ident-avs)
                          tx-ident-Aeavs (filter (fn [[A e a v]] (= attr a))
                                                 tx)]
                    [rA re ra rv :as rAeav] (filter (partial temp-retract-ident-in-db? db?)
                                                    tx-ident-Aeavs)
                    :when rAeav
                    [dA de da dv :as dAeav] (filter (partial temp-add-diff-ident-not-db? db? [re ra rv])
                                                    tx-ident-Aeavs)
                    :when dAeav]
                  (some (partial diff-temp-add-diff-ident-not-db? [de da dv])
                        tx-ident-Aeavs))))))

(def datomic-cases
  {:datomic-case-1 '[[(neg? ?e)]
                     [(pos? ?e2)]
                     [$tx _ ?e ?a ?v]
                     [$tx _ ?e2 ?a ?v]]
   :datomic-case-2 '[[(neg? ?e)]
                     [(neg? ?e2)]
                     [$tx :db/add     ?e  ?a ?v]
                     [$tx :db/add     ?e2 ?a ?v]
                     [$tx :db/retract ?e  ?a ?v2]
                     [$tx :db/retract ?e2 ?a ?v2]
                     [(not= ?e ?e2)]
                     [(not= ?v ?v2)]]
   :datomic-case-3 '[[(neg? ?e)]
                     [$db ?e1 ?a ?v]
                     [$db ?e2 ?a ?v2]
                     [$tx _ ?e ?a ?v]
                     [$tx _ ?e ?a ?v2]
                     [(not= ?e1 ?e2)]
                     [(not= ?v ?v2)]]
   :datomic-case-4 '[[(neg? ?e)]
                     [(neg? ?e2)]
                     [$db _ ?a ?v]
                     [$tx ?A  ?e  ?a ?v]
                     [$tx ?A2 ?e  ?a ?v2]
                     [$tx _   ?e2 ?a ?v2]
                     [(not= ?A ?A2)]
                     [(not= ?e ?e2)]
                     [(not= ?v ?v2)]]
   :datomic-case-5 '[[(neg? ?e)]
                     [$db ?e1 ?a ?v]
                     [$db ?e2 ?a ?v2]
                     [$tx :db/add ?e  ?a ?v]
                     [$tx :db/add ?e1 ?a ?v2]
                     [$tx :db/add ?e2 ?a ?v3]
                     [(not= ?e1 ?e2)]
                     [(not= ?v ?v2)]
                     [(not= ?v2 ?v3)]
                     [(not= ?v ?v3)]]})

(def simple-options
  {:A #{:db/add :db/retract}
   :e [-1 -2 1 2]
   :a #{:schema/name}
   :v ["O" "X"]})

(def source-schema
  '{$db [:e :a :v]
    $tx [:A :e :a :v]})
              
(defn template-generator [query]
  ;; TODO what about bindings in multiple locations?
  (let [{db-ents '$db
         tx-ents '$tx
         preds nil} (group-by (comp '#{$db $tx} first) query)
        db-vars (mapcat (fn [[_ e a v :as full]]
                          [[:e (if (= e '_) (with-meta full {::_ true}) e)]
                           [:a (if (= a '_) (with-meta full {::_ true}) a)]
                           [:v (if (= v '_) (with-meta full {::_ true}) v)]]) db-ents)
        tx-vars (mapcat (fn [[_ A e a v :as full]]
                          [[:A (if (= A '_) (with-meta full {::_ true}) A)]
                           [:e (if (= e '_) (with-meta full {::_ true}) e)]
                           [:a (if (= a '_) (with-meta full {::_ true}) a)]
                           [:v (if (= v '_) (with-meta full {::_ true}) v)]]) tx-ents)]
    (group-by first (filter #(let [patt (second %)]
                               (or (and (symbol? patt)
                                        (= \? (first (name patt))))
                                   (and (vector? patt)
                                        (::_ (meta patt)))))
                            (into (set db-vars) tx-vars)))))

(defn map-permutations
  "Takes a map 'm of keys -> collections, returns a lazy seq of every map
  permutation of the same keys to each value from the colletion.

  e.g. {:a [1 2] :b #{3 4}} ->
       ({:a 1 :b 3} {:q 1 :b 4} {:a 2 :b 3} {:a 2 :b 4})"
  [m]
  (concat
    (when (every? #(= 1 (count (val %))) m)
      (lazy-cat [(zipmap (keys m)
                         (map first (vals m)))]))
    (let [[k vs] (first (filter #(< 1 (count (val %)))
                                m))]
      (mapcat #(lazy-seq (map-permutations (assoc m k [%])))
              vs))))

(defn map-permutations
  "Takes a map 'm of keys -> collections, returns a lazy seq of every map
  permutation of the same keys to each value from the colletion.

  e.g. {:a [1 2] :b #{3 4}} ->
       ({:a 1 :b 3} {:q 1 :b 4} {:a 2 :b 3} {:a 2 :b 4})"
  [m]
  (map #(zipmap (keys m) %)
       (apply combo/cartesian-product (vals m))))

(defn substitute [query sub-map]
  (let [patt-map (into {} (map (fn [[[pos patt] v]]
                                 [patt [pos v]])
                               sub-map))
        tx-pos-map (into {} (map vector [:DB :A :e :a :v] (range)))
        db-pos-map (into {} (map vector [:DB :e :a :v] (range)))]
    (prewalk #(let [[pos v] (get patt-map %)]
                (if v
                  (if (vector? %)
                    (assoc % (if (= '$db (first %))
                               (get db-pos-map pos)
                               (get tx-pos-map pos))
                             v)
                    v)
                  %))
             query)))

(defn case-generator [query options]
  (let [slots (template-generator query)
        bindings (into {}
                  (mapcat (fn [[k vs]]
                            (map #(vector % (get options k)) vs))
                          slots))
        substitutions (map-permutations bindings)]
    (prn :subs (take 3 substitutions))
    (map #(substitute query %) substitutions)))

(defn  query-case [query db-eavs tx]
  (when (d/q (into '[:find [?e] :in $db $tx :where] query)
             db-eavs tx)
     (prn :query-case db-eavs tx)
     [db-eavs tx]))

(defn get-db-tx [s]
  (let [{db-ents '$db
         tx-ents '$tx
         preds nil} (group-by (comp '#{$db $tx} first) s)]
    [(map rest db-ents) (map rest tx-ents)]))

;; TODO XXX NOTE: only handling :schema/name statically, not all unique/ident attributes
(defn- datomic-behavior? [db-eavs tx]
  (let [ident-attrs #{:schema/name}
        tx-idents (filter (fn [[A e a v]] (ident-attrs a)) tx)]
    (or (query-case (datomic-cases :datomic-case-1) db-eavs tx-idents)
        ;(query-case (datomic-cases :datomic-case-2) db-eavs tx-idents)
        (query-case (datomic-cases :datomic-case-3) db-eavs tx-idents)
        (query-case (datomic-cases :datomic-case-4) db-eavs tx-idents)
        (query-case (datomic-cases :datomic-case-5) db-eavs tx-idents))))


(defn fake-exception
  [ctx tx]
  ;; TODO XXX NOTE: doesn't handle card-many-attrs
  (let [datomic-ish-db (some #{:d-db :do-db} (keys ctx))
        d-db (get ctx datomic-ish-db)
        db-eavs (mapcat (fn [ent]
                          (let [e (-> ent meta :db/id)]
                            (for [[a v] ent]
                              [(DATOMIC-ID->ID e) a v])))
                        (:entities (meta d-db)))]
    (when (datomic-behavior? db-eavs tx)
      {:exception true :tx tx :msg "Avoiding datomic behavior"})))

;; TODO: Write our own 'shuffle taking a j.u.Random and have a Clara oracle that
;; shuffles the tx according to a seed on the metadata of the tx

(defn run-tx [ctx tx]
    (let [;; Apply single transaction to all stores/databases
          merge-meta (fn [x m] (with-meta x (merge (meta x) m)))
          ctx (if-let [exc (fake-exception ctx tx)]
                (cond-> ctx
                  true         (update :c-sess merge-meta exc)
                  true         (update :c-sess merge-meta exc)
                  (:ds-db ctx) (update :ds-db  merge-meta exc)
                  (:d-db ctx)  (update :d-db   merge-meta exc)
                  (:do-db ctx) (update :do-db  merge-meta exc))
                (cond-> ctx
                  true         (update :c-sess run-clara-tx tx)
                  true         (update :c-sess run-clara-store-tx tx)
                  (:ds-db ctx) (update :ds-db  run-datascript-tx tx)
                  (:d-db ctx)  (update :d-db   run-datomic-tx tx)
                  (:do-db ctx) (update :do-db  run-datomic-ordered-tx tx)))

          ;; Gather results: either exception map or list of current entities
          rfn (fn [res db]
                (let [m (meta db)]
                  (conj (or res []) (if (:exception m) m (get m :entities)))))
          new-res (cond-> (:res ctx)
                    true         (update :clara           rfn (:c-sess ctx))
                    true         (update :store           rfn (:c-sess ctx))
                    (:ds-db ctx) (update :datascript      rfn (:ds-db ctx))
                    (:d-db ctx)  (update :datomic         rfn (:d-db ctx))
                    (:do-db ctx) (update :datomic-ordered rfn (:do-db ctx)))]
      (assoc ctx :res new-res)))

(defn run-txs [txs oracles]
  (assert (not (and (:datascript oracles) (:datomic oracles)))
          "Simultaneous datascript and datomic oracles not yet supported")

  (let [ctx {:c-sess (if (set/intersection #{:datomic :datomic-ordered} oracles)
                       clara-session-like-datomic
                       clara-session-like-datascript)
             :ds-db (when (:datascript oracles) (ds/empty-db ds-schema))
             :d-db (when (:datomic oracles) (d/db d-conn))
             :do-db (when (:datomic-ordered oracles) (d/db d-conn))}
        ctx-res (reduce run-tx ctx txs)]
        
     #_(prn :count-txs (count txs) :max-tx-size (apply max 0 (map count txs))
            :exceptions (count (filter :exception (-> ctx-res :res :datomic))))
     ctx-res))
       


(defn zzprint [prefix & args]
  (let [s (with-out-str (apply zprint args))]
    (print (clojure.string/replace s #"(?m)^" prefix))))

(defn exception-elide [result]
  (into {} (for [[k vs] result]
             [k (vec (for [v vs]
                       (if (and (map? v) (contains? v :exception))
                         (assoc v :exception (with-meta [:ELIDED] v))
                         v)))])))

(defn check-txs [txs oracles]
  (println "----- using txs:")
  (zzprint "    " txs)
  (let [full-res (:res (run-txs txs oracles))
        res (exception-elide full-res)
        pass? (apply = (vals res))]
    (println "----- res:")
    (zzprint "    " res)
    (if pass?
      (do
       (println "SUCCESS (identical)")
       (assoc res :pass? pass? :txs txs))
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
       (when (:datomic-ordered oracles)
         (println "clara/datomic-ordered diff:")
         (let [[added removed] (differ/diff (:clara res) (:datomic-ordered res))]
           (zzprint "    " {:datomic-ordered added
                            :clara removed})))
       (println "-----")
       (assoc full-res :pass? pass? :txs txs)))))

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
    (tgen/not-empty
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
         tgen/boolean (tgen/vector tgen/nat 0 (count NAMES)))))))))) ;; aka

(def txs-prop-datascript
 (tprop/for-all
  [txs (create-gen-txs (partial id-remap-fn false))]
  (let [res (:res (run-txs txs #{:datascript}))]
    (apply = (vals (exception-elide res))))))

(def txs-prop-datomic
  (tprop/for-all
   [txs (create-gen-txs (partial id-remap-fn true))]
   (let [res (:res (run-txs txs #{:datomic}))]
     (apply = (vals (exception-elide res))))))

(defn do-gen-datascript [size]
  (quick-check size txs-prop-datascript))

(defn do-gen-datomic [size]
 (quick-check size txs-prop-datomic))

(defn run-generative-cycles [gen-test-fn size n]
  (loop [i n]
    (prn :cycle i)
    (let [r (gen-test-fn size)]
      (def res r)
      (if (and (< 0 i) (:pass? r))
        (recur (dec i)) r))))

(s/def ::cases (s/coll-of ::txseq))
(s/def ::txseq (s/coll-of ::store/tx))

(defn get-all-2x3-cases []
  (let [statements (for [v ["X" "O"] e [-1 -2] A [:db/add :db/retract]]
                      [A e :schema/name v])
        combos-1-3 (apply concat
                          (for [n (range 1 (inc 3))] ; how many statements / tx?
                            (combo/permuted-combinations statements n)))
        cases (apply concat
               (for [n (range 1 (inc 2))] ; how many transactions?
                 (combo/selections combos-1-3 n)))
        filtered (filter (fn [[[[A e a v]]]] (and (= e -1) (= v "X")))
                         cases)]
    filtered))

(defn get-all-2a-2r-cases [])



(defn run-2x3-cases
  [cases oracles]
  (pmap
    (fn [txs]
      (let [full-res (:res (run-txs txs oracles))
            res (exception-elide full-res)]
        {:txs txs
         :res res
         :pass? (apply = (vals res))}))
    cases))



;; (do (def res (do-gen-datascript 30)) (zprint res))
;; (do (def res (do-gen-datomic 30)) (zprint res))
;; (zprint (check-txs (-> res :shrunk :smallest (nth 0)) #{:datomic}))

#_(defn -main [& argv]
    (do-gen-dataomici 20))

