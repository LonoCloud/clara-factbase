(ns ^:no-doc clara-eav.store
  "A store keeps track of max-eid and maintains an EAV index."
  (:require [clojure.set :as set]
            [medley.core :as medley]
            #?@(:clj [[clara-eav.eav :as eav]
                      [clojure.spec.alpha :as s]
                      [clojure.core.match :as match]]
                :cljs [[clara-eav.eav :as eav :refer [EAV]]
                       [cljs.spec.alpha :as s]
                       [cljs.core.match :as match :include-macros true]]))
  #?(:clj (:import [clara_eav.eav EAV]
                   [java.util UUID])))

(def ^:dynamic *store*
  "Dynamic atom of store to be used in rule productions, similar to other
  mechanisms from Clara."
  nil)

(s/def ::ident ::eav/a)
(s/def ::cardinality #{:cardinality/one :cardinality/many})
(s/def ::valueType (s/or :ref #{:valueType/ref} :keyword-spec keyword? :predicate fn?))
(s/def ::doc string?)
(s/def ::unique #{:unique/identity :unique/value})
(s/def ::id ::eav/e)
(s/def ::isComponent boolean?)
;; schema-entries are generally structured after Datomic schema entries:
;; https://docs.datomic.com/cloud/schema/schema-reference.html
;; For now, it is unsure if we will support ::id.
(s/def ::schema-entry
  (s/and
      (s/keys :req-un [::ident ::cardinality ::valueType]
              :opt-un [::doc ::unique ::isComponent]) ; ::id])
    ;; cf. https://docs.datomic.com/on-prem/identity.html#unique-identities
    ;; "Uniqueness can be declared on attributes of any value type,
    ;; including references (:db.type/ref). Only (:db.cardinality/one)
    ;; attributes can be unique."
    ;; TODO: Remove and note Datomic documentation is broken
    #_#(or (not= :cardinality/many (:cardinality %)
               (not (:unique %))))))
(s/def ::schema
  ;; :kind clause is a work-around for https://clojure.atlassian.net/browse/CLJ-1975
  (s/coll-of ::schema-entry :kind #(not (record? %))))

;; Facts

(s/def ::record (s/and #(instance? EAV %)
                       (s/keys :req-un [::eav/e ::eav/a ::eav/v])))
(s/def ::record-seq (s/coll-of ::record))
(s/def ::vector (s/tuple ::eav/e ::eav/a ::eav/v))
(s/def ::vector-seq (s/coll-of ::vector))

(s/def ::eav (s/or ::record ::record
                   ::vector ::vector))
(s/def ::eav-seq (s/coll-of ::eav))
(s/def :eav/eid ::eav/e)
(s/def ::entity-card-many (s/or :seq sequential?
                                :set set?))
(s/def ::entity (s/merge (s/keys :opt [:eav/eid])
                         (s/map-of keyword? any?)))
(s/def ::entity-seq (s/coll-of ::entity))
(s/def ::facts (s/or ::record ::record
                     ::record-seq ::record-seq
                     ::vector ::vector
                     ::vector-seq ::vector-seq
                     ::eav ::eav
                     ::eav-seq ::eav-seq
                     ::entity ::entity
                     ::entity-seq ::entity-seq))

;; Transaction

(s/def ::action #{:db/add :db/retract})
(s/def ::tx-vector (s/tuple ::action ::eav/e ::eav/a ::eav/v))
(s/def ::tx-vector-seq (s/coll-of ::tx-vector))
(s/def ::tx-entry (s/or :entity ::entity :tx-vector ::tx-vector))
(s/def ::tx (s/coll-of ::tx-entry))

(s/fdef tempid?
  :args (s/cat :e ::eav/e)
  :ret boolean?)
(defn- tempid?
  "True if `e` is a tempid. Strings and negative ints are tempids; keywords,
  positive ints and uuids are not."
  [e]
  (or (string? e)
      (neg-int? e)))

(defn- tempid
  "Generates an uuid as a string to be used as a tempid for an entity map."
  []
  (str (medley/random-uuid)))

(s/def ::max-eid integer?)
(s/def ::entity-map (s/map-of ::eav/a ::eav/v))
(s/def ::eav-index (s/map-of ::eav/e ::entity-map))
(s/def ::av-index (s/map-of (s/tuple ::eav/a ::eav/v) ::eav))
(s/def ::ident-index ::av-index)
(s/def ::value-index ::av-index)
(s/def ::attrs (s/map-of ::eav/a ::schema-entry))
(s/def ::insertables ::record-seq)
(s/def ::retractables ::record-seq)
(s/def ::tempids (s/map-of tempid? integer?))
(s/def ::validation-mode #{:enforce :warn :ignore})

(s/def ::schema-mode ::validation-mode)
(s/def ::typecheck-mode ::validation-mode)
(s/def ::tx-conflicts-mode ::validation-mode)
(s/def ::multi-retract-submode ::validation-mode)
(s/def ::retract-tempid-mode ::validation-mode)

(s/def ::options (s/keys :opt-un [::schema ::schema-mode ::typecheck-mode ::tx-conflicts-mode ::retract-tempid-mode]))
(s/def ::store (s/keys :req-un [::max-eid ::options ::eav-index ::ident-index ::value-index ::attrs]))
(s/def ::store-tx
  (s/merge ::store
           (s/keys :opt-un [::insertables ::retractables ::tempids])))

(def default-options
  {:schema []
   :schema-mode :ignore
   :typecheck-mode :ignore
   :tx-conflicts-mode :ignore
   :multi-retract-submode :ignore
   :retract-tempid-mode :ignore})


(def datomic-default-options
  {:schema []
   :schema-mode :enforce
   ;:typecheck-mode :enforce
   :tx-conflicts-mode :enforce
   :multi-retract-submode :enforce
   :retract-tempid-mode :ignore})

(def datascript-default-options
  {:schema []
   :schema-mode :ignore
   ;:typecheck-mode :ignore
   :tx-conflicts-mode :ignore
   :multi-retract-submode :ignore
   :retract-tempid-mode :enforce})

(def default-store
  {:max-eid 0
   :options default-options
   :attrs {}
   :eav-index {}
   :ident-index {}
   :value-index {}})

(s/fdef schema->attrs
        :args (s/cat :schema ::schema)
        :ret ::attrs)
(defn schema->attrs
  [schema]
  (into {} (map (juxt :ident identity)) schema))

(s/fdef init
  :args (s/cat :options ::options)
  :ret ::store)
(defn init
  [options]
  (s/assert ::options options)
  (let [{:keys [schema] :as merged-options} (merge default-options options)]
      (merge default-store {:options merged-options
                            :attrs (schema->attrs schema)})))

(s/fdef state
  :args (s/cat :store ::store-tx)
  :ret ::store)
(defn state
  "Remove extra keys from intermediary steps of computations and returns just
  the store state."
  [store]
  (dissoc store :insertables :retractables :tempids))

(s/fdef dump-entity-maps
  :args (s/cat :store ::store-tx)
  :ret ::entity-seq)
(defn dump-entity-maps
  "Dump all entity maps from the given `store`."
  [{:keys [eav-index]}]
  (map (fn [[e avs]]
         (assoc avs :eav/eid e))
       eav-index))

(s/fdef entity->eav-seq
  :args (s/cat :attrs ::attrs :entity ::entity)
  :ret ::record-seq)
(defn entity->eav-seq
  "Transforms an `entity` map into a list of EAVs. If the `entity` has a
  `:eav/eid` (set to non-tempid) subsequent operations will have upsert
  semantics. If not, a `:eav/eid` is generated as a tempid and subsequent
  operations will have insert semantics."
  [attrs entity]
  (let [e (:eav/eid entity (tempid))
        ->eav (fn [[a v]] (if (= :cardinality/many (-> a attrs :cardinality))
                              (do
                                (s/assert ::entity-card-many v)
                                (for [v' v]
                                  (eav/->EAV e a v')))
                              [(eav/->EAV e a v)]))
        entity' (dissoc entity :eav/eid)]
    ;; TODO: recursively handle :db.type/ref nested maps
    (mapcat ->eav entity')))

(s/fdef tx-seq
        :args (s/alt :no-schema (s/cat :tx ::tx)
                     :attrs (s/cat :attrs ::attrs :tx ::tx)
                     :schema (s/cat :schema ::schema :tx ::tx))
        :ret ::tx-vector-seq)
(defn tx-seq
  "Transforms transaction data `tx` into a sequence of action eav vectors."
  ([tx] (tx-seq {} tx))
  ([schema-or-attrs tx]
   (let [attrs (if (map? schema-or-attrs)
                 schema-or-attrs
                 (schema->attrs schema-or-attrs))]
     (apply concat
      (for [e tx]
        (match/match (s/conform ::tx-entry e)
                     ::s/invalid (throw (ex-info "Invalid tx entry" {:tx-entry e}))
                     [:tx-vector tx-vector] [tx-vector]
                     [:entity entity] (map #(into [:db/add] ((juxt :e :a :v) %))
                                           (entity->eav-seq attrs entity))))))))

(s/fdef eav-seq
        :args (s/alt :no-schema (s/cat :facts ::facts)
                     :attrs (s/cat :attrs ::attrs :tx ::facts)
                     :schema (s/cat :schema ::schema :tx ::facts))
        :ret ::record-seq)
(defn eav-seq
  "Transforms transaction data `facts` into a sequence of eav records."
  ([facts] (eav-seq {} facts))
  ([schema-or-attrs facts]
   (let [attrs (if (map? schema-or-attrs)
                 schema-or-attrs
                 (schema->attrs schema-or-attrs))]
     (match/match (s/conform ::facts facts)
                  ::s/invalid (throw (ex-info "Invalid fact data" {:facts facts}))
                  [::record eav-record] [eav-record]
                  [::record-seq record-seq] record-seq
                  [::vector eav-vector] [(apply eav/->EAV eav-vector)]
                  [::vector-seq vector-seq] (mapcat #(eav-seq attrs %) vector-seq)
                  [::eav-seq record-or-vector-seq] (mapcat #(eav-seq attrs (second %)) record-or-vector-seq)
                  [::entity entity] (entity->eav-seq attrs entity)
                  [::entity-seq entity-seq] (mapcat #(entity->eav-seq attrs %) entity-seq)))))

(s/fdef merges
  :args (s/cat :sets (s/coll-of set?) :s set?)
  :ret (s/coll-of set?))
(defn- merges
  "Merge intersecting set 's into collection of 'sets"
  [sets s]
  (mapv #(if (seq (set/intersection % s))
          (with-meta (into % s) {::merged true})
          (with-meta % {}))
       sets))

(s/fdef unify-sets
  :args (s/cat :sets (s/coll-of set?))
  :ret (s/coll-of set?))
(defn- unify-sets
  "Perform unification across a collection of 'sets"
  [sets]
  ;; We use a metadata check (::merged) instead of a check for equality.
  ;; This is because a subset merged to a set doesn't change that set.
  (let [sets' (reduce #(let [result (merges %1 %2)]
                        (if (some (comp ::merged meta) result)
                          result
                          (conj %1 %2)))
                      [(first sets)]
                      (rest sets))]
    (if (not= sets sets')
      (recur sets')
      sets)))

(s/fdef collect-eids
  :args (s/cat :eavs ::record-seq)
  :ret (s/map-of (s/tuple ::eav/a ::eav/v) (s/coll-of ::eav/e :kind set?)))
(defn- collect-eids
  "Takes a list of eavs and returns a map of [a v] to a set of entity ids."
  [eavs]
  (reduce (fn [m [e a v]]
           (update m [a v] (fnil conj #{}) e))
        {}
        eavs))

(s/fdef match-db-eavs
  :args (s/cat :av-index ::av-index
               :eavs ::record-seq)
  :ret (s/coll-of ::eav))
(defn- match-db-eavs
  [av-index eavs]
  (->> eavs
    (into #{} (map (juxt :a :v)))
    (select-keys av-index)
    vals))

(s/fdef merge-av-e-set
  :args (s/cat :av-index ::av-index
               :eavs (s/nilable ::record-seq))
  :ret (s/map-of (s/tuple ::eav/a ::eav/v)
                 (s/coll-of ::eav/e :kind set?)))
(defn- merge-av-e-set
  "Find eavs in DB 'idx matching attr-val of 'eavs. Return map of
  'attr-val' to 'set of eids' across DB eavs and 'eavs."
  [idx eavs]
  (let [db-eavs (match-db-eavs idx (sequence eavs))
        av-e-set (collect-eids (concat db-eavs eavs))]
    av-e-set))

(s/fdef update-av-index
  :args (s/cat :store ::store-tx
               :eav ::record)
  :ret ::store-tx)
(defn- update-av-index
  [{:keys [attrs] :as store} [e a v :as eav]]
  (let [action (-> eav meta :action)
        att (-> a attrs :unique)
        idx-name (get {:unique/identity :ident-index
                       :unique/value :value-index}
                      att)
        idx-eav (get-in store [idx-name [a v]])]
    (if idx-name
      (cond
        (and (= :db/add action) (not= eav idx-eav))
        (update store idx-name assoc [a v] eav)

        (and (= :db/retract action) (= eav idx-eav))
        (update store idx-name dissoc [a v] eav)

        :default
        store)
      store)))

(s/fdef update-eav-position
        :args (s/cat :position #{:e :v}
                     :id-mapping (s/map-of ::eav/e ::eav/e)
                     :eav ::eav)
        :ret ::eav)
(defn- update-eav-position
  "Substitute 'id-mapping in 'eav at :e or :v 'position"
  [position id-mapping eav]
  (update eav position #(get id-mapping % %)))

(s/fdef ref-eav?
        :args (s/cat :attrs ::attrs
                     :eav ::eav)
        :ret boolean?)
(defn- ref-eav?
  [attrs {:keys [e a v]}]
  (-> a attrs :valueType (= :valueType/ref)))

#_
[[-1 :name "Dad"]
 [-2 :name "Son"]
 [-2 :parent -1]]

(s/fdef remap-ids
  :args (s/cat :mkey #{:pre-upsert :pre-newid}
               :attrs ::attrs
               :id-mapping (s/nilable (s/map-of ::eav/e ::eav/e))
               :eavs ::record-seq)
        :ret ::record-seq)
(defn- remap-ids [mkey attrs id-mapping eavs]
  (as-> eavs new-eavs
    (map (fn [[e a v :as eav]]
           (let [e' (get id-mapping e e)
                 v' (if (ref-eav? attrs a)
                      (get id-mapping v v)
                      v)]
             (with-meta (eav/->EAV e' a v')
               (assoc (meta eav) mkey eav))))
         eavs)))

#_
[[1 :uniqidref 3]
 [1 :uniqid "foo"]
 [2 :uniqid "bar"]
 [3 :uniqid "baz"]]
#_
[[:db/add -1 :uniqidref -3]
 [:db/add -2 :uniqid "bar"]
 [:db/add -3 :uniqid "baz"]]

(s/fdef merge-unique-identities
  :args (s/cat :store ::store-tx
               :tx-eavs ::record-seq)
  :ret ::record-seq)
(defn- merge-unique-identities
  ""
  [{:keys [attrs ident-index]} tx-eavs]
  (loop [eavs tx-eavs]
    (let [tx-ident-eavs (filter (comp #{:unique/identity} :unique attrs :a) eavs)
          ;; Map of attr-val to set of eids for identity eavs across the DB and tx eavs
          temp-eavs (filter (comp tempid? :e) tx-ident-eavs)
          ident-av-e-set (merge-av-e-set ident-index temp-eavs)
          ;; Create unification ready list of all tx-eav entities (as
          ;; singleton sets) and the entity sets from ident-av-e-set.
          all-eid-sets (concat (map (comp hash-set :e) eavs)
                               (vals ident-av-e-set))
          unified-eids (unify-sets all-eid-sets)
          ;; Categorize counts the number of /actual/ non-temp eids
          ;; TODO: simplify as we only get 0 or 1 eid
          categorize #(case (count %) 0 :zero-eids, 1 :one-eid, :many-eids-violations)
          {:keys [zero-eids one-eid many-eids-violations]}
          , (group-by #(categorize (remove tempid? %)) unified-eids)
          id-mapping (into {}
                           (for [tmpid-set (concat zero-eids one-eid)
                                 :let [id (or (first (remove tempid? tmpid-set))
                                              (first (sort-by str tmpid-set)))]
                                 tmpid tmpid-set]
                             [tmpid id]))
          unified-eavs (remap-ids :pre-upsert attrs id-mapping eavs)]

      ;; TODO: remove the below
      #_(when (seq many-eids-violations)
          (throw (ex-info "Violation of :unique/ident attributes."
                          {:ident-av-e-set ident-av-e-set
                           :unified-eids unified-eids})))

      (if (= unified-eavs eavs)
        unified-eavs
        (recur unified-eavs)))))

(s/fdef upsert-derived-retracts
  :args (s/cat :store ::store-tx
               :eavs ::record-seq)
  :ret ::record-seq)
(defn upsert-derived-retracts
  [{:keys [attrs eav-index]} eavs]
  (concat eavs
          (keep (fn [[e a v :as eav]]
                  (let [eav-meta (meta eav)
                        idx-v (get-in eav-index [e a] ::absentx)]
                   (when (and (= :db/add (:action eav-meta))
                              (= :cardinality/one (-> a attrs :cardinality))
                              (not= idx-v ::absentx)
                              (not= v idx-v))
                     (prn :keeping e a v idx-v)
                     (with-meta (eav/->EAV e a idx-v)
                                (merge eav-meta {:action :db/retract
                                                 :synthetic-retract true})))))
                eavs)))

(s/fdef apply-new-ids
  :args (s/cat :store ::store-tx
         :tx-eavs ::record-seq))
(defn- apply-new-ids
  [{:keys [max-eid attrs eav-index] :as store} tx-eavs]
  (let [tempids (set (for [eav tx-eavs
                           pos [:e :v]
                           :when (or (= pos :e) (ref-eav? attrs eav))
                           :let [id (get eav pos)]
                           :when (tempid? id)]
                       id))
        ;; Set up temp eids for mapping
        next-eid (inc max-eid)
        existing-eids (set (concat (keys eav-index) (remove (comp tempid? :e) tx-eavs)))
        new-eids (take (count tempids)
                       (remove existing-eids (map #(+ next-eid %) (range))))
        new-max-eid (or (last new-eids) max-eid)
        ;; Create a map of tempid to new-eid
        tempid-newid-mapping (zipmap tempids new-eids)
        ;; Apply tempid-newid-mapping to tx-eavs using the existing
        ;; value if not in the mapping.
        tx-eavs (remap-ids :pre-newid attrs tempid-newid-mapping tx-eavs)]
    {:store (assoc store
                   :tempids tempid-newid-mapping
                   :max-eid new-max-eid)
     :eavs tx-eavs}))

(s/fdef -eav
  :args (s/cat :store ::store-tx
               :eav ::record)
  :ret ::store-tx)
(defn- -eav
  "Subtracts `eav` from `store` updating the indicies. Returns the updated
  `store` including `:retractables` eavs."
  [{:keys [attrs tempids] :as store} eav]
  (let [{:keys [e a v]} eav
        cardinality (get-in attrs [a :cardinality] :cardinality/one)
        update-fn ({:cardinality/one  #(if (= v (get-in % [e a]))
                                         (medley/dissoc-in % [e a])
                                         %)
                    :cardinality/many #(let [v-set (get-in % [e a] #{})
                                             v-set' (disj v-set v)]
                                         (if (empty? v-set')
                                           (medley/dissoc-in % [e a])
                                           (assoc-in % [e a] v-set')))}
                   cardinality)]
    (-> store
        (update :retractables conj eav)
        (update :eav-index update-fn)
        (update-av-index eav))))

(s/fdef -eavs
  :args (s/cat :store ::store
               :eavs ::record-seq)
  :ret ::store-tx)
(defn -eavs
  "Called in retractions to obtain retractables. Throws if tempids are present
  in `eavs`, otherwise updates `store`'s `:eav-index`. Returns the updated store
  including `:retractables` eavs."
  [{:keys [tempids] :as store} eavs]
  (reduce -eav store (map (fn [[e a v]]
                              (eav/->EAV (get tempids e e) a v))
                          eavs)))

(s/fdef +eav
  :args (s/cat :store ::store-tx
               :eav ::record)
  :ret ::store-tx)
(defn- +eav
  "Adds `eav` to `store` updating `:eav-index`. Returns the
  updated `store` including `:insertables` eavs, `:retractables` eavs."
  [{:keys [eav-index attrs] :as store} {:keys [e a v] :as eav}]
  (let [cardinality (get-in attrs [a :cardinality] :cardinality/one)
        update-fn ({:cardinality/one  (fn [old new] new)
                    :cardinality/many (fnil conj #{})}
                   cardinality)
        differs?  ({:cardinality/one  (fn [idx-v v] (not= v idx-v))
                    :cardinality/many (fn [idx-v v] (not (contains? idx-v v)))}
                   cardinality)]
    (if (= :eav/transient a)
      (update store :insertables conj eav)
      (let [idx-v (get-in eav-index [e a] #{})]
        (if (differs? idx-v v)
         (as-> store store
           (update store :insertables conj eav)
           (update-in store [:eav-index e a] update-fn v)
           (update-av-index store eav))
         store)))))

(s/fdef +eavs
  :args (s/cat :store ::store
               :eavs ::record-seq)
  :ret ::store-tx)
(defn +eavs
  "Called in upserts to obtain insertables and retractables. Resolves tempids in
  `eavs` and updates `store`'s `:max-id` and `:eav-index`. Returns the updated
  store including `insertables` and `retractables` eavs and resolved tempids map
  {tempid -> eid}."
  [{:keys [attrs options] :as store} eavs]
  (reduce +eav store eavs))

(defn check-schema
  [mode attrs eavs]
  (when (#{:enforce :warn} mode)
     (let [no-schema (remove (comp attrs :a) eavs)
           msg "Attributes not found in schema"]
      (when-not (empty? no-schema) (case mode
                                     :enforce (throw (ex-info msg {:no-schema no-schema}))
                                     :warn (println (str msg " " (pr-str no-schema))))))))

(s/fdef check-retracts
  :args (s/cat :store ::store-tx
               :eavs ::record-seq))

(defn- check-retracts
  [{:keys [options]} eavs]
  (let [{:keys [retract-tempid-mode]} options]
    (when (#{:enforce :warn} retract-tempid-mode)
       (let [bad-retracts (filter (fn [eav]
                                    (let [eav-meta (meta eav)]
                                      (and (-> eav-meta :action (= :db/retract))
                                           (-> eav-meta :pre-newid :e tempid?))))
                                  eavs)
             msg "Tempids not allowed in retractions"]
           (when-not (empty? bad-retracts)
             (let [bad-orig-eavs (map (comp :pre-upsert meta) bad-retracts)]
               (case retract-tempid-mode
                 :enforce (throw (ex-info msg {:bad-retracts bad-orig-eavs}))
                   :warn (binding [*out* *err*] (println (str msg " " (pr-str bad-orig-eavs)))))))))))

(s/fdef check-uniqueness-violations
  :args (s/cat :store ::store-tx
               :eavs ::record-seq))
(defn- check-uniqueness-violations
  [{:keys [attrs ident-index value-index]} eavs]
  (let [{ident-eavs :unique/identity value-eavs :unique/value}
        , (group-by (comp :unique attrs :a) eavs)
        addition? (comp #{:db/add} :action meta)
        ident-av-e-set (merge-av-e-set ident-index (filter addition? ident-eavs))
        value-av-e-set (merge-av-e-set value-index (filter addition? value-eavs))
        ident-violations (filter (fn [[k v]] (< 1 (count v))) ident-av-e-set)
        value-violations (filter (fn [[k v]] (< 1 (count v))) value-av-e-set)]
    (when (seq ident-violations)
      (throw (ex-info "Violation of :unique/ident attributes."
                      {:ident-violations ident-violations})))
    (when (seq value-violations)
     (throw (ex-info "Violation of :unique/value attributes."
                     {:value-av-e-set value-av-e-set})))))

(s/fdef check-tx-conflicts
  :args (s/cat :store ::store-tx
               :eavs ::record-seq))
(defn- check-tx-conflicts
  "Checks for the following conditions depending on mode:
     :retract-tempid-mode - Disallows retractions still containing tempids
     :tx-conflicts-mode  - Disallows assert & retract of identical EAV
                         - Disallows card/one asserts xor retracts of different values for given EA
         :multi-retract-submode - Allows retracts of differing card/one values for given EA

   Datomic has the following semantics:
   - temp ids ARE allowed for retracts
   - order is not allowed to change the outcome
     - Adds and retracts of the same EAV triple are never allowed
     - Adds or retracts of different values of the same card/one attribute for a
       given entity are not allowed
       - Technically, retracts of different values should be fine. This is an overstep.
   Datascript has the following semantics:
   - temp ids ARE NOT allowed for retracts
   - anything else goes"
  [{:keys [schema-mode options attrs ident-index value-index]} eavs]
  (let [{:keys [tx-conflicts-mode multi-retract-submode retract-tempid-mode]} options]
    (when (#{:enforce :warn} tx-conflicts-mode)
       (let [attr-card-map (->> (vals attrs)
                              (map (juxt :ident :cardinality))
                              (into {}))
             ;; cxeav - Cardinality action(X) Entity Attribute Value
             cxeav-fn (fn [x [e a v]] [(attr-card-map a :cardinality/one) x e a v])
             {tx-adds :db/add tx-retracts :db/retract}
             , (group-by (comp :action meta)
                         (remove (comp :synthetic-retract meta) eavs))
             #_ (prn :tx-adds tx-adds)
             #_ (prn :tx-retracts tx-retracts)
             cxeavs (concat (map #(cxeav-fn :db/add %)     tx-adds)
                            (map #(cxeav-fn :db/retract %) tx-retracts))
             #_ (prn :cxeavs cxeavs)
             collisions [{:reason :eav-add-retract
                          :msg "Cannot add and retract the same EAV"
                          :col-fn (fn [c x e a v]
                                   {:group [e a v] :collides [x]})}
                         {:reason :card-one-collide
                          :msg "Multiple adds/retracts of same cardinality/one attribute"
                          :col-fn (fn [c x e a v]
                                   (when (and
                                          (= c :cardinality/one)
                                          ;; :multi-retract-submode enforcement
                                          (or (= x :db/add)
                                              (and (#{:enforce :warn} multi-retract-submode)
                                                   (= x :db/retract))))
                                     {:group [x e a] :collides [v]}))}]
             ;; Group for potential collisionx where key is collision context and value is
             ;; part that can collide.
             ;; Metadata has full context and reason.
             groups (group-by (comp :group meta)
                            (for [[c x e a v] cxeavs
                                  {:keys [reason col-fn]} collisions
                                  :let [{:keys [group collides] :as hit?} (col-fn c x e a v)]
                                  :when hit?]
                             (with-meta collides {:group group :ctx [c x e a v] :reason reason})))
             #_ (prn :groups groups)
             tx-conflicts (filter (fn [[k v]]
                                   (< 1 (count (set v))))
                                groups)]
        (when-not (empty? tx-conflicts)
          (let [msg "Conflicting entries within transaction"
                entries (for [[k vs] tx-conflicts
                              v vs]
                          (let [{:keys [group ctx reason]} (meta v)
                                lookup (zipmap (map :reason collisions)
                                               collisions)]
                            {:msg (:msg (lookup reason))
                             :reason reason
                             :ctx ctx
                             :group group
                             :collision v}))]
           (case tx-conflicts-mode
            :enforce (throw (ex-info msg {:tx-conflicts tx-conflicts :entries entries}))
            :warn (binding [*out* *err*] (println (str msg " " (pr-str tx-conflicts)))))))))))

(s/fdef tx->eav-seq
  :args (s/cat :action ::action
               :tx ::tx-vector-seq)
  :ret ::eav-seq)
(defn- tx->eav-seq [tx]
  (for [[A e a v] tx]
    (with-meta (eav/->EAV e a v)
               {:action A})))

(s/fdef transact*
  :args (s/cat :store ::store-tx
               :tx    ::tx-vector-seq)
  :ret ::store-tx)
(defn transact*
  [{:keys [schema-mode attrs] :as store} tx]
  (let [store (assoc store :insertables [] :retractables []
                           :tempids {}) ;; TODO rename to tempid-map
        eavs (tx->eav-seq tx)
        _ (check-schema schema-mode attrs eavs)
        eavs (merge-unique-identities store eavs)
        eavs (upsert-derived-retracts store eavs)
        {:keys [store eavs]} (apply-new-ids store eavs)
        _ (check-retracts store eavs)
        store (reduce -eav store (filter (comp #{:db/retract} :action meta) eavs))
        _ (check-uniqueness-violations store eavs)
        store (reduce +eav store (filter (comp #{:db/add} :action meta) eavs))
        _ (check-tx-conflicts store eavs)]
    store))
