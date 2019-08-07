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
    #(or (not= :cardinality/many (:cardinality %))
         (not (:unique %)))))
(s/def ::schema
  (s/coll-of ::schema-entry))

(s/def ::record (s/and #(instance? EAV %)
                       (s/keys :req-un [::eav/e ::eav/a ::eav/v])))
(s/def ::record-seq (s/coll-of ::record))
(s/def ::vector (s/tuple ::eav/e ::eav/a ::eav/v))
(s/def ::vector-seq (s/coll-of ::vector))

(s/def ::eav (s/or ::record ::record
                   ::vector ::vector))
(s/def ::eav-seq (s/coll-of ::eav))
(s/def :eav/eid ::eav/e)
(s/def ::entity (s/merge (s/keys :opt [:eav/eid])
                         (s/map-of keyword? any?)))
(s/def ::entity-seq (s/coll-of ::entity))
(s/def ::tx (s/or ::record ::record
                  ::record-seq ::record-seq
                  ::vector ::vector
                  ::vector-seq ::vector-seq
                  ::eav ::eav
                  ::eav-seq ::eav-seq
                  ::entity ::entity
                  ::entity-seq ::entity-seq))

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
(s/def ::tx-overwrite-mode ::validation-mode)
(s/def ::options (s/keys :opt-un [::schema ::schema-mode ::typecheck-mode ::tx-overwrite-mode]))
(s/def ::store (s/keys :req-un [::max-eid ::options ::eav-index ::ident-index ::value-index ::attrs]))
(s/def ::store-tx
  (s/merge ::store
           (s/keys :opt-un [::insertables ::retractables ::tempids])))

(def default-options
  {:schema []
   :schema-mode :ignore
   :typecheck-mode :ignore})

(def default-store
  {:max-eid 0
   :options default-options
   :attrs {}
   :eav-index {}
   :ident-index {}
   :value-index {}})

(s/fdef init
  :args (s/cat :options ::options)
  :ret ::store)
(defn init
  [options]
  (s/assert ::options options)
  (let [{:keys [schema] :as merged-options} (merge default-options options)
        {idents :unique/identity values :unique/value} (group-by :unique schema)]
    (merge default-store {:options merged-options
                          :attrs (into {} (map (juxt :ident identity)) schema)})))


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
  :args (s/cat :entity ::entity)
  :ret ::record-seq)
(defn- entity->eav-seq
  "Transforms an `entity` map into a list of EAVs. If the `entity` has a
  `:eav/eid` (set to non-tempid) subsequent operations will have upsert
  semantics. If not, a `:eav/eid` is generated as a tempid and subsequent
  operations will have insert semantics."
  [entity]
  (let [e (:eav/eid entity (tempid))
        ->eav (fn [[k v]] (eav/->EAV e k v))
        entity' (dissoc entity :eav/eid)]
    (map ->eav entity')))

(s/fdef eav-seq
        :args (s/cat :tx ::tx)
        :ret ::record-seq)
(defn eav-seq
  "Transforms transaction data `tx` into a sequence of eav records."
  [tx]
  (match/match (s/conform ::tx tx)
    ::s/invalid (throw (ex-info "Invalid transaction data (tx)" {:tx tx}))
    [::record eav-record] [eav-record]
    [::record-seq record-seq] record-seq
    [::vector eav-vector] [(apply eav/->EAV eav-vector)]
    [::vector-seq vector-seq] (mapcat eav-seq vector-seq)
    [::eav-seq record-or-vector-seq] (mapcat (comp eav-seq second) record-or-vector-seq)
    [::entity entity] (entity->eav-seq entity)
    [::entity-seq entity-seq] (mapcat entity->eav-seq entity-seq)))

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
  ;; Check for equality (i.e. unchanged sets) doesn't catch when the merging set
  ;; is a subset of the merged set, hence the metadata check.
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

(s/fdef av-index-merge
  :args (s/cat :idx ::av-index
               :eavs (s/nilable ::record-seq))
  :ret ::av-index)
(defn- av-index-merge [idx eavs]
  (merge idx (zipmap (map (juxt :a :v) eavs)
                     eavs)))

(s/fdef update-eav-positions
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
  (-> a
      attrs
      :valueType
      (= :valueType/ref)))

(s/fdef remap-ids
        :args (s/cat :attrs ::attrs
                     :id-mapping (s/map-of ::eav/e ::eav/e)
                     :eavs ::record-seq)
        :ret ::record-seq)
(defn- remap-ids [attrs id-mapping eavs]
  (->> eavs
       (map #(update-eav-position :e id-mapping %))
       (map #(if (ref-eav? attrs %)
               (update-eav-position :v id-mapping %)
               %))))

(s/fdef merge-unique-identities
  :args (s/cat :store ::store-tx
               :tx-eavs ::record-seq)
  :ret ::record-seq)
(defn- merge-unique-identities
  [{:keys [attrs ident-index]} tx-eavs]
  (loop [eavs tx-eavs]
    (let [tx-ident-eavs (filter (comp #{:unique/identity} :unique attrs :a) eavs)
          ;; Map of attr-val to set of eids for identity eavs across the DB and tx eavs
          ident-av-e-set (merge-av-e-set ident-index tx-ident-eavs)
          ;; Create unification ready list of all tx-eav entities (as
          ;; singleton sets) and the entity sets from ident-av-e-set.
          all-eid-sets (concat (map (comp hash-set :e) eavs)
                               (vals ident-av-e-set))
          unified-eids (unify-sets all-eid-sets)
          ;; Categorize counts the number of /actual/ non-temp eids
          categorize #(case (count %) 0 :zero-eids, 1 :one-eid, :many-eids-violations)
          {:keys [zero-eids one-eid many-eids-violations]}
          , (group-by #(categorize (remove tempid? %)) unified-eids)
          id-mapping (into {}
                           (for [tmpid-set (concat zero-eids one-eid)
                                 :let [id (or (first (remove tempid? tmpid-set))
                                              (first (sort-by str tmpid-set)))]
                                 tmpid tmpid-set]
                             [tmpid id]))
          new-eavs (remap-ids attrs id-mapping eavs)]
      (when (seq many-eids-violations)
        (throw (ex-info "Violation of :unique/ident attributes."
                        {:ident-av-e-set ident-av-e-set
                         :unified-eids unified-eids})))
      (if (= new-eavs eavs)
        new-eavs
        (recur new-eavs)))))

(s/fdef resolve-uniqueness-attrs
  :args (s/cat :store ::store-tx
               :tx-eavs ::record-seq)
  :ret ::store-tx)
(defn- resolve-uniqueness-attrs
  "Resolve :unique/{identity,value} attributes in 'tx-eavs. For
  :unique/identity, upsert by unifiying tmpids and mapping new eids.
  :unique/value attributes do not upsert and can only collide if
  duplicated. Insert the upserted and validated :unique/* eavs to the
  appropriate indicies in the store."
  [{:keys [max-eid attrs eav-index ident-index value-index] :as store} tx-eavs]
  (let [new-eavs (merge-unique-identities store tx-eavs)
        tempids (set (for [eav new-eavs
                           pos [:e :v]
                           :when (or (= pos :e) (ref-eav? attrs eav))
                           :let [id (get eav pos)]
                           :when (tempid? id)]
                       id))
        ;; Set up temp eids for mapping
        next-eid (inc max-eid)
        existing-eids (set (concat (keys eav-index) (remove (comp tempid? :e) new-eavs)))
        new-eids (take (count tempids)
                       (remove existing-eids (map #(+ next-eid %) (range))))
        new-max-eid (or (last new-eids) max-eid)
        ;; Create a map of tempid to new-eid
        tempid-newid-mapping (into {}
                                   (for [[new-eid tempid]
                                         (map list
                                              new-eids
                                              tempids)]
                                     [tempid new-eid]))
        ;; Apply tempid-newid-mapping to new-eavs using the existing
        ;; value if not in the mapping.
        new-eavs (remap-ids attrs tempid-newid-mapping new-eavs)
        ;; Re-gather ident and value eavs with resolved eids
        {new-ident-eavs :unique/identity new-value-eavs :unique/value}
        , (group-by (comp :unique attrs :a) new-eavs)
        ;; Map of attr-val to set of eids for value eavs across the DB and new tx eavs
        value-av-e-set (merge-av-e-set value-index new-value-eavs)
        value-violations (filter (fn [[k v]] (< 1 (count v))) value-av-e-set)
        ;; Merge new eavs into appropriate indicies
        new-ident-index (av-index-merge ident-index new-ident-eavs)
        new-value-index (av-index-merge value-index new-value-eavs)]
    (when (seq value-violations)
     (throw (ex-info "Violation of :unique/value attributes."
                     {:value-av-e-set value-av-e-set})))
    {:store (assoc store
                   :max-eid new-max-eid
                   :ident-index new-ident-index
                   :value-index new-value-index)
     :eavs new-eavs}))

(s/fdef -eav
  :args (s/cat :store ::store-tx
               :eav ::record)
  :ret ::store-tx)
(defn- -eav
  "Subtracts `eav` from `store` updating it's `:eav-index`. Returns the updated
  `store` including `:retractables` eavs."
  [store eav]
  (let [{:keys [e a]} eav]
    (if (tempid? e)
      (throw (ex-info "Tempids not allowed in retractions" {:e e}))
      (-> store
          (update :retractables conj eav)
          (medley/dissoc-in [:eav-index e a])))))

(s/fdef -eavs
  :args (s/cat :store ::store
               :eavs ::record-seq)
  :ret ::store-tx)
(defn -eavs
  "Called in retractions to obtain retractables. Throws if tempids are present
  in `eavs`, otherwise updates `store`'s `:eav-index`. Returns the updated store
  including `:retractables` eavs."
  [store eavs]
  (reduce -eav
          (assoc store :retractables [])
          eavs))

(s/fdef +eav
  :args (s/cat :store ::store-tx
               :eav ::record)
  :ret ::store-tx)
(defn- +eav
  "Adds `eav` to `store` updating it's `:max-eid` and `:eav-index`. Returns the
  updated `store` including `:insertables` eavs, `:retractables` eavs and
  resolved `:tempids` map of {tempid -> eid}."
  [store eav]
  (let [{:keys [tempids max-eid eav-index options]} store
        {:keys [e a v]} eav
        transient? (= :eav/transient a)]
    (if (tempid? e)
      (if-some [eid (get tempids e)]
        (-> store
            (update :insertables conj (assoc eav :e eid))
            (cond-> (not transient?) (assoc-in [:eav-index eid a] v)))
        (let [new-eid (inc max-eid)]
          (-> store
              (update :insertables conj (assoc eav :e new-eid))
              (assoc-in [:tempids e] new-eid)
              (assoc :max-eid new-eid)
              (cond-> (not transient?) (assoc-in [:eav-index new-eid a] v)))))
      (if transient?
        (update store :insertables conj eav)
        (if-some [v' (get-in eav-index [e a])]
          (cond-> store
                  (not= v v') (-> (update :insertables conj eav)
                                  (update :retractables conj (assoc eav :v v'))
                                  (assoc-in [:eav-index e a] v)))
          (-> store
              (update :insertables conj eav)
              (assoc-in [:eav-index e a] v)))))))


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
  (let [{:keys [store eavs]} (resolve-uniqueness-attrs store eavs)
        {:keys [schema-mode tx-overwrite-mode]} options]
    (when (#{:enforce :warn} schema-mode)
      (let [no-schema (remove (comp attrs :a) eavs)
            msg "Attributes not found in schema"]
        (when-not (empty? no-schema)
          (case schema-mode
            :enforce (throw (ex-info msg {:no-schema no-schema}))
            :warn (println (str msg " " (pr-str no-schema)))))))
    (when (#{:enforce :warn} tx-overwrite-mode)
      (let [{card-one-eavs :cardinality/one card-default-eavs nil} (group-by (comp :cardinality attrs :a) eavs)
            tx-overwrites (filter (fn [[k v]]
                                    (< 1 (count (set v))))
                                  (group-by (juxt :e :a) (concat card-one-eavs card-default-eavs)))
            msg "Values will overwrite during tx"]
        (when-not (empty? tx-overwrites)
          (case tx-overwrite-mode
            :enforce (throw (ex-info msg {:tx-overwrites tx-overwrites}))
            :warn (binding [*out* *err*] (println (str msg " " (pr-str tx-overwrites))))))))
    (reduce +eav
              (assoc store :insertables []
                           :retractables []
                           :tempids {})
              eavs)))
