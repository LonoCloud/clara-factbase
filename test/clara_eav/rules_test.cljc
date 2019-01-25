(ns clara-eav.rules-test
  (:require [clara-eav.test-helper :as test-helper]
            [clara-eav.store :as store]
    #?@(:clj [[clara.rules :as rules]
              [clara-eav.rules :as eav.rules]
              [clojure.test :refer [deftest testing is are use-fixtures]]]
        :cljs [[clara.rules :as rules :include-macros true]
               [clara-eav.rules :as eav.rules :include-macros true]
               [cljs.test :refer-macros [deftest testing is are
                                         use-fixtures]]])))

(use-fixtures :once test-helper/spec-fixture)

;; Entity maps

(def new1 #:todo{:eav/eid :new :text "..." :done false})
(def milk1 #:todo{:eav/eid :milk :text "Buy milk" :done false})
(def eggs1 #:todo{:eav/eid :eggs :text "Buy eggs" :done true})
(def flakes #:todo{:text "Buy flakes" :done false})

(def new2 (assoc new1 :todo/text "!!!"))
(def milk2 (assoc milk1 :todo/text "Buy milk2"))
(def eggs2 (assoc eggs1 :todo/text "Buy two eggs"))
(def ham2 #:todo{:text "Buy ham" :done false})
(def cookie-a #:todo{:eav/eid :task-c-a :text "Buy cookie a" :done false})
(def cookie-b #:todo{:eav/eid :task-c-b :text "Buy cookie b" :done false})

(def toast5 #:todo{:eav/eid "toast-tempid" :text "Buy toast" :done true})
(def jam5 #:todo{:eav/eid -7 :text "Buy jam" :done true})

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
  (are [x y] (test-helper/set= x y)
    expectation (-> (todos session) test-helper/strip-ids)
    expectation (-> (:store session) store/dump-entity-maps test-helper/strip-ids)))


(deftest upsert-retract-accumulate-test
  (testing "Upsert, retract, accumulate entities"
    (let [
          ;; - upsert via call
          ;; - upsert-unconditional! via milk-and-flakes-r rule
          ;; - entity accumulator via todo-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session1 (upsert session [new1 milk1 eggs1])
          eav-index (-> [new1 milk1 eggs1 flakes] test-helper/strip-ids)
          _ (are [x y] (test-helper/set= x y)
              new1 (todo session1 :new))
          _ (test-session eav-index session1)


          ;; - upsert via call
          ;; - upsert! via milk2-and-cookies-r rule
          ;; - entity accumulator via todo-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session2 (upsert session1 [new2 milk2 ham2 eggs2])
          eav-index (-> [new2 milk2 eggs2 flakes ham2 cookie-a cookie-b] test-helper/strip-ids)
          _ (are [x y] (test-helper/set= x y)
              new2 (todo session2 :new))
          _ (test-session eav-index session2)

          ;; - retract via call
          ;; - entity accumulator via todo-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session3 (retract session2 [new2 eggs2])
          eav-index (-> [flakes milk2 ham2 cookie-a cookie-b] test-helper/strip-ids)
          _ (are [x y] (test-helper/set= x y)
              nil (todo session3 :new))
          _ (test-session eav-index session3)

          ;; - upsert transient via call
          ;; - retract! via remove-r rule
          ;; - eav binding via transients-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session4 (upsert session3 [[::remove :eav/transient :task-c-a]
                                     [::remove :eav/transient :task-c-b]])
          eav-index (-> [flakes milk2 ham2] test-helper/strip-ids)
          _ (are [x y] (test-helper/set= x y)
              [] (transients session4))
          _ (test-session eav-index session4)

          ;; - upsert via call
          ;; - tempids (string and negative int) resolution
          ;; - eav binding via transients-q query
          ;; - entities accumulator via todos-q query
          ;; - store maintenance
          session5 (upsert session4 [toast5 jam5])
          eav-index (-> [flakes milk2 ham2 jam5 toast5] test-helper/strip-ids)
          _ (are [x y] (test-helper/set= x y)
              [] (transients session5))
          _ (test-session eav-index session5)])))
