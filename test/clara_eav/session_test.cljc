(ns clara-eav.session-test
  (:require [spy.core :as spy]
            [clara-eav.test-helper :as test-helper]
            [clara.rules.engine :as engine]
            [clara-eav.store :as store]
            [clara-eav.session :as session]
    #?@(:clj [[clojure.test :refer [deftest testing is are use-fixtures]]
              [spy.protocol :as protocol]]
        :cljs [[cljs.test :refer-macros [deftest testing is are use-fixtures]]]))
  #?(:cljs (:require-macros [spy.protocol :as protocol])))

(use-fixtures :once test-helper/spec-fixture)

(deftest session-test
  (testing "Session delegation"
    (let [session-spy (protocol/spy engine/ISession)
          wrapper (session/wrap session-spy store/default-options)]
      (are [x] (session/session? x)
        (engine/insert wrapper [])
        (engine/retract wrapper [])
        (engine/fire-rules wrapper)
        (engine/fire-rules wrapper {}))
      (engine/query wrapper 'some-query {})
      (let [spies (meta session-spy)]
        (are [f] (spy/called-once? (f spies))
          :insert
          :retract
          :query)
        (is (spy/called-n-times? (:fire-rules spies) 2)))))
  (testing "Store binding"
    (let [is-binded (fn [& _] (is (= (store/init store/default-options) @store/*store*)))
          spy (protocol/spy engine/ISession {:fire-rules (spy/spy is-binded)})
          wrapper (session/wrap spy store/default-options)]
      (engine/fire-rules wrapper)
      (is (spy/called-once? (:fire-rules (meta spy)))))))
