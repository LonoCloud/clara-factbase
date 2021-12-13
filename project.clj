(defproject clyfe/clara-eav "0.1.8-SNAPSHOT"
  :description "EAV triplets for Clara Rules"
  :url         "https://github.com/clyfe/clara-eav"
  :license      {:name "MIT"
                 :url "https://github.com/clyfe/clara-eav/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [medley "1.3.0"]
                 [com.cerner/clara-rules "0.21.0"]

                 ;; clara-eav.main generative testing deps
                 [expound "0.7.1"]
                 [differ "0.3.3"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [datascript "0.18.4"]]
  :plugins [[lein-cljsbuild "1.1.8" :exclusions [org.clojure/clojure]]
            [lein-cloverage "1.2.1"]]
  :codox {:metadata {:doc/format :markdown}}

  :aliases
  {"test-cljs" ["with-profile" "test,provided" "cljsbuild" "test"]
   "test-all" ["do" ["test"] ["test-cljs"]]
   "gentest-datomic" ["with-profile" "datomic" "run"]}

  :main clara-eav.main

  :profiles
  {:provided {:dependencies [[org.clojure/clojurescript "1.10.520"]]}
   :datomic {:dependencies [[com.datomic/datomic-free "0.9.5703.21"]]}
   :test {:cljsbuild {:test-commands {"node" ["node" "target/main.js"]}
                      :builds [{:id "test"
                                :source-paths ["src" "test"]
                                :compiler {:output-to "target/main.js"
                                           :output-dir "target"
                                           :main clara-eav.test-runner
                                           :optimizations :simple}}]}
          :dependencies [[org.clojure/test.check "0.10.0-alpha3"]
                         [expound "0.7.1"]]
          :injections [(do (require '[expound.alpha :as expound])
                           (require '[clojure.spec.alpha :as s])
                           (s/check-asserts true)
                           #_(set! s/*explain-out* expound/printer))]}})
