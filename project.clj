(defproject clyfe/clara-eav "0.1.6"
  :description "EAV triplets for Clara Rules"
  :url         "https://github.com/clyfe/clara-eav"
  :license      {:name "MIT"
                 :url "https://github.com/clyfe/clara-eav/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/spec.alpha "0.2.168"]
                 [medley "1.0.0"]
                 [com.cerner/clara-rules "0.18.0"]

                 ;; clara-eav.main generative testing deps
                 [expound "0.7.1"]
                 [differ "0.3.3"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [datascript "0.18.4"]
                 ]
  :plugins [[lein-cljsbuild "1.1.7" :exclusions [org.clojure/clojure]]
            [lein-doo "0.1.10" :exclusions [org.clojure/clojure]]
            [lein-cloverage "1.0.13"]]
  :codox {:metadata {:doc/format :markdown}}
  :doo {:paths {:rhino "lein run -m org.mozilla.javascript.tools.shell.Main"}}

  :aliases
  {"build-cljs" ["with-profile" "test,provided" "cljsbuild" "once"]
   "test-cljs-just" ["with-profile" "test,provided" "doo" "rhino" "test" "once"]
   "test-cljs" ["do" ["build-cljs"] ["test-cljs-just"]]
   "test-all" ["do" ["test"] ["test-cljs"]]
   "gentest-datomic" ["with-profile" "datomic" "run"]}

  :main clara-eav.main

  :profiles
  {:provided {:dependencies [[org.clojure/clojurescript "1.9.946"]]}
   :datomic {:dependencies [[com.datomic/datomic-free "0.9.5703.21"]]}
   :test {:dependencies [[lein-doo "0.1.10" :exclusions [org.clojure/clojure]]
                         [org.mozilla/rhino "1.7.7"]
                         [tortue/spy "1.4.0"]
                         [expound "0.7.1"]]
          :injections [(do (require '[expound.alpha :as expound])
                           (require '[clojure.spec.alpha :as s])
                           (s/check-asserts true)
                           #_(set! s/*explain-out* expound/printer))]
          :cljsbuild {:builds [{:id "test"
                                :source-paths ["src" "test"]
                                :compiler {:output-to "target/main.js"
                                           :output-dir "target"
                                           :main clara-eav.test-runner
                                           :optimizations :simple}}]}}})
