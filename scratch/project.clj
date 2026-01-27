(defproject scratch "0.1.0-SNAPSHOT"
  :description "Wireless nREPL on Arduino UnoQ"
  :url "https://localhost"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies
  [[org.clojure/clojure "1.11.1"]]

  :main ^:skip-aot scratch.core
  :target-path "target/%s"

  :profiles
  {:dev
   {:dependencies
    [[cider/cider-nrepl "0.50.2"]
     [refactor-nrepl "3.11.0"]]

    :plugins
    [[cider/cider-nrepl "0.50.2"]]

    ;; These are optional defaults; your CLI flags still override them.
    :repl-options
    {:host "0.0.0.0"
     :port 7888
     :timeout 120000}}

   :uberjar
   {:aot :all
    :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
