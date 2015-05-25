(defproject arcane-lab "0.1.0-SNAPSHOT"
  :description "Webapp to manipulate magic cards, like when building a sealed deck."
  :url "https://github.com/aperiodic/arcane-lab"
  :license {:name "GNU General Public License"
            :url "https://gnu.org/licenses/gpl-3.0.txt"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]
                 [org.clojure/clojurescript "0.0-2411"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [bigml/sampling "3.0"]
                 [cheshire "5.4.0"]
                 [clj-http "1.0.1"]
                 [cljs-ajax "0.3.8"]
                 [org.clojars.franks42/cljs-uuid-utils "0.1.3"]
                 [compojure "1.3.1"]
                 [hiccup "1.0.5"]
                 [jamesmacaulay/zelkova "0.2.0"]
                 [om "0.7.3"]
                 [ring "1.3.2"]
                 [ring/ring-defaults "0.1.3"]]
  :profiles {:dev {:plugins [[lein-cljsbuild "1.0.3"]
                             [lein-ring "0.8.11"]]}}
  :ring {:handler arcane-lab.site/handler}
  :source-paths ["src/cljs" "src/clj"]
  :target-path "target/jvm/%s"
  :cljsbuild {:builds [{:id "arcane-lab"
                        :source-paths ["src/cljs"]
                        :compiler {:output-to "target/js/arcane-lab.js"
                                   :output-dir "target/js"
                                   :optimizations :none
                                   :source-map true}}]})
