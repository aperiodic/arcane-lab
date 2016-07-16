(defproject arcane-lab "0.1.0-SNAPSHOT"
  :description "Webapp to manipulate magic cards, like when building a sealed deck."
  :url "https://github.com/aperiodic/arcane-lab"
  :license {:name "GNU General Public License"
            :url "https://gnu.org/licenses/gpl-3.0.txt"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.2.374"]
                 [bigml/sampling "3.0"]
                 [cheshire "5.4.0"]
                 [clj-http "1.0.1"]
                 [clj-time "0.9.0"]
                 [cljs-ajax "0.3.8"]
                 [compojure "1.4.0"]
                 [hiccup "1.0.5"]
                 [jamesmacaulay/zelkova "0.4.0"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]
                 [puppetlabs/http-client "0.4.4"]
                 [org.omcljs/om "0.9.0"]
                 [ring "1.3.2"]
                 [ring/ring-defaults "0.1.3"]
                 [org.slf4j/slf4j-nop "1.7.6"]]
  :profiles {:dev {:plugins [[lein-cljsbuild "1.1.3"]
                             [lein-ring "0.9.7"]]}}
  :jvm-opts ["-Xmx512m"]

  :ring {:handler arcane-lab.site/handler}
  :source-paths ["src/cljs" "src/clj"]
  :target-path "target/jvm/%s"
  :cljsbuild {:builds [{:id "arcane-lab"
                        :source-paths ["src/cljs"]
                        :compiler {:output-to "target/js/arcane-lab.js"
                                   :output-dir "target/js"
                                   :optimizations :none
                                   :source-map true}}]})
