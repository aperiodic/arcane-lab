(defproject arcane-lab "0.1.0-SNAPSHOT"
  :description "Webapp to manipulate magic cards, like when building a sealed deck."
  :url "https://github.com/aperiodic/arcane-lab"
  :license {:name "GNU General Public License"
            :url "https://gnu.org/licenses/gpl-3.0.txt"}
  :dependencies [[org.clojure/clojure "1.8.0-RC5"]
                 [org.clojure/clojurescript "1.9.671"]
                 [org.clojure/core.async "0.2.395"]
                 [bigml/sampling "3.0"]
                 [cheshire "5.4.0"]
                 [clj-time "0.9.0"]
                 [cljs-ajax "0.5.8"]
                 [compojure "1.4.0"]
                 [cypress "0.2.0"]
                 [hiccup "1.0.5"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]
                 [puppetlabs/http-client "0.6.0"]
                 [org.omcljs/om "0.9.0"]
                 [ring "1.3.2"]
                 [ring/ring-defaults "0.1.3"]
                 [org.slf4j/slf4j-nop "1.7.6"]
                 [useful "0.8.8"]]
  :profiles {:dev {:plugins [[lein-cljsbuild "1.1.6"]
                             [lein-ring "0.9.7"]]}}
  :jvm-opts ^:replace ["-Xmx1024m"]

  :ring {:handler arcane-lab.site/handler}
  :source-paths ["src/cljs" "src/clj" "src/cljc"]
  :target-path "target/jvm/%s"
  :cljsbuild {:builds [{:id "arcane-lab"
                        :source-paths ["src/cljs" "src/cljc"]
                        :compiler {:output-to "target/js/arcane-lab.js"
                                   :output-dir "target/js"
                                   :foreign-libs [{:file "src/js/imagesloaded.js"
                                                   :provides ["js.imagesloaded"]}]
                                   :optimizations :none
                                   :source-map true}}]})
