(defproject embang "1.0.0-SNAPSHOT"
  :description "m!, a probabilistic programming system"
  :url "http://bitbucket.org/dtolpin/embang"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.json "0.2.6"]
                 [com.taoensso/timbre "3.4.0"]
                 [colt "1.2.0"]
                 [net.mikera/core.matrix "0.34.0"]
                 [net.mikera/vectorz-clj "0.29.0"]]
  :plugins [[codox "0.8.11"]
            [dtolpin/lein-gorilla "0.3.5-SNAPSHOT"]]
  :deploy-repositories [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                     :creds :gpg}]
                         ["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]]
  :scm {:name "git"
        :url "https://bitbucket.org/dtolpin/embang"}
  :pom-addition [:developers [:developer
                              [:name "David Tolpin"]
                              [:url "http://offtopia.net/"]
                              [:email "dvd@offtopia.net"]
                              [:timezone "+2"]]]
  :repl-options {:init-ns embang.core
                 :timeout 600000}
  :jvm-opts ["-Xmx1024m" "-Xss1024k"]
  :main ^:skip-aot embang.core
  :target-path "target/%s"
  :resource-paths ["../examples"]
  :profiles {:jar {:aot :all}
             :uberjar {:aot :all}}
  :deploy-branches ["master"]
  :aliases {"publish" ["do" ["clean"] ["test"] ["uberjar"]]})
