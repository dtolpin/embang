(defproject embang "1.0.0-SNAPSHOT"
  ;; Attributes for humans and repositories
  :description "m!, a probabilistic programming system"
  :url "http://bitbucket.org/dtolpin/embang"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :scm {:name "git"
        :url "https://bitbucket.org/dtolpin/embang"}
  :pom-addition [:developers [:developer
                              [:name "David Tolpin"]
                              [:url "http://offtopia.net/"]
                              [:email "dvd@offtopia.net"]
                              [:timezone "+2"]]]

  ;; Deployment
  :deploy-branches ["master"]
  ;; Deploying to Maven Central; project name must be changed 
  ;; to net.offtopia/embang
  :deploy-repositories 
  [["releases"
    {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
     :creds :gpg}]
   ["snapshots"
    {:url "https://oss.sonatype.org/content/repositories/snapshots/"
     :creds :gpg}]]

  ;; Dependencies
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.json "0.2.6"]
                 [com.taoensso/timbre "4.0.2"]
                 [colt "1.2.0"]
                 [net.mikera/core.matrix "0.36.1"]
                 [net.mikera/vectorz-clj "0.30.1"]]
  :plugins [[codox "0.8.11"]
            [dtolpin/lein-gorilla "0.3.5-SNAPSHOT"]]

  ;; Run options
  :repl-options {:init-ns embang.core
                 :timeout 600000}
  :main ^:skip-aot embang.core
  :target-path "target/%s"
  :profiles {:repl {:resource-paths ["../examples"]}
             :jar {:aot :all}
             :uberjar {:aot :all}}
  :aliases {"publish" ["do" ["clean"] ["test"] ["uberjar"]]})
