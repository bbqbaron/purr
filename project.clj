(defproject purr "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [instaparse "1.4.9"]
                 [org.clojure/clojurescript "1.10.339"]
                 [rum "0.11.2"]
                 [com.bhauman/figwheel-main "0.1.9"]
                 [com.bhauman/rebel-readline-cljs "0.1.4"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild
  {:builds
   [{:source-paths  ["src"]
     :optimizations :advanced}]}
  :resource-paths ["target" "resources"]
  :aliases
  {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
   "build-dev" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]})



