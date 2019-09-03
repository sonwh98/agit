(defproject  stigmergy/clgit "0.0.1-SNAPSHOT"
  :min-lein-version "2.8.3" 
  :dependencies [[org.clojure/clojure "1.10.1"]  
                 [org.clojure/clojurescript "1.10.520"]
                 [stigmergy/tily "0.1.8-SNAPSHOT"]
                 [stigmergy/voodoo "0.0.1-SNAPSHOT"]
                 [org.clojure/core.async "0.4.490"]
                 [binaryage/devtools "0.9.10"]
                 [org.clojure/core.async "0.4.500"]
                 [com.taoensso/timbre "4.10.0"]
                 [commons-codec/commons-codec "1.13"]
                 ]

  :plugins [[lein-figwheel "0.5.18"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    :target-path]

  :figwheel {:server-port 3449}
  
  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
                :figwheel {:on-jsload "stigmergy.cdr.main/jsload"
                           :websocket-host :js-client-host}
                :compiler {:main stigmergy.cdr.init
                           :asset-path "js/compiled/dev"
                           :output-to "resources/public/js/compiled/cdr.js"
                           :output-dir "resources/public/js/compiled/dev"
                           :source-map-timestamp true
                           :preloads [devtools.preload]}}
               ]}
  
  :profiles {:project/dev {:dependencies [[figwheel-sidecar "0.5.18"]
                                          [cider/piggieback "0.4.0"]]
                           :source-paths ["src/clj" "src/cljc" "env/dev/clj"]}
             :project/prod {:prep-tasks ["compile" ["cljsbuild" "once" "prod"]]
                            :source-paths ["src/clj" "src/cljc"]
                            :main stigmergy.cdr.server
                            :aot :all}

             :dev [:project/dev]
             :uberjar [:project/prod]
             }

  )
