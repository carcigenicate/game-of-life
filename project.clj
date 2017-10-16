(defproject game-of-life "1"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [helpers "1"]
                 [quil "2.4.0"]
                 [seesaw "1.4.5"]]
  :main game-of-life.color.quil-main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
