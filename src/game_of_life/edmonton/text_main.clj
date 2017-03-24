(ns game-of-life.edmonton.text-main
  (:require [helpers.general-helpers :as g]
            [game-of-life.edmonton.environment :as e])
  (:gen-class))

(def default-width 30)
(def default-height 30)

(defn new-rand-gen-hasher
  "If seed? is nil, no seed is supplied to Random.
  Hashes the seed to ensure it's numeric."
  [seed?]
  (if seed?
    (g/new-rand-gen seed?)
    (g/new-rand-gen)))

(defn -main [&[width? height? seed?]]
  (let [width (if width? (Integer/parseInt width?) default-width)
        height (if height? (Integer/parseInt height?) default-height)
        test? (and seed? (.equalsIgnoreCase seed? "test"))
        rand-gen (new-rand-gen-hasher seed?)
        env (if test?
              e/test-env
              (e/random-environment [width height] 0.6 rand-gen))]
    (reduce
      (fn [e n]
        (Thread/sleep 150)
        (println n "\n"(str e) "\n----------")
        (e/advance-environment e))
      env
      (range 10))))
