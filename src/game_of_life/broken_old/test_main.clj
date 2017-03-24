(ns game-of-life.broken_old.test-main
  (:require [game-of-life.broken_old.enviro :as e]
            [helpers.general-helpers :as g]))

(def rand-gen (g/new-rand-gen 993061001))

(defn -main []
  (let [enviro (e/random-alive-enviro 150 50 30 rand-gen)
        spit-path "c:/users/slomi/Desktop/GOL-record.txt"
        divider (apply str (replicate (-> enviro :dims first) "-"))
        counter (atom 0)]
    (do
      (spit spit-path (str (e/enviro-board-str enviro 0) divider "\n"))
      (e/sim-gens enviro 50 (fn [e]
                              (do
                                (if (= (rem @counter 10) 0)
                                  (println (str @counter)))
                                (spit spit-path (str (e/enviro-board-str e 0) divider "\n") :append true)
                                (swap! counter inc))))
      (println "Done."))))
