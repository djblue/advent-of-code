(ns advent-of-code.core
  (:require [clojure.test :refer [run-tests]]
            advent-of-code.core-2018
            advent-of-code.core-2019
            advent-of-code.core-2020
            advent-of-code.core-2024))

(defn -main []
  (run-tests 'advent-of-code.core-2018)
  (run-tests 'advent-of-code.core-2019)
  (run-tests 'advent-of-code.core-2020)
  (run-tests 'advent-of-code.core-2024))

