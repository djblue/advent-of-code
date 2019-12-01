(ns advent-of-code.reload
  (:require [ns-tracker.core :as tracker]))

; https://cmdrdats.wordpress.com/2012/08/14/auto-reloading-code-with-clojure/

(defn check-namespace-changes [track]
  (try
    (doseq [ns-sym (track)]
      (require ns-sym :reload))
    (catch Throwable e (.printStackTrace e)))
  (Thread/sleep 500))

(defn start-nstracker []
  (let [track (tracker/ns-tracker ["src"])]
    (while true
      (check-namespace-changes track))))

(defn -main [] (start-nstracker))
