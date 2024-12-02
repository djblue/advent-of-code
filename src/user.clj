(ns user
  (:require [portal.api :as p]))

(add-tap #'p/submit)
(p/open {:launcher :auto})
(.addShutdownHook (Runtime/getRuntime) (Thread. #(p/close)))