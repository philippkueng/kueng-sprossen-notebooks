(ns user
  (:require [nextjournal.clerk :as clerk]))

;; Commands copied from https://github.com/nextjournal/clerk-demo/blob/main/dev/user.clj
(clerk/serve! {:watch-paths ["notebooks"] :show-filter-fn #(clojure.string/starts-with? % "notebooks")})
