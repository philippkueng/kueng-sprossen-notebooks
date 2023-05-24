;; this is a hack that we currently need for clerk to play nice with specter
^{:nextjournal.clerk/error-on-missing-vars :off}
{}

;; ---

;; # Utilities

(ns utilities
  (:require [cheshire.core :as json]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [com.rpl.specter :as sp]
            [tick.core :as t]))

(defn bins-for-values
  [step values]
  (let [min-value (apply min values)
        max-value (apply max values)]
    (loop [current-step min-value
           bins []]
      (let [next-step (+ current-step step)]
        (if (< current-step max-value)
          (recur next-step (conj bins {:bin_start current-step
                                       :bin_end next-step
                                       :count 0}))
          bins)))))

(defn histogram
  [field step title records]
  (let [bin-values (let [values (map #(get % field) records)
                         bins (bins-for-values step values)]
                     (reduce (fn [bins value]
                               (sp/transform [sp/ALL #(and
                                                        (>= value (:bin_start %))
                                                        (<= value (:bin_end %))) :count]
                                 inc
                                 bins))
                       bins
                       values))]
    (clerk/vl
      {:data {:values bin-values}
       :title title
       :width 600
       :height 400
       :mark {:type "bar"}
       :encoding {:x {:field :bin_start
                      :bin {:binned true
                            :step step}}
                  :x2 {:field :bin_end}
                  :y {:field :count
                      :type :quantitative}}})))

;; - find the overall bins and use them for all the groups so we can compare the results
;; - let's say we got 24 groups, how would we display them and how would we know what to call them?

(defn multi-histogram
  [field step title-fn x-axis-title-fn group-fn records]
  (let [values (map #(get % field) records)]
    (let [holistic-bins (bins-for-values step values)
          grouped-records (group-by group-fn records)]
      (->> grouped-records
        (map (fn [[k records]]
               {:k k
                :bins (reduce (fn [bins value]
                                (sp/transform [sp/ALL #(and
                                                         (>= value (:bin_start %))
                                                         (<= value (:bin_end %))) :count]
                                  inc
                                  bins))
                        holistic-bins
                        (map #(get % field) records))}))
        (sort-by :k)
        (partition 3 3 nil)
        ((fn [row-partitions]
           (->> row-partitions
             (map (fn [row]
                    (->> row
                      (map (fn [chart-values]
                             (clerk/vl
                               {:data {:values (:bins chart-values)}
                                :mark "area"
                                :width 140
                                :height 70
                                :title (title-fn (:k chart-values))
                                :encoding {:x {:field :bin_start
                                               :title x-axis-title-fn
                                               :type :quantitative
                                               :axis {:grid false}}
                                           :y {:field :count
                                               :type :quantitative}}
                                :embed/opts {:actions false}})))
                      (apply v/row))))
             (apply v/col))))))))

