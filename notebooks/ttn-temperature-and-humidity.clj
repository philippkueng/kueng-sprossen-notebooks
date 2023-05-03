;; # Looking at the temperature and humidity data collected via the TTN

;; ^{::clerk/visibility :fold}
(ns ttn-temperature-and-humidity
  (:require [cheshire.core :as json]
            [nextjournal.clerk :as clerk]
            [com.rpl.specter :as sp]
            [tick.core :as t]))

(def dataset-file "dataset/uplink_messages.json")

;; have a look at the last value that got collected
(def single-event
  (with-open [reader (clojure.java.io/reader dataset-file)]
    (-> (line-seq reader)
      last
      (json/parse-string true))))

;; parse the `received_at` value and eg. return the hour it was captured
(-> single-event
  :received_at
  (t/instant)
  (t/in "UTC")
  (t/hour))

;; extract the values of sensor 3
(sp/select [:uplink_message :decoded_payload :values 2] single-event)

;; ## Utility functions

(defn- nth-sensor-values [event idx]
  (-> (sp/select
        [:uplink_message :decoded_payload :values idx]
        event)
    first))


(defn- parse-event [event]
  {:received_at (t/instant (:received_at event))
   :sensor-0 (nth-sensor-values event 0)
   :sensor-1 (nth-sensor-values event 1)
   :sensor-2 (nth-sensor-values event 2)})

(parse-event single-event)

;; ## Parse all the events within this JSON file
(def events
  (with-open [reader (clojure.java.io/reader dataset-file)]
    (->> (line-seq reader)
      (map (fn [line]
             (-> line
               (json/parse-string true)
               (parse-event))))
      (remove #(nil? (:sensor-0 %)))
      doall)))

(def number-of-events
  (count events))
(first events)
(last events)

(def number-of-days-data-has-been-collected-for
  (->> (t/between (:received_at (first events)) (:received_at (last events)))
    (t/days)))

;; we've been collecting a new data point about every 2 mins
(-> (/ number-of-events number-of-days-data-has-been-collected-for)
  (/ 24)
  (/ 60))

#_(let [events (list
                 {:temperature 23
                  :received_at 1}
                 {:temperature 25
                  :received_at 2}
                 {:temperature 22
                  :received_at 3})]
    (reduce
      (fn [state new-value]
        (if (< (:temperature state) (:temperature new-value))
          new-value
          state))
      {:temperature 0}
      events))

(defn- get-event-by-criteria
  [events criteria]
  (let [criteria-fn (condp = criteria
                      :max (fn [state new-value]
                             (< (:temperature (:sensor-2 state))
                               (:temperature (:sensor-2 new-value))))
                      :min (fn [state new-value]
                             (> (:temperature (:sensor-2 state))
                               (:temperature (:sensor-2 new-value)))))]
    (reduce
      (fn [state new-value]
        ;; if we don't have an initial value take the new-value
        (if (nil? state)
          new-value
          ;; check if the new-value is better suited given the criteria than the one in `state`.
          (if (criteria-fn state new-value)
            new-value
            state)))
      events)))


#_(get-event-by-criteria (take 100 events) :max)

;; ## Get all the per day values calculated
#_(def per-day-values
    (->> (group-by #(t/date (:received_at %)) events)
      (map (fn [[date events]]
             {:date (t/format :iso-local-date date)
              :max-temperature (->> events
                                 (map #(-> % :sensor-2 :temperature))
                                 ;; todo replace max function with a reduce to capture the time the max temperature occurs
                                 (apply max))
              :min-temperature (->> events
                                 (map #(-> % :sensor-2 :temperature))
                                 ;; todo replace max function with a reduce to capture the time the max temperature occurs
                                 (apply min))}))))




;; ## Get the max temperatures of sensor-2 per day (this is the sensor closest to the ceiling)
(def highest-temperatures
  (->> (group-by #(t/date (:received_at %)) events)
    (map (fn [[date events]]
           {:date (t/format :iso-local-date date)
            :value (->> events
                     (map #(-> % :sensor-2 :temperature))
                     ;; todo replace max function with a reduce to capture the time the max temperature occurs
                     (apply max))}))))

^{::clerk/visibility :fold}
(clerk/vl
  {:data {:values highest-temperatures}
   :title "Highest temperature of the day for sensor 3"
   :width 600
   :height 400
   :mark {:type "line"
          :tooltip {:field :date}}
   :encoding {:x {:field :date
                  :type :temporal}
              :y {:field :value
                  :type :quantitative}}})

(def lowest-temperatures-closest-to-ceiling
  (->> (group-by #(t/date (:received_at %)) events)
    (map (fn [[date events]]
           {:date (t/format :iso-local-date date)
            :value (->> events
                     (map #(-> % :sensor-2 :temperature))
                     ;; todo replace max function with a reduce to capture the time the max temperature occurs
                     (apply min))}))))

^{::clerk/visibility :fold}
(clerk/vl
  {:data {:values lowest-temperatures-closest-to-ceiling}
   :title "Lowest temperature of the day for sensor 3"
   :width 600
   :height 400
   :mark {:type "line"
          :tooltip {:field :date}}
   :encoding {:x {:field :date
                  :type :temporal}
              :y {:field :value
                  :type :quantitative}}})

^{::clerk/visibility :fold}
(def temperature-differences-for-ceiling-sensor
  (->> (group-by #(t/date (:received_at %)) events)
    (map (fn [[date events]]
           (let [highest (->> events
                           (map #(-> % :sensor-2 :temperature))
                           (apply max))
                 lowest (->> events
                          (map #(-> % :sensor-2 :temperature))
                          (apply min))]
             {:date (t/format :iso-local-date date)
              :value (- highest lowest)})))))

(clerk/vl
  {:data {:values (->> temperature-differences-for-ceiling-sensor
                    (remove #(< 10 (:value %))))}
   :title "Temperature difference of the day for sensor 3"
   :width 600
   :height 400
   :mark {:type "line"
          :tooltip {:field :date}}
   :encoding {:x {:field :date
                  :type :temporal}
              :y {:field :value
                  :type :quantitative}}})

(def temperature-events-closest-to-ceiling
  (->> (group-by #(t/date (:received_at %)) events)
    (map (fn [[date events]]
           (let [highest (get-event-by-criteria events :max)
                 lowest (get-event-by-criteria events :min)]
             (let [max-time (t/time (:received_at highest))
                   max-hour (t/hour max-time)
                   min-time (t/time (:received_at lowest))
                   min-hour (t/hour min-time)]
               {:date (t/format :iso-local-date date)
                :max-event-hour max-hour
                :min-event-hour min-hour}))))))

#_(take 10 max-temperature-event-closest-to-ceiling)

#_(comment

    (let [event (get-event-by-criteria (take 100 events) :max)
          received-at (:received_at event)]
      (t/format :iso-local-time (t/time received-at)))

    )

;; ^{::clerk/visibility :fold}
#_(clerk/vl
    {:data {:values max-temperature-event-closest-to-ceiling}
     :title "Time of day the maximal temperature is being recorded"
     :width 600
     :height 400
     :mark {:type "line"
            :tooltip {:field :max-event-hour}}
     :encoding {:x {:field :date
                    :type :temporal}
                :y {:field :max-event-hour
                    :type :quantitative}}})

;; it seems that he was using the cheaper energy over night and hence the peak temperature was recorded in the early mornings around 10 when he was mostly turning the lights off

(clerk/vl
  {:data {:values temperature-events-closest-to-ceiling}
   :title "Time of day the maximal and minimal temperature is being recorded"
   :width 600
   :height 400
   :transform [{:window [{:field :max-event-hour
                          :op :mean
                          :as :rolling_mean_max}]
                :frame [-30 30]}
               {:window [{:field :min-event-hour
                          :op :mean
                          :as :rolling_mean_min}]
                :frame [-30 30]}]
   :layer [#_{:mark {:type :point
                     :opacity 0.3}
              :encoding {:y {:field :max-event-hour}}}
           {:mark {:type :line
                   :color :red
                   :size 3}
            :encoding {:y {:field :rolling_mean_max
                           :title "max temperature"}}}
           {:mark {:type :line
                   :color :blue
                   :size 3}
            :encoding {:y {:field :rolling_mean_min
                           :title "min temperature"}}}]
   :encoding {:x {:field :date
                  :type :temporal}
              :y {:field :max-event-hour
                  :type :quantitative}}})


#_(def temperature-events-closest-to-ceiling
    (->> (group-by #(t/date (:received_at %)) events)
      (map (fn [[date events]]
             (let [highest (get-event-by-criteria events :max)
                   lowest (get-event-by-criteria events :min)]
               (let [max-time (t/time (:received_at highest))
                     max-hour (t/hour max-time)
                     min-time (t/time (:received_at lowest))
                     min-hour (t/hour min-time)]
                 {:date (t/format :iso-local-date date)
                  :max-event-hour max-hour
                  :min-event-hour min-hour}))))))

#_(def temperature-events-by-time-of-day
    (->> events
      (map (fn [event]
             {:time (t/format :iso-instant (:received_at event))
              :temperature (-> event :sensor-2 :temperature)}))
      #_(take 200)))

#_(clerk/vl
    {:data {:values temperature-events-by-time-of-day}
     :title "Temperatures by time of day"
     :width 600
     :height 400
     :transform [{:window [{:field :temperature
                            :op :mean
                            :as :rolling_mean}]
                  :frame [-30 30]}]
     :layer [{:mark {:type :point
                     :opacity 0.3}
              :encoding {:y {:field :temperature}}}
             {:mark {:type :line
                     :color :blue
                     :size 3}
              :encoding {:y {:field :rolling_mean
                             :title "temperature"}}}]
     :encoding {:x {:field :time
                    :type :temporal
                    :timeUnit :minutes
                    }
                :y {:field :temperature
                    :type :quantitative}}})

(defn average [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(def temperature-by-time-of-day
  (->> events
    (group-by #(t/hour (:received_at %)))
    (map (fn [[hour events]]
           {:hour hour
            :temperature (->> events
                           (map #(-> % :sensor-2 :temperature))
                           average)}))))

(clerk/vl
  {:data {:values temperature-by-time-of-day}
   :title "Average temperature by time of day (averaged by hour)"
   :width 600
   :height 400
   :mark {:type "line"
          :tooltip {:field :temperature}}
   :encoding {:x {:field :hour
                  :type :quantitative}
              :y {:field :temperature
                  :type :quantitative}}})

(comment

  ;; get both the hour and the temp value of sensor 2
  (->> events
    (map (fn [event]
           {:time (t/format (t/formatter "hh:mm") (t/time (:received_at event)))
            :temperature (-> event :sensor-2 :temperature)}))
    (take 10))

  )


;; ---

;; - [x] looking at rolling averages of max temperatures in a given month https://vega.github.io/vega-lite/examples/layer_line_rolling_mean_point_raw.html
;; - [ ] when do we have the biggest difference between min-temp and max-temp?
;; - [ ] check if the rssi and snr changes depending on the month or time of day
;; - [x] around what time of day do we usually reach the max temperature?
;; - [ ] boxplot of temperatures per month
;; - [ ] show the time range for a given day between the lowest and highest temperature and the average one
;; - [x] over all the days, how does the temperature usually rise and fall?



