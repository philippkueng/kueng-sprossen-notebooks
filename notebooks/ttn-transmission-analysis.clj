;; this is a hack that we currently need for clerk to play nice with specter
^{:nextjournal.clerk/error-on-missing-vars :off}
{}

;; ---

;; # Analysis of technical transmission data

;; Are there any patterns in the transmission quality during particular months or time of day?

(ns ttn-transmission-analysis
  (:require [cheshire.core :as json]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [com.rpl.specter :as sp]
            [tick.core :as t]
            [utilities :refer [bins-for-values histogram multi-histogram]]))

(def dataset-file "dataset/uplink_messages.json")

(def single-event
  (with-open [reader (clojure.java.io/reader dataset-file)]
    (-> (line-seq reader)
      last
      (json/parse-string true))))

;; extract the rssi and snr values
(sp/select [:uplink_message :rx_metadata sp/ALL :rssi] single-event)

;; # RSSI
;; from https://www.thethingsnetwork.org/docs/lorawan/rssi-and-snr/  
;; RSSI (Received Signal Strength Indicator) is a relative measurement that helps you determine if the received signal is strong enough to get a good wireless connection from the transmitter. Since LoRaWAN supports bi-directional communication, RSSI is an important measurement for both gateways and end devices. RSSI is measured in dBm and its value is a negative form. The closer the RSSI value is to zero, the received signal is stronger.

;; Apart from the output power of the transmitter, the following factors mainly influence the RSSI:

;; - Path loss
;; - Antenna gain
;; - Cable/connector loss


;; # SNR

;; from https://www.thethingsnetwork.org/docs/lorawan/rssi-and-snr/  
;; SNR (Signal-to-Noise Ratio), often written as S/N, is the ratio of the received signal power to the noise floor. SNR is commonly used to determine the quality of the received signal.

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
   :sensor-2 (nth-sensor-values event 2)
   :rssi (->> event
           (sp/select [:uplink_message :rx_metadata sp/ALL :rssi])
           first)
   :snr (->> event
          (sp/select [:uplink_message :rx_metadata sp/ALL :snr])
          first)})

(parse-event single-event)

;; ## Parse all the events within this JSON file
(def events
  (with-open [reader (clojure.java.io/reader dataset-file)]
    (->> (line-seq reader)
      (map (fn [line]
             (-> line
               (json/parse-string true)
               (parse-event))))
      (remove #(or
                 (nil? (:sensor-0 %))
                 (nil? (:snr %))))
      doall)))

;; do we have any events with multiple `:rssi` values?
(comment
  (->> events
    (map #(count (:rssi %)))
    frequencies))
;; this means we only got events with a single `:rx_metadata` entry

;; ---

;; ## What's the distribution of the RSSI values?


(histogram :rssi 1 "Histogram of RSSI values" events)

;; we can see that the received signal is right at the end of being receivable. The closer to 0 the better.

(histogram :snr 1 "Histogram of SNR values" events)

;; same story here, a positive SNR value means the signal is stronger than the noise which in this case doesn't happen very often. On the contrary, with an SNR value of -25 it's in the range where even LoRa can't demodulate the signal anymore. Meaning the transmission quality is really poor and just about suited to receive data (sending it might be a different matter)

;; ## Questions
;; - [ ] how many packets are lost if we expect the events to be sent every 2ish mins?
;; - [x] histogram of RSSI values per hour of day OR per month of year
;; - [x] histogram of SNR values per hour of day OR per month of year

;; ## An ad-hoc table
#_(let [field :rssi
        step 1
        title "Histogram of RSSI values"
        records events
        bin-values (let [values (map #(get % field) records)
                         bins (bins-for-values step values)]
                     (reduce (fn [bins value]
                               (sp/transform [sp/ALL #(and
                                                        (>= value (:bin_start %))
                                                        (<= value (:bin_end %))) :count]
                                 inc
                                 bins))
                       bins
                       values))]
    (v/col
      (v/row
        (clerk/vl
          {:data {:values bin-values}
           :mark "bar"
           :title "RSSI for JUNE"
           :width 140
           :encoding {:x {:field :bin_start
                          :bin {:binned true
                                :step step}}
                      :x2 {:field :bin_end}
                      :y {:field :count
                          :type :quantitative}}})
        (clerk/vl
          {:data {:values bin-values}
           :mark "area"
           :width 140
           :height 70
           :title "JUNE"
           :encoding {:x {:field :bin_start
                          :title "RSSI value"
                          :type :quantitative
                          :axis {:grid false}}
                      :y {:field :count
                          :type :quantitative}}
           :embed/opts {:actions false}})
        (clerk/vl
          {:data {:values bin-values}
           :mark "bar"
           :width 140
           :height 70
           :encoding {:x {:field :bin_start
                          :bin {:binned true
                                :step step}}
                      :x2 {:field :bin_end}
                      :y {:field :count
                          :type :quantitative}}}))
      (v/row
        (clerk/vl
          {:data {:values bin-values}
           :mark "bar"
           :title "RSSI for JUNE"
           :width 140
           :encoding {:x {:field :bin_start
                          :bin {:binned true
                                :step step}}
                      :x2 {:field :bin_end}
                      :y {:field :count
                          :type :quantitative}}})
        (clerk/vl
          {:data {:values bin-values}
           :mark "area"
           :width 140
           :height 70
           :title "JUNE"
           :encoding {:x {:field :bin_start
                          :title "RSSI value"
                          :type :quantitative
                          :axis {:grid false}}
                      :y {:field :count
                          :type :quantitative}}
           :embed/opts {:actions false}})
        (clerk/vl
          {:data {:values bin-values}
           :mark "bar"
           :width 140
           :height 70
           :encoding {:x {:field :bin_start
                          :bin {:binned true
                                :step step}}
                      :x2 {:field :bin_end}
                      :y {:field :count
                          :type :quantitative}}}))))

#_(let [field :rssi
        step 1
        title-fn (fn [k] (format "RSSI values for %s" k))
        x-axis-title-fn "RSSI value"
        group-fn (fn [r] (t/month (:received_at r)))
        records events
        values (map #(get % field) records)]
    (let [holistic-bins (bins-for-values step values)
          grouped-records (group-by group-fn records)]
      #_(sort (keys grouped-records))
      #_holistic-bins
      #_grouped-records
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
             (apply v/col)))))))

;; # RSSI
;; What's the distribution of RSSI values over the months? Are there some that show better receival strength?
(multi-histogram
  :rssi
  1
  #(format "RSSI values for %s" %)
  "RSSI value"
  #(t/month (:received_at %))
  events)

;; Is there any particular distribution in the values -100 (as with June)?
(multi-histogram
  :rssi
  1
  #(format "RSSI values for %s" %)
  "RSSI value"
  #(t/month (:received_at %))
  (remove #(< -100 (:rssi %)) events))


;; it does appear as if some months are marginally better in terms of receival strength than others.  

;; Does the time of day make a difference?
(multi-histogram
  :rssi
  1
  #(format "RSSI values for hour: %s" %)
  "RSSI value"
  #(t/hour (:received_at %))
  (remove #(< -100 (:rssi %)) events))

;; it does appear as if the time of day doesn't make a difference in the signal strength received.

;; # SNR
;; What's the distribution of SNR values over the months?
(multi-histogram
  :snr
  1
  #(format "SNR values for %s" %)
  "SNR value"
  #(t/month (:received_at %))
  events)

;; it doesn't look like months behave much different to each other.

;; Does the time of day make a difference?
(multi-histogram
  :snr
  1
  #(format "SNR values for hour: %s" %)
  "SNR value"
  #(t/hour (:received_at %))
  events)

;; NOPE, the time of day doesn't make a difference either. The signal strength and it's receival is just poor consistently.



