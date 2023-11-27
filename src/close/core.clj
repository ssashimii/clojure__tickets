(ns close.core)



;; bfs function
(defn bfs [graph starting-mark destination-spec budget max-flights];;define breadth-first search function
  (let [starting-cost (get-edge-weight graph starting-mark starting-mark)
        queue (ref [[{:vertex starting-mark :cost (or starting-cost 0)}]])
        plans (ref [])];;explores graph from starting vertex to find valid flight plans
    (while (not (empty? @queue))
      (let [flight (first @queue)]
        (dosync (ref-set queue (rest @queue)))
        (let [current-vertex (-> flight last :vertex)
              costing (-> flight last :cost);;check if flight plan meets the destination, budget, and max-flights criteria
              current-vertex-data (get @(:vertices graph) current-vertex)]
          (when (and (and (string? destination-spec) (= current-vertex destination-spec))
                     (<= costing budget)
                     (<= (- (count flight) 1) max-flights));;update plans with valid flight plan
            (dosync (ref-set plans (conj @plans {:flight (map (fn [p] {:city (:vertex p) :cost (:cost p)}) flight) :total-cost costing}))))
          (let [neighbors (neighbors-graph graph current-vertex)]
            (doseq [neighbor neighbors]
              (let [edge-cost (get-edge-weight graph current-vertex neighbor)
                    total-cost (+ costing edge-cost)];;check if neighbor is a valid next destination based on criteria
                (when (and (not (some #(= neighbor (:vertex %)) flight))
                           (<= total-cost budget);;Enqueue updated flight plan with the neighbor as the next destination
                           (< (- (count flight) 1) max-flights))
                  (dosync
                    (alter queue conj (conj flight {:vertex neighbor :cost total-cost}))))))))))
    @plans))




;; sorting function

(defn sort-plans [plans]
  (->> plans (sort-by (fn [plan] [(-> plan :total-cost -) (-> plan :flight count)]))))

;; searching funct

(defn find-and-sort [graph starting-mark destination-name budget max-flights]
  (let [plans (bfs graph starting-mark destination-name budget max-flights)]
    (sort-plans plans)))


;; filtering duplicates function

(defn remove-duplicates [plans];;filters out duplicates based on number of flights
  (let [seen-flights (atom #{})]
    (filter (fn [plan]
              (let [num-flights (- (count (:flight plan)) 1)]
                (if (contains? @seen-flights num-flights);Creates a set to track number of flights already seen
                  false
                  (do;Checks if  number of flights has been seen before
                    (swap! seen-flights conj num-flights);Adds the number of flights to set of seen flights
                    true))));Return filtered list of flight plans
            plans)))

(defn output [flight]
  (->> flight
       (map-indexed (fn [idx {:keys [city cost]}]
                      (if (zero? idx) ;; Check if it's the first item in the flight list.
                        city ;; If it's the first item, return just the city name.
                        (str city " (" cost ")")))) ;; For subsequent items, format as "city (cost)".
       (clojure.string/join " to "))) ;; Join the formatted items with " to " in between.




;; flipping function

(defn flip-costs [flight]
  (loop [prev nil ;; Initialize 'prev' to nil, which holds the previous flight segment.
         remainder flight ;; Initialize 'remainder' to the input flight list.
         result []]
    (if (empty? remainder) ;; Check if the 'remainder' flight list is empty.
      (reverse result) ;; If it's empty, reverse the 'result' list and return it as the final output.
      (let [current (first remainder) ;; Get the first flight segment from 'remainder'.
            new-cost (if prev (+ (:cost prev) (:cost current)) (:cost current))] ;; Calculate the new cost for the current flight segment.
        (recur current (rest remainder) (conj result (assoc current :cost new-cost))))))) ;; Recur with updated values, removing the processed segment and adding it to 'result'.


