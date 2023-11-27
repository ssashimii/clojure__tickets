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




