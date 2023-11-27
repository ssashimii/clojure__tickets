(ns close.core
    (:require [clojure.data.csv :as csv];;Importing libraries for handling CSV data and file I/O
      [clojure.java.io :as io]))

(defrecord graph[vertices edges])
;;Define record named 'graph' with fields 'vertices' and 'edges'
(defn make-graph[];;Define function 'make-graph' to create and return new graph
      (->graph(ref {})(ref {})))

(defrecord vertex[mark visited neighbors cost-currently flight]);;Define record names

(defn make-vertex[mark];;Define function that takes a 'mark' and creates new vertex
      (vertex. mark (ref 0)(ref '())(ref 0)(ref '())))
(defn graph-add-vertex![graph mark];;Define function that adds vertex with specified 'mark'
      (dosync;;'!'indicates modifying state
        (alter(:vertices graph) assoc mark(make-vertex mark))))

(defrecord edge [from to mark weight]);;record with fields 'from', 'to', 'mark', and 'weight'

(defn make-edge [from to mark weight]
      (->edge from to mark weight));;creates and returns a new edge
(defn graph-edge-key [from to];;returns a sorted list of the two values
      (->> [from to] sort list));;creating a key for edge lookup in the graph
(defn graph-add-edge! [graph from to mark weight]
      (let [vertices (:vertices graph)
            from-vertex (get @vertices from)
            to-vertex (get @vertices to)
            from-vertex-neighbors @(:neighbors from-vertex)
            to-vertex-neighbors @(:neighbors to-vertex)
            new-edge (make-edge from to mark weight)
            new-edge-key (graph-edge-key from to)]
           (dosync;;update graph's edges with new edge
             (ref-set (:edges graph) (assoc @(:edges graph) new-edge-key new-edge));;update neighbors of 'from' vertex with 'to'
             (ref-set (:neighbors from-vertex) (conj from-vertex-neighbors to))
             (ref-set (:neighbors to-vertex) (conj to-vertex-neighbors from)))))

(defn fetch-csv [csv-file];;reads and parses a CSV file, returning its content
      (with-open [file (io/reader csv-file)]
                 (-> file
                     slurp
                     csv/read-csv)))

(def cities-file (fetch-csv "src/clojure_airlines/flight_countries.csv"))
;;load CSV data from csv into 'cities-file'
(def g (make-graph));;Create empty graph 'g'

(defn csv-graph [cities-file g];;populates graph with vertices and edges based on CSV data
      (let [existing-vertex-marks (atom [])]
           (doseq [vector cities-file]
                  (doseq [vec (vec (take 2 vector))]
                         (if (not (.contains @existing-vertex-marks vec))
                           (do ;;add new vertex to the graph with the current vector data
                             (graph-add-vertex! g (str vec))
                             (reset! existing-vertex-marks (conj @existing-vertex-marks vec)))))
                  (graph-add-edge! g;;add new edge to the graph based on the current vector data
                                   (str (get vector 0))
                                   (str (get vector 1))
                                   (str (get vector 0) " " (get vector 1) " " (get vector 2))
                                   (Integer/parseInt (get vector 2))))))

(csv-graph cities-file g);;populate graph 'g' with data from CSV file

(defn neighbors-graph [graph mark]
      (if-let [vertex (get @(:vertices graph) mark)];;returns the neighbors of the vertex
              @(:neighbors vertex) ;;if vertex is found, return neighbors
              (do (println (str "No corresponding vertex for the mark" mark));;if vertex isn't found print error message and return an empty list
                  [])))

(defn graph-has-vertex? [graph mark];;checks if vertex with the given 'mark' exists in the graph
      (contains? @(:vertices graph) mark))

(defn graph-has-edge? [graph from to];;checks if edge from 'from' to 'to' exists in the graph
      (contains? @(:edges graph) (graph-edge-key from to)))

(defn graph-reset! [graph];;resets 'visited' property of all vertices in the graph to 0
      (doseq [vertex (vals @(:vertices graph))]
             (alter (:visited vertex) (constantly 0))))

(defn get-edge-weight [graph from to];;retrieves weight of edge from 'from' to 'to' in the graph
      (:weight (get @(:edges graph) (graph-edge-key from to))))

(defn reset-costs! [graph];;resets 'cost-currently' property of all vertices in the graph to 0
      (doseq [vertex (vals @(:vertices graph))]
             (dosync
               (ref-set (:cost-currently vertex) 0))))



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

(defn print-ticket [output-format total-cost flights]
  (let [data [(str "Travel Plan: " output-format)
              (str (- flights 1) " connected flights");prints formatted information about a flight plan
              (str "Total Cost: $" total-cost)]
        maximum (apply max (map count data))];determine maximum length among the lines of information
    (doseq [i (range (count data))]
      (let [info-line (nth data i nil)]
        (println (if info-line
                   (str "~ " info-line (apply str (repeat (- maximum (count info-line)) " ")))
                   (apply str (repeat maximum " "))))));print separator lines or spaces as needed
    (println)));extra lines for better readability

(defn print-plans [plans]
  (doseq [plan plans]
    (let [{:keys [flight total-cost]} plan
          output-format (output flight)] ;; Generate the output format for the flight plan using the 'output' function.
      (print-ticket output-format total-cost (count flight))))) ;; Print the flight plan with its output format and total cost.
