(ns clojure_airlines.broker.sales_routine
  (:require [clojure_airlines.broker.broker :as broker])
  ; TODO replace this link with your engine
  (:require [clojure_airlines.broker.search-engine :as your_engine])
  )

; TODO SET YOUR TEAM NUMBER: 1-7
(def team_number 4)
(def search_ticket_function your_engine/prepare_travel_plan)
(broker/run team_number search_ticket_function)