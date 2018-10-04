(ns purr.client
  (:require [purr.run :refer [run]]
            [rum.core :as rum]))

(enable-console-print!)

(def state
  (atom
    {}))

(defn- get-val [event]
  (-> event .-target .-value))

(rum/defc compiler < rum/reactive []
  (let [{:keys [expr error]} (rum/react state)]
    [:div
     [:h3 "type some code!"]
     [:input {:on-change (fn [evt]
                           (let [val (get-val evt)]
                             (swap! state
                                    (fn [old]
                                      (-> old
                                          (assoc :code val)
                                          (assoc :expr (run val)))))))}]
     (when expr
       [:p "Result: " expr])
     (when error
       [:p "Error: " error])]))

(rum/mount (compiler) (.-body js/document))