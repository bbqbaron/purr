(ns purr.client
  (:require [purr.run :refer [run]]
            [rum.core :as rum]))

(enable-console-print!)

(def state
  (atom
    {}))

(defn- get-val
  "Gets value from a text change."
  [event]
  (-> event .-target .-value))

(rum/defc compiler < rum/reactive []
  (let [{:keys [expr error]} (rum/react state)]
    [:div
     [:h3 "Type some code!"]
     [:textarea
      {:auto-focus true
       :on-change (fn [evt]
                    (let [val (get-val evt)
                          result (try
                                   {:expr  (run val)
                                    :error nil}
                                   (catch js/Error e
                                     {:error (.-message e)
                                      :expr  nil}))]
                      (swap! state
                             (fn [old]
                               (-> old
                                   (assoc :code val)
                                   (merge result))))))
       :rows 5}]
     (when expr
       [:p "Result: " expr])
     (when error
       [:p "Error: " error])]))

(rum/mount (compiler) (.-body js/document))