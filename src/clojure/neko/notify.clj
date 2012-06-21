(ns neko.notify
  "Provides convenient wrappers for Toast and Notification APIs."
  (:require [neko.application :as app])
  (:import clojure.lang.Keyword
           android.content.Context android.widget.Toast))

;; Stores constants that represent toast's visible timespan.
;;
(def ^:private toast-length {:short Toast/LENGTH_SHORT
                             :long Toast/LENGTH_LONG})

(defn toast
  "Creates a Toast object using a text message and a keyword
  representing how long a toast should be visible (`:short` or
  `:long`). The application context wiil be used."
  ^Toast [message length]
  {:pre [(contains? toast-length length)]}
  (Toast/makeText app/context message (toast-length length)))
