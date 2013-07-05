(ns neko.notify
  "Provides convenient wrappers for Toast and Notification APIs."
  (:use [neko.context :only [context]])
  (:import android.content.Context android.widget.Toast
           android.app.Notification android.content.Intent
           android.app.PendingIntent android.app.NotificationManager))

;; ### Toasts

(def ^{:doc "Stores constants that represent toast's visible timespan."
       :private true}
  toast-length {:short Toast/LENGTH_SHORT
                :long Toast/LENGTH_LONG})

(defn toast
  "Creates a Toast object using a text message and a keyword
  representing how long a toast should be visible (`:short` or
  `:long`). The application context will be used. One-argument version
  takes only message and assumes length to be :long."
  ([message]
     (toast message :long))
  ([^String message, length]
     {:pre [(contains? toast-length length)]}
     (.show
      ^Toast (Toast/makeText context message ^int (toast-length length)))))

;; ### Notifications

(def ^:private default-notification-icon (atom nil))

(defn set-default-notification-icon! [icon]
  (reset! default-notification-icon icon))

(defn- ^NotificationManager notification-manager
  "Returns the notification manager instance."
  []
  (.getSystemService context Context/NOTIFICATION_SERVICE))

(defn construct-pending-intent
  "Creates a PendingIntent instance from a vector where the first
  element is a keyword representing the action type, and the second
  element is a action string to create an Intent from."
  [[action-type, ^String action]]
  (let [^Intent intent (Intent. action)]
    (case action-type
      :activity (PendingIntent/getActivity context 0 intent 0)
      :broadcast (PendingIntent/getBroadcast context 0 intent 0)
      :service (PendingIntent/getService context 0 intent 0))))

(defn notification
  "Creates a Notification instance. If icon is not provided uses the
  default notification icon."
  [& {:keys [icon ticker-text when content-title content-text action]
      :or {icon @default-notification-icon, when (System/currentTimeMillis)}}]
  {:pre [icon]}
  (let [notification (Notification. icon ticker-text when)]
    (.setLatestEventInfo notification context content-title content-text
                         (construct-pending-intent action))
    notification))

;; This atom stores the mapping of keywords to integer IDs that
;; represent the notification IDs.
;;
(def ^:private notification-ids (atom {}))

;; A simple counter that will increment by one after each call.
;;
(def ^:private new-id
  (let [ctr (atom 0)]
    (fn []
      (swap! ctr inc)
      @ctr)))

(defn fire
  "Sends the notification to the status bar. ID is optional and could be
  either an integer or a keyword."
  ([notification]
     (.notify (notification-manager) (new-id) notification))
  ([id notification]
     (let [id (if (keyword? id)
                (if (contains? @notification-ids id)
                  (@notification-ids id)
                  (let [number-id (new-id)]
                    (swap! notification-ids assoc id number-id)
                    number-id))
                id)]
       (.notify (notification-manager) id notification))))

(defn cancel
  "Removes a notification by the given ID from the status bar."
  [id]
  (let [id (if (keyword? id)
             (@notification-ids id)
             id)]
    (.cancel (notification-manager) id)))
