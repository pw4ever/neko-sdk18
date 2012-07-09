; Copyright © 2011 Sattvik Software & Technology Resources, Ltd. Co.
; All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.activity
  "Utilities to aid in working with an activity."
  {:author "Daniel Solano Gómez"}
  (:import android.app.Activity
           android.view.View)
  (:require [neko.context :as context])
  (:use neko.-protocols.resolvable
        neko.-utils
        [neko.debug :only [safe-for-ui]]
        [neko.context :only [*context*]]))

(def 
  ^{:doc "The current activity to operate on."
    :dynamic true}
  *activity*)

(defmacro with-activity
  "Evaluates body such that both *activity* and *context* are bound to the given activiy."
  [activity & body]
  `(let [activity# ~activity]
     (binding [context/*context* activity#
               *activity* activity#]
       ~@body)))

(defn activity?
  "Determines whether the argument is an instance of Activity."
  [x]
  (instance? Activity x))

(defn has-*activity*?
  "Ensures that the calling context has a valid *activity* var."
  []
  (and (bound? #'*activity*)
       (activity? *activity*)))

(defn set-content-view!
  "Sets the content for the activity.  The view may be one of:

  + A view object, which will be used directly
  + An integer presumed to be a valid layout ID
  + A keyword used to resolve to a layout ID using
    (neko.context/resolve-resource)"
  ([view]
   {:pre [(or (instance? View view)
              (resolvable? view))]}
   (set-content-view! *activity* view))
  ([^Activity activity view]
   {:pre [(activity? activity)
          (or (instance? View view)
              (resolvable? view))]}
   (cond
     (instance? View view)
       (.setContentView activity ^View view)
     (integer? view)
       (.setContentView activity ^Integer view)
     :else
       (.setContentView activity ^Integer (resolve-layout view activity)))))

(defn request-window-features!
  "Requests the given features for the activity.  The features should be
  keywords such as :no-title or :indeterminate-progress corresponding
  FEATURE_NO_TITLE and FEATURE_INDETERMINATE_PROGRESS, respectively.  Returns a
  sequence of boolean values corresponding to each feature, where a true value
  indicates the requested feature is supported and now enabled.

  If within a with-activity form, supplying an activity as the first argument
  is not necessary.

  This function should be called before set-content-view!."
  {:arglists '([& features] [activity & features])}
  [activity & features]
  {:pre  [(or (activity? activity)
              (and (keyword? activity)
                   (has-*activity*?)))
          (every? keyword? features)]
   :post [%
          (every? (fn [x] (instance? Boolean x)) %)]}
  (let [[^Activity activity features]
          (if (instance? Activity activity)
            [activity features]
            [*activity* (cons activity features)])
        keyword->int (fn [k]
                       (static-field-value android.view.Window
                                           k
                                           #(str "FEATURE_" %)))
        request-feature  (fn [k]
                           (try
                             (.requestWindowFeature activity (keyword->int k))
                             (catch NoSuchFieldException _
                               (throw (IllegalArgumentException.
                                        (format "‘%s’ is not a valid feature."
                                                k))))))]
    (doall (map request-feature features))))

(defmacro defactivity
  "Creates an activity with the given full package-qualified name.
  Optional arguments should be provided in a key-value fashion.

  Available optional arguments:

  :extends, :prefix - same as for `gen-class`.

  :def - symbol to bind the Activity object to in the onCreate
  method. Relevant only if :create is used.

  :create - takes a two-argument function. Generates a handler for
  activity's `onCreate` event which automatically calls the
  superOnCreate method and creates a var with the name denoted by
  `:def` (or activity's lower-cased name by default) to store the
  activity object. Then calls the provided function onto the
  Application object.

  :start, :restart, :resume, :pause, :stop, :destroy - same as :create
  but require a one-argument function."
  [name & {:keys [extends prefix on-create def] :as options}]
  (let [options (or options {}) ;; Handle no-options case
        sname (simple-name name)
        prefix (or prefix (str sname "-"))
        def (or def (symbol (unicaseize sname)))]
    `(do
       (gen-class
        :name ~name
        :main false
        :prefix ~prefix
        :extends ~(or extends Activity)
        :exposes-methods {~'onCreate ~'superOnCreate
                          ~'onStart ~'superOnStart
                          ~'onRestart ~'superOnRestart
                          ~'onResume ~'superOnResume
                          ~'onPause ~'superOnPause
                          ~'onStop ~'superOnStop
                          ~'onDestroy ~'superOnDestroy})
       ~(when on-create
          `(defn ~(symbol (str prefix "onCreate"))
             [~(vary-meta 'this assoc :tag name),
              ^android.os.Bundle ~'savedInstanceState]
             (.superOnCreate ~'this ~'savedInstanceState)
             (def ~(vary-meta def assoc :tag name) ~'this)
             (~on-create ~'this ~'savedInstanceState)))
       ~@(map #(let [func (options %)
                     event-name (keyword->camelcase %)]
                 (when func
                   `(defn ~(symbol event-name)
                      [~(vary-meta 'this assoc :tag name)]
                      (~(symbol (str ".super" (capitalize event-name))) ~'this)
                      (~func ~'this))))
              [:on-start :on-restart :on-resume
               :on-pause :on-stop :on-destroy]))))

(defn on-ui*
  "Runs the given nullary function on the UI thread.  If this function is
  called on the UI thread, it will evaluate immediately.

  Also dynamically binds the current values of `*activity*` and
  `*context*` inside the thread. If they were not bound in the thread
  called from, the values of `*activity*` and `*context*` would be
  bound to the `activity` argument."
  [^Activity activity f]
  (let [dynamic-activity (if (bound? #'*activity*) *activity* activity)
        dynamic-context (if (bound? #'*context*) *context* activity)]
    (.runOnUiThread activity
                    (reify Runnable
                      (run [this]
                        (binding [*activity* dynamic-activity
                                  *context* dynamic-context]
                          (safe-for-ui (f))))))))

(defmacro on-ui
  "Runs the macro body on the UI thread.  If this macro is called on the UI
  thread, it will evaluate immediately."
  [activity & body]
  `(on-ui* ~activity (fn [] ~@body)))
