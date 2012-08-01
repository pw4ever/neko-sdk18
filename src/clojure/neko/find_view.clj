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

(ns neko.find-view
  "Home of the ViewFinder protocol, which should simplify and unify use of the
  findViewById method introduced in various Android classes.  To use the
  protocol, you need an object that supports findViewById, and these are:

  + activities
  + dialogs
  + views
  + windows

  Given one of these objects, you can (find-view obj id).

  In addition, if within a with-activity form, you can leave out the activity
  argument, simplifying the above call to:

    (find-view R$id/my_view)"
  {:author "Daniel Solano Gómez"}
  (:use neko.activity))

(defn- nil-or-view?
  [x]
  (or (nil? x)
      (instance? android.view.View x)))

(defprotocol ViewFinder
  "Protocol for finding child views by an ID."
  (find-view [id] [finder id]
    "The two-arg version is the general version used outside of any context.
    The one-arg version is designed for use within a (with-activity)
    context."))

(extend-protocol ViewFinder
  android.view.Window
  (find-view [window id]
    {:pre  [(integer? id)]
     :post [(nil-or-view? %)]}
    (.findViewById window id))

  android.app.Activity
  (find-view [activity id]
    {:pre  [(integer? id)]
     :post [(nil-or-view? %)]}
    (.findViewById activity id))

  android.view.View
  (find-view [view id]
    {:pre  [(integer? id)]
     :post [(nil-or-view? %)]}
    (.findViewById view id))

  android.app.Dialog
  (find-view [dialog id]
    {:pre  [(integer? id)]
     :post [(nil-or-view? %)]}
    (.findViewById dialog id))

  Integer
  (find-view [id]
    {:pre [(has-*activity*?)]
     :post [(nil-or-view? %)]}
    (find-view *activity* id))

  Long
  (find-view [id]
    {:pre [(has-*activity*?)]
     :post [(nil-or-view? %)]}
    (find-view *activity* (.intValue id))))
