; Copyright © 2012 Alexander Yakushev.
; All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.ui.adapters
  "Contains custom adapters for ListView and Spinner."
  (:use [neko.threading :only [on-ui]])
  (:import neko.ui.adapters.InterchangeableListAdapter))

(defn ref-adapter
  "Takes a function that creates a View, a function that updates a
  view according to the element and a reference type that stores the
  data. Returns an Adapter object that displays ref-type contents.
  When ref-type is updated, Adapter gets updated as well.

  `create-view-fn` is a function of no arguments. `update-view-fn` is
  a function of four arguments: element position, view to update,
  parent view container and the respective data element from the
  ref-type. `access-fn` argument is optional, it is called on the
  value of ref-type to get the list to be displayed."
  ([create-view-fn update-view-fn ref-type]
     (ref-adapter create-view-fn update-view-fn ref-type identity))
  ([create-view-fn update-view-fn ref-type access-fn]
     {:pre [(fn? create-view-fn) (fn? update-view-fn)
            (instance? clojure.lang.IFn access-fn)
            (instance? clojure.lang.IDeref ref-type)]}
     (let [adapter (InterchangeableListAdapter. create-view-fn update-view-fn
                                                (access-fn @ref-type))]
       (add-watch ref-type ::adapter-watch
                  (fn [_ __ ___ new-state]
                    (on-ui (.setData adapter (access-fn new-state)))))
       adapter)))
