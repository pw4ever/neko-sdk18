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

(ns neko.ui.traits
  "Contains trait declarations for various UI elements."
  (:require [neko.ui.mapping :as kw]
            [neko.resource :as res])
  (:use [neko.ui :only [deftrait]]
        [neko.listeners.view :only [on-click-call]])
  (:import [android.widget LinearLayout$LayoutParams]
           [android.view ViewGroup$LayoutParams]))

;; ### Def attribute

(deftrait :def
  "Takes a symbol provided to `:def` and binds the object to it.

  Example: `[:button {:def ok}]` defines a var `ok` which stores the
  button object."
  (fn [_ obj attributes code __]
    [(dissoc attributes :def)
     (conj code `(def ~(:def attributes) ~obj))]))

;; ### Basic traits

(deftrait :text
  "Sets the element's text to a string, integer ID or a keyword
  representing the string resource provided to `:text` attribute."
  (fn [_ obj attributes code __]
    (let [value (:text attributes)]
     [(dissoc attributes :text)
      (conj code `(.setText ~obj ~(if (string? value)
                                    value
                                    `(res/get-string ~value))))])))

;; ### Layout parameters attributes

(defn default-layout-params
  "Construct LayoutParams instance from the given arguments. Arguments
could be either actual values or keywords `:fill` and `:wrap`."
  ([width height]
     (let [real-width (kw/value :layout-params (or width :wrap))
           real-height (kw/value :layout-params (or height :wrap))]
       `(new ~ViewGroup$LayoutParams ~real-width ~real-height))))

(defn linear-layout-params
  "Construct LinearLayout-specific LayoutParams instance from the
given arguments. Arguments could be either actual values or keywords
`:fill` and `:wrap`."
  ([width height weight]
     (let [real-width (kw/value :layout-params (or width :wrap))
           real-height (kw/value :layout-params (or height :wrap))
           real-weight (or weight 0)]
       `(new ~LinearLayout$LayoutParams ~real-width
             ~real-height ~real-weight))))

(deftrait :layout-params
  "Takes `:layout-width`, `:layout-height` and `:layout-weight`
  attributes and sets a proper LayoutParams according to container
  type."
  #(some #{:layout-width :layout-height :layout-weight} (keys %))
  (fn [_ obj {:keys [layout-width layout-height layout-weight] :as attributes}
      code container]
    [(dissoc attributes :layout-width :layout-height :layout-weight)
     (case container
      :linear-layout
      (conj code
            `(.setLayoutParams ~obj
              ~(linear-layout-params layout-width layout-height layout-weight)))

      (conj code
            `(.setLayoutParams ~obj
              ~(default-layout-params layout-width layout-height))))]))

;; ### Listener traits

(deftrait :on-click
  "Takes :on-click attribute, which should be function, and creates a
  ClickListener from it."
  (fn [_ obj attributes code __]
    [(dissoc attributes :on-click)
     (conj code
           `(.setOnClickListener ~obj
                                 (on-click-call ~(:on-click attributes))))]))
