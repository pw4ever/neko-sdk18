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
            [neko.resource :as res]
            [neko.context :as context]
            [neko.listeners.view :as view-listeners])
  (:use [neko.-utils :only [memoized]])
  (:import [android.widget LinearLayout$LayoutParams TextView]
           [android.view View ViewGroup$LayoutParams]
           android.util.TypedValue
           java.util.HashMap))

;; ## Infrastructure for traits and attributes

(defmulti apply-trait
  "Transforms the given map of attributes into the valid Java-interop
code of setters.

`trait` is the keyword for a transformer function over the attribute map.

`object-symbol` is an symbol for the UI element to apply setters to.

`attributes-map` is a map of attributes to their values.

`generated-code` is an attribute-setter code generated so far. The
code this method generates should be appended to it.

`options-map` is a map of additional options that come from higher
level elements to their inside elements. A transformer can use this
map to provide some arguments to its own inside elements.

 Returns a vector that looks like `[new-generated-code
attributes-update-fn options-update-fn]`. `attributes-update-fn`
should take attributes map and remove processed attributes from it.
`options-update-fn` should remove old or introduce new options for
next-level elements."
  (fn [trait widget attributes-map options-map]
    trait))

(defmacro deftrait
  "Defines a trait with the given name.

  `match-pred` is a function on attributes map that should return a
  logical truth if this trait should be executed against the argument
  list. By default it checks if attribute with the same name as
  trait's is present in the attribute map.

  `codegen-fn` is a function that should take the same arguments as
  `transform-attributes` and return either the generated code directly
  or a map with the following keys: `:code`, `:attribute-fn`,
  `:options-fn`. In the former case `attribute-fn` defaults to
  dissoc'ing trait's name from attribute map, and `options-fn`
  defaults to identity function."
  ^{:arglists '([name docstring? match-pred? codegen-fn])}
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [match-pred args] (if-not (vector? (first args))
                            [(first args) (next args)]
                            [name args])
        codegen-body args
        dissoc-fn `(fn [a#] (dissoc a# ~name))]
    `(do
       (alter-meta! #'apply-trait
                    assoc-in [:trait-doc ~name] ~docstring)
       (defmethod apply-trait ~name
         [trait# widget# attributes# options#]
         (if (~match-pred attributes#)
           (let [result# ((fn ~@codegen-body) widget# attributes# options#)]
             (if (map? result#)
               [(:attributes-fn result# ~dissoc-fn)
                (:options-fn result# identity)]
               [~dissoc-fn identity]))
           [identity identity])))))

;; ## Implementation of different traits

;; ### Def attribute

(deftrait :def
  "Takes a symbol provided to `:def` and binds the widget to it.

  Example: `[:button {:def ok}]` defines a var `ok` which stores the
  button object."
  [wdg {:keys [def]} _]
  (intern (symbol (namespace def)) (symbol (name def)) wdg))

;; ### Basic traits

(deftrait :text
  "Sets widget's text to a string, integer ID or a keyword
  representing the string resource provided to `:text` attribute."
  [^TextView wdg, {:keys [text]} _]
  (if (keyword? text)
    (.setText wdg ^int (res/get-string text))
    (.setText wdg ^CharSequence text)))

;; ### Layout parameters attributes

(defn- default-layout-params
  "Construct LayoutParams instance from the given arguments. Arguments
  could be either actual values or keywords `:fill` and `:wrap`."
  [width height]
  (ViewGroup$LayoutParams. (kw/value :layout-params (or width :wrap))
                           (kw/value :layout-params (or height :wrap))))

(defn- linear-layout-params
  "Construct LinearLayout-specific LayoutParams instance from the
  given arguments. Arguments could be either actual values or keywords
  `:fill` and `:wrap`."
  [width height weight]
  (LinearLayout$LayoutParams. (kw/value :layout-params (or width :wrap))
                              (kw/value :layout-params (or height :wrap))
                              (or weight 0)))

(deftrait :layout-params
  "Takes `:layout-width`, `:layout-height` and `:layout-weight`
  attributes and sets a proper LayoutParams according to container
  type."
  #(some % [:layout-width :layout-height :layout-weight])
  [^View wdg, {:keys [layout-width layout-height layout-weight]}
   {:keys [container-type]}]
  (case (kw/keyword-by-classname container-type)
    :linear-layout
    (.setLayoutParams
     wdg (linear-layout-params layout-width layout-height layout-weight))

    (.setLayoutParams
     wdg (default-layout-params layout-width layout-height)))
  {:attributes-fn #(dissoc % :layout-width :layout-height
                           :layout-weight)})

(defn- kw->unit-id [unit-kw]
  (case unit-kw
    :px TypedValue/COMPLEX_UNIT_PX
    :dp TypedValue/COMPLEX_UNIT_DIP
    :dip TypedValue/COMPLEX_UNIT_DIP
    :sp TypedValue/COMPLEX_UNIT_SP
    :pt TypedValue/COMPLEX_UNIT_PT
    :in TypedValue/COMPLEX_UNIT_IN
    :mm TypedValue/COMPLEX_UNIT_MM
    TypedValue/COMPLEX_UNIT_PX))

(memoized
 (defn- get-display-metrics
   "Returns Android's DisplayMetrics object from application context."
   []
   (.. context/context (getResources) (getDisplayMetrics))))

(defn- padding-value->pixels [value]
  (if (vector? value)
    (Math/round ^float
     (TypedValue/applyDimension (kw->unit-id (second value)) (first value)
                                (get-display-metrics)))
    value))

(deftrait :padding
  "Takes `:padding`, `:padding-bottom`, `:padding-left`,
  `:padding-right` and `:padding-top` and set element's padding
  according to their values. Values might be either integers or
  vectors like `[number unit-kw]`, where unit keyword is one of the
  following: :px, :dip, :sp, :pt, :in, :mm."
  #(some % [:padding :padding-bottom :padding-left :padding-right :padding-top])
  [wdg {:keys [padding padding-bottom padding-left
               padding-right padding-top]} _]
  (.setPadding ^View wdg
               (padding-value->pixels (or padding-left padding 0))
               (padding-value->pixels (or padding-top padding 0))
               (padding-value->pixels (or padding-right padding 0))
               (padding-value->pixels (or padding-bottom padding 0)))
  {:attributes-fn #(dissoc % :padding :padding-bottom :padding-left
                           :padding-right :padding-top)})

(deftrait :container
  "Puts the type of the widget onto the options map so subelement can
  use the container type to choose the correct LayoutParams instance."
  (constantly true)
  [wdg _ __]
  {:options-fn #(assoc % :container-type (type wdg))})

;; ### Listener traits

(deftrait :on-click
  "Takes :on-click attribute, which should be function of one
  argument, and sets it as an OnClickListener for the widget."
  [^View wdg, {:keys [on-click]} _]
  (.setOnClickListener wdg (view-listeners/on-click-call on-click)))

(deftrait :on-create-context-menu
  "Takes :on-create-context-menu attribute, which should be function
  of three arguments, and sets it as an OnCreateContextMenuListener
  for the object."
  [^View wdg, {:keys [on-create-context-menu]} _]
  (.setOnCreateContextMenuListener
   wdg (view-listeners/on-create-context-menu-call on-create-context-menu)))

(deftrait :on-focus-change
  "Takes :on-focus-change attribute, which should be function of two
  arguments, and sets it as an OnFocusChangeListener for the object."
  [^View wdg, {:keys [on-focus-change]} _]
  (.setOnFocusChangeListener
   wdg (view-listeners/on-focus-change-call on-focus-change)))

(deftrait :on-key
  "Takes :on-key attribute, which should be function of three
  arguments, and sets it as an OnKeyListener for the widget."
  [^View wdg, {:keys [on-key]} _]
  (.setOnKeyListener wdg (view-listeners/on-key-call on-key)))

(deftrait :on-long-click
  "Takes :on-long-click attribute, which should be function of one
  argument, and sets it as an OnLongClickListener for the widget."
  [^View wdg, {:keys [on-long-click]} _]
  (.setOnLongClickListener
   wdg (view-listeners/on-long-click-call on-long-click)))

(deftrait :on-touch
  "Takes :on-touch attribute, which should be function of two
  arguments, and sets it as an OnTouchListener for the widget."
  [^View wdg, {:keys [on-touch]} _]
  (.setOnTouchListener wdg (view-listeners/on-touch-call on-touch)))

;; ### ID storing traits

(deftrait :id-holder
  "Takes `:id-holder` attribute which should equal true and marks the
  widget to be a holder of lower-level element IDs. IDs are stored in
  a map which is accessible by calling `.getTag` on the holder widget.

  Example:

  (def foo (make-ui [:linear-layout {:id-holder true}
                     [:button {:id ::abutton}]]))
  (::abutton (.getTag foo)) => internal Button widget."
  [^View wdg, _ __]
  (.setTag wdg (HashMap.))
  {:options-fn #(assoc % :id-holder wdg)})

(deftrait :id
  "Takes `:id` attribute, which should be a keyword, and stores the
  current widget in ID-holder's tag (see docs for `:id-holder`
  trait)."
  [wdg {:keys [id]} {:keys [^View id-holder]}]
  (if (nil? id-holder)
    (throw (Exception. ":id trait: id-holder is undefined in this UI tree."))
    (.put ^HashMap (.getTag id-holder) id wdg)))
