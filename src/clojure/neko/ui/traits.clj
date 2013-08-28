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
            [neko.listeners.view :as view-listeners]
            neko.listeners.search-view)
  (:use [neko.-utils :only [memoized]])
  (:import [android.widget LinearLayout$LayoutParams TextView SearchView]
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

(defn- add-attributes-to-meta
  "Appends information about attribute to trait mapping to `meta`."
  [meta attr-list trait]
  (reduce (fn [m att]
            (update-in m [:attributes att]
                       #(if %
                          (conj % trait)
                          #{trait})))
          meta attr-list))

(defmacro deftrait
  "Defines a trait with the given name.

  `match-pred` is a function on attributes map that should return a
  logical truth if this trait should be executed against the widget
  and the map. By default it checks if attribute with the same name as
  trait's is present in the attribute map.

  The parameter list is the following: `[widget attributes-map
  options-map]`.

  Body of the trait can optionally return a map with the following
  keys: `:attribute-fn`, `:options-fn`, which values are functions to
  be applied to attributes map and options map respectively after the
  trait finishes its work. If they are not provided, `attribute-fn`
  defaults to dissoc'ing trait's name from attribute map, and
  `options-fn` defaults to identity function."
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [param-map args] (if (map? (first args))
                           [(first args) (next args)]
                           [{} args])
        match-pred (cond (:applies? param-map)
                         (:applies? param-map)

                         (:attributes param-map)
                         `(fn [kw#] (some kw# ~(:attributes param-map)))

                         :else name)
        codegen-body args
        dissoc-fn (if (:attributes param-map)
                    `(fn [a#] (dissoc a# ~@(:attributes param-map)))
                    `(fn [a#] (dissoc a# ~name)))]
    `(do
       (alter-meta! #'apply-trait
                    (fn [m#]
                      (-> m#
                          (assoc-in [:trait-doc ~name] ~docstring)
                          (add-attributes-to-meta
                           (or ~(:attributes param-map) [~name]) ~name))))
       (defmethod apply-trait ~name
         [trait# widget# attributes# options#]
         (if (~match-pred attributes#)
           (let [result# ((fn ~@codegen-body) widget# attributes# options#)
                 attr-fn# ~dissoc-fn]
             (if (map? result#)
               [(:attributes-fn result# attr-fn#)
                (:options-fn result# identity)]
               [attr-fn# identity]))
           [identity identity])))))

(alter-meta! #'deftrait
             assoc :arglists '([name docstring? match-pred? [params*] body]))

;; ## Implementation of different traits

;; ### Def attribute

(deftrait :def
  "Takes a symbol provided to `:def` and binds the widget to it.

  Example: `[:button {:def ok}]` defines a var `ok` which stores the
  button object."
  [wdg {:keys [def]} _]
  (assert (and (symbol? def) (namespace def)))
  (intern (symbol (namespace def)) (symbol (name def)) wdg))

;; ### Basic traits

(deftrait :text
  "Sets widget's text to a string, integer ID or a keyword
  representing the string resource provided to `:text` attribute."
  [^TextView wdg, {:keys [text]} _]
  (.setText wdg ^CharSequence (res/get-string text)))

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

(defn to-dimension [value]
  (if (vector? value)
    (Math/round
     ^float (TypedValue/applyDimension (kw->unit-id (second value))
                                       (first value) (get-display-metrics)))
    value))

(deftrait :text-size
  "Takes `:text-size` attribute which should be either integer or a
  dimenstion vector, and sets it to the widget."
  [^TextView wdg, {:keys [text-size]} _]
  (.setTextSize wdg (to-dimension text-size)))

;; ### Layout parameters attributes

(defn- default-layout-params
  "Construct LayoutParams instance from the given arguments. Arguments
  could be either actual values or keywords `:fill` and `:wrap`."
  [width height]
  (ViewGroup$LayoutParams. ^int (kw/value :layout-params (or width :wrap))
                           ^int (kw/value :layout-params (or height :wrap))))

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
  {:attributes [:layout-width :layout-height :layout-weight]}
  [^View wdg, {:keys [layout-width layout-height layout-weight]}
   {:keys [container-type]}]
  (case (kw/keyword-by-classname container-type)
    :linear-layout
    (.setLayoutParams
     wdg (linear-layout-params layout-width layout-height layout-weight))

    (.setLayoutParams
     wdg (default-layout-params layout-width layout-height))))

(deftrait :padding
  "Takes `:padding`, `:padding-bottom`, `:padding-left`,
  `:padding-right` and `:padding-top` and set element's padding
  according to their values. Values might be either integers or
  vectors like `[number unit-kw]`, where unit keyword is one of the
  following: :px, :dip, :sp, :pt, :in, :mm."
  {:attributes [:padding :padding-bottom :padding-left
                :padding-right :padding-top]}
  [wdg {:keys [padding padding-bottom padding-left
               padding-right padding-top]} _]
  (.setPadding ^View wdg
               (to-dimension (or padding-left padding 0))
               (to-dimension (or padding-top padding 0))
               (to-dimension (or padding-right padding 0))
               (to-dimension (or padding-bottom padding 0))))

(deftrait :container
  "Puts the type of the widget onto the options map so subelement can
  use the container type to choose the correct LayoutParams instance."
  {:applies? (constantly true)}
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

(deftrait :on-query-text
  "Takes `:on-query-text-change` and `:on-query-text-submit`
  attributes, which should be functions of one or two arguments,
  depending on the context of usage. If widget is used as an action
  item in a menu, two arguments are passed to the function - query
  text and the menu item, for which widget is being action item to.
  Otherwise only query text is passed to the functions.

  Then OnQueryTextListener object is created from the functions and
  set to the widget."
  {:attributes [:on-query-text-change :on-query-text-submit]}
  [^SearchView wdg, {:keys [on-query-text-change on-query-text-submit]}
   {:keys [menu-item]}]
  (.setOnQueryTextListener
   wdg (neko.listeners.search-view/on-query-text-call
        (if (and menu-item on-query-text-change)
          (fn [q] (on-query-text-change q menu-item))
          on-query-text-change)
        (if (and menu-item on-query-text-submit)
          (fn [q] (on-query-text-submit q menu-item))
          on-query-text-submit))))

;; ### ID storing traits

(deftrait :id-holder
  "Takes `:id-holder` attribute which should equal true and marks the
  widget to be a holder of lower-level elements. Elements are stored
  by their IDs as keys in a map, which is accessible by calling
  `.getTag` on the holder widget.

  Example:

  (def foo (make-ui [:linear-layout {:id-holder true}
                     [:button {:id ::abutton}]]))
  (::abutton (.getTag foo)) => internal Button widget."
  [^View wdg, _ __]
  (.setTag wdg (HashMap.))
  {:options-fn #(assoc % :id-holder wdg)})

(defn to-id
  "Makes an ID from arbitrary object by calling .hashCode on it."
  [obj]
  (.hashCode ^Object obj))

(deftrait :id
  "Takes `:id` attribute, which can either be an integer or a
  keyword (that would be transformed into integer as well) and sets it
  as widget's ID attribute. Also, if an ID holder was declared in
  this tree, stores the widget in id-holder's tag (see docs for
  `:id-holder`trait)."
  [^View wdg, {:keys [id]} {:keys [^View id-holder]}]
  (.setId wdg (to-id id))
  (when id-holder
    (.put ^HashMap (.getTag id-holder) id wdg)))
