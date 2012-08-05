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
            [neko.listeners view])
  (:import [android.widget LinearLayout$LayoutParams]
           [android.view ViewGroup$LayoutParams]
           java.util.HashMap))

;; ## Infrastructure for traits and attributes

(defmulti transform-attributes
  "Transforms the given map of attributes into the valid Java-interop
code of setters.

`trait` is the keyword for a transformer function over the attribute map.

`object-symbol` is an symbol for the UI element to apply setters to.

`attributes-map` is a map of attribtues to their values.

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
  (fn [trait object-symbol attributes-map generated-code options-map]
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
        match-pred (if (> (count args) 1)
                     (first args) name)
        codegen-body (last args)
        dissoc-fn `(fn [a#] (dissoc a# ~name))]
    `(do
       (alter-meta! #'transform-attributes
                    assoc-in [:trait-doc ~name] ~docstring)
       (defmethod transform-attributes ~name
         [trait# object# attributes# generated-code# options#]
         (if (~match-pred attributes#)
           (let [result# (~codegen-body object# attributes#
                                        generated-code# options#)]
             (if (map? result#)
               [(:code result#) (:attributes-fn result# ~dissoc-fn)
                (:options-fn result# identity)]
               [result# ~dissoc-fn identity]))
           [generated-code# identity identity])))))

;; ## Implementation of different traits

;; ### Def attribute

(deftrait :def
  "Takes a symbol provided to `:def` and binds the object to it.

  Example: `[:button {:def ok}]` defines a var `ok` which stores the
  button object."
  (fn [obj attributes code _]
    (conj code `(def ~(:def attributes) ~obj))))

;; ### Basic traits

(deftrait :text
  "Sets the element's text to a string, integer ID or a keyword
  representing the string resource provided to `:text` attribute."
  (fn [obj {:keys [text]} code _]
    (conj code `(.setText ~obj ~(if (keyword? text)
                                  `(res/get-string ~text)
                                  text)))))

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
  (fn [obj {:keys [layout-width layout-height layout-weight]}
      code {:keys [container-type]}]
    {:attributes-fn #(dissoc % :layout-width :layout-height :layout-weight)
     :code
     (case container-type
       :linear-layout
       (conj code
             `(.setLayoutParams ~obj
                                ~(linear-layout-params
                                  layout-width layout-height layout-weight)))

       (conj code
             `(.setLayoutParams ~obj
                                ~(default-layout-params
                                   layout-width layout-height))))}))

;; ### Listener traits

(deftrait :on-click
  "Takes :on-click attribute, which should be function of one
  argument, and sets it as an OnClickListener for the object."
  (fn [obj {:keys [on-click]} code _]
    (conj code
          `(.setOnClickListener ~obj
            (neko.listeners.view/on-click-call ~on-click)))))

(deftrait :on-create-context-menu
  "Takes :on-create-context-menu attribute, which should be function
  of three arguments, and sets it as an OnCreateContextMenuListener
  for the object."
  (fn [obj {:keys [on-create-context-menu]} code _]
    (conj code
          `(.setOnCreateContextMenuListener ~obj
            (neko.listeners.view/on-create-context-menu-call
             ~on-create-context-menu)))))

(deftrait :on-focus-change
  "Takes :on-focus-change attribute, which should be function of two
  arguments, and sets it as an OnFocusChangeListener for the object."
  (fn [obj {:keys [on-focus-change]} code _]
    (conj code
          `(.setOnFocusChangeListener ~obj
            (neko.listeners.view/on-focus-change-call ~on-focus-change)))))

(deftrait :on-key
  "Takes :on-key attribute, which should be function of three
  arguments, and sets it as an OnKeyListener for the object."
  (fn [obj {:keys [on-key]} code _]
    (conj code
          `(.setOnKeyListener ~obj
            (neko.listeners.view/on-key-call ~on-key)))))

(deftrait :on-long-click
  "Takes :on-long-click attribute, which should be function of one
  argument, and sets it as an OnLongClickListener for the object."
  (fn [obj {:keys [on-long-click]} code _]
    (conj code
          `(.setOnLongClickListener ~obj
            (neko.listeners.view/on-long-click-call ~on-long-click)))))

(deftrait :on-touch
  "Takes :on-touch attribute, which should be function of two
  arguments, and sets it as an OnTouchListener for the object."
  (fn [obj {:keys [on-touch]} code _]
    (conj code
          `(.setOnTouchListener ~obj
            (neko.listeners.view/on-touch-call ~on-touch)))))

;; ### ID storing traits

(deftrait :id-holder
  "Takes `:id-holder` attribute which should equal true and marks the
  object to be a holder of lower-level element IDs. IDs are stored in
  a map which is accessible by calling `.getTag` on the holder object.

  Example:

  (def foo (defui [:linear-layout {:id-holder true}
                   [:button {:id ::abutton}]]))
  (::abutton (.getTag foo)) => internal Button object."
  (fn [obj _ code __]
    {:options-fn #(assoc % :id-holder obj)
     :code
     (conj code
           `(.setTag ~obj (HashMap.)))}))

(deftrait :id
  "Takes `:id` attribute, which should be a keyword, and stores the
  current object in ID-holder's tag (see docs for `:id-holder`
  trait)."
  (fn [obj {:keys [id]} code {:keys [id-holder]}]
    (when (nil? id-holder)
      (throw (Exception. ":id trait: id-holder is undefined in this UI tree.")))
    (conj code
          `(.put (.getTag ~id-holder) ~id ~obj))))
