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

(ns neko.ui
  "Tools for defining and manipulating Android UI elements."
  (:use [neko.context :only [context]]
        [neko.-utils :only [capitalize keyword->setter]]
        [neko.listeners.view :only [on-click-call]])
  (:require [neko.ui.mapping :as kw])
  (:import [android.widget LinearLayout$LayoutParams]
           [android.view ViewGroup$LayoutParams]))

;; ## Attributes

(defmulti transform-attributes
  "Transforms the given map of attributes into the valid Java-interop
code of setters.

`el-type` is the keyword for either the UI element type or any of the
trait types.

`object-symbol` is an symbol for the UI element to apply setters to.

`attributes-map` is a map of attribtues to their values.

`generated-code` is an attribute-setter code generated so far. The
code this method generates should be appended to it.

Returns a vector that looks like
`[attributes-without-processed-ones new-generated-code]`. The method
should remove the attributes it processed from the map."
  (fn [el-type object-symbol attributes-map generated-code container-type]
    el-type))

;; ### Def attribute

;; Automatically binds the object with this attribute to the var which
;; name is specified in attribute value.
;; Example: `:def ok-button`
(defmethod transform-attributes :def [_ obj attributes generated-code __]
  [(dissoc attributes :def)
   (if-let [sym (:def attributes)]
     (conj generated-code `(def ~sym ~obj))
     generated-code)])

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

(defmethod transform-attributes :layout-params [el-type obj attributes
                                                generated-code container-type]
  (case container-type
   :linear-layout
   [(dissoc attributes :layout-width :layout-height :layout-weight)
    (conj generated-code
          `(.setLayoutParams ~obj
             ~(linear-layout-params (:layout-width attributes)
                                    (:layout-height attributes)
                                    (:layout-weight attributes))))]

   [(dissoc attributes :layout-width :layout-height :layout-weight)
    (conj generated-code
          `(.setLayoutParams ~obj
             ~(default-layout-params (:layout-width attributes)
                (:layout-height attributes))))]))

(defmethod transform-attributes :on-click [_1 obj attributes generated-code _2]
  (if-let [handler (:on-click attributes)]
    [(dissoc attributes :on-click)
     (conj generated-code
           `(.setOnClickListener ~obj (on-click-call ~handler)))]
    [attributes generated-code]))

;; ### Default attributes

;; If there is no method for the given element type then return
;; unmodified `attributes` and `generated-code`.
;;
(defmethod transform-attributes :default [_1 _2 attributes generated-code _3]
  [attributes generated-code])

(defn default-setters-from-attributes
  "Takes an element type keyword, an object symbol and the attributes
  map after all `transform-attributes` methods have been called on it.
  Transforms each attribute into a call to (.set_CapitalizedKey_ obj
  value). If value is a keyword then it is looked up in the
  keyword-mapping or if it is not there, it is perceived as a static
  field of the class. Returns a list of setter calls."
  [el-type obj attributes]
  (map (fn [[attr value]]
         (let [real-value (kw/value el-type value)]
           `(~(keyword->setter attr) ~obj ~real-value)))
       attributes))

(defn process-attributes
  "Takes an UI element type, its object symbol and a map of
  attributes. Consequently calls transform-attributes methods on all
  element's traits, in the end calls `default-setters-from-attributes`
  on what is left from the attributes map. Returns the generated code
  for all attributes."
  [el-type obj attributes container-type]
  (let [all-attributes (merge (kw/default-attributes el-type) attributes)
        [rest-attributes generated-code]
        (reduce (fn [[attrs gen-code] type]
                  (transform-attributes type obj attrs gen-code container-type))
                [all-attributes ()]
                (conj (kw/all-traits el-type) el-type))]
    (concat generated-code
            (default-setters-from-attributes el-type obj rest-attributes))))

(defn make-constructor-call
  "Takes an object class name and the attributes map to extract
  additional arguments (if available) and returns a form that
  constructs the object."
  [klass context {:keys [constructor-args]}]
  `(new ~klass ~context ~@constructor-args))

;; ## Top-level code-generation facilities

(defn make-ui-element
  "Takes a tree of elements and generates the code to create a new
  element object, set all attributes onto it and add all inside
  elements to it. `make-ui-element` is called recursively on each
  internal element. The second argument is a keyword that represents
  the type of the container UI element will be put in."
  [element container-type context]
  (if (vector? element)
    (let [[el-type attributes & inside-elements] element
          ^Class klass (kw/classname el-type)
          obj (gensym (.getSimpleName klass))]
      `(let [~obj ~(make-constructor-call klass context attributes)]
         ~@(process-attributes el-type obj attributes container-type)
         ~@(map (fn [el]
                  `(.addView ~obj ~(make-ui-element el el-type context)))
                inside-elements)
         ~obj))
    (if (and (sequential? element) (= (first element) 'quote))
      (second element)
      (make-ui-element (eval element) container-type context))))

(defmacro defui
  "Takes a tree of elements and creates Android UI elements according
  to this tree. A tree has a form of a vector that looks like following:

  `[element-name map-of-attributes & subelements]`

  where `map-of-attributes` is a map of attribute names to their
  values, and subelement is itself a tree of this form.

  Two-argument version takes an arbitrary Context object to use in UI
  elements constructor."
  ([tree]
     (make-ui-element tree nil `context))
  ([custom-context tree]
     (make-ui-element tree nil custom-context)))
