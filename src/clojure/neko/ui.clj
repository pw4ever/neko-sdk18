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

`trait` is the keyword for a transformer function over the attribute map.

`object-symbol` is an symbol for the UI element to apply setters to.

`attributes-map` is a map of attribtues to their values.

`generated-code` is an attribute-setter code generated so far. The
code this method generates should be appended to it.

Returns a vector that looks like
`[attributes-without-processed-ones new-generated-code]`. The method
should remove the attributes it processed from the map."
  (fn [trait object-symbol attributes-map generated-code container-type]
    trait))

(defmacro deftrait
  "Defines a trait with the given name.

  `match-pred` is a function that should return a logical truth if
  this trait should be executed against the argument list. By default
  it checked if attribute with the same name as trait's is present in
  the attribute map.

  `dissoc-fn` is a function that removes processed attributes from the
  map. By default it dissocs attribute with trait's name from the map.

  `codegen-fn` takes four arguments: object, attribute map, generated
  code and a container type, and should append its own code to the
  generated code."
  ^{:arglists '([name docstring? match-pred? dissoc-fn? codegen-fn])}
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [match-pred args] (if (> (count args) 1)
                            [(first args) (next args)]
                            [name args])
        [dissoc-fn args] (if (> (count args) 1)
                            [(first args) (next args)]
                            [`(fn [a#] (dissoc a# ~name)) args])
        codegen-fn (first args)]
    `(do
       (alter-meta! #'transform-attributes
                    assoc-in [:trait-doc ~name] ~docstring)
       (defmethod transform-attributes ~name
         [trait# object# attributes# generated-code# container-type#]
         (if (~match-pred attributes#)
           [(~dissoc-fn attributes#)
            (~codegen-fn object# attributes# generated-code# container-type#)]
           [attributes# generated-code#])))))

;; ### Default attributes

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
  for all attributes.

  `container-type` is a keyword for a container type of this element.
  It is needed for `:layout-params` trait which should know the
  container type in order to use correct LayoutParams instance."
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

(defmacro config!
  [el-type element & attributes]
  (let [attributes (apply hash-map attributes)
        obj (gensym (name el-type))]
    `(let [~obj ~element]
       ~@(process-attributes el-type obj attributes nil))))
