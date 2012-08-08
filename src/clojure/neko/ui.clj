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
        [neko.listeners.view :only [on-click-call]]
        [neko.ui.traits :only [transform-attributes]])
  (:require [neko.ui.mapping :as kw])
  (:import [android.widget LinearLayout$LayoutParams]
           [android.view ViewGroup$LayoutParams]))

;; ## Attributes

(defn default-setters-from-attributes
  "Takes element type keyword, object symbol and attributes map after
  all `transform-attributes` methods have been called on it.
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
  "Takes UI element type, its object symbol, a map of attributes and
  options. Consequently calls transform-attributes methods on all
  element's traits, in the end calls `default-setters-from-attributes`
  on what is left from the attributes map. Returns the vector like
  `[generated-code updated-options]`.

  Options is a map of additional arguments that come from container
  elements to their inside elements. Note that all traits of the
  current element will receive the initial options map, and
  modifications will only appear visible to the subsequent elements."
  [el-type obj attributes options]
  (let [all-attributes (merge (kw/default-attributes el-type) attributes)]
    (loop [[trait & rest] (kw/all-traits el-type), gen-code ()
           attrs all-attributes, new-options options]
      (if trait
       (let [[gen-code attributes-fn options-fn]
             (transform-attributes trait obj attrs gen-code options)]
         (recur rest gen-code (attributes-fn attrs) (options-fn new-options)))
       [(concat gen-code
                (default-setters-from-attributes el-type obj attrs))
        new-options]))))

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
  internal element.

  `options` is a map constructed by container elements to pass down to
  their inside elements. For top level element options map is empty.

  `context` is Context instance to be used in elements' constructors."
  [element options context]
  (if (vector? element)
    (let [[el-type attributes & inside-elements] element
          _ (assert (and (keyword? el-type) (map? attributes)))
          ^Class klass (kw/classname el-type)
          obj (gensym (.getSimpleName klass))
          ;; Remove :constructor-args from attribues since it is used
          ;; by constructor and is not a real attribute.
          [attributes-code new-options]
          (process-attributes el-type obj (dissoc attributes :constructor-args)
                              options)
          ;; Add to new-options information about the type of the
          ;; container. Traits can't do that themselves because they
          ;; don't know the type of the element they are working on.
          new-options (assoc new-options :container-type el-type)]
      `(let [~obj ~(make-constructor-call klass context attributes)]
         ~@attributes-code
         ~@(map (fn [el]
                  `(.addView ~obj ~(make-ui-element el new-options context)))
                inside-elements)
         ~obj))
    (if (and (sequential? element) (= (first element) 'quote))
      (second element)
      (make-ui-element (eval element) options context))))

(defmacro make-ui
  "Takes a tree of elements and creates Android UI elements according
  to this tree. A tree has a form of a vector that looks like following:

  `[element-name map-of-attributes & subelements]`

  where `map-of-attributes` is a map of attribute names to their
  values, and subelement is itself a tree of this form.

  Two-argument version takes an arbitrary Context object to use in UI
  elements constructor."
  ([tree]
     (make-ui-element tree {} `context))
  ([custom-context tree]
     (make-ui-element tree {} custom-context)))

(defmacro defui
  "This macro is DEPRECATED and replaced by `make-ui` due to confusive
  name. Please use `make-ui` instead.

  `defui` will be removed in the next minor release." [& args]
  (println "WARNING: defui is deprecated, please use make-ui instead.")
  `(make-ui ~@args))

(defmacro config!
  [el-type element & attributes]
  (let [attributes (apply hash-map attributes)
        obj (gensym (name el-type))]
    `(let [~obj ~element]
       ~@(process-attributes el-type obj attributes nil))))
