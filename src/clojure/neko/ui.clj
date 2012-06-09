(ns neko.ui
  "Tools for defining and manipulating Android UI elements."
  (:use [neko.activity :only [*activity*]]
        [neko.context :only [*context*]]
        [neko.-utils :only [capitalize]])
  (:require [clojure.string :as string])
  (:import [android.widget Toast LinearLayout Button]
           [android.view View ViewGroup$LayoutParams]
           android.widget.LinearLayout$LayoutParams
           android.content.Context))

;; ### Element relations

;; This section provides utilities to connect the keywords to the
;; actual UI classes, define the hierarchy relations between the
;; elements and the values for the keywords representing values.

;; This atom keeps all the relations inside the map.
(def ^{:private true} keyword-mapping
  (atom {}))

(defn keyword-add-classname-relation
  "Connects the given keyword to the classname."
  [kw classname]
  (swap! keyword-mapping #(assoc-in % [kw :classname] classname)))

(defn classname
  "Gets the classname from the keyword-mapping map if the argument is
  a keyword. Otherwise considers the argument to already be a
  classname."
  [classname-or-kw]
  (if (keyword? classname-or-kw)
    (-> @keyword-mapping classname-or-kw :classname)
    classname-or-kw))

(defn keyword-add-parent
  "Defines the `parent-kw` to be the parent of `kw`."
  [kw parent-kw]
  (swap! keyword-mapping #(update-in % [kw :parents] conj parent-kw)))

(defn all-parents
  "Returns the list of all unique parents for `kw`. The list is built
  recursively."
  [kw]
  (let [first-level-parents (-> @keyword-mapping kw :parents)]
    (->> first-level-parents
         (mapcat all-parents)
         (concat first-level-parents)
         distinct)))

(defn keyword-add-value
  "Associate the value keyword with the provided value for the given
  keyword representing the UI element."
  [element-kw value-kw value]
  (swap! keyword-mapping #(assoc-in % [element-kw :values value-kw] value)))

(defn kw-to-static-field
  "Takes a keyword, capitalizes its name and replaces all dashes with
  underscores."
  [kw]
  (.toUpperCase (string/replace (name kw) \- \_)))

(defn attribute-value
  "If the value is a keyword then returns the value for it from the
keyword-mapping. If the value-keyword isn't present in the
keyword-mapping, form the value as
`classname-for-element-kw/CAPITALIZED-VALUE-KW`."
  [element-kw value]
  (if-not (keyword? value)
    value
    (or (-> @keyword-mapping element-kw :values value)
        (symbol (str (.getName (classname element-kw))
                     \/ (kw-to-static-field value))))))

;; These would be moved somewhere at some point.
;;
(do
  (keyword-add-classname-relation :button android.widget.Button)
  (keyword-add-classname-relation :linear-layout android.widget.LinearLayout)
  (keyword-add-classname-relation :layout-params android.view.ViewGroup$LayoutParams)
  (keyword-add-parent :button :layout-params)
  (keyword-add-parent :linear-layout :layout-params)
  (keyword-add-parent :button :id)
  (keyword-add-parent :linear-layout :id)
  (keyword-add-value :layout-params :fill ViewGroup$LayoutParams/FILL_PARENT)
  (keyword-add-value :layout-params :wrap ViewGroup$LayoutParams/WRAP_CONTENT))

;; ## Attributes

(defmulti transform-attributes
  "Transforms the given map of attributes into the valid Java-interop
code of setters.

`el-type` is the keyword for either the UI element type or any of the
parent types.

`object-symbol` is an symbol for the UI element to apply setters to.

`attributes-map` is a map of attribtues to their values.

`generated-code` is an attribute-setter code generated so far. The
code this method generates should be appended to it.

Returns a vector that looks like
`[attributes-without-processed-ones new-generated-code]`. The method
should remove the attributes it processed from the map."
  (fn [el-type object-symbol attributes-map generated-code] el-type))

;; ### ID attribute

;; An atom that stores the mapping of IDs (preferably
;; namespace-qualified keywods) to actual UI objects.
;;
(def ^{:private true} ui-elements
  (atom {}))

(defn register-ui-element
  "Adds the relation between the `id` and `element` to the
  `ui-elements` map."
  [id element]
  (swap! ui-elements #(assoc % id element)))

(defn by-id
  "Resolves an UI object by its ID."
  [id]
  (if-let [element (@ui-elements id)]
    element
    (throw (Exception. (str "The element with the ID " id
                            " is not present in the elements map")))))

(defmethod transform-attributes :id [el-type obj attributes generated-code]
  [(dissoc attributes :id)
   (if-let [id (:id attributes)]
     (conj generated-code `(register-ui-element ~id ~obj))
     generated-code)])

;; ### Layout parameters attributes

(defn layout-params
  "Construct LayoutParams instance from the given arguments. Arguments
could be either actual values or keywords `:fill` and `:wrap`.

Taken from clj-android by remvee."
  ([width height weight]
     (let [real-width (attribute-value :layout-params (or width :wrap))
           real-height (attribute-value :layout-params (or height :wrap))
           real-weight (or weight 0)]
       `(new LinearLayout$LayoutParams ~real-width ~real-height ~real-weight))))

(defmethod transform-attributes :layout-params [el-type obj attributes
                                                generated-code]
  [(dissoc attributes :layout-width :layout-height :layout-weight)
   (conj generated-code
         `(.setLayoutParams ~obj
                            ~(layout-params (:layout-width attributes)
                                            (:layout-height attributes)
                                            (:layout-weight attributes))))])

;; ### Default attributes

;; If there is no method for the given element type then return
;; unmodified `attributes` and `generated-code`.
;;
(defmethod transform-attributes :default [_ _ attributes generated-code]
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
         (let [real-value (if (keyword? value)
                            (attribute-value el-type value)
                            value)]
           `(~(symbol (str ".set" (capitalize (name attr)))) ~obj ~real-value)))
       attributes))

(defn process-attributes
  "Takes an UI element type, its object symbol and a map of
  attributes. Consequently calls transform-attributes methods on all
  element's parents, in the end calls
  `default-setters-from-attributes` on what is left from the
  attributes map. Returns the generated code for all attributes."
  [el-type obj attributes]
  (let [[rest-attributes generated-code]
        (reduce (fn [[attrs gen-code] type]
                  (transform-attributes type obj attrs gen-code))
                [attributes ()]
                (conj (all-parents el-type) el-type))]
    (concat generated-code
            (default-setters-from-attributes el-type obj rest-attributes))))

;; ## Top-level code-generation facilities

(defn make-ui-element
  "Takes a tree of elements and generates the code to create a new
  element object, set all attributes onto it and add all inside
  elements to it. `make-ui-element` is called recursively on each
  internal element. Presumes the `*context*` var to be bound."
  [[el-type attributes & inside-elements]]
  (let [klass (classname el-type)
        obj (gensym (.getSimpleName klass))]
    `(let [~obj (new ~klass *context*)]
       ~@(process-attributes el-type obj attributes)
       ~@(map (fn [el]
                `(.addView ~obj ~(make-ui-element el)))
              inside-elements)
       ~obj)))

(defmacro defui [tree]
  (make-ui-element tree))
