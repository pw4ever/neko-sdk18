(ns neko.ui
  "Tools for defining and manipulating Android UI elements."
  (:use [neko.activity :only [*activity*]]
        [neko.context :only [*context*]])
  (:import [android.widget Toast LinearLayout Button]
           [android.view View ViewGroup$LayoutParams]
           android.content.Context))

;; A map that contains the mapping of auxiliary keywords to the most
;; used constants.
;;
(def ^{:private true}
  attribute-values {;; LinearLayout orientation
                    :horizontal LinearLayout/HORIZONTAL
                    :vertical LinearLayout/VERTICAL

                    ;; LayoutParams constants
                    :fill-parent ViewGroup$LayoutParams/FILL_PARENT
                    :fill ViewGroup$LayoutParams/FILL_PARENT
                    :wrap-content ViewGroup$LayoutParams/WRAP_CONTENT
                    :wrap ViewGroup$LayoutParams/WRAP_CONTENT})

;; ### Element relations

;; This section provides utilities to connect the keywords to the
;; actual UI classes and define the hierarchy relations between the
;; elements.

;; This atom keeps all the relations inside the map.
(def ^{:private true} keyword-mapping
  (atom {}))

(defn add-keyword-classname-relation
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

(defn add-parent
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

;; These would be moved somewhere at some point.
;;
(do
  (add-keyword-classname-relation :button android.widget.Button)
  (add-keyword-classname-relation :linear-layout android.widget.LinearLayout)
  (add-parent :button :layout-params)
  (add-parent :linear-layout :layout-params)
  (add-parent :button :id)
  (add-parent :linear-layout :id))

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
  ([both]
    `(let [both# ~(or (attribute-values both)
                                ViewGroup$LayoutParams/WRAP_CONTENT)]
       (new ViewGroup$LayoutParams both# both#)))
  ([width height]
    `(let [width# ~(or (attribute-values width)
                                 ViewGroup$LayoutParams/WRAP_CONTENT)
           height# ~(or (attribute-values height)
                                  ViewGroup$LayoutParams/WRAP_CONTENT)]
       (new ViewGroup$LayoutParams width# height#))))

(defmethod transform-attributes :layout-params [el-type obj attributes
                                                generated-code]
  [(dissoc attributes :layout-width :layout-height)
   (conj generated-code
         `(.setLayoutParams ~obj
                            ~(layout-params (:layout-width attributes)
                                            (:layout-height attributes))))])

;; ### Default attributes

;; If there is no method for the given element type then return
;; unmodified `attributes` and `generated-code`.
;;
(defmethod transform-attributes :default [_ _ attributes generated-code]
  [attributes generated-code])

(defn capitalize
  "Takes a string and upper-cases the first letter in it."
  [s]
  (str (.toUpperCase (.substring s 0 1)) (.substring s 1)))

(defn default-setters-from-attributes
  "Takes an object symbol and the attributes map after all
  `transform-attributes` methods have been called on it. Transforms
  each attribute into a call to (.set_CapitalizedKey_ obj value). If
  value is a keyword then it is looked up in the attribute-values map.
  Returns a list of setter calls. "
  [obj attributes]
  (map (fn [[attr value]]
         (let [real-value (if (and (keyword? value)
                                   (contains? attribute-values value))
                            (attribute-values value)
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
            (default-setters-from-attributes obj rest-attributes))))

;; ## Top-level code-generation facilities

(defn- gen-meaningful-sym
  "Generates a symbol with a prefix taken from the `class-name` without
  the package path."
  [class-name]
  (let [[_ last-name] (re-find #".+\.(.+)" (str class-name))]
    (gensym last-name)))

(defn make-ui-element
  "Takes a tree of elements and generates the code to create a new
  element object, set all attributes onto it and add all inside
  elements to it. `make-ui-element` is called recursively on each
  internal element. Presumes the `*context*` var to be bound."
  [[el-type attributes & inside-elements]]
  (let [klass (classname el-type)
        obj (gen-meaningful-sym klass)]
    `(let [~obj (new ~klass *context*)]
       ~@(process-attributes el-type obj attributes)
       ~@(map (fn [el]
                `(.addView ~obj ~(make-ui-element el)))
              inside-elements)
       ~obj)))

(defmacro defui [tree]
  (make-ui-element tree))
