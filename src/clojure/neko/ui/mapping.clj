(ns neko.ui.mapping
  "This namespace provides utilities to connect the keywords to the
  actual UI classes, define the hierarchy relations between the
  elements and the values for the keywords representing values."
  (:require [clojure.string :as string])
  (:import [android.widget LinearLayout Button EditText]
           [android.view ViewGroup$LayoutParams]))

;; This atom keeps all the relations inside the map.
(def ^{:private true} keyword-mapping
  (atom
   {:button {:classname android.widget.Button
             :parents [:layout-params :id :on-click]}
    :linear-layout {:classname android.widget.LinearLayout
                    :parents [:layout-params :id]}
    :edit {:classname android.widget.EditText
           :parents [:layout-params :id]}
    :layout-params {:classname ViewGroup$LayoutParams
                    :values {:fill ViewGroup$LayoutParams/FILL_PARENT
                             :wrap ViewGroup$LayoutParams/WRAP_CONTENT}}}))

(defn set-classname!
  "Connects the given keyword to the classname."
  [kw classname]
  (swap! keyword-mapping assoc-in [kw :classname] classname))

(defn classname
  "Gets the classname from the keyword-mapping map if the argument is
  a keyword. Otherwise considers the argument to already be a
  classname."
  [classname-or-kw]
  (if (keyword? classname-or-kw)
    (or (get-in @keyword-mapping [classname-or-kw :classname])
        (throw (Exception. (str "The class for " classname-or-kw
                                " isn't present in the mapping."))))
    classname-or-kw))

(defn add-parent!
  "Defines the `parent-kw` to be the parent of `kw`."
  [kw parent-kw]
  (swap! keyword-mapping update-in [kw :parents] conj parent-kw))

(defn all-parents
  "Returns the list of all unique parents for `kw`. The list is built
  recursively."
  [kw]
  (let [first-level-parents (get-in @keyword-mapping [kw :parents])]
    (->> first-level-parents
         (mapcat all-parents)
         (concat first-level-parents)
         distinct)))

(defn set-value!
  "Associate the value keyword with the provided value for the given
  keyword representing the UI element."
  [element-kw value-kw value]
  (swap! keyword-mapping assoc-in [element-kw :values value-kw] value))

(defn kw-to-static-field
  "Takes a classname and a keyword, capitalizes the latter's name and
  replaces all dashes with underscores."
  [class-name kw]
  (symbol (str (.getName class-name) \/
               (.toUpperCase (string/replace (name kw) \- \_)))))

(defn value
  "If the value is a keyword then returns the value for it from the
keyword-mapping. If the value-keyword isn't present in the
keyword-mapping, form the value as
`classname-for-element-kw/CAPITALIZED-VALUE-KW`."
  [element-kw value]
  (if-not (keyword? value)
    value
    (get-in @keyword-mapping [element-kw :values value]
            (kw-to-static-field (classname element-kw) value))))

(defn defelement
  "Defines the element of the given class with the provided name to
  use in the UI construction. Takes the element's classname, a list of
  parents and a map of specific values as optional arguments."
  [kw-name class-name & {:keys [parents values]}]
  (set-classname! kw-name class-name)
  (when parents
    (swap! keyword-mapping assoc-in [kw-name :parents] parents))
  (when values
    (swap! keyword-mapping assoc-in [kw-name :values] values)))
