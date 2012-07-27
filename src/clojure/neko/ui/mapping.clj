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

(ns neko.ui.mapping
  "This namespace provides utilities to connect the keywords to the
  actual UI classes, define the hierarchy relations between the
  elements and the values for the keywords representing values."
  (:require [clojure.string :as string])
  (:use [neko.-utils :only [keyword->static-field]])
  (:import [android.widget LinearLayout Button EditText ListView]
           android.app.ProgressDialog
           [android.view ViewGroup$LayoutParams]))

;; This atom keeps all the relations inside the map.
(def ^{:private true} keyword-mapping
  (atom
   ;; UI widgets
   {:view {:traits [:def :layout-params :on-click :text]}
    :button {:classname android.widget.Button
             :inherits :view
             :attributes {:text "Default button"}}
    :linear-layout {:classname android.widget.LinearLayout
                    :inherits :view}
    :edit {:classname android.widget.EditText
           :inherits :view}
    :text-view {:classname android.widget.TextView
                :inherits :view}
    :list-view {:classname android.widget.ListView
                :inherits :view}

    ;; Other
    :layout-params {:classname ViewGroup$LayoutParams
                    :values {:fill ViewGroup$LayoutParams/FILL_PARENT
                             :wrap ViewGroup$LayoutParams/WRAP_CONTENT}}
    :progress-dialog {:classname android.app.ProgressDialog
                      :values {:horizontal ProgressDialog/STYLE_HORIZONTAL
                               :spinner ProgressDialog/STYLE_SPINNER}}
    }))

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

(defn add-trait!
  "Defines the `kw` to implement trait specified with `trait-kw`."
  [kw trait-kw]
  (swap! keyword-mapping update-in [kw :traits] conj trait-kw))

(defn all-traits
  "Returns the list of all unique traits for `kw`. The list is built
  recursively."
  [kw]
  (let [own-traits (get-in @keyword-mapping [kw :traits])
        parent (get-in @keyword-mapping [kw :inherits])]
    (concat own-traits (when parent
                         (all-traits parent)))))

(defn set-value!
  "Associate the value keyword with the provided value for the given
  keyword representing the UI element."
  [element-kw value-kw value]
  (swap! keyword-mapping assoc-in [element-kw :values value-kw] value))

(defn value
  "If the value is a keyword then returns the value for it from the
  keyword-mapping. The value is sought in the element itself and all
  its parents. If the value-keyword isn't present in any element's
  keyword-mapping, form the value as
  `classname-for-element-kw/CAPITALIZED-VALUE-KW`."
  [element-kw value]
  (let [mapping @keyword-mapping
        recursive-find (fn [kw]
                         (when kw
                           (or (get-in mapping [kw :values value])
                               (recur (get-in mapping [kw :inherits])))))]
    (if-not (keyword? value)
      value
      (or (recursive-find element-kw)
          (keyword->static-field (classname element-kw) value)))))

(defn add-default-atribute-value!
  "Adds a default attribute value for the given element."
  [element-kw attribute-kw value]
  (swap! keyword-mapping
         update-in [element-kw :attributes attribute-kw] value))

(defn default-attributes [element-kw]
  "Returns a map of default attributes for the given element keyword
  and all its parents."
  [element-kw]
  (merge (when element-kw
           (default-attributes (get-in @keyword-mapping
                                       [element-kw :inherits])))
         (get-in @keyword-mapping [element-kw :attributes])))

(defn defelement
  "Defines the element of the given class with the provided name to
  use in the UI construction. Takes the element's classname, a parent
  it inherits, a list of traits and a map of specific values as
  optional arguments.

  Optional arguments
  - :classname, :inherits, :traits, :values, :attributes. "
  [kw-name & args]
  (swap! keyword-mapping assoc kw-name (apply hash-map args)))
