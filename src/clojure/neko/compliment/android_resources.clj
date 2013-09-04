(ns neko.compliment.android-resources
  "Compliment source for keywords that represent Android resources."
  (:require [neko.context :as app]
            [clojure.string :as string]
            [neko.resource :as droid-res]))

(defn keyword-symbol?
  "Tests if prefix is a keyword."
  [x]
  (re-matches #":.*" x))

(def ^{:doc "Stores cached resources to allow faster lookup."
       :private true}
  resource-cache (atom {}))

(def ^{:doc "Resource types to be completed."
       :private true}
  resource-types [:id :string :drawable :layout])

(defn res->keyword
  "Replaces all underscores in resource name to dashes."
  [res-name]
  (string/replace res-name \_ \-))

;; ## Candidates

(defn- get-package-resources
  "Returns a map for package `pkg-name` where keys are resource
  keywords and values are R class members. `type` defines which
  resources should be returned.

  If `append-ns` is true, add namespace name to resource keywords."
  [pkg-name type append-ns]
  (into {}
        (when-let [cls ((resolve 'compliment.utils/resolve-class)
                        (symbol (str pkg-name ".R$" (name type))))]
          (for [field (.getDeclaredFields cls)
                :let [field-name (.getName field)]]
            [(if append-ns
               (str ":" pkg-name "/" (res->keyword field-name))
               (str ":" (res->keyword field-name)))
             (list field)]))))

(defn populate-cache
  "Saves resource keywords for `pkg-name` to the cache."
  [pkg-name append-ns]
  (swap! resource-cache assoc pkg-name
         (apply merge-with concat
                (map #(get-package-resources pkg-name % append-ns)
                     resource-types))))

(defn get-resource-cache
  "Returns a list of resource keywords for the given `pkg-name`.
  Populates cache if it is empty."
  [pkg-name]
  (let [append-ns (boolean pkg-name)
        pkg-name (or pkg-name (.getPackageName app/context))]
    (when-not (@resource-cache pkg-name)
      (populate-cache pkg-name append-ns))
    (@resource-cache pkg-name)))

(defn candidates
  "Returns a list of resource keywords completions for the keyword
  prefix. If prefix doesn't have a namespace, assumes application
  package to be a source."
  [^String prefix, ns context]
  (when (keyword-symbol? prefix)
    (let [[_ pkg-name] (re-matches #":(.+)/.*" prefix)]
      (for [^String res-str (keys (get-resource-cache pkg-name))
            :when (.startsWith res-str prefix)]
        res-str))))

;; ## Documentation

(defn- get-field-doc
  "Returns a docstring for the given R class field member."
  [field]
  (let [[_ pkg type name] (re-matches #".+ ([^$]+)\$(\w+)\.(.+)" (str field))]
    (str (string/capitalize type) " resource: " pkg "/" name
         (when (= type "string")
           (str " = \"" (droid-res/get-string (.get field nil)) "\""))
         "\n")))

(defn get-resource-doc
  "Returns a docstring for the given `symbol-str` in package `pkg-name`."
  [pkg-name symbol-str]
  (when-let [ress ((get-resource-cache pkg-name) symbol-str)]
    (string/join (map get-field-doc ress))))

(defn doc
  "Tries to get a docstring for the given completion candidate."
  [^String symbol-str, ns]
  (when (keyword-symbol? symbol-str)
    (let [[_ pkg-name] (re-matches #":(.+)/.*" symbol-str)]
      (get-resource-doc pkg-name symbol-str))))

;; ## Source definition

(defn init-source
  "Initializes this completion source if Compliment is available."
  []
  (try (require 'compliment.core)
       ((resolve 'compliment.sources/defsource) ::android-resources
        :candidates #'candidates
        :doc #'doc)
       (catch Exception ex nil)))
