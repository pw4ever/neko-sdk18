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

(ns neko.doc
  "This namespace contains functions that help the developer with
  documentation for different parts of neko. "
  (:require [neko.ui.traits :as traits]
            [neko.ui.mapping :as mapping])
  (:use [clojure.string :only [join]]))

(defn get-element-doc
  [el-type el-mapping verbose?]
  (let [{:keys [classname attributes values]} el-mapping
        traits (mapping/all-traits el-type)]
   (format "%s - %s\n%s%s%s\n"
           el-type
           (or classname "no matching class")
           (if (empty? traits) ""
               (format "Implements traits: %s\n"
                       (if verbose?
                         (join (for [t traits]
                                 (str "\n" (get-trait-doc t))))
                         (pr-str traits))))
           (if (empty? attributes) ""
               (format "Default attributes: %s\n"
                       (pr-str attributes)))
           (if (empty? values) ""
               (format "Special values: %s\n"
                       (pr-str values))))))

(defn- get-trait-doc [trait]
  (when-let [doc (get-in (meta #'traits/transform-attributes)
                            [:trait-doc trait])]
    (str trait " - " doc)))

(get-element-doc :button (get @@#'mapping/keyword-mapping :button) true)

(defn describe
  ([]
     (let [all-elements @@#'mapping/keyword-mapping]
       (doseq [[el-type parameters] all-elements]
         (print (get-element-doc el-type parameters false)))))
  ([kw]
     (let [parameters (get @@#'mapping/keyword-mapping kw)
           trait-doc (get-trait-doc kw)]
       (cond
        parameters (print "Elements found:\n"
                          (get-element-doc kw parameters true))
        trait-doc (print "\nTraits found:\n" trait-doc)
        :else (print (str "No elements or traits were found for " kw))))))
