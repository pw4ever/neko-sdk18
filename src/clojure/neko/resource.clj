; Copyright © 2011 Sattvik Software & Technology Resources, Ltd. Co.,
; Alexander Yakushev. All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.resource
  (:require [clojure.string :as string])
  (:use [neko.init.options :only [*package-name*]]
        [neko.context :only [context]]))

(def package-name *package-name*)

(defn- resource-symbol
  "Returns a symbol that represents a resource field specified by type
  and name keywords. If `name` is not a keyword, just returns it back."
  [type name]
  (if (not (keyword? name))
    name
    (let [package (or (namespace name) package-name)
          type    (clojure.core/name type)
          name    (-> (clojure.core/name name)
                      (string/replace \- \_)
                      (string/replace \. \_))]
      (symbol (str package ".R$" type "/" name)))))

(defmacro resolve-resource
  "Resolves the resource ID of a given type with the given name.  For example,
  to refer to what in Java would be R.string.my_string, you can use:

    (resolve-resource :string :my_string)

  The type should be a keyword corresponding to a resource type such as
  :layout, :attr, or :id.

  The name should be a keyword.  If the keyword has a namespace, it will be
  used as the package from which to retrieve the resources.  Generally, this is
  not required as the default will be the application package.  However, this
  can be used to access the resources from the platform.  For example, the
  equivalent to android.R.layout.simple_list_item_1 is:

    (resolve-resource :layout :android/simple_list_item_1)

  The name portion of the name argument will be converted to a string and any
  hyphens or periods will be transformed to underscores.  Note that hyphens are
  not valid in Android names, but are allowed here to be Clojure friendly.

  If the name argument is an integer, it is assumed to be a valid resource ID
  and will be returned as is without any processing."
  [type name]
  {:pre  [(keyword? type)]}
  (resource-symbol type name))

(defmacro get-id
  "Finds the ID for the XML item with the given name.  This is simply a
  convenient way of calling (resolve-resource :id name)."
  [name]
  (resource-symbol :id name))

(defmacro get-string
  "Gets the localized string with the given ID or name from the context.
  The name will be resolved using resolve-resource.

  If additional arguments are supplied, the string will be interpreted as a
  format and the arguments will be applied to the format."
  ([name]
     `(.getString context ~(resource-symbol :string name)))
  ([name & args]
     `(.getString context ~(resource-symbol :string name) (to-array ~args))))

(defmacro get-layout
  "Finds the resource ID for the layout with the given name.  This is simply a
  convenient way of calling (resolve-resource context :layout name)."
  [name]
  (resource-symbol :layout name))
