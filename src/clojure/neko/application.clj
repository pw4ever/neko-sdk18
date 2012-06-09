(ns neko.application
  "Contains the tools to create and manipulate Application instances."
  (:use [neko.-utils :only [simple-name]])
  (:import android.app.Application))

(defmacro defapplication
  "Creates an application class with the given full package-qualified
  name. Optional arguments should be provided in a key-value fashion.

  Available optional arguments:

  :extends, :prefix - same as for `gen-class`.

  :create - takes a one-argument function. Generates a handler for
  application's `onCreate` event which automatically calls the
  superOnCreate method and then calls the provided function onto the
  Application object."
  [name &
  {:keys [extends prefix create]}]
  (let [prefix (or prefix (str (simple-name name) "-"))]
   `(do
      (gen-class
        :name ~name
        :main false
        :prefix ~prefix
        :extends ~(or extends android.app.Application)
        :exposes-methods {~'onCreate ~'superOnCreate})
      ~(when create
         `(defn ~(symbol (str prefix "onCreate")) [~'this]
            (.superOnCreate ~'this)
            (~create ~'this))))))
