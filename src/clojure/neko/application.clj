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

(ns neko.application
  "Contains the tools to create and manipulate Application instances."
  (:use [neko.-utils :only [simple-name]]
        [neko.init :only [init]])
  (:import android.app.Application))

(defn define-context
  "Define a var to store the application context."
  [ctx]
  (def context ctx))

(defmacro defapplication
  "Creates an application class with the given full package-qualified
  name. Optional arguments should be provided in a key-value fashion.

  Available optional arguments:

  :extends, :prefix - same as for `gen-class`.

  :create - takes a one-argument function. Generates a handler for
  application's `onCreate` event which automatically calls the
  superOnCreate method and then calls the provided function onto the
  Application object."
  [name & {:keys [extends prefix create]
           :or {extends android.app.Application, create init,
                prefix (str (simple-name name) "-")}}]
  `(do
     (gen-class
      :name ~name
      :main false
      :prefix ~prefix
      :extends ~extends
      :exposes-methods {~'onCreate ~'superOnCreate})
     ~(when (not= create :later)
        `(defn ~(symbol (str prefix "onCreate")) [~'this]
           (.superOnCreate ~'this)
           (define-context ~'this)
           (~create ~'this)))))
