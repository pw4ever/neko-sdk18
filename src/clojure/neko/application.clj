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
  "Contains tools to create and manipulate Application instances."
  (:use [neko.-utils :only [simple-name]]
        [neko.init :only [init]]
        [neko.context :only [context]]
        [neko.resource :only [package-name]]
        [neko.threading :only [init-threading]])
  (:import android.app.Application
           android.content.Context))

(defmacro defapplication
  "Creates an application class with the given full package-qualified
  name. Optional arguments should be provided in a key-value fashion.

  Available optional arguments:

  :extends, :prefix - same as for `gen-class`.

  :on-create - takes a one-argument function. Generates a handler for
  application's `onCreate` event which automatically calls the
  superOnCreate method and then calls the provided function onto the
  Application object."
  [name & {:keys [extends prefix on-create nrepl-port]
           :or {extends android.app.Application
                prefix (str (simple-name name) "-")}}]
  `(do
     (gen-class
      :name ~name
      :main false
      :prefix ~prefix
      :extends ~extends
      :exposes-methods {~'onCreate ~'superOnCreate})
     ~(when (not= on-create :later)
        `(defn ~(symbol (str prefix "onCreate"))
           [~(vary-meta 'this assoc :tag name)]
           (.superOnCreate ~'this)
           (alter-var-root #'context (constantly ~'this))
           (alter-var-root #'package-name (constantly (.getPackageName ~'this)))
           (init ~'this :port ~(or nrepl-port 9999))
           (init-threading)
           ~(when on-create
              `(~on-create ~'this))))))
