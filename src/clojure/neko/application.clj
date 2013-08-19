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
  (:require neko.context neko.init)
  (:use [neko.-utils :only [simple-name]]
        [neko.resource :only [package-name]]
        [neko.threading :only [init-threading]])
  (:import android.app.Application
           android.content.Context))

(defmacro defapplication
  [& args]
  (throw (Exception. "defapplication is deprecated, please define
  Application class from Java. Default `:on-create` moved to
  `init-application`.")))

(defn init-application
  "Performs necessary preparations for Neko and REPL development."
  [context & {:keys [extends prefix on-create nrepl-port]
              :or {extends android.app.Application
                   prefix (str (simple-name name) "-")}}]
  (alter-var-root #'neko.context/context (constantly context))
  (alter-var-root #'package-name (constantly (.getPackageName context)))
  (neko.init/init context :port (or nrepl-port 9999))
  (init-threading))


