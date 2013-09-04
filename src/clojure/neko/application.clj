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
  "Contains tools to create and manipulate Application instances. This
  namespace is deprecated and exists only for backward compatibility
  purposes."
  (:require neko.init)
  (:import android.app.Application
           android.content.Context))

(defmacro defapplication
  [& args]
  (throw (Exception. "defapplication is deprecated, please define
  Application class from Java. Default `:on-create` moved to
  `init-application`.")))

(defn init-application
  "DEPRECATED: Performs necessary preparations for Neko and REPL
  development. You should call `neko.init/init` instead."
  [context]
  (neko.init/init context))
