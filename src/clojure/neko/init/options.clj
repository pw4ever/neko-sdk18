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

(ns neko.init.options
  "Vars in this namespace are bound in compile time for build-specific
  macro bodies. To be able to use this vars in runtime the should be
  initialized manually on application start.")

(def
  ^{:dynamic true
    :doc "True if the current build is a release one."}
  *release-build*)

(def
  ^{:dynamic true
    :doc
    "True if dynamic compilation should be enabled (regardless of the
    build type)."}
  *enable-dynamic-compilation*)

(def
  ^{:dynamic true
    :doc
    "True if nREPL server should be started (regardless of the build
    type)." }
  *start-nrepl-server*)

(def
  ^{:dynamic true
    :doc "Application's root package name."}
  *package-name*)
