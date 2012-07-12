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

(ns neko.init
  "Contains functions for neko initialization and setting runtime flags."
  (:require neko.compilation))

(defn compile-time-property
  "Returns the value of system property for the given keyword.

  This is used to modify the code macros will compile into during
  AOT-compilation. When called in runtime the propery won't be present
  in the Properties map and the macros in the REPL will be recompiled
  differently. If you want to ensure compile-time/runtime macro
  consistency, make sure to alter the var values on which these macros
  depend upon manually in the runtime."
  [property]
  (System/getProperty (name property)))

(defn- property-set?
  "Checks if the given property keyword is set to true during the
  compilation of neko."
  [property]
  (= (compile-time-property property) "true"))

;; A list of all properties supported and tracked by neko.
;;
(def ^:private properties-list
  [:android-dynamic-compilation :android-start-nrepl-server
   :android-release-build])

;; Gather JVM properties during the compile time that indicates the
;; parameters of the build.
;;
(def ^:private properties
  (zipmap properties-list (map property-set? properties-list)))

;; True if the current build is a debuggable one or if
;; either the dynamic compillation or nREPL server are enabled
;; explicitly.
(def enable-dynamic-compilation
  (or (not (properties :android-release-build))
      (properties :android-start-nrepl-server)
      (properties :android-enable-dynamic-compilation)))

;; True if the current build is a debuggable one or if the
;; nREPL server is enabled explicitly.
(def start-nrepl-server
  (or (not (properties :android-release-build))
      (properties :android-start-nrepl-server)))

;; True if the current build is a debuggable one.
(def is-debug
  (not (properties :android-release-build)))

(defn start-repl
  "Starts a remote nREPL server. Creates a `user` namespace because
  nREPL expects it to be there while initializing. References nrepl's
  `start-server` function on demand because the project can be
  compiled without nrepl dependency (if it's the release build)."
  [& repl-args]
  (binding [*ns* (create-ns 'user)]
    (refer-clojure)
    (use 'clojure.tools.nrepl.server)
    (apply (resolve 'start-server) repl-args)))

(defn init
  "Initializes neko library.

  Initializes compilation facilities and runs nREPL server if
  appropriate. Takes the application context and optional arguments in
  key-value fashion. The value of `:classes-dir` specifies the path
  where neko should store compiled files. Other optional arguments are
  directly feeded to the nREPL's `start-server` function. "
  [context & {:keys [classes-dir port] :or {classes-dir "classes", port 9999}
              :as args}]
  (when enable-dynamic-compilation
    (neko.compilation/init context classes-dir))
  (when start-nrepl-server
    ;; Ensure that `:port` is provided, pass all other arguments as-is.
    (apply start-repl :port port
           (mapcat identity (dissoc args :classes-dir :port)))))

(defmacro with-properties
  "Sets the given properties specified by keywords to the respective
  values to run the code provided in `body`. At the end restore the
  initial property values."
  [bindings & body]
  (let [bindings (partition 2 bindings)
        initial (gensym)]
    `(let [~initial (hash-map ~@(mapcat (fn [[prop _]]
                                          [prop `(str (System/getProperty (name ~prop)))])
                                        bindings))]
       ~@(for [[prop newval] bindings]
           `(System/setProperty (name ~prop) (str ~newval)))
       (let [result# (do ~@body)]
         ~@(for [[prop newval] bindings]
             `(System/setProperty (name ~prop) (get ~initial ~prop)))
         result#))))
