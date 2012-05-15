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

(ns neko.repl
  "Utilities to start the REPL on the remote device."
  {:author "Alexander Yakushev"}
  (:import java.io.FileNotFoundException)
  (:require [neko.compilation :as neko]))

;; A symbol that matches the nREPL namespace with `start-server`
;; function.
(def ^{:private true} nrepl-ns-symbol 'clojure.tools.nrepl.server)

(defn try-start-repl
  "Initializes neko's compilation facilities and tries to launch the
  remote nREPL server. If the nrepl namespace is not present in the
  runtime (probably because the current build is the release one) does
  nothing."
  [context & repl-args]
  (try
    (let [user-ns (create-ns 'user)]
      (binding [*ns* user-ns]
        (refer-clojure)
        (use nrepl-ns-symbol)
        (let [start-server-fn (resolve 'start-server)]
          (neko/init context "classes")
          (apply start-server-fn repl-args))))
    (catch FileNotFoundException e nil)))
