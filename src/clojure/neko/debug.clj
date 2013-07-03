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

(ns neko.debug
  "Contains useful tools to be used while developing the application."
  (:use [neko.init.options :only [*release-build*]]
        [neko.log :only [log-exception]]
        [neko.notify :only [toast]]))

;; This atom stores the last exception happened on the UI thread.
;;
(def ^:private ui-exception (atom nil))

(defn handle-exception-from-ui-thread
  "Displays an exception message using a Toast and stores the
  exception for the future reference."
  [e]
  (reset! ui-exception e)
  (log-exception e)
  (toast (str e) :long))

(defn ui-e
  "Returns an uncaught exception happened on UI thread."
  [] @ui-exception)

(defmacro catch-all-exceptions [func]
  (if *release-build*
    `(~func)
    `(try (~func)
          (catch Throwable e#
            (handle-exception-from-ui-thread e#)))))

(defn safe-for-ui*
  "Wraps the given function inside a try..catch block and notify user
  using a Toast if an exception happens."
  [f]
  (catch-all-exceptions f))

(defmacro safe-for-ui
  "A conditional macro that will protect the application from crashing
  if the code provided in `body` crashes on UI thread in the debug
  build. If the build is a release one returns `body` as is."
  [& body]
  `(safe-for-ui* (fn [] ~@body)))
