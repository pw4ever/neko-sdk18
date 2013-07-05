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

(ns neko.ui.listview
  "Contains utilities to work with ListView."
  (:import android.util.SparseBooleanArray
           android.widget.ListView))

(defn get-checked
  "Returns a vector of indices for items being checked in a ListView.
  The two-argument version additionally takes a sequence of data
  elements of the ListView (usually the data provided to the adapter)
  and returns the vector of only those elements that are checked. "
  ([^ListView lv]
     (let [^SparseBooleanArray bool-array (.getCheckedItemPositions lv)
           count (.getCount lv)]
       (loop [i 0, result []]
         (if (= i count)
           result
           (if (.get bool-array i)
             (recur (inc i) (conj result i))
             (recur (inc i) result))))))
  ([^ListView lv, items]
     (let [^SparseBooleanArray bool-array (.getCheckedItemPositions lv)
           count (.getCount lv)]
       (loop [i 0, [curr & rest] items, result []]
         (if (= i count)
           result
           (if (.get bool-array i)
             (recur (inc i) rest (conj result curr))
             (recur (inc i) rest result)))))))

(defn set-checked!
  "Given a sequence of numbers checks the respective ListView
  elements."
  [^ListView lv, checked-ids]
  (doseq [i checked-ids]
    (.setItemChecked lv i true)))
