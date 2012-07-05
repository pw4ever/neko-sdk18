(ns neko.ui.listview
  "Contains utilities to work with ListView."
  (:import android.util.SparseBooleanArray))

(defn get-checked
  "Returns a vector of indices for items being checked in a ListView.
  The two-argument version additionally takes a sequnce of data
  elements of the ListView (usually the data provided to the adapter)
  and returns the vector of only those elements that are checked. "
  ([lv]
     (let [bool-array (.getCheckedItemPositions lv)
           count (.getCount lv)]
       (loop [i 0, result []]
         (if (= i count)
           result
           (if (.get bool-array i)
             (recur (inc i) (conj result i))
             (recur (inc i) result))))))
  ([lv items]
     (let [bool-array (.getCheckedItemPositions lv)
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
  [lv checked-ids]
  (doseq [i checked-ids]
    (.setItemChecked lv i true)))

