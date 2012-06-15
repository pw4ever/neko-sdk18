(ns neko.ui.-common
  "Contains the initialization and configuration of the most common
  Android UI elements."
  (:use neko.ui.mapping)
  (:import [android.widget LinearLayout Button EditText]
           [android.view ViewGroup$LayoutParams]))

(defn init-common-elements []
  (defelement :button android.widget.Button
    :parents [:layout-params :id :on-click])

  (defelement :linear-layout android.widget.LinearLayout
    :parents [:layout-params :id])

  (defelement :edit android.widget.EditText
    :parents [:id :layout-params])

  (defelement :layout-params ViewGroup$LayoutParams
    :values {:fill ViewGroup$LayoutParams/FILL_PARENT
             :wrap ViewGroup$LayoutParams/WRAP_CONTENT}))

(init-common-elements)
