(ns neko.data
  "Contains utilities to manipulate data that is passed between
  Android entities via Bundles and Intents."
  (:import android.os.Bundle android.content.Intent))

(defprotocol GenericKey
  "If given a string returns itself, otherwise transforms a argument
  into a string."
  (generic-key [key]))

(extend-protocol GenericKey
  String
  (generic-key [s] s)

  clojure.lang.Keyword
  (generic-key [k] (.getName k)))

(defprotocol MapLike
  "Returns a wrapper of the provided object that allows to extract
  values from it like from an ordinar map."
  (like-map [this]))

(extend-protocol MapLike
  Bundle
  (like-map [b]
    (proxy [clojure.lang.APersistentMap] []
      (containsKey [k] (.containsKey b (generic-key k)))
      (valAt
        ([k] (.get b (generic-key k)))
        ([k default] (let [key (generic-key k)]
                       (if (.containsKey b key)
                         (.get b (generic-key key))
                         default))))
      (seq [] (map (fn [k] [k (.get b k)])
                   (.keySet b)))))

  Intent
  (like-map [i]
    (like-map (.getExtras i))))
