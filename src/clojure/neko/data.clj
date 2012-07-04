(ns neko.data
  "Contains utilities to manipulate data that is passed between
  Android entities via Bundles and Intents."
  (:import android.os.Bundle android.content.Intent))

(defprotocol GenericExtrasKey
  "If given a string returns itself, otherwise transforms a argument
  into a string."
  (generic-key [key]))

(extend-protocol GenericExtrasKey
  String
  (generic-key [s] s)

  clojure.lang.Keyword
  (generic-key [k] (.getName k)))

;; This type acts as a wrapper around Bundle instance to be able to
;; access it like an ordinar map.
;;
(deftype MapLikeBundle [^Bundle bundle]
  clojure.lang.Associative
  (containsKey [this k] (.containsKey bundle (generic-key k)))
  (entryAt [this k] (clojure.lang.MapEntry. k (.get bundle (generic-key k))))
  (valAt [this k] (.get bundle (generic-key k)))
  (valAt [this k default] (let [key (generic-key k)]
                      (if (.containsKey bundle key)
                        (.get bundle (generic-key key))
                        default)))
  (seq [this] (map (fn [k] [k (.get bundle k)])
                   (.keySet bundle))))

(defprotocol MapLike
  "A protocol that helps to wrap objects of different types into
  MapLikeBundle."
  (like-map [this]))

(extend-protocol MapLike
  Bundle
  (like-map [b]
    (MapLikeBundle. b))

  Intent
  (like-map [i]
    (if-let [bundle (.getExtras i)]
      (MapLikeBundle. bundle)
      {})))
