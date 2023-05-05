(ns radio.util)

(defn fetch
  "Fetches a value from a given object with a keyword key.
  Takes the object, keyword key, and value to fetch."
  [^js obj kw-key val]
  (.fetch (aget obj (name kw-key)) val))

(defn then
  "Attaches a callback to a promise.
  Takes the promise object and callback function."
  [^js promise callback-fn]
  (.then promise callback-fn))

(defn on
  "Attaches an event listener to an object.
  Takes the object, event keyword, and listener function."
  [^js obj event-kw listener-fn]
  (.on obj (name event-kw) listener-fn))

(defn kget
  [^js obj k]
  (get obj (name k)))
