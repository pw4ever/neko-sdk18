(ns neko.listeners.search-view
  (:import android.widget.SearchView))

(defn on-query-text-call
  "Takes onQueryTextChange and onQueryTextSubmit functions and yields
  a SearchView.OnQueryTextListener object that will invoke the
  functions. Both functions take string argument, a query that was
  entered."
  [change-fn submit-fn]
  (reify android.widget.SearchView$OnQueryTextListener
    (onQueryTextChange [this query]
      (if change-fn
        (do (change-fn query)
            true)
        false))
    (onQueryTextSubmit [this query]
      (if submit-fn
        (do (submit-fn query)
            true)
        false))))
