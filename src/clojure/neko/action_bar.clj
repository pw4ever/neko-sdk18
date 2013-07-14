(ns neko.action-bar
  (:require neko.ui)
  (:use [neko.ui.mapping :only [defelement]]
        [neko.ui.traits :only [deftrait]]
        [neko.-utils :only [call-if-nnil]])
  (:import [android.app ActionBar ActionBar$Tab ActionBar$TabListener]
           android.app.Activity android.app.Fragment android.R$id))

;; ## Listener helpers

(defn tab-listener
  "Creates a TabListener from the provided functions for selected,
  unselected and reselected events."
  [{:keys [on-tab-selected on-tab-unselected on-tab-reselected]}]
  (reify ActionBar$TabListener
    (onTabSelected [this tab ft]
      (call-if-nnil on-tab-selected tab ft))
    (onTabUnselected [this tab ft]
      (call-if-nnil on-tab-unselected tab ft))
    (onTabReselected [this tab ft]
      (call-if-nnil on-tab-reselected tab ft))))

(defn simple-tab-listener
  "Creates a TabListener that shows the specified fragment on
  selecting and hides it on deselecting."
  [tag, ^Fragment fragment]
  (reify ActionBar$TabListener
    (onTabReselected [this tab ft])
    (onTabUnselected [this tab ft]
      (when (.isAdded fragment)
        (.detach ft fragment)))
    (onTabSelected [this tab ft]
      (if (.isDetached fragment)
        (.attach ft fragment)
        (.add ft R$id/content fragment tag)))))

;; ## Functions for declarative definition

(defelement :action-bar
  :classname android.app.ActionBar
  :inherits nil
  :traits [:tabs :display-options]
  :values {:standard ActionBar/NAVIGATION_MODE_STANDARD
           :list ActionBar/NAVIGATION_MODE_LIST
           :tabs ActionBar/NAVIGATION_MODE_TABS})

(defelement :action-bar-tab
  :classname android.app.ActionBar$Tab
  :inherits nil
  :traits [:tab-listener])

(deftrait :tabs
  "Takes `:tabs` attribute which should be a sequence of tab
  definitions. Each tab definition is itself a sequence of `:tab`
  keyword and an attribute map. Creates tabs for the definitions and
  adds the tabs to the action bar."
  [^ActionBar action-bar, {:keys [tabs]} _]
  (doseq [[_ tab-attributes] tabs
          :let [tab (.newTab action-bar)]]
    (neko.ui/apply-attributes :action-bar-tab tab tab-attributes {})
    (.addTab action-bar tab)))

(defn display-options-value
  "Returns an integer value for the given keyword, or the value itself."
  [value]
  (if (keyword? value)
    (case value
      :home-as-up  ActionBar/DISPLAY_HOME_AS_UP
      :show-home   ActionBar/DISPLAY_SHOW_HOME
      :show-custom ActionBar/DISPLAY_SHOW_CUSTOM
      :show-title  ActionBar/DISPLAY_SHOW_TITLE
      :use-logo    ActionBar/DISPLAY_USE_LOGO)
    value))

(deftrait :display-options
  "Takes `:display-options` attribute, which could be an integer value
  or one of the following keywords: `:home-as-up`, `:show-home`,
  `:show-custom`, `:show-title`, `:use-logo`, or a vector with these
  values, to which bit-or operation will be applied."
  [^ActionBar action-bar, {:keys [display-options]} _]
  (let [value (if (vector? display-options)
                (apply bit-or (map display-options-value display-options))
                (display-options-value display-options))]
    (.setDisplayOptions action-bar value)))

(deftrait :tab-listener
  "Takes `:tab-listener` attribute which should be TabListener object
  and sets it to the tab. Attribute could also be a fragment, in which
  case a listener would be created that shows and hides the fragment
  on tab selection and deselection respectively."
  [^ActionBar$Tab tab, {:keys [tab-listener]} _]
  (let [listener (if (instance? Fragment tab-listener)
                   (simple-tab-listener (str tab-listener) tab-listener)
                   tab-listener)]
    (.setTabListener tab listener)))

(defn setup-action-bar
  "Configures activity's action bar according to the attributes
  provided in key-value fashion. For more information,
  see `(describe :action-bar)`."
  [^Activity activity & {:as attributes}]
  (let [action-bar (.getActionBar activity)]
    (neko.ui/apply-attributes :action-bar action-bar attributes {})))
