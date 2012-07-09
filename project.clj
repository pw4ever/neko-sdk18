(defproject neko "1.1.6"
  :description "Neko is a toolkit designed to make Android development using Clojure easier and more fun."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[android/clojure "1.4.0"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]

  :android {:library true
            :target-sdk "10"})
