(defproject org.clojars.pw4ever/neko-sdk18 "3.0.2"
  :description "Neko is a toolkit designed to make Android development using Clojure easier and more fun."
  :url "https://github.com/clojure-android/neko"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure-android/clojure "1.6.0-RC1"]]
  :source-paths ["src" "src/clojure"]
  :java-source-paths ["src/java" "gen"]

  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :android {:library true
            :target-version 18})
