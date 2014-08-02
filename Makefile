.PHONY: all doc build

DOC:=docs/uberdoc.html
SRC:=$(shell find src -type f) project.clj

all: doc

doc: $(DOC)
	git checkout gh-pages
	cp $(DOC) index.html
	-git commit -a -m 'update doc'
	git checkout master

# https://groups.google.com/d/msg/clojure-android/ATO-DZNZExY/2LmV6FuP4hEJ
$(DOC): $(SRC)
	lein droid jar
	lein marg

build:
	lein droid jar
	lein marg
