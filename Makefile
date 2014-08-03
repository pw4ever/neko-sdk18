.PHONY: all doc build push

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
	lein do droid jar, marg, pom

build:
	lein do droid jar, marg, pom

push:
	-git checkout gh-pages; git push
	-git checkout master; git push
