all: dist/index.html

dist/index.html: $(shell find src vendor -type f -name '*.elm' -o -name '*.js')
	elm-make src/App.elm --yes --warn --output=$@
