all: dist/index.html

dist/index.html: $(shell find src -type f -name '*.elm')
	elm-make src/App.elm --yes --warn --output=$@
