.SILENT: build
.PHONY: build
build:
	elm make --optimize --output=elm.js src/Main.elm
