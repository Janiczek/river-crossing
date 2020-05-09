.SILENT: build
.PHONY: build
build:
	#elm make --optimize --output=elm.js src/Main.elm
	elm make --output=elm.js src/Main.elm
