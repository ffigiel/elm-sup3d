.PHONY: clean dev check lint test test-watch

.DEFAULT_GOAL:= dist

clean:
	rm -rf .cache dist

dist: clean
	npx parcel build --no-source-maps src/index.html

dev:
	npx parcel src/index.html

check: lint test

lint:
	npx elm-analyse

test:
	npx elm-test

test-watch:
	npx elm-test --watch
