PARCEL=node_modules/.bin/parcel
ELM_TEST=node_modules/.bin/elm-test
ELM_ANALYSE=node_modules/.bin/elm-analyse

.PHONY: dev check lint test test-watch

.DEFAULT_GOAL:= dist

clean:
	rm -rf .cache dist elm-stuff

dist: clean
	"${PARCEL}" build --no-source-maps src/index.html

dev:
	"${PARCEL}" src/index.html

check: lint test

lint:
	"${ELM_ANALYSE}"

test:
	"${ELM_TEST}"

test-watch:
	"${ELM_TEST}" --watch
