
build: deps
	rebar compile

deps:
	rebar get-deps

shell:

	erl -pa deps/*/ebin ebin

run: build
	erl -pa deps/*/ebin ebin -eval "counter_proper:run(), init:stop()." -noshell -noinput
