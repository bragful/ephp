all: ephp

force-deps:
	./rebar get-deps
	./rebar update-deps
	./rebar compile

deps:
	./rebar get-deps
	./rebar compile

doc:
	./rebar doc skip_deps=true

compile: deps
	./rebar compile skip_deps=true

test: deps
	./rebar eunit skip_deps=true

ephp: compile
	./rebar escriptize skip_deps=true

.PHONY: doc test compile force-deps all

