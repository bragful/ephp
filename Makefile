all: ephp

doc:
	./rebar3 edown

clean:
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 do eunit, cover
	./covertool -cover _build/test/cover/eunit.coverdata -appname ephp -output cobertura.xml

ephp: compile
	./rebar3 escriptize
	cp -f _build/default/bin/ephp .

.PHONY: doc test compile all

