all: ephp

doc:
	./rebar3 edown

clean-devel: clean
	-rm -rf _build

clean:
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 do xref, eunit, cover
	./covertool -cover _build/test/cover/eunit.coverdata -appname ephp -output cobertura.xml

ephp: compile
	./rebar3 escriptize
	cp -f _build/default/bin/ephp .

shell:
	./rebar3 shell

.PHONY: doc test compile all shell

