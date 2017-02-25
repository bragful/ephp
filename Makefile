all: ephp

doc:
	./rebar3 edown

clean-devel: clean
	-rm -rf _build

clean:
	-rm -f .build_date
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 xref
	./rebar3 eunit
	./rebar3 cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname ephp \
		-output cobertura.xml

ephp: compile
	./rebar3 escriptize
	cp -f _build/default/bin/ephp .
	-rm -f .build_date

shell:
	./rebar3 shell

.PHONY: doc test compile all shell
