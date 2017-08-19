all: ephp

doc:
	./rebar3 as doc edown

clean-devel: clean
	-rm -rf _build

clean:
	-rm -f .build_date
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 do xref, eunit, cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname ephp \
		-output cobertura.xml > /dev/null
	-rm -f .build_date request_ets cobertura_test.xml

ephp: compile
	./rebar3 escriptize
	cp -f _build/default/bin/ephp .
	-rm -f .build_date

shell:
	./rebar3 as dev shell

.PHONY: doc test compile all shell
