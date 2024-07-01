all: ephp

doc:
	./rebar3 as doc do ex_doc

clean-devel: clean
	-rm -rf _build

clean:
	-rm -f .build_date
	./rebar3 clean

compile:
	-rm -f ebin
	./rebar3 compile

test:
	./rebar3 do xref, eunit, cover, covertool generate
	mv _build/test/covertool/ephp.covertool.xml cobertura.xml
	-rm -f .build_date cobertura_test.xml asc\:data

ephp:
	./rebar3 escriptize
	cp -f _build/default/bin/ephp .
	-rm -f .build_date

shell:
	./rebar3 as dev shell

check:
	./rebar3 dialyzer

publish: compile
	ln -snf _build/default/lib/ephp/ebin
	./rebar3 hex publish
	rm -f ebin

.PHONY: doc test compile all shell ephp publish
