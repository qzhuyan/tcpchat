app=tcs
logPath=/tmp/_${app}_ct/
all: compile eunit  dialyzer rel tar 

compile: rebar.conf
	./rebar co

eunit:  compile
	./rebar eu

clean:  
	./rebar clean
	rm -rf out/*
	rm -rf rel/tcs

rel:   rel_check 
	cd rel/; ../rebar compile generate

rel_check: test dialyzer

tar:	rel
	cd rel;tar czf ../out/${app}-`git  rev-parse   --short HEAD`.tar.gz tcs

plt:    
	dialyzer --check_plt --apps erts kernel stdlib crypto sasl  \
	--output_plt plts/OTP_APPS.plt

dialyzer: plt
	dialyzer --plts plts/OTP_APPS.plt  --src  src/ || exit 1;

ct:	eunit
	rm -rf ${logPath}
	mkdir -p ${logPath}
	ct_run -name ct@127.0.0.1 -suite test/suite/chat_trpt_SUITE.erl -logdir  ${logPath}

test:	eunit ct
