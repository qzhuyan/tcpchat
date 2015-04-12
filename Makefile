app=tcs
logPath=/tmp/_${app}_ct/
all: tar 

compile: rebar.conf
	./rebar co

eunit:  compile
	./rebar eu

clean:  
	./rebar clean
	rm -rf out/*
	rm -rf rel/tcs

rel:    compile eunit
	cd rel/; ../rebar generate
	touch out/.rel

tar:	dialyzer rel
	cd rel;tar czf ../out/${app}-`git  rev-parse   --short HEAD`.tar.gz tcs

plt:    
	dialyzer --build_plt --apps erts kernel stdlib crypto sasl  \
	--output_plt plts/OTP_APPS.plt

dialyzer: compile plt
	dialyzer --plts plts/OTP_APPS.plt  --src  src/ || exit 1;

ct:	eunit
	rm -rf ${logPath}
	mkdir -p ${logPath}
	ct_run -name ct@127.0.0.1 -suite test/suite/chat_trpt_SUITE.erl -logdir  ${logPath}

#keep the order of starting/stopping app
test:	eunit start_app ct stop_app

start_app:
	./app start
	sleep 3
stop_app:
	./app stop
