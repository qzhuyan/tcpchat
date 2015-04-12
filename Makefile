app=tcs

all: compile eunit  dialyzer generate tar 

compile:
	./rebar co

eunit:  compile
	./rebar eu

clean:  
	./rebar clean

generate: compile 
	cd rel/; ../rebar compile generate

tar:	generate
	cd rel;tar czf ../out/$app-`git  rev-parse   --short HEAD`.tar.gz tcs

plt:    
	dialyzer --check_plt --apps erts kernel stdlib crypto sasl  \
	--output_plt plts/OTP_APPS.plt

dialyzer: plt
	dialyzer --plts plts/OTP_APPS.plt  --src  src/ || exit 1;

