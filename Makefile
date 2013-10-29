
all: deps compile

deps:
	rebar get-deps

compile:
	rebar compile escriptize

clean:
	rebar clean
	rm -fr deps ebin

