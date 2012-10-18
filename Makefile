all: compile

compile:
	rebar compile

test: compile
	rebar ct skip_deps=true

clean:
	rebar clean

