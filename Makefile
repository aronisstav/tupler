TESTS=$(wildcard test/*.erl)
TESTBEAMS=$(TESTS:%.erl=%.beam)

all: ebin/tupler.beam

ebin/tupler.beam: src/tupler.erl ebin
	erlc +debug_info -o ebin $<

ebin:
	mkdir ebin

test: $(TESTBEAMS)

test/%.beam: test/%.erl
	erlc +debug_info -o test $<
