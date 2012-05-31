TESTS=$(wildcard test/*.erl)
TESTBEAMS=$(TESTS:%.erl=%.beam)

all: ebin/recorder.beam

ebin/recorder.beam: src/recorder.erl ebin
	erlc +debug_info -o ebin $<

ebin:
	mkdir ebin

test: $(TESTBEAMS)

test/%.beam: test/%.erl
	erlc +debug_info -o test $<
