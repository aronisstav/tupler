ebin/tupler.beam: src/tupler.erl ebin
	erlc +debug_info -o ebin $<

ebin:
	mkdir ebin