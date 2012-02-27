.PHONY: default dialyzer clean dist

TARGETS=ebin/gta2k.beam ebin/client.beam ebin/util.beam

ERLC_ARGS=-Wall

ifneq ($(DEBUG),)
	ERLC_ARGS+=+debug_info
endif

default: $(TARGETS)

ebin/%.beam: src/%.erl
	@test -d ebin || mkdir -p ebin
	erlc $(ERLC_ARGS) -o ebin $^

debug:
	make clean
	make DEBUG=1

dialyzer:
	dialyzer -Wunderspecs -Wunmatched_returns -Werror_handling -Wrace_conditions -c ebin/*.beam

clean:
	rm -rf ebin

dist:
	git archive --prefix=gta2k4lin/ HEAD | bzip2 > gta2kserver-$(shell git describe --tags --always HEAD).tar.bz2
