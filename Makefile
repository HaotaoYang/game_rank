APP_NAME = game_rank
all: compile test

###===================================================================
### build
###===================================================================
.PHONY: get-deps co compile run

get-deps:
	rebar3 get-deps

co:compile
compile: get-deps
	rebar3 compile

### clean
.PHONY: clean distclean
clean:
	rebar3 clean

distclean: test_clean
	rebar3 clean -a

###===================================================================
### check
###===================================================================
.PHONY: elvis dialyzer xref
## 检查代码样式
elvis:
	elvis rock

dialyzer:
	rebar3 dialyzer

xref:
	rebar3 xref

###===================================================================
### test
###===================================================================
.PHONY: test eunit ct test_shell test_run test_clean

test: epmd
	rebar3 do eunit -v, ct -v, cover -v

eunit: epmd
	rebar3 do eunit -v, cover

ct: epmd
	rebar3 do ct -v, cover

test_shell:
	rebar3 as test compile
	erl -pa _build/test/lib/*/ebin -pa _build/test/lib/$(APP_NAME)/test

test_run: epmd
	rebar3 as test shell

test_clean:
	@rm -rf _build/test/lib/$(APP_NAME)/test _build/test/logs _build/test/cover

###===================================================================
### other
###===================================================================
.PHONY: help tree epmd

help:
	rebar3 help

tree:
	rebar3 tree

epmd:
	@pgrep epmd 2> /dev/null > /dev/null || epmd -daemon || true
