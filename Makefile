# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
# REBAR=$(shell which rebar)
REBAR=./rebar
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile clean dialyze typer distclean \
  deps rebuild test help bench

all: deps compile

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	- $(REBAR) get-deps
	- $(REBAR) compile

compile:
	- $(REBAR) skip_deps=true compile

debug:
	- DEBUG=true LSTATS=true $(REBAR) skip_deps=true compile

clean:
	- $(REBAR) clean

test: debug
	- $(REBAR) eunit

bench:
	git clone git://github.com/basho/basho_bench.git bench/basho_bench || true
	rm -f bench/basho_bench/src/basho_bench_driver_erlqueue.erl
	ln -s ../../basho_bench_driver_erlqueue.erl \
		  bench/basho_bench/src/basho_bench_driver_erlqueue.erl
	cd bench/basho_bench; make; cd ..
	./scripts/ipcrmall
	cd bench; \
		basho_bench/basho_bench --results-dir basho_bench/tests \
			basho_bench_erlqueue.config;
	cd basho_bench; make results

distclean: clean
	- rm -rf .rebar
	- rm -rf deps
	- rm -rf ebin
	- $(REBAR) clean

rebuild: distclean compile dialyze
