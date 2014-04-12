# Output folder
EBIN_FOLDER="ebin/"

# Erlang program
ERLANG="erl"
RUN_OPTS="-pa"
DEBUG_OPTS="-init_debug"

# Using rebar to compile erlang
REBAR="rebar"
REBAR_FLAGS=""

# Make targets
all: compile

compile: deps
	$(REBAR) $(REBAR_FLAGS) compile

deps:
	$(REBAR) get-deps

test:
	$(REBAR) $(REBAR_FLAGS) skip_deps=true eunit

run: compile
	$(ERLANG) $(RUN_OPTS) $(EBIN_FOLDER)

debug: compile
	$(ERLANG) $(DEBUG_OPTS) $(RUN_OPTS) $(EBIN_FOLDER)

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps



