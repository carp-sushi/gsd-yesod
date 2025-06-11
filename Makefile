.PHONY: all format build lint run clean

all: format build

format:
	@fourmolu -q -i src/*.hs app/*.hs

build:
	@stack build

lint:
	@hlint src/*.hs app/*.hs

run:
	@stack run -- config/settings

clean:
	@stack purge
