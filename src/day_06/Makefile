INPUT_FILE = input.txt

.PHONY: all compile clean run run1 run2

all: compile run

compile: part1.erl part2.erl
	@echo "Compiling Erlang modules..."
	erlc part1.erl
	erlc part2.erl

run1: part1.beam $(INPUT_FILE)
	@echo "Running Part 1..."
	@erl -noshell -s part1 main -s init stop

run2: part2.beam $(INPUT_FILE)
	@echo "Running Part 2..."
	@erl -noshell -s part2 main -s init stop

run: run1 run2

clean:
	@echo "Cleaning up compiled Erlang files..."
	rm -f part1.beam part2.beam