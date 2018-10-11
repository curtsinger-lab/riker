
all: trace

clean:
	rm -f trace

trace: trace.c
	clang -o trace trace.c

