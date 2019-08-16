all: A B

A: A.c
	gcc A.c -o A

B: B.c
	gcc B.c -o B

.PHONY: clean
clean:
	@rm -f A
	@rm -f B
