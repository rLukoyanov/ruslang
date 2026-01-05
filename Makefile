# Makefile
all: compiler

compiler: main.go
	go build -o compiler main.go

run: compiler
	./compiler example.model

clean:
	rm -f compiler generated.go

test:
	go test -v ./...

.PHONY: all run clean test