all: grammar

grammar:
	@java -cp 'lib/*' org.antlr.Tool -fo src/java \
		src/antlr/Template.g
	@lein javac

.PHONY: all grammar
