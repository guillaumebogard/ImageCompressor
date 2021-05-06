##
## EPITECH PROJECT, 2021
## stack
## File description:
## Makefile
##

BINARY_PATH		:= $(shell stack path --local-install-root)

COVERAGE_PATH   := $(shell stack path --local-hpc-root)

STACK_NAME		=	ImageCompressor

NAME			=	imageCompressor

all:
	stack build
	cp $(BINARY_PATH)/bin/$(STACK_NAME)-exe ./$(NAME)
.PHONY:	all

clean:
	stack clean
.PHONY:	clean

fclean: clean
	stack purge
	$(RM) $(NAME)
.PHONY:	fclean

re:: fclean
re:: all
.PHONY:	re

tests_run:
	stack test --coverage
.PHONY:	tests_run

coverage:
	google-chrome-stable $(COVERAGE_PATH)/index.html
.PHONY:	coverage
