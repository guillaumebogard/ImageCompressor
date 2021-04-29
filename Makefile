##
## EPITECH PROJECT, 2021
## stack
## File description:
## Makefile
##

BINARY_PATH		:= $(shell stack path --local-install-root)

COVERAGE_PATH   := $(shell stack path --local-hpc-root)

NAME			=	ImageCompressor

TA_NAME			=	imageCompressor

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(TA_NAME)

clean:
	stack clean

fclean: clean
	stack purge
	$(RM) $(TA_NAME)

re:: fclean
re:: all

tests_run:
	stack test --coverage

coverage:
	google-chrome-stable $(COVERAGE_PATH)/index.html


.PHONY: all clean fclean re tests_run coverage
