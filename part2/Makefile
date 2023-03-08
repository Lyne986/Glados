##
## EPITECH PROJECT, 2023
## Untitled (Workspace)
## File description:
## Makefile
##

STACK		=	stack

LOCAL_PATH	=	$(shell stack path --local-install-root)
EXE			=	$(LOCAL_PATH)/bin
NAME		=	glados

all: build
	cp $(EXE)/part2 ./$(NAME)

build:
	$(STACK) build

clean:
	@echo "Cleaning..."
	$(STACK) clean
	@echo "Cleaning done."

fclean:
	@echo "Cleaning..."
	@$(RM) $(NAME)
	@$(RM) -r .stack-work
	@echo "Cleaning done."

tests: all
	@echo "Testing..."
	@$(STACK) test

ftests: all
	@echo "functionnal Testing..."
	@./test/launch_tests.sh

re: fclean all
