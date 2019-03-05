##
## EPITECH PROJECT, 2019
## FUN_deBruijn_2018
## File description:
## Makefile
##

NAME	=	deBruijn

SRC	=	app/Main.hs		\

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .
	mv deBruijn-exe $(NAME)

clean:
	stack clean
	rm -rf .stack-work deBruijn.cabal 

fclean:	clean
	rm -f $(NAME)

re:	fclean all
