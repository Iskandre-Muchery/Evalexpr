##
## EPITECH PROJECT, 2020
## fun_evalexpr
## File description:
## Makefile
##

CC	=	stack build --test -v --copy-bins --local-bin-path

CP	=	cp

MV	=	mv

RM	=	rm -rf

CLEAN	=	stack clean

ACTUALNAME	=	funEvalExpr-exe

NAME    =    funEvalExpr

all:    $(NAME)

$(NAME):
	$(CC) ./
	$(MV) $(ACTUALNAME)	$(NAME)

clean:
	$(CLEAN)

fclean:    clean
	$(RM)	$(NAME)


re:    fclean all