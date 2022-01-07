##
## EPITECH PROJECT, 2022
## B-PDG-300-STG-3-1-PDGD02-sylvian.burn
## File description:
## Makefile
##

CC    =    ghc -dynamic

SRC    =    DoOp.hs

EXEC    =    doop

all:    $(EXEC)

$(EXEC):    $(OBJ)

$(CC) -o $(EXEC) $(SRC)

clean:
    rm -rf *.hi *.o

fclean: clean
    rm -rf $(EXEC)

re:    fclean all

.PHONY: clean fclean re all