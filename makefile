# All Targets
CC=gcc

CISC_SRC=test1.c
CISC_OUT=test1

all: clean Run

Run: Cisc

Cisc: 
	$(CC) -o $(CISC_OUT) $(CISC_SRC)
clean: 
	rm -f $(CISC_OUT)
