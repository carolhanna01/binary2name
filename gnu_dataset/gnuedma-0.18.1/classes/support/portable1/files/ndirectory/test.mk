
CC=gcc
CFLAGS=`edma-config --cflags-exe`
LIBS=`edma-config --libs-exe`

all: test
test: test.c
	$(CC) $(CFLAGS) $< -o $@ $(LIBS)
configure:
	echo "No configuration is necessary."
clean:
	rm -f core *.o *~ test
