
CC=gcc
CFLAGS=`edma-config --cflags-exe`
LIBS=`edma-config --libs-exe`

all: fs_test
fs_test: fs_test.c
	$(CC) $(CFLAGS) $< -o $@ $(LIBS)
configure:
	echo "No configuration is necessary."
clean:
	rm -f core *.o *~ fs_test
