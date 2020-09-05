/*
   cc -g -O2 -o sha512loop sha512loop.c
*/

#define HAVE_STDINT_H
#include "sha512.c"

int main(int argc, char **argv)
{
 struct sha512_ctx ctx;
 int num= 1000000, len, i, j;
 char resbuf[64];
 char *text= "The 80 character long default text for being hashed by SHA512, if not  argc > 2.";

 if(argc > 1)
   sscanf(argv[1], "%d", &num);
 if(argc > 2)
   text= argv[2];

 len= strlen(text);
 for(i= 0; i < num; i++) {

   sha512_init_ctx(&ctx);
   sha512_process_bytes(text, len, &ctx);
   sha512_finish_ctx(&ctx, resbuf);

   /* On first pass show result for comparison with: echo -n ... | sha512sum */
   if(i == 0) {
     printf("%s\n", text);
     for(j= 0; j < 64; j++)
       printf("%2.2x", ((unsigned char *) resbuf)[j]);
     printf("\n");
   }

 }
 return(0);
}

