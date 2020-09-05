#include "swuser_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "strob.h"

main ()
{
   STROB * strb;
   int i=0,j=0,c;
   int u_n,u_size, counter;
   char *p;
   char ch='0', str[1024];
   long s0,s1,d0,d1,offset;


   i=0;
   for(;;)
     {
       printf("\n"
"STROB test program menu:\n"       
"0.exit  1.open  2.close  3.strob_cpystr  4. strob_catstr 5. len 6. reopen \n");
       printf(" ENTER CHOICE: <%c> ", ch); scanf ( "%c", &ch);

        switch(ch)
          {
            case '0':
              exit(0);

            case '1':
              printf("strob_open: ");
              strb = strob_open(0);
              if(!strb)
                { 
                  printf(" Open failure on filename: \n");
                  break;
                } 
              break;  

            case '2':
              printf("strob_close:\n");
             if(strob_close(strb))
                printf(" edclose error\n");
              break;  

            case '3':
              printf("strb_cpystr:\n "); 
              printf("Enter string :\n");
              scanf ("%s", &str);
              printf("strob_cpystr: enter index ? <%d> ",i=0); 
              scanf ( "%d", &i);
              if(!strob_cpystr(strb, i, str))
                printf("ERROR \n");
              break;

            case '4':
              printf("strob_catstr\n");
              printf("Enter string :\n");
              gets(str);
              if(!strob_catstr(strb,str))
                printf("ERROR \n");
              else 
                printf("%s\n",str);  
              break;

            case '5':
              printf("strob_strlen: %d\n", (int)strob_strlen (strb)); 
              break;  

            case '6':
              printf("strob_reopen : enter new size \n"); 
              scanf ("%d", &i);
              if (! strob_reopen (i, strb ))
                printf ("reopen ERROR \n");
              break;
         }

     getchar ();
     }
}

