/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#include <stdio.h>
#include <math.h>
#include <time.h>
#include "at.h"

#define SIZE 200


/* Simple parallel Gaussian elimination */
void init_matrix();
void gauss();
void eliminate_elt(int p, int row, at_sema_t *sema);
void print_matrix(char *str);

float a[SIZE][SIZE];
int n;


int main(int argc, char **argv){
  int concurrency=0;
  /*hrtime_t start, finish;*/
  clock_t start, finish;

   if(argc>1){
     concurrency = atoi(argv[1]);
   }
   
   n= SIZE;

   at_init(concurrency, 0x1000, 0);

   init_matrix(a);
   
   /*   print_matrix("BEFORE"); */
   start=clock();

   gauss();
   
   finish=clock();
   /*print_matrix("AFTER");*/
   printf("GAUSS finished\n");
   printf("time in us %e\n", (double)(finish-start));
   exit(0);
}

void init_matrix(){
  int i, j;
  for(i=0; i<n; i++){
    for(j=0; j<n; j++){
      a[i][j] = ((int)rand())%10;
    }
  }
}


void print_matrix(char *str){
  int i, j;
  printf("%s\n", str);
  for(i=0; i<n; i++){
    for(j=0; j<n; j++){
      printf("%6.2f", a[i][j]);
    }
    printf("\n");
  }
}

void gauss(){
  int j, p;
  at_sema_t *sema;

  sema = at_sema_create(0);
  
  /* Loop over pivots */
  for(p=0; p<n; p++){
    for(j=p+1; j<n; j++){
      /* Create a thread for each row */
      at_create_3(at_get_focus(), AT_UNBOUND, (at_userf_3_t *)eliminate_elt, p, j, (int)sema);
    }
    /* wait until all done */
    for(j=p+1; j<n; j++){
      at_sema_wait(sema);
    }
  }
}

void eliminate_elt(int p, int row, at_sema_t *sema){
  int j;
  float k;

  k = a[row][p]/a[p][p];
  
  for(j=0; j<n; j++){
    /* for(j=p; j<n; j++){ */
    a[row][j]=a[row][j]-k*a[p][j];
    /*a[j][row]=a[j][row]-k*a[j][p];*/
  }
  /* signal completion */
  at_sema_signal(sema);
}


