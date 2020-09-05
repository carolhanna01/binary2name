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

#define SIZE 100
#define ELT float


void init_matrix(ELT mat[SIZE][SIZE]);
void zero_matrix(ELT mat[SIZE][SIZE]);
void print_matrix(char *str, ELT mat[SIZE][SIZE]);
void mm(ELT a[SIZE][SIZE], ELT b[SIZE][SIZE], ELT c[SIZE][SIZE]);

float a[SIZE][SIZE], b[SIZE][SIZE], c[SIZE][SIZE];;
at_sema_t *sema;

int main(int argc, char **argv){
  int concurrency=0;
  /*   hrtime_t start, finish;*/
  clock_t start, finish;

   if(argc>1){
     concurrency = atoi(argv[1]);
   }
   
   at_init(concurrency, 2048, 0);
   sema = at_sema_create(0);

   init_matrix(a);
   init_matrix(b);
   zero_matrix(c);

   start=clock();

   mm(a,b,c);
   
 
   finish=clock();

   /*
     print_matrix("A",a);
     print_matrix("B",b);
     print_matrix("C",c);
     */
   printf("Matrix/Matrix product finished\n");
   printf("time in us %e\n", (double)(finish-start));
   exit(0);
}

void init_matrix(ELT mat[SIZE][SIZE]){
  int i, j;
  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      mat[i][j] = ((int)rand())%10;
    }
  }
}

void zero_matrix(ELT mat[SIZE][SIZE]){
  int i, j;
  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      mat[i][j] = 0;
    }
  }
}


void print_matrix(char *str, ELT mat[SIZE][SIZE]){
  int i, j;
  printf("%s\n", str);
  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      printf("%10.2f", mat[i][j]);
    }
    printf("\n");
  }
}

void dot_product(ELT a[SIZE][SIZE], int row, ELT b[SIZE][SIZE], int col,
		ELT *r, at_sema_t *sema) {
  int k;
  ELT res=0;
  for(k=0; k<SIZE; k++){
    res += a[row][k]*b[k][col];
  }
  *r = res;
  at_sema_signal(sema);
}

void mm(ELT a[SIZE][SIZE], ELT b[SIZE][SIZE], ELT c[SIZE][SIZE]){
  int i, j;
  for(i=0; i<SIZE; i++){
    for(j=0; j<SIZE; j++){
      /*c[i][j] = dot_product(a,i,b,j);*/
      at_create_6(at_get_focus(), AT_UNBOUND, (at_userf_6_t *)dot_product, 
		  (at_word_t)a, i, (at_word_t)b, j, (at_word_t)&c[i][j],
		  (at_word_t)sema);
    }
  }
  /* Wait until all threads terminate */
  for(i=0; i<SIZE*SIZE; i++){
    at_sema_wait(sema);
  }
}

