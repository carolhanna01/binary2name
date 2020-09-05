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

#define N 1000
#define M 1000
#define ELT float


void init_matrix(ELT mat[N][M]);
void init_vector(ELT v[N]);
void zero_matrix(ELT mat[N][M]);
void print_matrix(char *str, ELT mat[N][M]);
void print_vector(char *str, ELT v[N]);
void vm(ELT v[N], ELT a[N][M], ELT r[N]);

ELT a[N][M];
ELT v[N], r[M];

at_sema_t *sema;

clock_t start, finish;
  int concurrency=0;

int main(int argc, char **argv){

   if(argc>1){
     concurrency = atoi(argv[1]);
   }
   
   at_init(concurrency, 0x5000, 0);
   sema = at_sema_create(0);

   init_matrix(a);
   init_vector(v);


   vm(v,a, r);
   
   /*   print_vector("Vector v", v);
   print_matrix("Matrix a", a);
   print_vector("Result r", r);*/
   exit(0);
}

void init_matrix(ELT mat[N][M]){
  int i, j;
  for(i=0; i<N; i++){
    for(j=0; j<M; j++){
      mat[i][j] = ((int)rand())%10;
    }
  }
}

void init_vector(ELT v[N]){
  int i;
  for(i=0; i<N; i++){
    v[i] = ((int)rand())%10;
  }
}

void zero_matrix(ELT mat[N][M]){
  int i, j;
  for(i=0; i<N; i++){
    for(j=0; j<M; j++){
      mat[i][j] = 0;
    }
  }
}


void print_matrix(char *str, ELT mat[N][M]){
  int i, j;
  printf("%s\n", str);
  for(i=0; i<N; i++){
    for(j=0; j<M; j++){
      printf("%10.2f", mat[i][j]);
    }
    printf("\n");
  }
}

void print_vector(char *str, ELT v[N]){
  int i;
  printf("%s\n", str);
  for(i=0; i<N; i++){
      printf("%10.2f", v[i]);
  }
  printf("\n");
}

void dot_product(ELT v[N], ELT a[N][M], int col, 
		 ELT *r, at_sema_t *sema) {
  int i;
  ELT res=0;

  /*  at_printf("proc: %d  col: %d\n", at_vproc(), col);*/
  for(i=0; i<N; i++){
    res += v[i]*a[i][col];
  }
  *r = res;
  at_sema_signal(sema);
}

void vm(ELT v[N], ELT a[N][M], ELT r[M]){
  int i;
  
  /* Stop to start running everything simulaneously */
  at_stop();


  for(i=0; i<M; i++){
    at_create_5(at_get_focus(), AT_UNBOUND, (at_userf_5_t *)dot_product, 
		(at_word_t)v, (at_word_t)a, i, (at_word_t)&r[i], (at_word_t)sema); 
    /*dot_product(v,a,i,&r[i],sema);*/    
    /*at_create_5((i/16)%concurrency, dot_product, v, a, i, &r[i], sema);     */

  }
 
  start=clock();

  at_continue();

  /* Wait until all terminate */
  for(i=0; i<M; i++){
    at_sema_wait(sema);
  }

  finish=clock();

  printf("vector/matrix product finished\n");
  printf("time in us %e\n", (double)(finish-start));

}


