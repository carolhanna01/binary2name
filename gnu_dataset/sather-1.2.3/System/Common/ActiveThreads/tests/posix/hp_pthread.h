/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

/*
  An ugly hack - needed this, because Berkeley HPs running HPUX 10.20
  for some reason are missing pthread.h and other DCE headers..
  Had to figure out DCE types to do measurements ;-)
  */

typedef struct {
  char buf[1000];
}pthread_t;

typedef struct {
  char buf[1000];
}pthread_mutex_t;

typedef struct{
  unsigned long f1;
  unsigned long f2;
} pthread_attr_t;


extern pthread_attr_t pthread_attr_default;
extern pthread_attr_t pthread_mutexattr_default;
