/* test.c -- 

   This code is a test for Dionysus.
   Dionysus is a search engine for scientific constants and 
   engineering parameters.

   Copyright (C) 2009 Jean Michel Sellier
   <jeanmichel.sellier@gmail.com>
 
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include<getopt.h>
#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include "dionysus.h"

// global structures here
struct option longopts[] =
{
  { "version", no_argument, NULL, 'v' },
  { "help", no_argument, NULL, 'h' }
};

// global strings here
static char *progname;

// =========================================================================
// File Name     : dionysus.c
// Version       : release 1.0.0
// Creation      : 07 Sept.2009, West Lafayette, IN, USA, Jean Michel Sellier
// Last Revision : 14 September 2009, Cassibile (SR), Italy, JM Sellier
// =========================================================================

int main(int argc,char* argv[]){

  int optc;
  int h=0,v=0,lose=0,z=0;

  progname=argv[0];

  while((optc=getopt_long(argc,argv,"hv",longopts,(int *) 0))!= EOF)
   switch(optc){
    case 'v':
     v=1;
    break;
    case 'h':
     h=1;
    break;
    default:
     lose=1;
    break;
   }

  // check the number of arguments first
  if(argc!=4 && h!=1){
   printf("The number of arguments is wrong\n");
   printf("Try `%s --help' for more information.\n",progname);
   exit(1);
  }

  /* `help' should come first.  If `help' is requested, ignore the other
     options. */
  if(h){
   // Print help info and exit
   // TRANSLATORS: --help output 1 no-wrap
   printf("Dionysus, a search engine for scientific constants and engineering parameters.\n");
   printf("\n");
   // TRANSLATORS: --help output 2 no-wrap
   printf("Usage: %s DBfile constant_name info_name...\n",progname);
   printf("\n");
   printf("Examples:\n");
   printf("> %s dionysus.ddb elementary_charge value\n",progname);
   printf("1.60217646e-19\n");
   printf("> %s dionysus.ddb boltzmann_constant reference\n",progname);
   printf("\"CODATA Recommended Values of the Fundamental Physical Constants: 2006\", Rev. Mod. Phys. 80: 633–730\n");
   printf("\n");
   // TRANSLATORS: --help output 3 : options 1/2 no-wrap
   printf("Report bugs to jeanmichel.sellier@gmail.com\n");
   exit(1);
  }

  if(v){
   // Print version number.
   printf("test for dionysus - GNU dionysus 1.0.0\n");
   // xgettext: no-wrap
   printf("\n");
   printf("\
Copyright (C) %s Sellier Jean Michel.\n\
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A\n\
PARTICULAR PURPOSE.\n\
You may redistribute copies of GNU %s under the terms\n\
of the GNU General Public License.\n\
For more information about these matters, see the file named COPYING.\n",
              "2009","Dionysus");
   exit(1);
  }

  // IF EVERYTHING IS OK THE PROGRAM RUNS HERE
  char output[256];
  dionysus_query(argv[1],argv[2],argv[3],output);
  printf("%s\n",output);

 return 0;
}
