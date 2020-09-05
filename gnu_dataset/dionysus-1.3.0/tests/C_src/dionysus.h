/* dionysus.h -- 

   This code is part of Dionysus.
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

// **********************************************************************
// Created : 07 sept. 2009, Jean Michel Sellier, West Lafayette, IN, USA
// Last modified : 14 sept. 2009, JM Sellier, Cassibile (SR), Italy
// **********************************************************************

// C query in the specified DB
void dionysus_query(char *dbfilename,char *material,char *option,char *retvalue){
 int err;
 FILE *fp;
 char string[512];
 sprintf(string,"dionysus_query %s %s %s > diony.out",dbfilename,material,option);
 err=system(string);
 if(err!=0){
  printf("An error occured while trying to query the specified DB!\n");
  exit(0);
 }
 fp=fopen("diony.out","r");
 fscanf(fp,"%s",string);
 fclose(fp);
 sprintf(retvalue,"%s",string);
 system("rm diony.out");
}

