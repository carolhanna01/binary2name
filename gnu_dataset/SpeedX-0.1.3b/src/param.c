/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

/*
   Version : 0.1.2c

   License : GPL  

   Command options
 */

#include <unistd.h>

#include <stdio.h>

#include <stdlib.h>

#if 0

#include <getopt.h>

#endif

#include "const.h"

extern int Road_type;

extern int vehicule;

extern int autorun;

extern int dble;

extern int multi;

extern int level;

extern char DisplayName[256];

int OptionParsing(int argc, char *argv[])
{

	int c = 0;

	char optstring[] = "-2SaDd:hVt:v:l:";

#if 0

	struct option longopts[] =
	{

		{"demo", no_argument, NULL, 'a'},

		{"double", no_argument, NULL, 'D'},

		{"multi", no_argument, NULL, '2'},

		{"Road_type", required_argument, NULL, 't'},

		{"vehicule", required_argument, NULL, 'v'},

		{"help", no_argument, NULL, 'h'},

		{"display", required_argument, NULL, 'd'},

		{"version", no_argument, NULL, 'V'},

		{NULL, no_argument, NULL, 0}
	};

	int longindex;

#endif

	while ((c = getopt(argc, argv, optstring)) != EOF) {

		switch (c) {

		case 0:

/*

   if (optarg)

   printf(" with arg %s", optarg);                              

   printf("\n");

 */

			break;

		case 'l':

			level = atoi(optarg);

			if (level < 0)
				level = 0;

			if (level > 3)
				level = 3;

			break;

/*

   case 'S':

   printf("High Speed ..\n");

   high_speed = 1;

   break;

 */

		case 'D':


			dble = 1;

			break;

		case 'd':

			strcpy(DisplayName, optarg);

			break;

		case '2':


		  //multi = 1;
	/* That seems to not work on every machines .. 
			multi = fork() ;
	*/
			multi = 0 ;
			break;

		case 'a':

			autorun = 1;

			break;

		case 'h':

			printf("\n");

			printf
			    ("-S              : High Speed (DEPRECATED)\n");

			printf("-a --demo       : Autorun\n");

			printf
			    ("-D --double     : Double screen (not yet good ..)\n");

			printf
			    ("-2 --multi      : 2 players, 8 4 6 2 and E S F C (AZERTY)\n");

			printf
			    ("-t <num> --Road_type <num>   : Road Choice num = 0 .. 2\n");

			printf
			    ("-v <num> --vehicule <num>  : Vehicule Choice num = 0 .. 2\n");

			printf("-l              : Level  0 .. 3\n");

			printf("-h --help       : Help\n");

			printf("-V --version    : Version\n");

			printf("\n");

			exit(0);

			break;

		case 't':

			Road_type = atoi(optarg);

			if ((Road_type < 0) || (Road_type > 2))
				Road_type = 0;

			break;

		case 'v':

			vehicule = atoi(optarg);

			if ((vehicule < 0) || (vehicule > 2))
				vehicule = 0;

			break;

		case 'V':

			printf("SpeedX version %s\n", VERSION);

			break;

		default:

			exit(1);

			break;

		}

	}

	return 0;

}
