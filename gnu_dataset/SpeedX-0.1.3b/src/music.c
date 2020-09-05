
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

#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include "music.h"

#include "config.h"

#if LIBMIKMOD

#include <mikmod.h>


extern int mikmod;

void init_music(void)
{
	/*

	   Libmikmod Initialisation

	 */

#if LIBMIKMOD

	printf(". Libmikmod Initialisation ...");

	fflush(stdout);

//        MikMod_RegisterDriver(&drv_nas);
	MikMod_RegisterDriver(&drv_AF);
	MikMod_RegisterDriver(&drv_esd);
	MikMod_RegisterDriver(&drv_aix);
	MikMod_RegisterDriver(&drv_alsa);
	MikMod_RegisterDriver(&drv_hp);
	MikMod_RegisterDriver(&drv_oss);
	MikMod_RegisterDriver(&drv_sam9407);
	MikMod_RegisterDriver(&drv_sun);
	MikMod_RegisterDriver(&drv_ultra);


	MikMod_RegisterAllLoaders();


	md_mode |= DMODE_INTERP;

	md_reverb = 1;

	if (MikMod_Init("")) {

		printf("Error: (%s)\n", MikMod_strerror(MikMod_errno));

		mikmod = 0;

	} else {

		printf("OK\n");

		mikmod = 1;

	}

#else

	printf("Libmikmod disactivated.\n");
#endif
}

char *
 load_music(void)
{
	DIR *cdr;
	struct dirent *d;
	char *nom_music = NULL;

	fprintf(stderr, "Loading music ..\n");

	if ((cdr = opendir("music")) == NULL)
		fprintf(stderr, "Problems opening directory: music\n");
	else {
		int nb_music = 0;
		int choice = 0;

		while (readdir(cdr) != NULL)
			nb_music++;
		closedir(cdr);
		if (nb_music == 2)
			fprintf(stderr, "No music found in ./music ..\n");
		else {
			cdr = opendir("music");
			d = readdir(cdr);
			d = readdir(cdr);


			choice = 1 + rand() % (nb_music - 2);
			while (choice--)
				d = readdir(cdr);
			printf("Musique : %s\n", d->d_name);
			nom_music = (char *)
			    malloc((6 + strlen(d->d_name) * sizeof(char)));
			strcpy(nom_music, "music/");
			strcat(nom_music, d->d_name);
		}
	}

	return nom_music;
}
#endif
