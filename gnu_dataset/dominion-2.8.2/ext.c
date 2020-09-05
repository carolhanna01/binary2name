  /* declaration of various shared variables */

/*
 * Copyright (C) 1990 Free Software Foundation, Inc.
 * Written by the dominion project.
 *
 * This file is part of dominion.
 *
 * dominion is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "dominion.h"
#include "misc.h"

Suser user;
Sworld world;
int compressed_world=0;		/* is world file compressed? */
int slow_flag = 0;		/* slow down reading of world file? */
int timeout_secs = 0;           /* Don't timeout by default */
struct race_list *races;	/* list of races available */
int total_army_types;		/* how many total army types are there */
struct army_type *army_types;	/* array of available armies */
struct spirit_type *spirit_types; /* array of available spirits */
Sh_spell *hanging_spells;
char libdir[200];		/* the dir. the game lives in */
	/* users's current directory */
#ifdef BSD			/* BSD does not return a static string */
char current_dir[PATHLEN];
#else /* BSD */
char *current_dir;
#endif /* BSD */

int ruid, euid;			/* user ids */
char help_tag[EXECLEN];		/* current help topic */
int is_in_diplo = 0;

  /* each ascii key points to a function which handles that key */
int (*wrapx)();
int (*wrapy)();
int (*keymap[128])();

int debug = 0;

struct s_desig_map desig_map[]
  = {  {'x', "none", 1000, 30, 0, 7},
	 {'f', "farm", 5000, 100, 10, 500},
	 {'m', "mtl. mine", 10000, 100, 10, 800},
	 {'j', "jwl. mine", 10000, 100, 10, 800},
	 {'c', "city", 30000, 200, 300, 5000},
	 {'C', "capital", 50000, 300, 300, 7000},
	 {'u', "university", 10000, 30, 200, 1000},
	 {'+', "temple", 5000, 0, 200, 1000},
	 {'T', "town", 10000, 150, 10, 300},
	 {'!', "fort", 10000, 50, 10, 200},
	 {'h', "hospital", 10000, 100, 10, 300}};
/* SHU Kludge Time */
/*
	 {'r', "refinery", 8000, 130, 100, 200} };
*/

/* Lowest altitude must come first in this list and they must be in order */

struct s_altitude_map altitude_map[]
  = {  {'v',"Trench",TRENCH}, {'-',"Ocean Plain",OCEAN_PLAINS},
	 {'^',"Sea Mountain",SEA_MOUNT}, {'+',"Cont. Shelf",CONT_SHELF},
	 {'#',"Shallows",SHALLOWS}, {'~',"Sea Level",SEA_LEVEL},
	 {'.',"Lowlands",LOWLANDS}, {'-',"Plains",PLAINS},
	 {'%',"Hills",HILLS}, {'=',"Plateau",PLATEAU},
	 {'^',"Mountains",MOUNTAINS}, {'+',"Mountain Peak",MOUNTAIN_PEAK} };

struct item_map terrains[] 
  = {  {'O',"Ocean"}, {'B',"Bay"}, {'R',"Reef"}, {'L',"Lake"}, {'R',"River"},
	 {'#',"Ice"}, {'b',"Barren"}, {'s',"Swamped"}, {'g',"Grassy"}, 
	 {'B',"Brush"}, {'f',"Forested"}, {'j',"Jungle"} };

struct item_map climates[]
  = {  {'D',"Desert"}, {'d',"Semiarid"}, {'h',"Humid Subtropical"},
	 {'t',"Tropical Wet and Dry"}, {'T',"Rainy Tropical"},
	 {'s',"Dry Summer Subtropical"}, {'m',"Mid-latitude Marine"},
	 {'w',"Humid Continental Warm"}, {'c',"Humid Continental Cool"},
	 {'a',"Subarctic"}, {'A',"Polar"} };

char *dip_status[] = { " SELF" , " UNMET", "JIHAD", "WAR", "HOSTILE", 
                       "UNRECOGNIZED", "NEUTRAL", "RECOGNIZED", "FRIENDLY", 
                       "ALLIED", "TREATY" };

struct item_map tradeitems[] 
  = {  {'g', "GOLD"}, {'m', "METAL"}, {'j', "JEWELS"}, {'f', "FOOD"},
	 {'e', "END"} };

struct army_flags army_flags[] =
{
  {'F', "This army flies"},
  {'H', "This army is hidden"},
  {'V', "This army will possess the dead"},
  {'T', "This army is in transport"},
  {'^', "This army throws missiles"},
  {'W', "This army travels in water"},
  {'f', "This army is front line"},
  {'k', "This army is kamikaze"},
  {'m', "This army is a machine"},
  {'d', "This army can disguise itself"},
  {'w', "This army can cast spells"},
  {'s', "This army can do sorcery"},
  {'c', "This army can carry cargo"},
  {'U', "This army burrows underground"},
  {'L', "This army travels on land"},
  {'I', ""},
  {'u', "This army is uncontrolled"},
  {'R', "This army is race-specific"},
  {' ', ""},{' ', ""},{' ', ""},{' ', ""},
  {' ', ""},{' ', ""},{' ', ""},{' ', ""},{' ', ""},{' ', ""},{' ', ""},
  {' ', ""},{' ', ""},{' ', ""}};

char *update_time;
char *civ_move[] =  { "None", "Restricted", "Free" };
Sh_spell *dead_spells = NULL;
Sdiplo **initial_diplo;


