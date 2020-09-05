  /* dominion.h -- general include file for dominion */

#ifndef _DOMINION_H___
#define _DOMINION_H___

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

#include <curses.h>
#include <stdlib.h>

#include <config.h>
#include "dom_config.h"

#include "common.h"
#include "costs.h"
#include "bonds.h"
#include "cn.h"
/* note: at the end of this file we will be including proto.h */

/* user-definable params */
#define WORLD_FILE "world"
#define REF_CARD_FILE "doc/refcard" /* basic reference card */
#define INFO_FILE "doc/dominion.info" /* complete documentation */
#define RACES_FILE "misc/races"
#define DIPLO_FILE "dom_diplo"	/* diplomacy matrix stored here */
#define INIT_DIPLO_FILE "init_diplo" /* remember initial diplomacy matrix */
#define CARGO_FILE "misc/cargo" /* stores cargo exchanges */
#define HANGING_SPELLS_FILE "hanging_spells" /* currently active spells */
  /* any file that starts with the MAG_PREFIX is a description
     of a magical order.  the name of the order follows the prefix.
     a list of possible magic orders is in mag_orders.
   */
#define MAG_ORDERS "magic/mag_orders"
#define MAG_PREFIX "magic/mag_"
#define TECHNO_FILE "misc/techno_levels"
#define NEWS_POST_FILE "news.post"
#define EDITORS_FILE "safe_editors"
#define WORLD_OPTIONS "world_options"

#define MAIL_DIR   "mail"	/* Which dir (in lib) to place mail */
#define NEWS_DIR   "news"       /* Which dir (in lib) to keep news */
#define NGDB_FILE  "groups"     /* What file in NEWS_DIR to hold the news
				   groups database */
#define NEWS_GROUP "News"	/* What file for update information */

#define OPT_DIR    "options"    /* Which dir (in lib) to keep options */
#define BONDS_DIR  "bonds"	/* which dir (in lib) to keep bonds */

#define ARMY_TYPES_FILE "misc/army_types" /* army type descriptions are here */
 /* spirit type descriptions are here */
#define SPIRIT_TYPES_FILE "misc/spirit_types"
#define CN_FILE   "misc/cns"	/* file where cn info is kept */

  /* number of idle seconds before timeout;
     (5*60) would be 5 minutes; set this to 0 for no timeout
   */
#define TIMEOUT_SECS (5*60)

  /* now some defines for how the CNs move */
#define CN_FIGHT TRUE		/* if this is TRUE, CNs fight each other */
#define CN_VIEW 4		/* how far CN armies look to move */
#define CN_SIDE (CN_VIEW*2+3)	/* length of the side of the "looking" array */
#define MAX_TYPES 100		/* maximum number of armies CN considers */
#define DIP_CHANGE 75		/* Percent chance for cn diplomacy change */

#define NOT_CN 0		/* values of the Snation.cn_flag */
#define CN_NOMAIL 1		/* CN that does not get mail */
#define CN_MAIL 2		/* CN that gets mail */
  /* now a macro that allows us to quickly see if an CN should get mail */
#define gets_mail(np) ((np)->cn_flag == NOT_CN || (np)->cn_flag == CN_MAIL)

#define NAMELEN 20		/* length of most names */
#define PASSLEN 15
#define PATHLEN 200		/* length of directory paths */

/* should typedef mark for X and other windows...; some day if ever...  */
typedef char Symbol;		/* just a char for curses */

struct pt {
  int x, y;
};
typedef struct pt Pt;

  /* this structure identifies an army by its owner and id.
     it is used in the list of armies present in each sector.
   */
struct armyid {
  int owner, id;
  struct armyid *next;
};

  /* defines a cargo carried by some moving object (caravan or navy) */
struct scargo {
  int money, metal, jewels, food, people, army;
  Pt title;
};

typedef struct scargo Scargo;

struct sarmy {
  char type[NAMELEN],		/* string (i.e. "Infantry","ent")   */
       name[NAMELEN];		/* the army's name */
  int  n_soldiers,		/* number of soldiers in the army   */
       status,			/* ATACK|DEFEND|OCCUPY|...          */
       id,			/* army number                      */
       owner,			/* nation number of army owner      */
       mvpts,			/* move points                      */
       mvratio;			/* fraction of movement used        */
  long flags;			/* bitmap, fields defined in army.h */
  Pt   pos;			/* where the army is                */
  int  sp_bonus;          /* extra bonus, on top of basic nation bonus */
    /* the maintainance costs are all per-soldier, except the
       spell points, which apply to the entire army.
     */
  int  money_maint, metal_maint, jewel_maint, spell_pts_maint;
  Scargo cargo;
  struct sarmy *next;		/* pointer to next army             */
};
typedef struct sarmy Sarmy;

struct pt_list {
  Pt pt;			/* list of owned sectors */
  struct pt_list *next;
};

  /* this contains the basic properties of a race, which are read in
     from the "races" file.  this does NOT include other attributes
     gained later by a nation.
   */
struct srace {
  char name[NAMELEN];
  char mark;			/* to represent them on map */
  int strength, repro, mortality, intel, speed, stealth;
  int pref_alt, pref_terrain, pref_climate;
  int mag_apt, farming, mining;
};
typedef struct srace Srace;

struct race_list {
  Srace race;
  struct race_list *next;
};

/* This structures stores a nations options */
struct soptions {
  int expert_mode;
  int civ_movemode;
  char *news_forward;
  char *mail_forward;
  char *mail_reader;
  char *editor;
};
typedef struct soptions Soptions;

struct sconstruction {
/* These are the abilitiy levels of the nations to produce things */
  short roads,     /* This is the best roads that can be built */
        forts,     /* Theese are the best forts that can be built */
        reserved;  /* this is reserved for later expansion */
  int flags; /* These are the flag constructions.  On if allowed */
};
typedef struct sconstruction Sconstruction;

struct snation {
  int id;			/* for quick search */
  char name[NAMELEN];
  char leader[NAMELEN];
  char passwd[PASSLEN];		/* encrypted, of course */
  Pt capital;			/* coordinates of main city */
  Srace race;
  Symbol mark;			/* nation mark */
    /* parameters that influence the economy */
  int taxes, taxtype, charity, money, jewels, metal, food, n_sects,
      tech_r_d, tech_r_d_metal, mag_r_d, mag_r_d_jewels, spy_r_d,
      cn_flag;
  
  char mag_order[NAMELEN];	/* which magical order they belong to */
  int tech_skill, mag_skill, farm_skill, mine_skill; /* skills */
  int spell_pts;		/* available spell points */
    /* military stuff */
  int combat_bonus, spy, secrecy;
  int n_armies;
  Sarmy *armies;		/* linked list of armies */
  struct pt_list *ptlist;	/* list of owned points */
  int cur_mag_r_d, cur_mag_r_d_jewels;
  int cur_tech_r_d, cur_tech_r_d_metal;
  int cur_spy_r_d;
  Sconstruction consts;
  Sbonds *bondlist;		/* status of issued bonds */
  Soptions *opts;
  Scomp_params *cn_params;	/* Computer nation parameters */
};
typedef struct snation Snation;

#define NORMAL_MAP  0		/* map_style: non-compact map display */
#define COMPACT_MAP 1		/* map_style: compact map display */
#define DESIGNATION 0		/* display: show sector designation */
#define NATION_MARK 1		/* display: show nation mark */
#define SOIL        2		/* display: show vegetation */
#define METAL	    3		/* etc... */
#define JEWELS      4
#define ALTITUDE    5
#define CLIMATE     6
#define POPULATION  7
#define MOVECOST    8
#define TERRAIN     9
#define ARMY_MOVECOST 10

#define H_NONE        0		/*            don't highlight */
#define H_OWNED       1		/* highlight: if you own sector */
#define H_ARMIES      2		/*            if there are armies */
#define H_YOUR_ARMIES 3		/*            if they are your armies */
#define H_OTHER_ARMIES 4	/*            if they are not yours */
#define H_MOVE_LEFT   5		/*            if armies can move */
#define H_UNEMP       6		/*            if there are civ's unemployed */
#define H_HOSTILE     7		/*            if the sector is hostile */
#define H_PATROL      8		/*            if YOUR sector is patrolled */
#define H_INTERCEPT   9		/*            if YOUR sector is intercepted */
#define H_P_OR_I      10	/*            if YOUR sector is pat. or int. */

  /* the following prepare the ground for the exec list array */
#define N_EXEC_ARGS 12		/* for now, exec has 8 args */
#define N_EXECS     2		/* # of exec commands before we save to file */
#define EXECLEN     100		/* max length of exec string */

  /* spell structure, used for describing a spell, and also
     for giving each user a list of available spells
   */
struct sspell {
  char name[NAMELEN];		/* name of spell */
  int cost, duration;
/*  int (*spell_func)(); */     /* pointer to function that does the work */
  struct sspell *next;		/* for a linked list */
};
typedef struct sspell Sspell;

struct sh_spell {
  int nat_id;
  char name[NAMELEN];
  int thons_left;
  int n_lines;
  char **lines;
  struct sh_spell *next;
};
typedef struct sh_spell Sh_spell;

  /* avail army structure, used for describing available armies */
struct savail_army {
  char type[NAMELEN];		/* type of army */
  struct savail_army *next;	/* for a linked list */
};
typedef struct savail_army Savail_army;

  /* spirit structure, used for describing available spirits */
struct sspirit {
  char type[NAMELEN];		/* type of spirit */
    /* cost can be different for different
       mag orders, even if it is the same
       spirit
     */
  int cost;
  struct sspirit *next;		/* for a linked list */
};
typedef struct sspirit Sspirit;

/* the following structure is the matrix used for diplomacy */
/* for more info, see diplomacy.c */
struct sdiplo {
  int self_id, neighbor_id;  /* player, and his neighbor */
  int status;                /* status from player to neighbor */
};
typedef struct sdiplo Sdiplo;

  /* this structure describes run-time information for the user
     currently playing.  It is contains their nation struct.
   */
struct suser {
  int id;			/* nation id */
  Snation *np;			/* describe her/his nation */
  Pt cursor, center;		/* current sector and middle of screen */
  char help_char;		/* char used to get help */
    /* display styles */
  int map_style, display, highlight, underwater;
  int **visible_sectors;	/* which sectors can the user see? */
  Sdiplo **diplo_matrix;	/* fast-access diplo matrix */
  int n_execs;			/* how many exec lines already there */
  char exec_lines[N_EXECS][EXECLEN];
  int current_army;		/* army that has been picked */
  int just_moved;		/* has the user just moved? */
    /* how much did we have at the start of the turn */
  int init_money, init_metal, init_jewels, init_food;
/*should have other run-time info, taken from world parameters, like date*/
    /* available types armies to that user */
  Savail_army *avail_armies;
    /* this is read in at the beginning of a session, and has a
       list of spells available to this user.  same thing for spirits.
     */
  Sspell *spell_list;
  Sh_spell *h_spells;
  Sspirit *spirit_list;
    /* this is a funny global variable, needed for
       consistency in drawing the army list
     */
  int last_n_armies;
    /* number of army/spirit types there are */
  int n_army_types, n_spirit_types;
    /* now some user-configurable options */
  int xmode, show_sect_win;
};
typedef struct suser Suser;

struct ssector {
  Pt loc;		/* x, y coords */
	/* if terrain is water, some of these don't apply */
        /* a lot must be re-considered here */
  short int terrain, altitude, climate, designation, soil, metal, jewels;
  short int defense;			/* defense bonus in sector */
  short int roads;			/* how easy to travel */
  short int owner;			/* index of sector's owner */
  int n_people;			/* civilians in that sector */
  long flags;			/* sector flags */
  char *name;			/* you can name a sector!!! */
    /* the sector has a list of army id's, where the owner and army
       id are stored, so the army can be obtained from the nation's
       army list.
     */
  struct armyid *alist;
};
typedef struct ssector Ssector;


/* world topology type */
#define TORUS 0

extern int (*wrapx)(int x, int y), (*wrapy)(int x, int y);

struct sgeo {
  int topology;		        /* topology type */
  int pwater;			/* % water in world */
  double metal_avg, jewel_avg, soil_avg; /* averages for the world */
};			        /* geography */
typedef struct sgeo Sgeo;

struct sworld {
  Sgeo geo;			/* geography */
  int turn;
  int xmax, ymax;
  Ssector **map;
  int n_nations;		/* how many nations are actually there? */
/*  Snation nations[NATIONS]; */
  Snation *nations;
};
typedef struct sworld Sworld;

struct argument {
  int type;
  union {
    int num;
    char str[20];
  } data;
};

#define NUM 0    /* values for -type- field of above structure */
#define TXT 1

#define NUMCMDS 200

  /* declare malloc() as returning a void *.  Don't even
     try including malloc.h, since it is not there on very
     many systems.  If you don't have type "void", then
     you can make it be "char *malloc()".
   */
void *malloc();

#ifndef max
#define max(a,b) ((a)<(b) ? (b) : (a))
#endif
#ifndef min
#define min(a,b) ((a)>(b) ? (b) : (a))
#endif

#ifdef PMAX
#define mvwprintw mymvwprintw
#endif

extern char help_tag[];
extern Sdiplo **initial_diplo;

#include "proto.h"		/* include this once the types are defined */

#endif /* _DOMINION_H___ */
