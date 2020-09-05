 /* misc.h - various definitions used in dominion */

#ifndef _MISC_H___
#define _MISC_H___

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


int dom_max_int(int a, int b);
int dom_min_int(int a, int b);

/* Figuring in current expenditure stuff; notice that we first
   divide by 100.0 before we multiply, to avoid overflows.
 */
#define cur_expend(np) \
  ((int) (((np->cur_tech_r_d + np->cur_mag_r_d + np->cur_spy_r_d)/100.0) \
	   * np->money))
#define cur_expend_metal(np) ((int) ((np->cur_tech_r_d_metal/100.0) \
				     *np->metal))
#define cur_expend_jewels(np) ((int)((np->cur_mag_r_d_jewels/100.0) \
				     *np->jewels))

#define next_thon_money(np) ((np->money - cur_expend(np)) \
  + (calc_revenue(np) - calc_expend(np)))
#define next_thon_metal(np) ((np->metal - cur_expend_metal(np)) \
  + (calc_metal (np) - calc_expend_metal(np)))
#define next_thon_jewels(np) ((np->jewels - cur_expend_jewels(np)) \
  + (calc_jewels (np) - calc_expend_jewels(np)))

#define SALT ".."

/* Construction flags */
#define CF_BUBBLE 0x01 /* The nation can produce bubbles */
#define CF_REFINE 0x02 /* the nation can produce refineries */

#define can_bubble(np) (np->consts.flags & CF_BUBBLE)
#define can_refine(np) (np->consts.flags & CF_REFINE)

  /* sector flags */
#define SF_BUBBLE 0x01		/* a bubble has been built */
#define SF_QUARANTINE 0x02	/* people cannot migrate */
#define SF_HIDDEN 0x04		/* sector is cloaked */
#define SF_TRADED 0x08		/* sector has been traded */
#define SF_IMPENETRABLE 0x10	/* sector is impenetrable */
#define SF_HOSTILE 0x20	        /* sector is hostile */
#define SF_REFINE 0x40		/* a refineryhas been built */
#define SF_STOP 0x80            /* Stop enemy armies trying to pass */

  /* routines that check sector flags */
#define has_bubble(sp) (sp->flags & SF_BUBBLE)
#define has_quarantine(sp) (sp->flags & SF_QUARANTINE)
#define has_hidden(sp) (sp->flags & SF_HIDDEN)
#define has_traded(sp) (sp->flags & SF_TRADED)
#define has_impenetrable(sp) (sp->flags & SF_IMPENETRABLE)
#define has_hostile(sp) (sp->flags & SF_HOSTILE)
#define has_refine(sp) (sp->flags & SF_REFINE)
#define has_stop(sp) (sp->flags & SF_STOP)

  /* this is to hide the absolute coordinates from a user */
/* #define xrel(a) (user.nation.id == 0 ? a : a - user.nation.capital.x)
#define yrel(b) (user.nation.id == 0 ? b : b - user.nation.capital.y)
*/
  /* definitions for sector visibility:  this is a bit-field */
#define SEE_NOTHING    0x00	/* not on your map at all */
#define SEE_LAND_WATER 0x01	/* see if it is land or water */
#define SEE_OWNER      0x02
#define SEE_DESIG      0x04
#define SEE_POPULATION 0x08
#define SEE_RESOURCES  0x10	/* see what metal etc... are there */
#define SEE_ARMIES     0x20
#define SEE_ALL        0xff	/* see everything (all bits set) */

                                /* Terrains */
#define MIN_TERRAIN  -5
#define JUNGLE        6
#define FOREST        5         /* Terrain describes how the land looks to */
#define BRUSH         4         /* the creatures on or around it. */
#define GRASSLANDS    3
#define SWAMP         2
#define BARREN        1
#define ICE           0
#define RIVER        -1
#define LAKE         -2
#define REEF         -3
#define BAY          -4
#define OCEAN        -5
                                /* Altitudes by */
#define MOUNTAIN_PEAK 6         
#define MOUNTAINS     5         /* Altitudes describe the height or depth of */
#define PLATEAU       4         /* the land on the specified sector. */
#define HILLS         3         
#define PLAINS        2         
#define LOWLANDS      1         
#define SEA_LEVEL     0         
#define SHALLOWS     -1
#define CONT_SHELF   -2
#define SEA_MOUNT    -3
#define OCEAN_PLAINS -4
#define TRENCH       -5

  /* possible sector designations */
#define D_NODESIG     0		/* designations */
/*#define D_RUIN        1 */
#define D_FARM        1
#define D_METAL_MINE  2
#define D_JEWEL_MINE  3
#define D_CITY        4		/* can draft and build; heavy tax */
#define D_CAPITAL     5		/* nation's capital; even more tax */
#define D_UNIVERSITY  6		/* increase intelligence (costs to maintain) */
#define D_TEMPLE      7		/* religion: + morale */
#define D_TOWN        8		/* in foreign land: allows trade */
#define D_FORT        9		/* must have a garrison */
#define D_HOSPITAL   10		/* decrease death rate (costs to maintain) */
#define D_MAX_DESIG  D_HOSPITAL+1 /* the biggest designation number + 1 */

  /* army_type is used for the general description of
     armies, and is loaded at the beginning of a session
     from the file ("army_types")
   */
struct army_type {
  char type[NAMELEN];		/* such as "Infantry" */
  char type_char;		/* could be 'i' for Infantry */
  float move_factor;		/* multiplies the nation's basic move rate */
    /* should we have separate attack and defense bonus? */
  int bonus;			/* added to nation's basic bonus */
    /* the following describe how much it costs to draft a
       SINGLE soldier of this type, and maintain it, except in
       the case of spell points, where it is the price for the
       ENTIRE army.
     */
  int money_draft, metal_draft, jewel_draft;
  int money_maint, metal_maint, jewel_maint, spell_pts_maint;
  int flags;			/* permanent flags */
  char draft_places [NAMELEN];
};

  /* spirit_type is used for the general description of
     armies, and is loaded at the beginning of a session
     from the file ("spirit_types")
   */
struct spirit_type {
  char type[NAMELEN];		/* such as "eagle" */
  char type_char;		/* could be 'E' for eagle */
  int size;			/* how many "men" */
  float move_factor;		/* multiplies the nation's basic move rate */
    /* should we have separate attack and defense bonus? */
  int bonus;			/* added to nation's basic bonus */
    /* the following describe how much it costs to draft a
       SINGLE soldier of this type, and maintain it, except in
       the case of spell points, where it is the price for the
       ENTIRE army.  The "flags" field is for permanenty army-type
       flags.
     */
  int spell_pts_draft, jewel_draft, jewel_maint;
  int flags;
};

  /* this describes the various types of designation, that their mark
     is, their name, and how much it costs to redesignate to that.  in
     future we might put a general description of designations, and
     also a per-turn cost.
   */
struct s_desig_map {
  char mark;
  char *name;
  int price;			/* how much it costs to redesignate */
  int revenue;			/* per capita revenue in sector */
  int min_employed;		/* how many you need to function */
  int max_employed;		/* how many can be gainfully employed? */
};

struct s_altitude_map {
  char mark;
  char *name;
  int value;
};

struct army_flags {
  char flag;
  char description [EXECLEN];
};

  /* diplomacy statuses */
#define SELF          0
#define UNMET         1
#define JIHAD         2
#define WAR           3
#define HOSTILE       4
#define UNRECOGNIZED  5
#define NEUTRAL       6
#define RECOGNIZED    7
#define FRIENDLY      8
#define ALLIED        9
#define TREATY        10

#define TOO_MUCH_MOVE_COST 5000	/* basically impenetrable */
#define STOP_MOVE_COST 1000     /* Move cost to signify stop flag */
#define PEAK_MOVE_COST 10       /* Move cost for mountain peaks. */

struct item_map { char mark; char *name; }; /* for many things */

#endif /* _MISC_H___ */
