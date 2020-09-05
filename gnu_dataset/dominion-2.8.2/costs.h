  /* costs.h - costs of various things in dominion */

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

#ifndef _COSTS_H
#define _COSTS_H

/* See if this is the option we are considering */
#define is_option(str, sstr) (!(strncmp(str, sstr, strlen(sstr))))

/* 
  NOTE!!!!!  If you want to add a tunable parameter, then you need
  to add one entry in the first half of the #ifdef, and then another
  entry referencing the value as an external variable after the #else.
*/

#ifdef IN_FILE_C
int FORT_COST_MONEY = 1000;     /* for each 10 levels you construct */
int FORT_COST_METAL = 1500;
int ROADS_COST_MONEY = 100;     /* for each road you construct */
int ROADS_COST_METAL = 150;
int BUBBLE_COST = 20000;        /* cost to build a bubble */
int BUBBLE_COST_METAL = 20000;
int REFINE_COST = 8000;        /* cost to build a refinary */
int REFINE_COST_METAL = 1000;
int ARMY_OVERHEAD = 2000;       /* overhead cost to maintain an army/turn */
int HOSPITAL_MAINT_COST = 4000; /* cost to maintain a hospital */
int UNIV_MAINT_COST = 2000;     /* cost to maintain a university */
int OCCUPYING_SOLDIERS = 100;   /* min. soldiers to occupy a sector */
double REFINERY_FACT = (20.0/100.0); /* percent added by an active refinery */
int FOOD_PROD = 4;              /* production of food/farmer : tons */
double EAT = 1.3;               /* how much a person eats in 1 thon : tons */
double SOLD_EAT_FACTOR = 1.2;   /* multiplies the eat rate */
int CARAVAN_CAPACITY = 250;     /* how many civilians on 1 caravan unit */
double JEWEL_WEIGHT = 0.01;     /* the next four are in relation to one */
double MONEY_WEIGHT = 0.01;     /* person's weight (i.e. jewels weigh */
double METAL_WEIGHT = 0.1;      /* 1/100th of a person) */
double FOOD_WEIGHT =  0.05;      
double DISBAND_RETURN = (0.5);  /* what fraction metal is returned w/disband */

/* how much do money/metal/jewels affect R&D */
double TECH_MONEY_FACTOR = 1.0/10.0;
double TECH_METAL_FACTOR = 1.0/40.0;
double MAG_MONEY_FACTOR = 1.0/5.0;
double MAG_JEWEL_FACTOR = 1.0/200.0;
double SPY_MONEY_FACTOR = 1.0/200.0;
double SPY_SECRECY_FACTOR = 1.0/1300.0;

  /* these parameters determine how far you can see */
int LAND_SIGHT = 3;   /* how far you see from an owned sector */
int WATER_SIGHT = 2;  /* ...             across water */
int ARMY_SIGHT = 2;   /* ...                  an army */

  /* end of user-defined parameters */
  /* fraction of the dead that vampire units raise from the dead */
double VAMPIRE_FRACT = 0.33;  /* 1/3 of the dead rise */
int SACRIFICED_FRACT = 12;   /* 1 pt. per 12 people */
int INITIATION_JEWELS = 5000; /* cost to initiate a mage (jewels) */
int MAGE_JEWELS_MAINT = 1000; /* cost to maintain a mage (jewels) */
int MAGE_MOVE_FACTOR = 2;     /* mages move 2*basic_rate */
int SORCERER_MOVE_FACTOR = 1; /* sorcerers move at basic rate */
int MEN_PER_MACHINE = 10;     /* # of men to be assisted by one machine */
int FORT_BONUS_INCREASE = 3;  /* how much bonus for passing time in forts */
int MIN_STOP_CIV = 50;    /* Minimum number of people for stop flag */
int SPELLPNTS_PER_MAINT = 6;  /* number of spell points for one maintnence */

double DESTROY_MONEY_COST = 0.1; /* % of construction costs to destroy */
double DESTROY_METAL_REC = 0.2;   /* % of metal that can be recycled    */

int GEOGRAPHY_BONUS = 3; /* The % penalty in battle for diff geography */
double BATTLE_INTENSITY = 1.0; /* The extra death factor.  see battle.c */

#else /* If we are not in the main function, define them extern */

extern int FORT_COST_MONEY, FORT_COST_METAL, ROADS_COST_MONEY,
   ROADS_COST_METAL, BUBBLE_COST_METAL, BUBBLE_COST, ARMY_OVERHEAD,
   REFINE_COST_METAL, REFINE_COST,
   HOSPITAL_MAINT_COST, UNIV_MAINT_COST	, OCCUPYING_SOLDIERS, FOOD_PROD,
   CARAVAN_CAPACITY, LAND_SIGHT, WATER_SIGHT, ARMY_SIGHT, SACRIFICED_FRACT,
   INITIATION_JEWELS, MAGE_JEWELS_MAINT, MAGE_MOVE_FACTOR, 
   SORCERER_MOVE_FACTOR, MEN_PER_MACHINE, FORT_BONUS_INCREASE,
   MIN_STOP_CIV, SPELLPNTS_PER_MAINT, GEOGRAPHY_BONUS;
extern double REFINERY_FACT, EAT, SOLD_EAT_FACTOR, JEWEL_WEIGHT, MONEY_WEIGHT,
   METAL_WEIGHT, FOOD_WEIGHT, DISBAND_RETURN, TECH_MONEY_FACTOR,
   TECH_METAL_FACTOR, MAG_MONEY_FACTOR, MAG_JEWEL_FACTOR, SPY_MONEY_FACTOR,
   SPY_SECRECY_FACTOR, VAMPIRE_FRACT, DESTROY_MONEY_COST, DESTROY_METAL_REC,
   BATTLE_INTENSITY;

#endif

#endif /* _COSTS_H */
