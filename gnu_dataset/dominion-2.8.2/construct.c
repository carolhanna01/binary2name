/* construct.c -- routines dealing with construction */

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

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "dominion.h"
#include "misc.h"
#include "army.h"
#include "cur_stuff.h"

extern Suser user;
extern Sworld world;
extern char help_tag[];
extern WINDOW * sectw;

extern int (*wrapx)(), (*wrapy)();
void construct_fortification(Snation *np, Ssector *sp, WINDOW *w);
void construct_roads(Snation *np, Ssector *sp, WINDOW *w);
void construct_bubble(Snation *np, Ssector *sp, WINDOW *w);
void destruct_refinery(Snation *np, Ssector *sp, WINDOW *w);
void unconstruct();
void destruct_fortification(Snation *np, Ssector *sp, WINDOW *w);
void destruct_roads(Snation *np, Ssector *sp, WINDOW *w);
void destruct_bubble(Snation *np, Ssector *sp, WINDOW *w);
void construct_refinery(Snation *np, Ssector *sp, WINDOW *w);

/* construct a constructible object */
void construct()
{
  WINDOW *cw;
  char c;
  int done=0;
  Snation *np = user.np;
  Ssector *sp =  &world.map[user.cursor.x][user.cursor.y];

  strcpy(help_tag, "Construction");

  if (user.xmode) {
    cw = NULL;
  } 
  else {
    cw = newwin(6, COLS/2+18, LINES-11, COLS/4-16);
  }

  while (!done) {
    if (cw != NULL) {
      statline("", "construct");
      mvwprintw(cw, 1, 1, "What type of object do you want to construct? ");
      mvwprintw(cw, 2, 1, " [f]ort, [r]oads, [b]ubble, ref[i]nery, [d]estroy");
      wmove(cw, 3, 1);
      wclrtobot(cw);
      box(cw, '|', '-');
      wrefresh(cw);
    }
    else {			/* expert mode */
      statline_prompt ("Construct: (f,r,b,i,d)", "construct");
    }

    switch (c = mygetch()) {
    case 'f':
      construct_fortification(np, sp, cw);
      break;
    case 'r':
      construct_roads(np, sp, cw);
      break;
    case 'b':
      construct_bubble(np, sp, cw);
      break;
    case 'i':
      construct_refinery(np, sp, cw);
      break;
    case 'd':
      unconstruct(sp, cw);
      break;
    case ' ':
      done = 1;
      break;
    default:
      break;
    }
  }

  if (cw != NULL) {
    wrefresh(cw);
    delwin(cw);
  }

  touch_all_wins();
  statline2("", "");
  show_armies(sp);
  return;
}

/* remove stuff from a sector */
void unconstruct()
{
  WINDOW *cw;
  char c;
  int done=0;
  Snation *np = user.np;
  Ssector *sp =  &world.map[user.cursor.x][user.cursor.y];

  strcpy(help_tag, "Destruction");

  if (user.xmode) {
    cw = NULL;
  } 
  else {
    cw = newwin(6, COLS/2+18, LINES-11, COLS/4-16);
  }

  while (!done) {
    if (cw != NULL) {
      statline("", "destruct");
      mvwprintw(cw, 1, 1, "What type of object do you want to destroy? ");
      mvwprintw(cw, 2, 1, " [f]ort, [r]oads, [b]ubble, ref[i]nery");
      wmove(cw, 3, 1);
      wclrtobot(cw);
      box(cw, '|', '-');
      wrefresh(cw);
    }
    else {			/* expert mode */
      statline_prompt ("Destruct: (f,r,b)", "destruct");
    }

    switch (c = mygetch()) {
    case 'f':
      destruct_fortification(np, sp, cw);
      break;
    case 'r':
      destruct_roads(np, sp, cw);
      break;
    case 'b':
      destruct_bubble(np, sp, cw);
      break;
    case 'i':
      destruct_refinery(np, sp, cw);
      break;
    case ' ':
      done = 1;
      break;
    default:
      break;
    }
  }

  if (cw != NULL) {
    wrefresh(cw);
    delwin(cw);
  }

  touch_all_wins();
  statline2("", "");
  show_armies(sp);
  return;
}

/* build fortification on a sector */
void construct_fortification(Snation *np, Ssector *sp, WINDOW *w)
{
  int fort_increase, cost, cost_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "hey dude, this sector is not yours");
    return;
  }
  if (sp->defense >= np->consts.forts) {
    statline2_err("hit space to go on", "You can't build better forts yet.");
    return;
  }
  cost = (int) (pow(2.0, (double) sp->defense/10)*FORT_COST_MONEY);
  cost_met = (int) (pow(2.0, (double) sp->defense/10)*FORT_COST_METAL);

  if (w != NULL) {
    mvwprintw(w, 3, 1, "Current fort %d; cost for 10 more: %d sk., %d met.",
	      sp->defense, cost, cost_met);
    mvwprintw(w, 4, 1, "Build ten more (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Current: %d, Cost +10 = %d sk., %d met. Build? ",
	     sp->defense, cost, cost_met);
    statline2 (s, "con_fort");
  }

  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) {
    statline2 ("", "");
  }
  fort_increase = 10;
    /* now see if we can afford the construction */
  if (cost > user.np->money) {
    statline2_err("space to go on", "not enough money to construct");
    return;
  }
  if (cost_met > user.np->metal) {
    statline2_err("space to go on", "not enough metal to construct");
    return;
  }

  /* if we have reached this point, it means we can construct! */

  sp->defense += fort_increase;
  np->money -= cost;
  np->metal -= cost_met;
    /* now prepare the exec string */
  sprintf(s, "CFORT_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, fort_increase);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, -cost_met);

  return;
}

/* build roads on a sector */
void construct_roads(Snation *np, Ssector *sp, WINDOW *w)
{
  int roads_increase, cost, cost_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "hey dude, this sector is not yours");
    return;
  }
  if (sp->roads >= np->consts.roads) {
    statline2_err("hit space to go on", "You can't build better roads yet.");
    return;
  }
  cost = (int) (pow(2.0, (double) sp->roads)*ROADS_COST_MONEY);
  cost_met = (int) (pow(2.0, (double) sp->roads)*ROADS_COST_METAL);

  if (w != NULL) {
    mvwprintw(w, 3, 1,
	      "Current roads %d; cost %d sk., %d met.", sp->roads, cost, cost_met);
    mvwprintw(w, 4, 1, "Build one more (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Current: %d, Cost +1 = %d sk., %d met. Build? ",
	     sp->roads, cost, cost_met);
    statline2 (s, "con_roads");
  }
  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }

  if (w == NULL) {
    statline2 ("", "");
  }

  roads_increase = 1;
    /* now see if we can afford the construction */
  if (roads_increase*cost > user.np->money) {
    statline2_err("space to go on", "not enough money to construct");
    return;
  }
  if (roads_increase*cost_met > user.np->metal) {
    statline2_err("space to go on", "not enough metal to construct");
    return;
  }

  /* if we have reached this point, it means we can construct! */

  sp->roads += roads_increase;
  np->money -= roads_increase*cost;
  np->metal -= roads_increase*cost_met;
    /* now prepare the exec string */
  sprintf(s, "CROADS_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, roads_increase);
  gen_exec(s);
  cmoney(np, -roads_increase*cost);
  cmetal(np, -roads_increase*cost_met);

  return;
}

/* build a bubble on a sector; allowing a
   race to live in a sector below (or above)
   water, when they normally could not.
 */
void construct_bubble(Snation *np, Ssector *sp, WINDOW *w)
{
  int cost, cost_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "Hey dude, this sector is not yours");
    return;
  }
  if (has_bubble(sp)) {
    statline2_err("hit space to go on", "There already is a bubble!");
    return;
  }
  if (!can_bubble(np)) {
    statline2_err("hit space to go on","You don't know how to build bubbles!");
    return;
  }
  cost = BUBBLE_COST;
  cost_met = BUBBLE_COST_METAL;

  if (w != NULL) {
    mvwprintw(w, 3, 1, "Cost %d sk., %d met.", cost, cost_met);
    mvwprintw(w, 4, 1, "Go ahead (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Cost = %d sk., %d met. Build? ",
	     cost, cost_met);
    statline2 (s, "con_bubble");
  }
  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) {
    statline2 ("", "");
  }

    /* now see if we can afford the construction */
  if (cost > user.np->money) {
    beep();
    statline2_err("space to go on", "not enough money to construct");
    return;
  }
  if (cost_met > user.np->metal) {
    beep();
    statline2_err("space to go on", "not enough metal to construct");
    return;
  }

  /* if we have reached this point, it means we can construct! */

  sp->flags |= SF_BUBBLE;
  np->money -= cost;
  np->metal -= cost_met;
    /* now prepare the exec string */
  sprintf(s, "FLAG_SET_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, SF_BUBBLE);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, -cost_met);

  return;
}

/* build a refinery on a sector; increasing metal output */
void construct_refinery(Snation *np, Ssector *sp, WINDOW *w)
{
  int cost, cost_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "Hey dude, this sector is not yours");
    return;
  }
  if (has_refine(sp)) {
    statline2_err("hit space to go on", "There already is a refineary!");
    return;
  }
  if (!can_refine(np)) {
    statline2_err("hit space to go on",
      "You don't know how to build refineries!");
    return;
  }
  cost = REFINE_COST;
  cost_met = REFINE_COST_METAL;

  if (w != NULL) {
    mvwprintw(w, 3, 1, "Cost %d sk., %d met.", cost, cost_met);
    mvwprintw(w, 4, 1, "Go ahead (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Cost = %d sk., %d met. Build? ",
	     cost, cost_met);
    statline2 (s, "con_bubble");
  }
  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) {
    statline2 ("", "");
  }

    /* now see if we can afford the construction */
  if (cost > user.np->money) {
    beep();
    statline2_err("space to go on", "not enough money to construct");
    return;
  }
  if (cost_met > user.np->metal) {
    beep();
    statline2_err("space to go on", "not enough metal to construct");
    return;
  }

  /* if we have reached this point, it means we can construct! */

  sp->flags |= SF_REFINE;
  np->money -= cost;
  np->metal -= cost_met;
    /* now prepare the exec string */
  sprintf(s, "FLAG_SET_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, SF_REFINE);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, -cost_met);

  return;
}

/* destroy fortification on a sector */
void destruct_fortification(Snation *np, Ssector *sp, WINDOW *w)
{
  int fort_increase, cost, gain_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "hey dude, this sector is not yours");
    return;
  }
  if (sp->defense < 1) {
    statline2_err("hit space to go on", 
       "there is no fortification here to remove");
    return;
  }
  cost = (int) (pow(2.0, (double)((sp->defense/10) - 1))*FORT_COST_MONEY);
  cost *= DESTROY_MONEY_COST;
  gain_met = (int) (pow(2.0, (double)((sp->defense/10) - 1))*FORT_COST_METAL);
  gain_met *= DESTROY_METAL_REC;

  if (w) {
    mvwprintw(w, 3, 1, "Current fort %d; cost to remove 10: %d sk.",
	      sp->defense, cost);
    mvwprintw(w, 4, 1, "Remove ten levels (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Current: %d, Cost -10 = %d sk. Destroy? ", sp->defense, cost);
    statline2 (s, "des_fort");
  }

  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) { statline2 ("", "");  }
  fort_increase = -10;
    /* now see if we can afford the construction */
  if (cost > user.np->money) {
    statline2_err("space to go on", "not enough money to destruct");
    return;
  }
  /* if we have reached this point, it means we can construct! */

  sp->defense += fort_increase;
  np->money -= cost;
  np->metal += gain_met;
    /* now prepare the exec string */
  sprintf(s, "CFORT_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, fort_increase);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, gain_met);

  return;
}

/* destroy roads on a sector */
void destruct_roads(Snation *np, Ssector *sp, WINDOW *w)
{
  int roads_increase, cost, gain_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "hey dude, this sector is not yours");
    return;
  }
  if (sp->roads < 1) {
    statline2_err("hit space to go on", "there are no roads here to remove");
    return;
  }
  cost = (int) (pow(2.0, (double)(sp->roads - 1))*ROADS_COST_MONEY);
  cost *= DESTROY_MONEY_COST;
  gain_met = (int) (pow(2.0, (double)(sp->roads - 1))*ROADS_COST_METAL);
  gain_met *= DESTROY_METAL_REC;
  if (w != NULL) {
    mvwprintw(w, 3, 1,
      "Current roads %d; cost %d sk.", sp->roads, cost);
    mvwprintw(w, 4, 1, "Remove one level (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Current: %d, Cost +1 = %d sk. Destroy? ", sp->roads, cost);
    statline2 (s, "des_roads");
  }
  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) {  statline2 ("", ""); }
  roads_increase = -1;  /* now see if we can afford the destruction */
  if (cost > user.np->money) {
    statline2_err("space to go on", "not enough money to destruct");
    return;
  }
  sp->roads--;
  np->money -= cost;
  np->metal += gain_met;
  sprintf(s, "CROADS_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, roads_increase);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, gain_met);

  return;
}

/* destroy a bubble on a sector */
void destruct_bubble(Snation *np, Ssector *sp, WINDOW *w)
{
  int cost, gain_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "Hey dude, this sector is not yours");
    return;
  }
  if (!has_bubble(sp)) {
    statline2_err("hit space to go on", "The sector doesn't have a bubble!");
    return;
  }
  cost = BUBBLE_COST * DESTROY_MONEY_COST ;
  gain_met = BUBBLE_COST_METAL * DESTROY_METAL_REC;

  if (w != NULL) {
    mvwprintw(w, 3, 1, "Cost %d sk.", cost);
    mvwprintw(w, 4, 1, "Go ahead (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Cost = %d sk. Destroy? ", cost);
    statline2 (s, "des_bubble");
  }
  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) { statline2 ("", ""); }

    /* now see if we can afford the construction */
  if (cost > user.np->money) {
    beep();
    statline2_err("space to go on", "not enough money to destruct");
    return;
  }
  /* if we have reached this point, it means we can construct! */
  sp->flags &= ~(SF_BUBBLE);
  np->money -= cost;
  np->metal += gain_met;
    /* now prepare the exec string */
  sprintf(s, "FLAG_CLEAR_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, SF_BUBBLE);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, gain_met);

  return;
}

/* destroy a refinery on a sector */
void destruct_refinery(Snation *np, Ssector *sp, WINDOW *w)
{
  int cost, gain_met;
  char s[EXECLEN];
  char c;

  if (sp->owner != user.id) {
    statline2_err("hit space to go on", "Hey dude, this sector is not yours");
    return;
  }
  if (!has_refine(sp)) {
    statline2_err("hit space to go on", "The sector doesn't have a refinery!");
    return;
  }
  cost = REFINE_COST * DESTROY_MONEY_COST ;
  gain_met = REFINE_COST_METAL * DESTROY_METAL_REC;

  if (w != NULL) {
    mvwprintw(w, 3, 1, "Cost %d sk.", cost);
    mvwprintw(w, 4, 1, "Go ahead (y/n)? ");
    box(w, '|', '-');
    wrefresh(w);
  }
  else {
    sprintf (s, "Cost = %d sk. Destroy? ", cost);
    statline2 (s, "des_refine");
  }
  if ((c = mygetch()) != 'y' && c != 'Y') {
    statline2 ("", "");
    return;
  }
  if (w == NULL) { statline2 ("", ""); }

    /* now see if we can afford the construction */
  if (cost > user.np->money) {
    beep();
    statline2_err("space to go on", "not enough money to destruct");
    return;
  }
  /* if we have reached this point, it means we can construct! */
  sp->flags &= ~(SF_REFINE);
  np->money -= cost;
  np->metal += gain_met;
    /* now prepare the exec string */
  sprintf(s, "FLAG_CLEAR_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, SF_REFINE);
  gen_exec(s);
  cmoney(np, -cost);
  cmetal(np, gain_met);

  return;
}
