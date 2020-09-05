 /* bonds.c -- routines that allow a nation to issue bonds */

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

#include <string.h>

#include "dominion.h"		/* this includes bonds.h */
#include "misc.h"
/* #include "bonds.h" */

extern Sworld world;
extern Suser user;
extern char *libdir;
extern char help_tag[];

/* each report returns a char */
char info_report(), production_report(), nations_report();
void draw_bonds_screen(WINDOW *bondsw, Snation *np);

/* thke bonds report: this gives status of all bonds issued
   by that nation, both domestically and on the forign market
 */
char bonds_report(Snation *np)
{
  WINDOW *bondsw;
  char c;
  int done = 0;

  strcpy(help_tag, "Debt");
  bondsw = newwin(LINES-2, COLS, 0, 0);	/* full screen */
  touchwin(bondsw);

  while (!done) {
    draw_bonds_screen(bondsw, np);

    statline("type spacw when done, F to dump to a file", "bonds_report");

    switch (c = getch()) {
    case CTL('L'):		/* redraw screen */
      wclear(bondsw);
      draw_bonds_screen(bondsw, np);
      break;
    case 'I':			/* issue more bonds */
      break;
    case ' ':
    case 'b':			/* go to other reports */
    case 'i':
    case 'p':
    case 'n':
    case 'd':
      done = 1;
      break;
    case '?':
      online_info();
      break;
    default:
      break;
    }
    wrefresh(bondsw);
    statline2("", "");
  }

  delwin(bondsw);
  return c;
}

/* the routine that actually draws the bonds report screen */
void draw_bonds_screen(WINDOW *bondsw, Snation *np)
{
  char s[EXECLEN];

  sprintf(s, "Bonds Report for Nation %s", np->name);
  wmove(bondsw, 0, (COLS-strlen(s))/2 - 4); /* make the string centered */
  wstandout(bondsw);
  waddstr(bondsw, s);
  wstandend(bondsw);
  mvwprintw(bondsw, 1, COLS/3+7, "Thon %d", world.turn);
  
/*
  mvwprintw(w, 3, 0, "Nation:  %s", np->name);
  mvwprintw(w, 4, 0, "Id:      %d", np->id);
  mvwprintw(w, 5, 0, "Leader:  %s    ", np->leader);
  mvwprintw(w, 6, 0, "Capital: (%d,%d)",
	    xrel(np->capital.x,np->capital.y,user.np->capital),
	    yrel(np->capital.x,np->capital.y,user.np->capital));
  mvwprintw(w, 7, 0, "Race:    %s", np->race.name);
  mvwprintw(w, 8, 0, "Mark:    %c", np->mark);
  mvwprintw(w, 3, COLS/3, "Sectors:  %d", np->n_sects);
  mvwprintw(w, 4, COLS/3, "Treasury: %d", np->money);
  mvwprintw(w, 5, COLS/3, "Jewels:   %d", np->jewels);
  mvwprintw(w, 6, COLS/3, "Metal:    %d", np->metal);
  mvwprintw(w, 7, COLS/3, "Food:     %d", np->food);
  mvwprintw(w,  3, (2*COLS)/3, "Birth rate:  %d%%", np->race.repro);
  mvwprintw(w,  4, (2*COLS)/3, "Mortality:   %d%%", np->race.mortality);
  mvwprintw(w,  5, (2*COLS)/3, "Strength:    %d", np->race.strength);
  mvwprintw(w,  6, (2*COLS)/3, "Intel: %d + %d = %d", np->race.intel,
	    univ_intel(np), np->race.intel + univ_intel(np));
  mvwprintw(w,  7, (2*COLS)/3, "Magic Apt: %d + %d = %d", np->race.mag_apt,
	    priestliness(np), np->race.mag_apt + priestliness(np));
  mvwprintw(w,  8, (2*COLS)/3, "Speed:       %d", np->race.speed);
  mvwprintw(w,  9, (2*COLS)/3, "Stealth:     %d", np->race.stealth);

  mvwprintw(w, 11, (2*COLS)/3, "Civilians:     %d", get_n_civil(np));
  mvwprintw(w, 12, (2*COLS)/3, "Soldiers:      %d", get_n_soldiers(np));
  mvwprintw(w, 13, (2*COLS)/3, "Armies:        %d", np->n_armies);
  mvwprintw(w, 14, (2*COLS)/3, "Combat bonus:  %d%%", np->combat_bonus);
  mvwprintw(w, 15, (2*COLS)/3, "Move points:   %d", basic_move_rate(np));
  mvwprintw(w, 16, (2*COLS)/3, "Best roads:    %d", np->consts.roads);
  mvwprintw(w, 17, (2*COLS)/3, "Best forts:    %d", np->consts.forts);

  mvwprintw(w, 10, 0, "Initiated to the magical order %s", np->mag_order);
  mvwprintw(w, 11, 0, "Magic skill:       %d", np->mag_skill);
  mvwprintw(w, 12, 0, "Spell points:      %d (%d in maint)", np->spell_pts,
	    military_maint_spell_pts(np));
  mvwprintw(w, 13, 0, "Technology skill:  %d", np->tech_skill);
  mvwprintw(w, 14, 0, "Farming skill:     %d", np->farm_skill);
  mvwprintw(w, 15, 0, "Mining skill:      %d", np->mine_skill);
  mvwprintw(w, 16, 0, "Spy skill:         %d", np->spy);
  mvwprintw(w, 17, 0, "Secrecy:           %d", np->secrecy);
*/

  mvwprintw(bondsw, LINES-5, 4,
	    "Options: [I]ssue more bonds, [p]ay debt");
  mvwprintw(bondsw, LINES-4, 4,
	    "Reports: [b]udget, [i]nfo, [p]roduction, [n]ations, [d]iplomacy");
  wclrtobot(bondsw);
  wrefresh(bondsw);
}

/* issues a  ??? */
void issue_new_bonds(Snation *np, int amount, int duration)
{
  Sbonds *bondlist;

  bondlist = np->bondlist;
  /* ??? */

}
