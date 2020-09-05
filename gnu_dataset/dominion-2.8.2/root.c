  /* root.c -- special super-user commands */

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
#include <ctype.h>
#include <string.h>

#include "dominion.h"

extern Sworld world;
extern Suser user;

void root_edit()
{
  int done = 0;

  while (!done) {
    statline_prompt ("Edit [s]ector or [n]ation? ", "edit");
    switch (mygetch ()) {
    case 's':
      root_edit_sector();
      break;
    case 'n':
      root_edit_nation();
      break;
    case ' ':
      done = 1;
      break;
    default:
      break;
    }
  }
}

void root_edit_sector()
{
  WINDOW *resw;
  int pop_change, done = 0, new_owner, change;
  char s [EXECLEN];

  Ssector *sp = &world.map[user.cursor.x][user.cursor.y];

  if (!user.xmode) {
    resw = newwin(8, COLS-2, LINES-10, 1);
  }
  else {
    resw = NULL;
  }
  while (!done) {
    if (resw) {
      werase(resw);
      mvwaddstr(resw, 5, 2, "Change: [p]opulation, [o]wner ");
      mvwaddstr(resw, 6, 2, "        [s]oil, [m]etal, [j]ewels, [a]ltitude");
      wrefresh(resw);
    } else {
      statline_prompt ("Change: [p,o,s,m,j,a]? ", "edit_sector");
    }
    switch (mygetch()) {
    case 'p':
      sprintf (s, "Sector (%d,%d), pop=%d, what population change? ",
	       sp->loc.x, sp->loc.y, sp->n_people);
      if (resw) {
	mvwaddstr (resw, 1, 1, s);
	wrefresh(resw);
      } else {
	statline_prompt(s, "population");
      }
      if (wget_number(resw, &pop_change) > 0) {
	sp->n_people += pop_change;
	cpeople_sector(sp, pop_change);
      }
      break;
    case 'o':
      sprintf(s, "Sector (%d,%d), owner=%d, which new owner id? ",
	      sp->loc.x, sp->loc.y, sp->owner);
      if (resw) {
	mvwaddstr (resw, 1, 1, s);
	wrefresh(resw);
      } else {
	statline_prompt (s, "owner");
      }
      if (wget_number(resw, &new_owner) > 0 && new_owner <= world.n_nations) {
	/* 1. remove from old owner's list
	   2. add to new owner's list
	   */
	if (sp->owner != 0) {
	  subtsector(&world.nations[sp->owner], sp->loc.x, sp->loc.y);
	}
	sp->owner = new_owner;
	cowner_sector(sp, new_owner);
	if (new_owner != 0) {	/* nation 0 has no sector list */
	  addsector(&world.nations[new_owner], sp->loc.x, sp->loc.y);
	}
      }
      break;
    case 's':
      sprintf(s, "Sector (%d,%d), soil=%d, what soil change? ",
	      sp->loc.x, sp->loc.y, sp->soil);
      if (resw) {
	mvwaddstr (resw, 1, 1, s);
	wrefresh(resw);
      } else {
	statline_prompt (s, "soil");
      }
      if (wget_number(resw, &change) > 0) {
	sp->soil += change;
	csoil_sector(sp, change);
      }
      break;
    case 'm':
      sprintf(s, "Sector (%d,%d), metal=%d, what metal change? ",
	      sp->loc.x, sp->loc.y, sp->metal);
      if (resw) {
	mvwaddstr (resw, 1, 1, s);
	wrefresh(resw);
      } else {
	statline_prompt (s, "metal");
      }
      if (wget_number(resw, &change) > 0) {
	sp->metal += change;
	cmetal_sector(sp, change);
      }
      break;
    case 'j':
      sprintf(s, "Sector (%d,%d), jewels=%d, what jewels change? ",
	      sp->loc.x, sp->loc.y, sp->jewels);
      if (resw) {
	mvwaddstr (resw, 1, 1, s);
	wrefresh(resw);
      } else {
	statline_prompt (s, "jewels");
      }
      if (wget_number(resw, &change) > 0) {
	sp->jewels += change;
	cjewels_sector(sp, change);
      }
      break;
    case 'a':
      sprintf(s,"Sector (%d,%d), altitude=%d, what altitude change? ",
	      sp->loc.x, sp->loc.y, sp->altitude);
      if (resw) {
	mvwaddstr (resw, 1, 1, s);
	wrefresh(resw);
      } else {
	statline_prompt (s, "altitude");
      }
      if (wget_number(resw, &change) > 0) {
	sp->altitude += change;
	caltitude_sector(sp, change);
      }
      break;
    case ' ':
      done = 1;
      break;
    default:
      break;
    }
  }
  if (resw) {
    delwin(resw);
    touchwin (stdscr);
  }
  just_moved ();
  refresh();
}

/* allows the game master to tinker with a nation */
void root_edit_nation()
{
  FILE *lock_fp, *is_locked();
  WINDOW *renw;
  int done = 0, change, id;
  Snation *np;
  char name[NAMELEN], s[EXECLEN];
  char c, oldmark;
  
  statline_prompt ("Which nation do you want to edit? ", "edit_nation");

  if (wget_name(NULL, name) <= 0) {
    return;
  }
  if (isdigit (name [0]) && are_all_digits (name)) {
    id = atoi (name);
  } else {
    id = get_nation_id (name); 
  }
  if (id < 0) {
    statline2_err ("Hit space to continue", "Nation does not exist");
    return;
  } else {
    np = &world.nations[id];
  }
  if ((lock_fp = is_locked(id))) {
    statline2_err("Hit space to continue","Nation is currently being played");
    fclose(lock_fp);
    return;
  }
  if (user.xmode) {
    renw = NULL;
  } else {
    renw = newwin(9, COLS-2, LINES-12, 1);
  }
  while (!done) {
    sprintf(s, "Editing nation %s [%c] (%d, %d)", np->name, np->mark,
	    np->capital.x, np->capital.y);
    switch (np->cn_flag) {
    case CN_NOMAIL:
      strcat(s, " [cn]     ");
      break;
    case CN_MAIL:
      strcat (s, " [cn+mail]");
      break;
    case NOT_CN:
    default:
      strcat (s, "           ");
      break;
    }
    statline2 ("", "");
    if (renw) {
      werase (renw);
      mvwaddstr (renw, 1, 6, s);
      mvwaddstr(renw, 4, 2,
		"Change: [N]ame, [p]assword, [D]estroy, [t]oggle cn");
      mvwaddstr(renw, 5, 2,"        [s]heckles, [m]etal, [j]ewels, [f]ood");
      mvwaddstr(renw, 6, 2,"        [T]ech skill, [M]ag skill, [S]pell pts");
      mvwaddstr(renw, 7, 2,"        [d]iplomacy, mag. [o]rder, nation mar[k]");
      if (np->cn_flag) {
	mvwaddstr(renw, 8, 20, "[c]omp nation params");
      }
      wrefresh(renw);
      sprintf (s, "nation %s", np->name);
      statline ("Hit space to exit", s);
    }
    else {
      if (np->cn_flag) {
	sprintf(s,"(%s [%c] Change: [N,p,D,t,s,m,j,f,T,M,S,d,o,k,a]: ",
		 np->name, np->mark);
      } else {
      	sprintf (s,"(%s) Change: [N,p,D,t,s,m,j,f,T,M,S,d,o,k]: ",
		 np->name);
      }
      statline_prompt (s, "edit_nation");
    }
    switch (mygetch()) {
    case 'N':
      sprintf (s, "Give new name for nation %s: ", np->name);
      statline2_prompt (s, "name");
      if (wget_name(NULL, name) > 0) {
	strcpy(np->name, name);
	sprintf(s, "NATION_NAME:%d:%s\n", np->id, name);
	gen_exec(s);
      }
      break;
    case 'p':
      change_passwd(np);
      break;
    case 'D':
      root_destroy_nation(np);
      break;
    case 't':			/* rotate values for the cn flag */
      np->cn_flag = (np->cn_flag + 1) % 3;
/*      if (np->cn_flag) { */
      sprintf(s, "SET_CN:%d:%d\n", np->id, np->cn_flag);
/*      } else {
	sprintf(s, "CLEAR_CN:%d\n", np->id);
      }
*/
      gen_exec(s);
      break;
    case 's':
      sprintf (s, "Nation's money=%d; change? ", np->money);
      statline2_prompt (s, "money");
      if (wget_number(NULL, &change) > 0) {
	np->money += change;
	cmoney(np, change);
      }
      break;
    case 'm':
      sprintf (s, "Nation's metal=%d; change? ", np->metal);
      statline2_prompt (s, "metal");
      if (wget_number (NULL, &change) > 0) {
	np->metal += change;
	cmetal(np, change);
      }
      break;
    case 'j':
      sprintf (s, "Nation's jewels=%d; change? ", np->jewels);
      statline2_prompt (s, "jewels");
      if (wget_number(NULL, &change) > 0) {
	np->jewels += change;
	cjewels(np, change);
      }
      break;
    case 'f':
      sprintf(s, "Nation's food=%d; change? ", np->food);
      statline2_prompt (s, "food");
      if (wget_number(NULL, &change) > 0) {
	np->food += change;
	cfood(np, change);
      }
      break;
    case 'T':
      sprintf (s, "Nation's tech skill=%d; change? ",
		np->tech_skill);
      statline2_prompt (s, "tech");
      if (wget_number(NULL, &change) > 0) {
	if (change < 0) { 
          statline2_err ("Hit space to continue",
             "Tech skill can only be raised.");
	} else {
  	  np->tech_skill += change;
	  ctech_skill(np, change);
	}
      }
      break;
    case 'M':
      sprintf (s, "Nation's mag skill=%d; change? ",
		np->mag_skill);
      statline2_prompt (s, "mag");
      if (wget_number(NULL, &change) > 0) {
	np->mag_skill += change;
	cmag_skill(np, change);
      }
      break;
    case 'S':
      sprintf (s, "Nation's spell pts=%d; change? ",
		np->spell_pts);
      statline2_prompt (s, "spell_pts");
      if (wget_number(NULL, &change) > 0) {
	np->spell_pts += change;
	cspell_pts(np, change);
      }
      break;
    case 'o':			/* change magic order */
      sprintf (s, "Give new magic order (currently %s) ",
	       np->mag_order);
      statline2_prompt (s, "mag_order");

      do { ; } while (wget_name(NULL, name) <= 0);
      if (is_good_order(name)) {
	strcpy(np->mag_order, name);
	sprintf(s, "NATION_ORDER:%d:%s\n", np->id, name);
	gen_exec(s);
      }
      break;
    case 'k':			/* change nation mark */
      oldmark = c;
      do {
	statline2_prompt("Give new nation mark (- for next): ", "nation_mark");
	c = mygetch ();
      } while (c != '-' && c != oldmark && !free_nation_mark(&world, c));
      if (c == '-') {
	c = next_free_nation_mark();
      }
      np->mark = c;
      sprintf(s, "NATION_MARK:%d:%c\n", np->id, c);
      gen_exec(s);
      break;
    case 'd':
      diplo_report(np);
      touchwin (stdscr);
      refresh ();
      break;
    case 'c':
      if (np->cn_flag) {
	load_params(np);
	change_params(np, stdscr);
	wclear(stdscr);
        touchwin (stdscr);
        refresh ();
      }
      break;
    case ' ':
      done = 1;
      break;
    default:
      break;
    }
  }
  if (renw) {
    delwin(renw);
    touchwin(stdscr);
  }
  statline2 ("", "");
  just_moved ();
  refresh();
}

/* here are some exec lines available only to the Gamemaster */
void cowner_sector(Ssector *sp, int owner)
{
  char s[EXECLEN];

  sprintf(s, "COWNER_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, owner);
  gen_exec(s);
}

void csoil_sector(Ssector *sp, int change)
{
  char s[EXECLEN];

  sprintf(s, "CSOIL_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, change);
  gen_exec(s);
}

void cmetal_sector(Ssector *sp, int change)
{
  char s[EXECLEN];

  sprintf(s, "CMETAL_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, change);
  gen_exec(s);
}

void cjewels_sector(Ssector *sp, int change)
{
  char s[EXECLEN];

  sprintf(s, "CJEWELS_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, change);
  gen_exec(s);
}

void caltitude_sector(Ssector *sp, int change)
{
  char s[EXECLEN];

  sprintf(s, "CALTITUDE_SECTOR:%d:%d:%d\n", sp->loc.x, sp->loc.y, change);
  gen_exec(s);
}

/* this is serious:  destroy a nation, interactively, as super user */
void root_destroy_nation(Snation *np)
{
  char c;
  char s[EXECLEN];

  statline2_prompt ("Are you quite sure? ", "This is serious!!! ");
  if ((c = mygetch()) == 'y' || c == 'Y') {
    statline2_prompt ("Confirm again, dude: ", "This is still serious!!!");
    if ((c = mygetch()) == 'y' || c == 'Y') {
      destroy_nation(np->id);
      sprintf(s, "DESTROY:%d\n", np->id);
      gen_exec(s);
    }
  }
}

#if 0
  /* allow the nation's aggressiveness to be modified.  we
     will make a new window so that this can be called from
     various different places (such as the root editing and
     the info report).
   */
/*  Commented out 6/9/93 because of new cn stuff, KAH
    set_aggressiveness(np, current_win)
     Snation *np;
     WINDOW *current_win;	** for redrawing purposes **
   {
  WINDOW *aggw;
  int aggr;
  char s[EXECLEN];


  if (current_win == NULL) {
    current_win = stdscr;
  }

  aggw = newwin(8, 30, LINES-14, 10);
  touchwin(aggw);
  mvwprintw(aggw, 3, 3, "Current aggressiveness %d", np->cn_agg);
  mvwprintw(aggw, 4, 3, "Give new value: ");
  box(aggw, '|', '-');
  wrefresh(aggw);
  if (wget_number(aggw, &aggr) < 0) { ** if user types nothing or bad value **
    delwin(aggw);
    touchwin(current_win);
    return;
  }
    ** set the new value and make sure we don't overflow **
  np->cn_agg = aggr;
  np->cn_agg = dom_min_int(np->cn_agg, 100);
  np->cn_agg = dom_max_int(np->cn_agg, 0);
  sprintf(s, "CN_PARAM:%d:%d:%d:%d\n", np->id, np->cn_agg,
	  np->cn_exp, np->cn_iso);
  gen_exec(s);
  delwin(aggw);
  touchwin(current_win);
  return;
}
*/
#endif /* 0 */
