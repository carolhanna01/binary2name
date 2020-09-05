   /* comp_params.c:  loading, saving, initializing, and modifying computer
      parameters. */

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
#include <string.h>

#include "dominion.h"

#define PARAM_DIR "params"

void change_params(Snation *np, WINDOW *w)
{
  int ch, tmp;

  echo();
  draw_comp_params(w, np);
  statline ("type space or q when done","comp_params");

  do {
    mvwprintw (w, 17,2, "Change which parameter?  ");
    ch = mvwgetch (w, 17, 26);
    if (ch == '') {
      draw_comp_params (w, np);
      continue;
      }
    if (strchr("hdaemAMGpP", ch)) {
      mvwprintw (w, 18, 2, "New value? ");
      wrefresh(w);
      wscanw (w, "%d", &tmp);
      if (tmp < 0) {
	mvwprintw (w, 18, 2, "Bad Value!");
	wrefresh(w);
	continue;
	}

      switch (ch) {

	case 'h':
	np->cn_params->hos = tmp;
	mvwprintw (w, 2,20, "%3d", np->cn_params->hos);
	break;

	case 'd':
	if (tmp > 100)
	  continue;
	np->cn_params->dip_chan = tmp;
	mvwprintw (w, 3,20, "%3d", np->cn_params->dip_chan);
	break;

	case 'a':
	np->cn_params->agg = tmp;
	mvwprintw (w, 4,20, "%3d", np->cn_params->agg);
	break;

	case 'e':
	np->cn_params->exp = tmp;
	mvwprintw (w, 5,20, "%3d", np->cn_params->exp);
	break;

	case 'm':
	np->cn_params->mil = tmp;
	mvwprintw (w, 6,20, "%3d", np->cn_params->mil);
	break;

	case 'A':
	np->cn_params->army_size = tmp;
	mvwprintw (w, 7,20, "%3d", np->cn_params->army_size);
	break;

	case 'M':
	np->cn_params->min_size = tmp;
	mvwprintw (w, 8,20, "%3d", np->cn_params->min_size);
	break;

	case 'G':
	if (tmp > 100)
	  continue;
	np->cn_params->merge_size_pct = tmp;
	mvwprintw (w, 9,20, "%3d", np->cn_params->merge_size_pct);
	break;

	case 'p':
	if (tmp > 100)
	  continue;
	np->cn_params->pat = tmp;
	mvwprintw (w, 10,20, "%3d", np->cn_params->pat);
	break;

	case 'P':
	if (tmp > 100)
	  continue;
	np->cn_params->pat_size_pct = tmp;
	mvwprintw (w, 11,20, "%3d", np->cn_params->pat_size_pct);
	break;
	}
      mvwprintw (w, 18, 2, "                ");
      wrefresh(w);
      }
    } while (ch != 'q' && ch != ' ');
  save_params(np);
  noecho();
}

void draw_comp_params(WINDOW *w, Snation *np)
{
  char s[80];

  wclear (w);
  wrefresh (w);
  sprintf (s, "Computer Nation Parameters for nation %s:", np->name);
  mvwprintw (w, 1, 35 - (strlen (s)) / 2, "%s", s);
  mvwprintw (w, 2, 1, "h: Hostility");
  mvwprintw (w, 2, 25, "Affects chance of lowering dip. status.  (20-300)");
  mvwprintw (w, 3, 1, "d: Diplo_change %%");
  mvwprintw (w, 3, 25, "%% chance of changing diplomatic status.  (10-100)");
  mvwprintw (w, 4, 1, "a: Aggressiveness");
  mvwprintw (w, 4, 25, "Affects value of enemy sectors.  (10-200)");
  mvwprintw (w, 5, 1, "e: Expansion");
  mvwprintw (w, 5, 25, "High values leave isolated sectors.  (5-160)");
  mvwprintw (w, 6, 1, "m: Military");
  mvwprintw (w, 6, 25, "Affects ratio of military to civilians.  (10-200)");
  mvwprintw (w, 7, 1, "A: Army Size");
  mvwprintw (w, 7, 25, "Affects number and size of individual armies. (12-200)");
  mvwprintw (w, 8, 1, "M: Min Army Size");
  mvwprintw (w, 8, 25, "Make armies at least this big when at war. (troops)");
  mvwprintw (w, 9, 1, "G: Merge Size %%");
  mvwprintw (w, 9, 25, "percent of optimal size considered too small. (25-90)");
  mvwprintw (w,10, 1, "p: Patrols %%");
  mvwprintw (w,10, 25, "percent of armies used to patrol.  (5-50)");
  mvwprintw (w,11, 1, "P: Patrol Size %%");
  mvwprintw (w,11, 25, "Size of patrol armies relative to others. (25-100)");

  mvwprintw (w, 2,20, "%3d", np->cn_params->hos);
  mvwprintw (w, 3,20, "%3d", np->cn_params->dip_chan);
  mvwprintw (w, 4,20, "%3d", np->cn_params->agg);
  mvwprintw (w, 5,20, "%3d", np->cn_params->exp);
  mvwprintw (w, 6,20, "%3d", np->cn_params->mil);
  mvwprintw (w, 7,20, "%3d", np->cn_params->army_size);
  mvwprintw (w, 8,20, "%3d", np->cn_params->min_size);
  mvwprintw (w, 9,20, "%3d", np->cn_params->merge_size_pct);
  mvwprintw (w, 10,20, "%3d", np->cn_params->pat);
  mvwprintw (w, 11,20, "%3d", np->cn_params->pat_size_pct);
  wrefresh(w);
  }

/*------------------------------load_params()--------------------------------*/
/* Loads the parameters that affect the way the update program plays a nation*/
/*---------------------------------------------------------------------------*/

void load_params(Snation *np)
{
  char param_file[PATHLEN], param_line[100];
  FILE *fparam;
  Scomp_params *ptr;

  init_params(np);
  ptr = np->cn_params;
  sprintf(param_file, "%s/param.%d", PARAM_DIR, np->id);
  if ((fparam = fopen(param_file,"r")) == NULL) {
    save_params(np);
    return;
  }
  if (fgets(param_line, 99, fparam) != NULL) {
    sscanf (param_line,
      "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
        &ptr->hos, &ptr->dip_chan, &ptr->agg, &ptr->exp,
	&ptr->mil, &ptr->army_size, &ptr->min_size,
	&ptr->merge_size_pct, &ptr->pat, &ptr->pat_size_pct);
    } else {
      fprintf(stderr,"Error: Could not read parameter line\n");
  }
  fclose(fparam);
}

/* Save all of the user's params to a file. */
void save_params(Snation *np)
{
  FILE *fparam;
  Scomp_params *ptr;
  char param_file[PATHLEN];

  sprintf(param_file, "%s/param.%d", PARAM_DIR, np->id);
  if ((fparam = fopen(param_file,"w")) == NULL) {
    clean_exit();
    fprintf(stderr,"Error: Cannot Open Parameter File %s\n",param_file);
    exit(1);
  }
  ptr = np->cn_params;
  fprintf (fparam, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d\n",
      ptr->hos, ptr->dip_chan, ptr->agg, ptr->exp,
      ptr->mil, ptr->army_size, ptr->min_size,
      ptr->merge_size_pct, ptr->pat, ptr->pat_size_pct);
  fclose (fparam);
}

/* Init an unused params record */
void init_params(Snation *np)
{
  if ((np->cn_params = (Scomp_params *)malloc(sizeof(Scomp_params))) == NULL) {
    mem_error();
    }
  
  np->cn_params->hos = 50;
  np->cn_params->dip_chan = 75;
  np->cn_params->agg = 50;
  np->cn_params->exp = 20;
  np->cn_params->mil = 50;
  np->cn_params->army_size = 50;
  np->cn_params->min_size = 150;
  np->cn_params->merge_size_pct = 67;
  np->cn_params->pat = 10;
  np->cn_params->pat_size_pct = 50;
}
