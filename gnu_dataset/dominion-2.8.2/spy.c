  /* spy.c -- things having to do with "other" nations and espionage */

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
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include "dominion.h"
#include "misc.h"

  /* these macros represent the difficulty in gathering
     inteligence in various areas.
   */
#define SPY_POP 90
#define SPY_ECO 80
#define SPY_TAX 50
#define SPY_MAG 80
#define SPY_SPELL_PTS 160
#define SPY_MIL 100
#define SPY_TECHNO 50
#define SPY_CAP 40
#define SPY_FACT_SCALE (1.0 / 25.0)
#define BRIBE_FACT_SCALE (5.0)
#define MIN_JEWELS 40000
#define GUASS_RND_NUM 6
#define CAPTURE_SPY 100
#define CAPTURE_STEAL 50

extern Sworld world;
extern Suser user;
extern char help_tag[];

  /* info on all nations in the world */
char nations_report()
{
  WINDOW *w;
  char s[200];
  Snation *np;
  char c;
  int i, done = 0, first_shown, n_shown, id;

  w = newwin(LINES-2, COLS, 0, 0); /* full screen */
  werase(w);
  touchwin(w);
  sprintf(s, "report on all nations");
  wmove(w, 0, (COLS-strlen(s))/2); /* make the string centered */
  wstandout(w);
  waddstr(w, s);
  wclrtoeol(w);
  wstandend(w);

    /* here go the guts */
  sprintf(s,"World size is: %dx%d, there are %d nations",
	    world.xmax, world.ymax, world.n_nations-1);
  wmove(w, 1, (COLS-strlen(s))/2); /* make the string centered */
  waddstr(w, s);
  mvwaddstr(w, 3, 0, " id");
  mvwaddstr(w, 4, 0, "---");
  mvwaddstr(w, 3, 5, "nation");
  mvwaddstr(w, 4, 5, "------");
  mvwaddstr(w, 3, 25, "mark");
  mvwaddstr(w, 4, 25, "----");
  mvwaddstr(w, 3, 31, "leader");
  mvwaddstr(w, 4, 31, "------");
  mvwaddstr(w, 3, 47, "race");
  mvwaddstr(w, 4, 47, "----");
  if (user.id == 0) {
    mvwaddstr(w, 3, 55, "money");
    mvwaddstr(w, 4, 55, "-----");
    mvwaddstr(w, 3, 65, "civil");
    mvwaddstr(w, 4, 65, "-----");
  }
    /* figure out which nations to show */
  first_shown = 1;
  n_shown = dom_min_int(world.n_nations, LINES-10);
  while (!done) {
    strcpy(help_tag, "Nations Report");
    for (i = 0; i < n_shown && i+first_shown < world.n_nations; ++i) {
      np = &(world.nations[i+first_shown]);
      mvwprintw(w, i+5, 0, "%3d", np->id);
      wclrtoeol(w);
      mvwaddstr(w, i+5, 5, np->name);
      mvwaddch(w, i+5, 26, np->mark);
      sprintf(s,"%-12.12s",np->leader);
      mvwaddstr(w, i+5, 31, s);

      mvwaddstr(w, i+5, 47, np->race.name);

      if (!is_active_ntn(np)) {
	mvwaddstr(w, i+5, 60, "DESTROYED");
      } else if (user.id == 0) {
	mvwprintw(w, i+5, 55, "%d", np->money);
	mvwprintw(w, i+5, 65, "%d", get_n_civil(np));
      }
      if (np->cn_flag) {
	mvwaddstr(w, i+5, 75, "cn");
      }
    }
    wclrtobot(w);
    mvwaddstr(w, LINES-4, 4,
   "Options: [s]py on a nation, [<]/[,] previous screen, [>]/[.] next screen");
    mvwaddstr(w, LINES-3, 4,
	      "Reports: [b]udget [p]roduction [i]nfo [d]iplomacy [B]onds");
    wrefresh(w);
    statline("type space when done, or F to dump to a file", "nations_report");

    switch (c = mygetch()) {
    case '>':
    case '.':
      if (first_shown+n_shown < world.n_nations) {
	first_shown += n_shown;
      }
      break;
    case '<':
    case ',':
      if (first_shown > 1) {
	first_shown -= n_shown;
      }
      break;
    case 's':
      mvwaddstr(w, LINES-3, 4, "  Number of nation to spy on? ");
      wclrtoeol(w);
      if (wget_number(w, &id) > 0 && is_active_ntn(&world.nations[id])
	  && id != user.np->id) {
	if (user.id == 0) {	/* for the game master, give the total info */
	  (void) info_report(&world.nations[id]);
	} else {
          if (get_diplo_status (initial_diplo, user.id, id) == UNMET) {
            statline2_err("Press <SPACE> to return",
               "You've not met them yet.");
	  } else {
	    spy_report(id);
	  }
	}
	touchwin(w);
      }
      break;
    case 'F':
      dump_current_screen(w, "nations_report");
      break;
    case ' ':
    case 'b':
    case 'p':
    case 'i':
    case 'd':
    case 'B':
      done = 1;
      break;
    case '?':
      online_info();
      break;
    default:
      break;
    }
  }
  delwin(w);
  return c;
}

/* allow a nation to spy on anther */
void spy_report(int id)
{
  WINDOW *spyw;
  Snation *spied_np = &world.nations[id], *spying_np = user.np;
  char s[EXECLEN];
  char c;
  int done = 0, bribe;		/* amount of jewels to get info */
  int x, y;			/* for capital locations */

  strcpy(help_tag, "Nations Report");
  spyw = newwin(18, 60, LINES-22, (COLS-60)/2);
  werase(spyw);
  touchwin(spyw);
  while (!done) {
    sprintf(s, "Espionage Report on nation %s", spied_np->name);
    wmove(spyw, 1, (60-strlen(s))/2);
    wstandout(spyw);
    waddstr(spyw, s);
    wclrtoeol(spyw);
    wstandend(spyw);
    /* put the guts between here and the wrefresh() */
    mvwaddstr(spyw, 3, 5, "Spy on: [p]opulation, [e]conomy");
    mvwaddstr(spyw, 4, 5, "        [m]ilitary, ma[g]ic, [C]apital location");
    mvwaddstr(spyw, 5, 5, "        [t]echology, [T]echnology theft");
    wclrtobot(spyw);
    box(spyw, '|', '-');
    wrefresh(spyw);
    statline("type space when done, or F to dump to a file", "spy_report");
    c = mygetch();
    if (strchr("pemgCtT", c) != NULL) {
      mvwaddstr(spyw, 10, 1,
		"How many jewels do you want to pay in bribes? ");
      if (wget_number(spyw, &bribe) < 1 || bribe <= 0) {
	continue;
      }
      if (bribe > spying_np->jewels) {
	statline2_err("Hit space", "You don't have enough jewels");
	continue;
      }
    }
    switch (c) {
    case '?':
      online_info();
      break;
    case ' ':
      done = 1;
      break;
    case 'p':			/* info on their population */
      mvwprintw(spyw, 12, 6, "Population is %d",
		spy_figure(get_n_civil(spied_np), bribe,
		spying_np, spied_np, SPY_POP) );
      break;
    case 'e':			/* info on their military */
      mvwprintw(spyw, 12, 4, "Money: %d",
		spy_figure(spied_np->money, bribe,
		spying_np, spied_np, SPY_ECO) );
      mvwprintw(spyw, 12, 20, "Jewels: %d",
		spy_figure(spied_np->jewels, bribe,
		spying_np, spied_np, SPY_ECO) );
      mvwprintw(spyw, 12, 36, "Metal: %d",
		spy_figure(spied_np->metal, bribe,
		spying_np, spied_np, SPY_ECO) );
      mvwprintw(spyw, 13, 4, "Food: %d",
		spy_figure(spied_np->food, bribe,
		spying_np, spied_np, SPY_ECO) );
      mvwprintw(spyw, 13, 20, "Tax: %d",
		spy_figure(spied_np->taxes, bribe,
		spying_np, spied_np, SPY_TAX) );
      break;
    case 'C':
      x = spy_figure(spied_np->capital.x, bribe, spying_np, spied_np, SPY_CAP);
      y = spy_figure(spied_np->capital.y, bribe, spying_np, spied_np, SPY_CAP);
      x = xrel(x, y, spying_np->capital);
      y = yrel(x, y, spying_np->capital);
      mvwprintw(spyw, 13, 4, "Capital is at (%d, %d) ", x, y);
      break;
    case 't':			/* info on their technology */
      mvwprintw(spyw, 12, 4, "Techno skill: %d",
		spy_figure(spied_np->tech_skill, bribe,
		spying_np, spied_np, SPY_TECHNO) );
      break;
    case 'm':			/* info on their military */
      mvwprintw(spyw, 12, 4, "Soldiers: %d",
		spy_figure(get_n_soldiers(spied_np), bribe,
		spying_np, spied_np, SPY_MIL) );
      break;
    case 'g':			/* info on their magic */
      mvwprintw(spyw, 12, 4, "Magic skill: %d",
		spy_figure(spied_np->mag_skill, bribe,
		spying_np, spied_np, SPY_MAG) );
      mvwprintw(spyw, 13, 4, "Spell pts.: %d",
		spy_figure(spied_np->spell_pts, bribe,
		spying_np, spied_np, SPY_SPELL_PTS) );
      break;
    case 'T':	{		/* steal their tech */
        int tech_stolen;

        tech_stolen = spy_tech_steal(spied_np->tech_skill-
           spying_np->tech_skill,bribe,spying_np, spied_np);
        mvwprintw(spyw, 12, 4, "Tech skill stolen: %d",tech_stolen);
	if (tech_stolen) {
  	  spying_np->tech_skill += tech_stolen;
          ctech_skill(spying_np, tech_stolen);
	}
        break;
      }
    }
        /* this section is common to all bribes */
    if (strchr("pemgCtT", c) != NULL) {
      mvwaddstr(spyw, 14, 10, "Hit space");
      wrefresh(spyw);
      get_space();
      spying_np->jewels -= bribe;
      cjewels(spying_np, -bribe);
      cjewels(spied_np, ((bribe * spied_np->taxes)/100));
    /* Add the tax rate in jewels from the bribe to the other nation */
    }
  }
  delwin(spyw);
}

#ifdef OLD_SPY_FIGURE
old_spy_figure(n, expend, spying_np, spied_np, cost_fact)
     int n,			/* number we modify here */
       expend;			/* jewels spent on bribes */
     Snation *spying_np, *spied_np; /* nations involved */
     int cost_fact;		/* cost factor for type of info */
{
  double accuracy, error;
  int figure;			/* the figure we actually return */

  if (spied_np->secrecy == 0) {
    spied_np->secrecy = 1;	/* avoid divide-by-zero */
  }
  accuracy =
    ((double) expend*spying_np->spy)/(cost_fact*spied_np->secrecy*100);
  
    /* now get the percent error */
  error = 100*( exp(-sqrt(accuracy)) + 1.0/(5.0+accuracy) );
  if (error > 100) {
    error = 100;
  }
/*  sprintf(s, "acc=%f,%%err=%f", accuracy, error); */
    /* now get the absolute error */
  error = 1 + n*error/100.0;
  figure = n + (RND() % ((int) error)) - (int) (error/2);

/*  statline2(s, ""); */

  return figure;
}
#endif /* OLD_SPY_FIGURE */

int spy_tech_steal(int n, int expend, Snation *spying_np, Snation *spied_np)
/* n:                    number we modify here */
/* expend:               jewels spent on bribes */
/* spying_np, spied_np:  nations involved */
{
  double spy_fact, bribe_fact, range, scale, capture;
  int figure;			/* the figure we actually return */
  int factor_diff, eff_jewels;
  
  /* We can't steal tech if they are worse than we are */
  if (n <= 0) { return 0; } 

  if ((factor_diff = spied_np->secrecy - spying_np->spy) >= 0) {
    spy_fact = 1.0 + (double)factor_diff * SPY_FACT_SCALE;
  } else {
    spy_fact = 1.0/(1.0 - (double)factor_diff * SPY_FACT_SCALE);
  }
  spy_fact = 1.0/sqrt(spy_fact);
  if ((eff_jewels = spied_np->jewels) < MIN_JEWELS) {
    eff_jewels = MIN_JEWELS;
  }
  bribe_fact = ((double)expend * 100.0)/(double)eff_jewels;
  bribe_fact *= ((double)BRIBE_FACT_SCALE / 5.0);
  bribe_fact = sqrt(bribe_fact);

  capture =  CAPTURE_STEAL * bribe_fact;
  if ((RND() % (int)capture) == 0) { /* One chance in capture */
    spy_found_notification(spying_np, spied_np);
  } else {
    capture = CAPTURE_STEAL * spy_fact;
    if ((RND() % (int)capture) == 0) {  /* One chance in capture */
      spy_found_notification(spying_np, spied_np);
    }
  }
  if ((scale = bribe_fact * spy_fact) < 1.0) {
    figure = 0;
  } else {
    int acc = 0, i;

    range = 0.9 / scale; /* 90% out if scale exaclty one */
    range = 1.0 - range; /* means 10% in if scale exactly one */
    range *= (double)n * 100;  /* Set that to 10% of the difference & scale */

    for (i = 0 ; i < GUASS_RND_NUM; i++) {
      /* Create guasian random number with full bell double range */
      acc += (RND() % ((int)((range * 2.0)/(double)GUASS_RND_NUM)));
    }
    acc -= (int)range;
    figure = (double)(abs(acc)) / 100.0; /* rescale the value */
    
/*
    sprintf(s, "range=%f,scale=%f,acc=%d", range/100.0, scale,acc);
    statline2(s, "");
*/
  }
  return figure;
}

int spy_figure(int n, int expend, Snation *spying_np, Snation *spied_np,
	       int cost_fact)
/* n:                      number we modify here */
/* expend:                 jewels spent on bribes */
/* spying_np, spied_np:    nations involved */
/* cost_fact:              cost factor for type of info */
{
  double error, spy_fact, bribe_fact, range, capture;
  int figure;			/* the figure we actually return */
  int factor_diff, eff_jewels;
  
  if ((factor_diff = spied_np->secrecy - spying_np->spy) >= 0) {
    spy_fact = 1.0 + (double)factor_diff * SPY_FACT_SCALE;
  } else {
    spy_fact = 1.0/(1.0 - (double)factor_diff * SPY_FACT_SCALE);
  }
  spy_fact = sqrt(spy_fact);
  if (spied_np->jewels < MIN_JEWELS) { 
    eff_jewels = MIN_JEWELS;
  } else {
    eff_jewels = spied_np->jewels;
  }
  bribe_fact = ((double)expend * 100.0)/(double)eff_jewels;
  bribe_fact *= BRIBE_FACT_SCALE;
  bribe_fact = 1.0/sqrt(bribe_fact); 

  range = (double)cost_fact * bribe_fact * spy_fact * 1000.0;

  error = (double)((RND() % ((int) range)) - (int) (range/2));
  figure = n * (1.0 + ((error/1000.0)/100.0));

  capture =  CAPTURE_SPY/ bribe_fact;
  if ((RND() % (int)capture) == 0) { /* One chance in capture */
    spy_found_notification(spying_np, spied_np);
  } else {
    capture = CAPTURE_SPY / spy_fact;
    if ((RND() % (int)capture) == 0) {  /* One chance in capture */
      spy_found_notification(spying_np, spied_np);
    }
  }
/*
  sprintf(s, "range=%f,%%err=%f", range/100000.0, error/1000.0);
  statline2(s, "");
*/
  return figure;
}

void spy_found_notification(Snation *spying_np, Snation *spied_np)
{
  FILE *mailfile, *fopen();
  char mailname[NAMELEN];

  sprintf(mailname,"spymail.%d",spying_np->id);
  if ((mailfile = fopen(mailname, "w")) == NULL) {
    printf("could not open mailfile %s\n", mailname);
    return;
  }
  fprintf(mailfile, "\nA spy from %s was found in your nation\n\n",
    spying_np->name);
  fclose(mailfile);
  mail_send(mailname, 0, spied_np->id, "Spy Discovered");
}
