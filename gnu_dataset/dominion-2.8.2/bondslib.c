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

#include "dominion.h"		/* this includes bonds.h */
#include "misc.h"

extern Sworld world;

/* reads the bonds issued by that nation from a file at loading time */
void load_bonds(Snation *np)
{
  char bonds_fname[PATHLEN];
  FILE *fbonds;
  Sbonds /* *bondlist, */ tmp_bond;

  sprintf(bonds_fname, "%s/bonds.%d", BONDS_DIR, np->id);
  if ((fbonds = fopen(bonds_fname, "r")) == NULL) { /* no bonds */
    return;
  }
  while (fread(&tmp_bond, sizeof(Sbonds), 1, fbonds) == sizeof(Sbonds)) {
    
  }

  fclose(fbonds);
}
