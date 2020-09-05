 /* bonds.h -- data structures for the system of bonds */

#ifndef _BONDS_H
#define _BONDS_H

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

#define BOND_VALUES 1000	/* 1 bond == 1000 sk. */

struct s_bonds {
  /* the number of bonds in this issue (in 1000 sk. units) */
  int amount;
  int mature_thon;		/* the thon this issue matures */
  struct s_bonds *next;		/* next in the list of what you've issued */
};

typedef struct s_bonds Sbonds;

#endif /* _BONDS_H */
