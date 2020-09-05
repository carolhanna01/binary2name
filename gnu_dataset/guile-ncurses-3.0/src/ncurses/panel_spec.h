/*
  panel_spec.h

  Copyright 2009, 2010, 2016 Free Software Foundation, Inc.

  This file is part of GNU Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#ifndef PANEL_SPEC_H
#define PANEL_SPEC_H

#include "visibility.h"

#include <libguile.h>

#if 0
SCM gucu_panel_above (SCM pan);
SCM gucu_panel_below (SCM pan);
SCM gucu_set_panel_userdata (SCM pan, SCM data);
SCM gucu_panel_userdata (SCM pan);
#endif
GUCU_API SCM gucu_panels_list (void);

GUCU_LOCAL void gucu_panel_init_special (void);

#endif
