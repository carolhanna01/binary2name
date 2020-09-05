/* Flat-file datastore API - functions operating on PR fields.
   Copyright (C) 2005,07 Free Software Foundation, Inc.
   Contributed by Mel Hatzis <hatzis@wattes.org>.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#include "ds.h"
#include "ds-file.h"

char *
field_cache_value (PR *pr, FieldIndex field)
{
  /* Houston, we have a problem.  For dates, the value that's
     stored in the index is an integer, not the actual value of
     the field...  \hbadness 10000.  XXX ??? !!! FIXME */
  return indexValue (pr, field);
}

