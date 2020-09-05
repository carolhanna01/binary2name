/* Functions and data types used throughout the flat-file datastore.
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

#ifndef _DS_FILE_H
#define _DS_FILE_H

typedef struct file_pr_info *FilePRInfo;
typedef struct file_db_info *FileDBInfo;

#include "index.h"

struct file_pr_info {
  Index *index;   /* Index entry for the PR. */
};

struct file_db_info {
      IndexDesc indexDesc;
};

#endif /* !_DS_FILE_H */
