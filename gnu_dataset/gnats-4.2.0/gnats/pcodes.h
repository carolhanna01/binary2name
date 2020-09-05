/* Directives for the pseudo-protocol.
   Copyright (C) 1997,2007 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#ifndef __PCODES_H
#define __PCODES_H

#define CODE_GREETING			200
#define CODE_CLOSING			201
#define CODE_OK				210
#define CODE_SEND_PR			211
#define CODE_SEND_TEXT			212
#define CODE_SEND_CHANGE_REASON		213
#define CODE_NO_PRS_MATCHED		220
#define CODE_NO_ADM_ENTRY		221

#define CODE_PR_READY			300
#define CODE_TEXT_READY			301
#define CODE_INFORMATION		350
#define CODE_INFORMATION_FILLER		351

#define CODE_NONEXISTENT_PR		400
#define CODE_EOF_PR			401
#define CODE_UNREADABLE_PR		402
#define CODE_INVALID_PR_CONTENTS	403
#define CODE_INVALID_FIELD_NAME		410
#define CODE_INVALID_ENUM               411
#define CODE_INVALID_DATE		412
#define CODE_INVALID_FIELD_CONTENTS	413
#define CODE_INVALID_SEARCH_TYPE	414
#define CODE_INVALID_EXPR		415
#define CODE_INVALID_LIST		416
#define CODE_INVALID_DATABASE		417
#define CODE_INVALID_QUERY_FORMAT	418
#define CODE_INVALID_FIELD_EDIT		419
#define CODE_NO_KERBEROS		420
#define CODE_AUTH_TYPE_UNSUP		421
#define CODE_NO_ACCESS			422
#define CODE_LOCKED_PR			430
#define CODE_GNATS_LOCKED		431
#define CODE_GNATS_NOT_LOCKED		432
#define CODE_PR_NOT_LOCKED		433
#define CODE_READONLY_FIELD		434
#define CODE_INVALID_FTYPE_PROPERTY	435
#define CODE_CMD_ERROR			440
#define CODE_WRITE_PR_FAILED		450

#define CODE_ERROR			600
#define CODE_TIMEOUT			610
#define CODE_NO_GLOBAL_CONFIG		620
#define CODE_INVALID_GLOBAL_CONFIG	621
#define CODE_INVALID_INDEX		622
#define CODE_NO_INDEX			630
#define CODE_FILE_ERROR			640

#endif /* __PCODES_H */
