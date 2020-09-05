/* Interface for sending mail and processing mail formats.
   Copyright (C) 2000,07 Free Software Foundation, Inc.
   Written by Bob Manson <manson@juniper.net>.

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

#ifndef _MAIL_H
#define _MAIL_H

typedef struct mail_address MailAddress;
typedef struct mail_address_list MailAddressList;
typedef struct mail_message_format *MailMessageFormat;
typedef struct format_named_parameter
{
  char *name;
  char *value;
  struct format_named_parameter *next;
} FormatNamedParameter;

#include "query.h"
#include "adm.h"

/* These addresses are always mapped through the responsible adm
   file.  */
struct mail_address
{
  /* A fixed name.  */
  char *fixedAddress;
  /* A list of addresses to try--each one is tried in turn until a
     non-empty one is found. */
  FieldList addresses;
};

struct mail_address_list 
{
  MailAddress *address;
  struct mail_address_list *next;
};

struct mail_message_format
{
  /* The name of this message format.  */
  char *name;

  MailAddressList *toAddresses;

  MailAddress *fromAddress;
  MailAddressList *replyTo;

  QueryFormat *header;
  QueryFormat *body;

  struct mail_message_format *next;
};

extern char *get_responsible_addr (const DatabaseInfo database,
				   int full, int strict, const char *name);
extern AdmEntry *get_responsible_address (const DatabaseInfo database, 
					  const char *);

extern void addMessageFormat (DatabaseInfo database, MailMessageFormat format);
extern MailMessageFormat findMailFormat (const DatabaseInfo database, 
					 const char *name);
extern char *generateMailAddress (PR *pr, PR *oldPR, MailAddress *address,
				  FormatNamedParameter *parameters);
extern int composeMailMessage (PR *pr, PR *oldPR,
			       const char *formatName,
			       FormatNamedParameter *parameters,
			       BadFields bad_fields,
			       ErrorDesc *err);

extern FormatNamedParameter *
allocateNamedParameter (const char *name, const char *value, 
			FormatNamedParameter *next);

extern const char *getNamedParameterValue (FormatNamedParameter *list, 
					   const char *name);

extern void freeFormatParameterList (FormatNamedParameter *list);

extern void freeMailAddress (MailAddress *addr);
extern void freeMailAddressList (MailAddressList *list);
extern void freeMessageFormat (MailMessageFormat format);
extern void freeMessageFormatList (MailMessageFormat formatList);

#endif
