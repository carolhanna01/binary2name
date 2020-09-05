/* Mail processing routines for GNATS.
   Copyright (C) 2000, 2002, 2007 Free Software Foundation, Inc.
   Contributed by Bob Manson (manson@juniper.net).

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

#include "gnats.h"
#include "query.h"
#include "pcodes.h"
#include "mail.h"

void
addMessageFormat (DatabaseInfo database, MailMessageFormat format)
{
  format->next = getMailFormatList (database);
  setMailFormatList (database, format);
}

MailMessageFormat
findMailFormat (const DatabaseInfo database, const char *name)
{
  MailMessageFormat p;

  for (p = getMailFormatList (database); p != NULL; p = p->next)
    {
      if (strcmp (p->name, name) == 0)
	{
	  break;
	}
    }
  return p;
}

/* get_responsible_address - dredges the responsible party out of the
     appropriate places.  This routine should be NIS Correct, but it isn't.  */

AdmEntry *
get_responsible_address (const DatabaseInfo database, const char *person)
{
  AdmEntry *res = NULL;
  AdmEntry *responsible_chain = NULL;

  if (fieldDefForIndex (RESPONSIBLE (database)) != NULL)
    {
      responsible_chain 
	= fieldDefForIndex (RESPONSIBLE (database))->adm_contents;
      res = find_chain_entry (responsible_chain, person);
    }

  /* First check the responsible file; if there's no entry, try the
     passwd file.  */
  if (res != NULL)
    {
      if (res->admFields[ResponsibleAdmAlias] == NULL ||
	  res->admFields[ResponsibleAdmAlias] == '\0')
	{
	  if (res->admFields[ResponsibleAdmAlias] != NULL)
	    {
	      free (res->admFields[ResponsibleAdmAlias]);
	    }
	  res->admFields[ResponsibleAdmAlias] 
	    = xstrdup (res->admFields[ResponsibleAdmKey]);
	}
    }
  else
    {
      struct passwd *passwd;
      char *p;

      res = alloc_adm_entry (3);
      res->field = RESPONSIBLE (database);

      /* We should always allow names to show up as responsible, even
	 if they aren't listed in the passwd file---folks don't remember
	 (or usually need) to change closed PRs if the listed person
	 happens to leave the company.  */
      res->admFields[ResponsibleAdmKey] = (char *) xstrdup (person);
      res->admFields[ResponsibleAdmAlias] = (char *) xstrdup (person);

      if ((passwd = getpwnam (person)) != 0)
	{
	  /* Some passwd entries have commas for finger programs that
	     understand office phone numbers, etc.  Chop 'em off.  */
	  p = (char *) strchr (passwd->pw_gecos, ',');
	  if (p != NULL)
	    *p = '\0';
	  res->admFields[ResponsibleAdmFullname] 
	    = (char *) xstrdup (passwd->pw_gecos);
	}
      else 
	{
	  res->admFields[ResponsibleAdmFullname] = xstrdup("");
	}
    } 

  return res;
}


static char *
get_one_responsible_addr (const DatabaseInfo database,
			  int full, int strict, const char *name)
{
  AdmEntry *r;
  char *p;
  char *address = NULL;

  /* If it already vaguely resembles a full email address, then just
     return it as is.  */
  if (strpbrk (name, "<(@") != NULL)
    {
      return xstrdup (name);
    }

  /* Strip off (Foo Bar) or anything after a space in the name. */
  p = (char *) strchr (name, ' ');
  if (p != (char *) NULL)
    {
      *p = '\0';
    }
  p = (char *) strchr (name, '(');
  if (p != (char *) NULL)
    {
      *p = '\0';
    }

  r = get_responsible_address (database, name);

  if (r != NULL && ((! strict)
		    || r->admFields[ResponsibleAdmFullname][0] != '\0'))
    {
      if (full)
	{
	  asprintf (&address, "%s:%s:%s", r->admFields[ResponsibleAdmKey],
		    r->admFields[ResponsibleAdmFullname],
		    r->admFields[ResponsibleAdmAlias]);
	}
      else
	{
	  /* Make sure if the person putting the entry in accidentally
	     added spaces or such after the colon, we strip them off. 

	     XXX ??? !!! We shouldn't do this here; instead, make sure
	     the entry we return from get_responsible_address () is
	     clean, or better still, do it when we read entries from
	     the adm files.  */
	  char *addr = r->admFields[ResponsibleAdmAlias];

	  while (*addr != '\0' && ! isalpha ((int) *addr))
	    {
	      addr++;
	    }
	  if (*addr != '\0')
	    {
	      address = xstrdup (addr);
	    }
	  else
	    {
	      address = xstrdup (r->admFields[ResponsibleAdmKey]);
	    }
	}
    }
  free_adm_entry (r);

  return address;
}

static char *
getOneAddress (const char **addrPtr)
{
  const char *addr = *addrPtr;
  const char *addrStart, *addrEnd;
  char *res;

  if (addrPtr == NULL || *addrPtr == NULL)
    {
      return NULL;
    }

  /* skip over leading white space */
  while (*addr && isspace((int)(unsigned char)*addr))
    {
      addr++;
    }
  addrStart = addr;

  while (*addr != ',' && *addr != '\0')
    {
      char nextc = '\0';

      switch (*addr)
	{
	case '"':
	  nextc = '"';
	  break;
	case '(':
	  nextc = ')';
	  break;
	case '<':
	  nextc = '>';
	  break;
	}
      if (nextc != '\0')
	{
	  /* XXX ??? !!! This probably isn't handling backquotes properly.  */
	  char *naddr = strchr (addr + 1, nextc);
	  if (naddr == NULL)
	    {
	      addr += strlen (addr);
	      break;
	    }
	  addr = naddr;
	}
      addr++;
    }
  addrEnd = addr;

  /* ignore any ending white space */
  while (addr > addrStart && isspace((int)(unsigned char)*(addr-sizeof(*addr))))
    {
      addr--;
    }

  if (addr <= addrStart)
    res = NULL;
  else
    res = xstrndup (addrStart, addr - addrStart);

  *addrPtr = (*addrEnd == '\0') ? NULL : addrEnd+1;
  return res;
}

char *
get_responsible_addr (const DatabaseInfo database,
		      int full, int strict, const char *name)
{
  const char *c = name;
  char *res = NULL;

  while (c != NULL)
    {
      char *respaddr;
      char *addr = getOneAddress (&c);
      if (addr == NULL || *addr == '\0')
        {
	  break;
        }
      respaddr = get_one_responsible_addr (database, full, strict, addr);

      if (respaddr != NULL)
	{
	  if (res != NULL)
	    {
	      append_string (&res, ",");
	    }
	  append_string (&res, respaddr);
	  free (respaddr);
	}
      free (addr);
    }
  return res;
}

char *
generateMailAddress (PR *pr, PR *oldPR, MailAddress *address,
		     FormatNamedParameter *params)
{
  FieldList addrList;

  if (address->fixedAddress != NULL)
    {
      return get_responsible_addr (pr->database, 0, 0, address->fixedAddress);
    }

  for (addrList = address->addresses;
       addrList != NULL;
       addrList = addrList->next)
    {
      int mustBeFreed = 0;

      const char *value = get_field_value (pr, oldPR, addrList->ent, params,
					   &mustBeFreed);

      if (value != NULL && value[0] != '\0')
	{
	  char *res = get_responsible_addr (pr->database, 0, 0, value);
	  size_t len = strlen (res);
	  /* Badness 10000. XXX ??? !!! */
	  if (res[len - 1] == '\n')
	    {
	      res[len - 1] = '\0';
	    }
	  if (mustBeFreed)
	    {
	      free ((char *) value);
	    }
	  return res;
	}
      if (mustBeFreed && value != NULL)
	{
	  free ((char *) value);
	}
    }
  return NULL;
}

static char *
insertAddress (char *addressList, const char *addrToInsert)
{
  if (addrToInsert == NULL)
    {
      return addressList;
    }
  else if (addressList == NULL)
    {
      return xstrdup (addrToInsert);
    }
  else
    {
      size_t oldlen = strlen (addressList);
      size_t addrToInsertLen = strlen (addrToInsert);
      size_t newlen = oldlen + addrToInsertLen + 2;
      char *p = addressList;
      char *res;

      while (*p != '\0')
	{
	  char *prevp = p;
	  char *lastCharInAddr;

	  p = strchr (p, ',');
	  if (p == NULL)
	    {
	      p = prevp + strlen (prevp);
	    }

	  lastCharInAddr = p - 1;

	  while (lastCharInAddr > prevp && isspace ((int)(unsigned char) *lastCharInAddr))
	    {
	      lastCharInAddr--;
	    }

	  if (((size_t) (lastCharInAddr - prevp + 1)) == addrToInsertLen
	      && memcmp (prevp, addrToInsert, addrToInsertLen) == 0)
	    {
	      return addressList;
	    }
	  if (*p == ',')
	    {
	      p++;
	    }
	}

      res = xrealloc (addressList, newlen);
      res[oldlen] = ',';
      strcpy (res + oldlen + 1, addrToInsert);
      return res;
    }
}

/* Composes a mail message using FORMAT as the mail message format. */
int
composeMailMessage (PR *pr, PR *oldPR, const char *formatName, 
		    FormatNamedParameter *parameters,
		    BadFields bad_fields,
		    ErrorDesc *err ATTRIBUTE_UNUSED)
{
  MailMessageFormat format = findMailFormat (pr->database, formatName);
  FILE *msg;
  char *fromAddr;
  char *toAddr = NULL;
  char *replyToAddr = NULL;
  MailAddressList *p;

  if (format == NULL)
    {
      return -1;
    }

  block_signals ();

  fromAddr = generateMailAddress (pr, oldPR, format->fromAddress, parameters);
  if (fromAddr == NULL)
    {
      fromAddr = (char *)gnatsAdminMailAddr (pr->database);
    }
  for (p = format->toAddresses; p != NULL; p = p->next)
    {
      char *theAddr = generateMailAddress (pr, oldPR, p->address, parameters);
      if (theAddr != NULL)
	{
	  toAddr = insertAddress (toAddr, theAddr);
	  free (theAddr);
	}
    }

  for (p = format->replyTo; p != NULL; p = p->next)
    {
      char *theAddr = generateMailAddress (pr, oldPR, p->address, parameters);
      if (theAddr != NULL)
	{
	  replyToAddr = insertAddress (replyToAddr, theAddr);
	  free (theAddr);
	}
    }

  msg = open_mail_file (pr->database);
  if (msg == NULL)
    {
      return -1;
    }
  fprintf (msg, "From: %s\n", fromAddr);
  fprintf (msg, "To: %s\n", toAddr);
  if (replyToAddr != NULL)
    {
      fprintf (msg, "Reply-To: %s\n", replyToAddr);
    }
  process_format (msg, NULL, pr, oldPR, format->header, "\n", parameters);
  fprintf (msg, "\n");

  /* XXX ??? !!! This should be handled as a format parameter.  */
  if (bad_fields != NULL)
    {
      /* Report the fields that were bad, separating them by an empty line.  */
      BadFields t;
      for (t = bad_fields; t != NULL ; t = nextBadField (t))
	{
	  FieldIndex field = badFieldIndex (t);
	  const char *value = badFieldValue (t);
	  if (value != NULL)
	    {
	      fprintf (msg,
		       "\tNote: There was a bad value `%s' for the field `%s'.\n\tIt was set to the default value of `%s'.\n\n",
		       value, fieldDefForIndex (field)->name,
		       fieldDefForIndex (field)->default_value);
	    }
	  else
	    {
	      fprintf (msg,
		       "\tNote: There was a missing value for the field `%s'.\n\tIt was set to the default value of `%s'.\n\n",
		       fieldDefForIndex (field)->name,
		       fieldDefForIndex (field)->default_value);
	    }
	}
    }

  process_format (msg, NULL, pr, oldPR, format->body, "\n", parameters);
  close_mail_file (msg);

  free (fromAddr);
  free (toAddr);
  if (replyToAddr != NULL)
    {
      free (replyToAddr);
    }

  unblock_signals ();

  return 0;
}

FormatNamedParameter *
allocateNamedParameter (const char *name, const char *value,
			FormatNamedParameter *next)
{
  FormatNamedParameter *res
    = (FormatNamedParameter *) xmalloc (sizeof (FormatNamedParameter));
  res->name = xstrdup (name);
  if (value != NULL)
    {
      res->value  = xstrdup (value);
    }
  else
    {
      res->value = NULL;
    }
  res->next = next;
  return res;
}

const char *
getNamedParameterValue (FormatNamedParameter *list, const char *name)
{
  while (list != NULL)
    {
      if (strcmp (list->name, name) == 0)
	{
	  return list->value;
	}
      list = list->next;
    }
  return NULL;
}

void
freeFormatParameterList (FormatNamedParameter *list)
{
  while (list != NULL)
    {
      FormatNamedParameter *next = list->next;
      free (list->name);
      free (list->value);
      free (list);
      list = next;
    }
}

void
freeMailAddress (MailAddress *p)
{
  if (p != NULL)
    {
      if (p->fixedAddress != NULL)
	{
	  free (p->fixedAddress);
	}
      if (p->addresses != NULL)
	{
	  freeFieldList (p->addresses);
	}
      free (p);
    }
}

void
freeMailAddressList (MailAddressList *p)
{
  while (p != NULL)
    {
      MailAddressList *next = p->next;
      freeMailAddress (p->address);
      free (p);
      p = next;
    }
}

void
freeMessageFormat (MailMessageFormat p)
{
  if (p->name != NULL)
    {
      free (p->name);
    }
  freeMailAddressList (p->toAddresses);
  freeMailAddress (p->fromAddress);
  freeMailAddressList (p->replyTo);
  freeQueryFormat (p->header);
  freeQueryFormat (p->body);
  free (p);
}

void
freeMessageFormatList (MailMessageFormat formatList)
{
  while (formatList != NULL)
    {
      MailMessageFormat next = formatList->next;
      freeMessageFormat (formatList);
      formatList = next;
    }
}
