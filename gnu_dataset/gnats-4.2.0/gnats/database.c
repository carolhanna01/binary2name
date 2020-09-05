/* Basic database info.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Bob Manson <manson@juniper.net>.

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
along with GNU GNATS; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"
#include "mail.h"
#include "pcodes.h"
#include "field.h"
#include "ds.h"

struct databaseInfo
{
  int debugModeFlag;
  int keepReceivedHeadersFlag;
  int notifyExpireFlag;
  char *binDirValue;
  int submitterAckFlag;
  unsigned int businessDay[2];
  unsigned int businessWeek[2];
  int createCategoryDirsFlag;
  int categoryDirPerms;
  QueryFormat *auditTrailFormat;
  QueryFormat *queryFormatList;
  char *databaseName;
  AdmEntry *databaseEnt;
  ChangeActions changeActions;
  FieldList requiredInputFields;
  DatabaseFieldInfo fieldInfo;
  AdmEntry *hostList;
  MailMessageFormat mailFormatList;
  InputTemplate *inputTemplate;
  FieldIndex builtinFields[NUM_BUILTIN_FIELDS];
  void *ds_private;    /* all the datastore specific stuff */
  /* Next and previous entries in the chain.  */
  struct databaseInfo *next, *prev;
};

static struct databaseInfo *databaseInfoList = NULL;

static AdmEntry *globalHostList = NULL;

int
databaseValid (const DatabaseInfo database)
{
  return database != NULL && database->databaseEnt != NULL;
}

static DatabaseInfo
newDatabaseInfo (void)
{
  DatabaseInfo res = (DatabaseInfo) xmalloc (sizeof (struct databaseInfo));
  int x;

  res->debugModeFlag = 0;
  res->keepReceivedHeadersFlag = 0;
  res->notifyExpireFlag = 0;
  res->binDirValue = NULL;
  res->submitterAckFlag = 0;
  res->businessDay[0] = 0; res->businessDay[1] = 0;
  res->businessWeek[0] = 0; res->businessWeek[1] = 0;
  res->createCategoryDirsFlag = 0;
#ifdef S_IRWXU
  res->categoryDirPerms = S_IRWXU | S_IRGRP | S_IXGRP;
#else
  res->categoryDirPerms = 0750;
#endif
  res->auditTrailFormat = NULL;
  res->queryFormatList = NULL;
  res->databaseName = NULL;
  res->databaseEnt = NULL;
  res->changeActions = NULL;
  res->requiredInputFields = NULL;
  res->fieldInfo = NULL;
  res->hostList = NULL;
  res->mailFormatList = NULL;
  res->inputTemplate = NULL;
  res->ds_private = NULL;

  for (x = 0; x < NUM_BUILTIN_FIELDS; x++)
    {
      res->builtinFields[x] = NULL;
    }

  res->next = databaseInfoList;
  res->prev = NULL;
  if (databaseInfoList != NULL)
    {
      databaseInfoList->prev = res;
    }
  databaseInfoList = res;

  return res;
}

static DatabaseInfo
findDatabaseEntry (AdmEntry *databaseEnt)
{
  DatabaseInfo res = databaseInfoList;
  while (res != NULL && res->databaseEnt != databaseEnt)
    {
      res = res->next;
    }
  return res;
}


DatabaseFieldInfo *
getDatabaseFieldInfoWrite (DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return &(database->fieldInfo);
    }
  else
    {
      return NULL;
    }
}

DatabaseFieldInfo
getDatabaseFieldInfo (DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->fieldInfo;
    }
  else
    {
      return NULL;
    }
}

const char *
gnatsAdminMailAddr (const DatabaseInfo database)
{
  return get_responsible_addr (database, 0, 0, "gnats-admin");
}

void
setDebugMode (DatabaseInfo database, int mode)
{
  if (databaseValid (database))
    {
      database->debugModeFlag = mode;
    }
}  

void
setKeepReceivedHeaders (DatabaseInfo database, int value)
{
  if (databaseValid (database))
    {
      database->keepReceivedHeadersFlag = value;
    }
}

void
setNotifyExpire (DatabaseInfo database, int value)
{
  if (databaseValid (database))
    {
      database->notifyExpireFlag = value;
    }
}

void
setBinDir (DatabaseInfo database, const char *value)
{
  if (databaseValid (database))
    {
      if (database->binDirValue != NULL)
	{
	  free (database->binDirValue);
	}
      database->binDirValue = xstrdup (value);
    }
}

void
setSubmitterAck (DatabaseInfo database, int value)
{
  if (databaseValid (database))
    {
      database->submitterAckFlag = value;
    }
}

void
setBusinessDay (DatabaseInfo database, unsigned int startTime,
		unsigned int endTime)
{
  if (databaseValid (database))
    {
      database->businessDay[0] = startTime;
      database->businessDay[1] = endTime;
    }
}

void
setBusinessWeek (DatabaseInfo database, unsigned int startDay,
		 unsigned int endDay)
{
  if (databaseValid (database))
    {
      database->businessWeek[0] = startDay;
      database->businessWeek[1] = endDay;
    }
}

void
setCreateCategoryDirs (DatabaseInfo database, int value)
{
  if (databaseValid (database))
    {
      database->createCategoryDirsFlag = value;
    }
}

void
setAuditTrailFormat (DatabaseInfo database, QueryFormat *format)
{
  if (databaseValid (database))
    {
      database->auditTrailFormat = format;
    }
}

void
setCategoryDirPerms (DatabaseInfo database, const char *value)
{
  if (databaseValid (database))
    {
      database->categoryDirPerms = strtol (value, NULL, 8);
    }
}

void
addGlobalChangeActions (DatabaseInfo database, ChangeActions actions)
{
  if (databaseValid (database))
    {
      ChangeActions *alist = &(database->changeActions);

      while (*alist != NULL)
	{
	  alist = &((*alist)->next);
	}

      *alist = actions;
    }
}

void
setRequiredInputFields (DatabaseInfo database, FieldList list)
{
  if (databaseValid (database))
    {
      database->requiredInputFields = list;
    }
}

void
setInputTemplate (DatabaseInfo database, InputTemplate *list)
{
  database->inputTemplate = list;
}

void
setQueryFormatList (DatabaseInfo database, QueryFormat *list)
{
  database->queryFormatList = list;
}

void
setBuiltinDBField (DatabaseInfo database, int whichField,
		   FieldIndex fieldIndex)
{
  if (whichField < 0 || whichField >= NUM_BUILTIN_FIELDS)
    {
      abort ();
    }
  else
    {
      database->builtinFields[whichField] = fieldIndex;
    }
}

void
setMailFormatList (DatabaseInfo database, MailMessageFormat formatList)
{
  database->mailFormatList = formatList;
}

void
setDatastorePrivate(DatabaseInfo database, void *data)
{
  database->ds_private = data;
}

int
keepReceivedHeaders (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->keepReceivedHeadersFlag;
    }
  else
    {
      return 0;
    }
}

int
debugMode (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->debugModeFlag;
    }
  else
    {
      return 0;
    }
}

char *
defaultSubmitter (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      AdmEntry *chain = fieldDefForIndex (SUBMITTER (database))->adm_contents;
      if (chain != NULL)
	{
	  char *res = xstrdup (chain->admFields[SubmitterAdmKey]);
	  return res;
	}
      else
	{
	  return xstrdup ("unknown");
	}
    }
  else
    {
      return NULL;
    }
}

char *
mailAgent (const DatabaseInfo database)
{
  const char *bdir = binDir (database);

  if (bdir != NULL)
    {
      char *agentPath;
      asprintf (&agentPath, "%s/mail-agent", bdir);
      return agentPath;
    }
  else
    {
      return NULL;
    }
}

char *
defaultCategory (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      AdmEntry *chain = fieldDefForIndex (CATEGORY (database))->adm_contents;
      if (chain != NULL)
	{
	  char *res = xstrdup (chain->admFields[CategoryAdmKey]);
	  return res;
	}
      else
	{
	  return xstrdup ("pending");
	}
    }
  else
    {
      return NULL;
    }
}

char *
defaultState (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      AdmEntry *chain = fieldDefForIndex (STATE (database))->adm_contents;
      if (chain != NULL)
	{
	  char *res = xstrdup (chain->admFields[StateAdmKey]);
	  return res;
	}
      else
	{
	  return xstrdup ("open");
	}
    }
  else
    {
      return NULL;
    }
}

int
notifyExpire (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->notifyExpireFlag;
    }
  else
    {
      return 0;
    }
}

const char *
binDir (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->binDirValue;
    }
  else
    {
      return NULL;
    }
}

int
submitterAck (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->submitterAckFlag;
    }
  else
    {
      return 0;
    }
}

unsigned int
businessDayStart (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->businessDay[0];
    }
  else
    {
      return 0;
    }
}

unsigned int
businessDayEnd (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->businessDay[1];
    }
  else
    {
      return 0;
    }
}

unsigned int
businessWeekStart (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->businessWeek[0];
    }
  else
    {
      return 0;
    }
}

unsigned int
businessWeekEnd (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->businessWeek[1];
    }
  else
    {
      return 0;
    }
}

int
createCategoryDirs (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->createCategoryDirsFlag;
    }
  else
    {
      return 0;
    }
}

int
categoryDirPerms (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->categoryDirPerms;
    }
  else
    {
      return 0700;
    }
}

char *
gnats_adm_dir (const DatabaseInfo database, const char *filename)
{
  const char *ddir = databaseDir (database);
  if (ddir != NULL)
    {
      char *ptr;
      asprintf (&ptr, "%s/gnats-adm/%s", ddir, filename);
      return ptr;
    }
  else
    {
      return NULL;
    }
}

ChangeActions
globalChangeActions (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->changeActions;
    }
  else
    {
      return NULL;
    }
}

QueryFormat *
getQueryFormatList (const DatabaseInfo database)
{
  return database->queryFormatList;
}

MailMessageFormat
getMailFormatList (const DatabaseInfo database)
{
  return database->mailFormatList;
}

void *
getDatastorePrivate(const DatabaseInfo database)
{
  return database->ds_private;
}

static AdmEntry *dbList = NULL;

static int
readAdmFile (const char *filename, int fields, AdmEntry **dest, 
	     int allowOptionalFields)
{
  char *line;
  FILE *f = fopen (filename, "r");

  if (f == NULL)
    {
      return -1;
    }

  while ((line = read_line (f, NULL)) != NULL)
    {
      if (line[0] != '#' && line[0] != '\n' && !isspace ((int)(unsigned char) line[0]))
	{
	  AdmEntry *nent = build_adm_entry (line, InvalidFieldIndex);

	  if (nent != NULL)
	    {
	      if (nent->fieldcount == fields
		  || (allowOptionalFields && (nent->fieldcount > fields)))
		{
		  *dest = nent;
		  dest =  &(nent->next);
		}
	      else
		{
		  free_adm_entry (nent);
		}
	    }
	}
      free (line);
    }
  fclose (f);
  return 0;
}


int
initDatabaseList (ErrorDesc *err)
{
  if (dbList != NULL)
    {
      freeAdmEntryChain (dbList);
      dbList = NULL;
    }
  /* Null out the databaseEnt pointers that just went away */
  if (databaseInfoList != NULL)
    {
      DatabaseInfo info = databaseInfoList;
      
      while (info != NULL)
	{
	  info->databaseEnt = NULL;
	  info = info->next;
	}
    }

  /* Ensure global_db_list_file points to something
   *
   * TODO: This should probably move to a global "set defaults"
   * routine
   **/
#ifdef GLOBAL_DB_LIST_FILE
  if (!global_db_list_file)
    global_db_list_file = GLOBAL_DB_LIST_FILE;
#endif

  if (readAdmFile (global_db_list_file, 3, &dbList, 1) < 0)
    {
      setError (err, CODE_FILE_ERROR, 
		"Unable to open global configuration file %s",
		global_db_list_file);
      dbList = NULL;
      return -1;
    }
  else if (dbList == NULL)
    {
      setError (err, CODE_FILE_ERROR,
		"List of databases in %s is empty?!",
		global_db_list_file);
      return -1;
    }
  else
    {
      return 0;
    }
}

static void
initGlobalHostList (void)
{
  if (globalHostList != NULL)
    {
      freeAdmEntryChain (globalHostList);
      globalHostList = NULL;
    }
  if (readAdmFile (GNATSD_HOST_ACCESS_FILE, 3,  &globalHostList, 0) < 0)
    {
      fprintf (stderr, "Unable to open %s\n", GNATSD_HOST_ACCESS_FILE);
      globalHostList = NULL;
    }
}

static void
initHostList (DatabaseInfo database)
{
  char *path;

  if (! databaseValid (database))
    {
      return;
    }
  path = gnats_adm_dir (database, DB_HOST_ACCESS_FILE);

  if (path != NULL)
    {
      if (fileExists (path))
	{
	  if (readAdmFile (path, 3, &(database->hostList), 0) < 0)
	    {
	      fprintf (stderr, "Unable to open %s\n", path);
	    }
	}
      free (path);
    }
}

AdmEntry *
getHostList (const DatabaseInfo database)
{
  if (! databaseValid (database))
    {
      return NULL;
    }

  return database->hostList;
}

AdmEntry *
getGlobalHostList (void)
{
  if (globalHostList == NULL)
    {
      initGlobalHostList ();
    }
  return globalHostList;
}

static DatabaseInfo
loadDatabase (const char *databaseName, AdmEntry *databaseEnt, ErrorDesc *err)
{
  DatabaseInfo res = newDatabaseInfo ();

  res->databaseName = xstrdup (databaseName);

  res->databaseEnt = databaseEnt;

  if (! fileExists (res->databaseEnt->admFields[DatabaseListPath]))
    {
      setError (err, CODE_INVALID_DATABASE,
	       "The directory %s does not exist for database %s",
	       res->databaseEnt->admFields[DatabaseListPath],
	       databaseName);
      freeDatabaseInfo (res);
      return NULL;
    }

  initHostList (res);

  if (db_init (res, err) < 0)
    {
      freeDatabaseInfo (res);
      return NULL;
    }

  return res;
}


DatabaseInfo
findOrLoadDatabase (const char *programName ATTRIBUTE_UNUSED,
		    const char *databaseName, ErrorDesc *err)
{
  DatabaseInfo res;
  AdmEntry *databaseEnt;

  if (databaseName == NULL)
    {
      databaseName = getenv ("GNATSDB");
      if (databaseName == NULL || databaseName[0] == '\0')
	{
	  databaseName = "default";
	}
    }

  databaseEnt = find_chain_entry_nocopy (dbList, databaseName);

  if (databaseEnt == NULL)
    {
      setError (err, CODE_INVALID_DATABASE, "No such database as %s",
		databaseName);
      res = NULL;
    }
  else
    {
      res = findDatabaseEntry (databaseEnt);
      if (res == NULL)
	{
	  res = loadDatabase (databaseName, databaseEnt, err);
	}
    }
  return res;
}

AdmEntry *
getDatabaseList (ErrorDesc *err)
{
  if (dbList == NULL)
    {
      initDatabaseList (err);
    }
  return dbList;
}

const char *
databaseDir (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->databaseEnt->admFields[DatabaseListPath];
    }
  else
    {
      return NULL;
    }
}

const char *
databaseName (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->databaseName;
    }
  else
    {
      return NULL;
    }
}

const char *
databaseDesc (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->databaseEnt->admFields[DatabaseListDesc];
    }
  else
    {
      return NULL;
    }
}

QueryFormat *
getAuditTrailFormat (const DatabaseInfo database)
{
  if (databaseValid (database))
    {
      return database->auditTrailFormat;
    }
  else
    {
      return NULL;
    }
}

FieldList
getRequiredInputFields (const DatabaseInfo database)
{
  return database->requiredInputFields;
}

InputTemplate *
getInputTemplate (const DatabaseInfo database)
{
  return database->inputTemplate;
}

FieldIndex
getBuiltinField (const DatabaseInfo database, int fieldNum)
{
  if (fieldNum < 0 || fieldNum >= NUM_BUILTIN_FIELDS)
    {
      abort ();
    }
  else
    {
      return database->builtinFields[fieldNum];
    }
}

static void
clearHostList (DatabaseInfo database)
{
  freeAdmEntryChain (database->hostList);
  database->hostList = NULL;
}

void
clearDbList (void)
{
  freeAdmEntryChain (dbList);
  dbList = NULL;
}

static void
clearInputTemplate (DatabaseInfo database)
{
  freeInputTemplate (database->inputTemplate);
  database->inputTemplate = NULL;
}

static void
clearGlobalChangeActions (DatabaseInfo database)
{
  freeChangeActions (database->changeActions);
  database->changeActions = NULL;
}

static void
clearQueryFormatList (DatabaseInfo database)
{
  if (database->queryFormatList != NULL)
    {
      freeQueryFormatList (database->queryFormatList);
      database->queryFormatList = NULL;
    }
}

static void
clearAuditTrailFormat (DatabaseInfo database)
{
  if (database->auditTrailFormat != NULL)
    {
      freeQueryFormatList (database->auditTrailFormat);
      database->auditTrailFormat = NULL;
    }
}

static void
clearMessageFormatList (DatabaseInfo database)
{
  if (database->mailFormatList != NULL)
    {
      freeMessageFormatList (database->mailFormatList);
      database->mailFormatList = NULL;
    }
}

void
freeDatabaseInfo (DatabaseInfo database)
{
  if (database != NULL)
    {
      clearQueryFormatList (database);
      clearMessageFormatList (database);
      if (database->ds_private != NULL)
        {
	  db_destroy (database);  /* free up the datastore specific stuff */
        }
      clearFieldList (database);
      clearAuditTrailFormat (database);
      clearHostList (database);
      clearInputTemplate (database);
      clearGlobalChangeActions (database);
      freeFieldList (database->requiredInputFields);
      if (database->databaseName != NULL)
	{
	  free (database->databaseName);
	}
      if (database->binDirValue != NULL)
	{
	  free (database->binDirValue);
	}
      if (database->next != NULL)
	{
	  database->next->prev = database->prev;
	}
      if (database->prev != NULL)
	{
	  database->prev->next = database->next;
	}
      else if (databaseInfoList == database)
	{
	  databaseInfoList = database->next;
	}
      database->next = NULL;
      database->prev = NULL;
      free (database);
    }
}

int
databaseSpecIsNetConn (const char *name)
{
  AdmEntry *dbEnt;
  ErrorDesc err;

  if (name == NULL || name[0] == '\0')
    {
      name = "default";
    }

  if (initDatabaseList (&err))
    {
      return 0;
    }
  dbEnt = find_chain_entry_nocopy (dbList, name);
  if (dbEnt == NULL)
    {
      return 0;
    }
  return (dbEnt->fieldcount > 3) ? 1 : 0;
}

const char *
databaseSpecServer (const char *name)
{
  AdmEntry *dbEnt;
  ErrorDesc err;

  if (name == NULL || name[0] == '\0')
    {
      name = "default";
    }
  if (dbList == NULL)
    {
      initDatabaseList (&err);
    }
  dbEnt = find_chain_entry_nocopy (dbList, name);
  if (dbEnt == NULL)
    {
      return NULL;
    }
  return (dbEnt->fieldcount > 3) ? dbEnt->admFields[DatabaseServer] : NULL;
}

const char *
databaseSpecPort (const char *name)
{
  AdmEntry *dbEnt;

  if (name == NULL || name[0] == '\0')
    {
      name = "default";
    }
  dbEnt = find_chain_entry_nocopy (dbList, name);
  if (dbEnt == NULL)
    {
      return NULL;
    }
  return (dbEnt->fieldcount > 4) ? dbEnt->admFields[DatabasePort] : NULL;
}
