/* Basic database info.
   Copyright (C) 1999,2007 Free Software Foundation, Inc.
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
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#ifndef _DATABASE_H
#define _DATABASE_H

struct databaseInfo;
typedef struct databaseInfo * DatabaseInfo;

#include "adm.h"
#include "mail.h"

/* Returns an entry for the database DATABASE_NAME; PROGRAM_NAME is the name
   of the program that's building the database entry. */
extern DatabaseInfo findDatabase (const char *program_name, 
				  const char *database_name,
				  ErrorDesc *err);

/* Return a writable pointer to the field info for this database. */
DatabaseFieldInfo *getDatabaseFieldInfoWrite (DatabaseInfo database);

DatabaseFieldInfo getDatabaseFieldInfo (DatabaseInfo database);

/* Initialize the list of databases. */
extern int initDatabaseList (ErrorDesc *err);

/* Returns the name of the database. */
extern const char *databaseName (const DatabaseInfo database);

/* Returns the human-readable description of the database. */
extern const char *databaseDesc (const DatabaseInfo database);

/* Returns the directory where the database is located. */
extern const char *databaseDir (const DatabaseInfo database);

/* Returns the list of databases. */
extern AdmEntry *getDatabaseList (ErrorDesc *err);

/* Returns a non-zero value if the database is valid. */
extern int databaseValid (const DatabaseInfo database);

/* Returns the host list for the database.  */
extern AdmEntry *getHostList (const DatabaseInfo database);

/* Returns the global host access control list.  */
extern AdmEntry *getGlobalHostList (void);

/* Returns the path to the GNATS administrative file with the base
   name of FILENAME.  The caller takes ownership of the malloc()ed version
   of the path, and is responsible for freeing it.  */
extern char *gnats_adm_dir (DatabaseInfo database, const char *filename);

/* Set the mail agent to NEWAGENT. */
extern void setMailAgent (DatabaseInfo database, const char *newAgent);

/* Enables or disables debug mode. */
extern void setDebugMode (DatabaseInfo database, int mode);

/* Sets the "keep received headers flag" to VALUE. */
extern void setKeepReceivedHeaders (DatabaseInfo database, int value);

extern void setNotifyExpire (DatabaseInfo database, int value);
extern void setBinDir (DatabaseInfo database, const char *directory);
extern void setSubmitterAck (DatabaseInfo database, int value);
extern void setBusinessDay (DatabaseInfo database, 
			    unsigned int startHour,
			    unsigned int endHour);
extern void setBusinessWeek (DatabaseInfo database, 
			     unsigned int firstDay,
			     unsigned int lastDay);
extern void setCreateCategoryDirs (DatabaseInfo database, 
				   int flagValue);
extern void setAuditTrailFormat (DatabaseInfo database, 
				 QueryFormat *format);
extern void addGlobalChangeActions (DatabaseInfo database, 
				    ChangeActions actions);
extern void setCategoryDirPerms (DatabaseInfo database, 
				 const char *value);
extern void setInputTemplate (DatabaseInfo database,
			      InputTemplate *template);
extern void setQueryFormatList (DatabaseInfo database,
				QueryFormat *format);
extern void setBuiltinDBField (DatabaseInfo database, int whichField,
			       FieldIndex fieldIndex);
extern void setMailFormatList (DatabaseInfo database,
			       MailMessageFormat mailFormatList);
extern void setRequiredInputFields (DatabaseInfo database, FieldList list);
extern void setDatastorePrivate(DatabaseInfo database, void *data);

extern int keepReceivedHeaders (const DatabaseInfo database);
extern int debugMode (DatabaseInfo database);
extern char *mailAgent (const DatabaseInfo database);
extern const char *gnatsAdminMailAddr (const DatabaseInfo database);
extern char *defaultSubmitter (const DatabaseInfo database);
extern char *defaultCategory (const DatabaseInfo database);
extern char *defaultState (const DatabaseInfo database);
extern int notifyExpire (const DatabaseInfo database);
extern const char *binDir (const DatabaseInfo database);
extern int submitterAck (const DatabaseInfo database);
extern unsigned int businessDayStart (const DatabaseInfo database);
extern unsigned int businessDayEnd (const DatabaseInfo database);
extern unsigned int businessWeekStart (const DatabaseInfo database);
extern unsigned int businessWeekEnd (const DatabaseInfo database);
extern ChangeActions globalChangeActions (const DatabaseInfo database);
extern int createCategoryDirs (const DatabaseInfo database);
QueryFormat *getAuditTrailFormat (const DatabaseInfo database);
extern int categoryDirPerms (const DatabaseInfo database);
extern FieldList getRequiredInputFields (const DatabaseInfo database);
extern InputTemplate *getInputTemplate (const DatabaseInfo database);
extern QueryFormat *getQueryFormatList (const DatabaseInfo database);
extern MailMessageFormat getMailFormatList (const DatabaseInfo database);
extern FieldIndex getBuiltinField (const DatabaseInfo database,
				   int whichField);
extern void *getDatastorePrivate(const DatabaseInfo database);

extern void freeDatabaseInfo (DatabaseInfo database);

extern DatabaseInfo findOrLoadDatabase (const char *programName,
					const char *databaseName,
					ErrorDesc *err);
extern void clearDbList (void);

extern int databaseSpecIsNetConn (const char *dbName);
extern const char *databaseSpecServer (const char *dbName);
extern const char *databaseSpecPort (const char *dbName);


#endif
