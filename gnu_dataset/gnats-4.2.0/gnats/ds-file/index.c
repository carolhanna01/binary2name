/* Miscellaneous index routines for use with GNATS. 
   Copyright (C) 1993,95,99,2000,01,07 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com) and
   by Tim Wicinski (wicinski@barn.com).
   
   Heavily revised by Bob Manson (manson@juniper.net); also added
   binary index support.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/


/* Binary format (all values are in MSB-first order):
 * 1 byte: number of fields in each index entry
 * For each record:
 *     2 bytes: total length of record, including this 2 byte length
 *     For each field:
 *         2 bytes: length of field, not including this 2 byte length or NUL
 *		    terminator
 *	   N bytes: data in field, NUL-terminated
 */
	

#include "ds.h"
#include "gnatsd.h"
#include "ds-file.h"

struct index_desc
{
  DatabaseInfo database;
  char *path;
  FieldList fields;
  char *separator;
  int isBinary;
  int numberOfFields;
  int *fieldInIndex;
  time_t mtime;
  int indexRead;
  PR *prChain;
};

typedef struct index_file_desc
{
  IndexDesc desc;
  FILE *fileDes;
} *IndexFileDesc;

struct indexEntry
{
  char *buf;
  char **fields;
  PR *nextPR;
  PR *prevPR;
};

static PR *nextIndexEntryBinary (IndexFileDesc fp);

static int
format_field (FieldIndex field, const char *value, const char *separator,
	      char **dest, size_t *length)
{
  char datebuf[30];
  size_t valLen;
  size_t destLen;
  size_t sepLen;

  if (value == NULL)
    {
      value = "";
    }

  if (fieldDefForIndex (field)->nospaces)
    {
      char *r = strchr (value, ' ');
      if (r != NULL)
	{
	  valLen = r - value;
	}
    }

  if (fieldDefForIndex (field)->datatype == Date)
    {
      time_t d = 0;
      if (value != NULL && *value != '\0')
	{
	  /* We used to fail here if the date was invalid, but that's
	     a mistake.  */
	  d = get_any_date (value);
	}
      sprintf (datebuf, "%d", (int) d);
      value = datebuf;
    }

  if (value == NULL)
    {
      value = "";
    }
  destLen = *length;
  valLen = strlen (value);
  sepLen = strlen (separator);

  *dest = xrealloc (*dest, destLen + valLen + sepLen + 1);
  memcpy (*dest + destLen, separator, sepLen);
  memcpy (*dest + destLen + sepLen, value, valLen);
  (*dest)[destLen + sepLen + valLen] = '\0';
  *length = destLen + sepLen + valLen;
  return sepLen + valLen;
}

char *
createIndexEntry (PR *pr, size_t *reclen)
{
  FieldList f;
  char *res;
  IndexDesc indexDesc = getIndexDesc (pr->database);

  if (indexIsBinary (pr->database))
    {
      return createIndexEntryBinary (pr, reclen);
    }

  /* XXX ??? !!! For now.  We need a way to refer to "PRID".  */
  asprintf (&res, "%s/%s", field_value (pr, CATEGORY (pr->database)),
	    field_value (pr, NUMBER (pr->database)));
  *reclen = strlen (res);

  for (f = indexDesc->fields; f != NULL; f = f->next)
    {
      FieldIndex i = simpleFieldIndexValue (f->ent);
      if (format_field (i, field_value (pr, i), indexDesc->separator, &res,
			reclen) < 0)
	{
	  free (res);
	  res = NULL;
	  break;
	}
    }
  append_string (&res, "\n");
  (*reclen)++;
  return res;
}

static char *
find_sep (IndexDesc indexDesc, char *start)
{
  char *end;
  int seplen = strlen (indexDesc->separator);

  do {
    end = strchr (start, indexDesc->separator[0]);
    if (end != NULL)
      {
	if (seplen == 1 || strncmp (start, indexDesc->separator, seplen) == 0)
	  {
	    return end;
	  }
	end++;
      }
  } while (end != NULL);

  return end;
}

static char *
readBinaryRecord (IndexFileDesc fp, size_t *length)
{
  char *res;
  unsigned char entlen[2];

  if (fread (entlen, 1, 2, fp->fileDes) != 2)
    {
      return NULL;
    }
  *length = entlen[0] * 256 + entlen[1] - 2;
  res = xmalloc (*length);
  if (fread (res, 1, *length, fp->fileDes) == *length)
    {
      return res;
    }
  else
    {
      free (res);
      return NULL;
    }
}

static char *
extractBinaryField (char **record, size_t *lengthPtr)
{
  size_t length = *lengthPtr;
  if (length > 0)
    {
      char *recLen = *record;
      size_t fieldLen = (recLen[0] & 255) * 256 + (recLen[1] & 255);
      if ((fieldLen + 3) <= length)
	{
	  char *recptr = *record + 2;
	  *lengthPtr = length - (fieldLen + 3);
	  (*record) = recLen + fieldLen + 3;
	  return recptr;
	}
    }
  return NULL;
}

/* Return the next index entry from FP.  */
static PR *
nextIndexEntry (IndexFileDesc fp)
{
  char *start, *end;
  char *buf;
  PR *res;
  int x;  
  int seplen;
  FieldList f;
  FilePRInfo pi;
  Index *p;

  if (fp->desc->isBinary)
    {
      return nextIndexEntryBinary (fp);
    }

  res = allocPR (fp->desc->database);
  pi = (FilePRInfo) res->ds_private;
  p = pi->index;
  seplen = strlen (fp->desc->separator);

  p->prevPR = NULL;
  p->nextPR = NULL;
  p->fields = (char **) xmalloc (sizeof (char *) 
				 * get_num_fields (fp->desc->database));
  memset (p->fields, 0, sizeof (char *) * get_num_fields (fp->desc->database));
  while ((buf = read_line (fp->fileDes, NULL)) != NULL)
    {
      if (buf[0] != '#')
	{
	  break;
	}
      else
	{
	  free (buf);
	}
    }

  if (buf == NULL)
    {
      goto no_entry;
    }

  p->buf = buf;
  x = strlen (buf) - 1;
  if (buf[x] == '\n')
    {
      buf[x] = '\0';
    }

  start = buf;
  end = strchr (start, '/');
  if (end == NULL)
    {
      goto no_entry;
    }
  *end = '\0';
  p->fields[fieldDefForIndex (CATEGORY (fp->desc->database))->number] = start;

  start = end + 1;
  end = find_sep (fp->desc, start);

  if (end == NULL)
    {
      goto no_entry;
    }
  *end = '\0';
  p->fields[fieldDefForIndex (NUMBER (fp->desc->database))->number] = start;

  for (f = fp->desc->fields; f != NULL && end != NULL; f = f->next)
    {
      FieldIndex i = simpleFieldIndexValue (f->ent);

      if (i != InvalidFieldIndex)
	{
	  start = end + seplen;
	  p->fields[fieldNumber (i)] = start;
	}
      if (f->next != NULL)
	{
	  end = find_sep (fp->desc, start);
	  if (end != NULL)
	    {
	      *end = '\0';
	    }
	}
    }

  if (f == NULL)
    {
      return res;
    }

no_entry:

  free_pr (res);
  return NULL;
}

static PR *
nextIndexEntryBinary (IndexFileDesc fp)
{
  char *record;
  PR *res = NULL;
  size_t length;
  FilePRInfo pi;

  record = readBinaryRecord (fp, &length);
  if (record != NULL)
    {
      FieldList f;
      Index *indexEnt;

      res = allocPR (fp->desc->database);
      pi = (FilePRInfo) res->ds_private;
      indexEnt = pi->index;
      indexEnt->prevPR = NULL;
      indexEnt->nextPR = NULL;
      indexEnt->fields 
	= (char **) xmalloc (sizeof (char *) 
			     * get_num_fields (fp->desc->database));
      indexEnt->buf = record;
      memset (indexEnt->fields, 0, sizeof (char *) 
	      * get_num_fields (fp->desc->database));
      indexEnt->fields[fieldDefForIndex (NUMBER (fp->desc->database))->number] 
	= extractBinaryField (&record, &length);
      indexEnt->fields[fieldDefForIndex (CATEGORY (fp->desc->database))->number] 
	= extractBinaryField (&record, &length);
      for (f = fp->desc->fields; res != NULL && f != NULL; f = f->next)
	{
	  FieldIndex i = simpleFieldIndexValue (f->ent);

	  if (i != InvalidFieldIndex)
	    {
	      indexEnt->fields[fieldNumber (i)]
		= extractBinaryField (&record, &length);
	      if (indexEnt->fields[fieldNumber (i)] == NULL)
		{
		  free_pr (res);
		  res = NULL;
		}
	    }
	  else
	    {
	      free_pr (res);
	      res = NULL;
	    }
	}
    }
  return res;
}

static void
appendBinaryFieldContents (char **dest,
			   const char *value, size_t *recLenPtr)
{
  size_t valLen;
  size_t recLen = *recLenPtr;

  if (value == NULL)
    {
      value = "";
      valLen = 0;
    }
  else
    {
      valLen = strlen (value);
    }
  *dest = xrealloc (*dest, recLen + valLen + 3);
  memcpy (*dest + recLen + 2, value, valLen + 1);
  (*dest)[recLen + 0] = valLen / 256;
  (*dest)[recLen + 1] = valLen % 256;
  *recLenPtr += valLen + 3;
}

char *
createIndexEntryBinary (PR *pr, size_t *recLen)
{
  const DatabaseInfo database = pr->database;
  const IndexDesc indexDesc = getIndexDesc (database);
  FieldList f;
  char *res;

  *recLen = 2;
  res = xmalloc (2);
  appendBinaryFieldContents (&res, field_value (pr, NUMBER (pr->database)),
			     recLen);
  appendBinaryFieldContents (&res, field_value (pr, CATEGORY (pr->database)),
			     recLen);
  for (f = indexDesc->fields; f != NULL; f = f->next)
    {
      FieldIndex i = simpleFieldIndexValue (f->ent);
      size_t fieldLenOffset = *recLen;
      int valLen;

      /* Two bytes for the field length. */
      *recLen += 2;
      valLen = format_field (i, field_value (pr, i), "", &res, recLen);
      res[fieldLenOffset + 0] = valLen / 256;
      res[fieldLenOffset + 1] = valLen & 255;
      /* One for the NUL character; format_field() always appends one,
	 but doesn't include it in the length.  */
      (*recLen)++;
    }
  res[0] = *recLen / 256;
  res[1] = *recLen % 256;
  return res;
}

static time_t
statIndex (char *name)
{
  struct stat buf;
  int i;

  i = stat (name, &buf);
  if (i < 0)
    {
      return (time_t)-1;
    }

  return buf.st_mtime;
}

static IndexFileDesc
openIndex (IndexDesc indexDesc, ErrorDesc *err)
{
  FILE *fp;
  time_t t;
  char *index_filename = gnats_adm_dir (indexDesc->database, indexDesc->path);
  IndexFileDesc res 
    = (IndexFileDesc) xmalloc (sizeof (struct index_file_desc));

  res->desc = indexDesc;

  fp = fopen (index_filename, "r");

  if (fp != NULL)
    {
      t = statIndex (index_filename);
      /* ??? XXX !!! Do we want to do something here? */
      if (t != (time_t)-1)
	{
	  indexDesc->mtime = t;
	}
      res->fileDes = fp;
      if (indexDesc->isBinary)
	{
	  unsigned char fieldCount;
	  if ((fread (&fieldCount, 1, 1, res->fileDes) != 1) 
	      || (fieldCount != indexDesc->numberOfFields))
	    {
	      setError (err, CODE_INVALID_INDEX, 
			"Index file %s contains incorrect number of fields",
			index_filename);
	      fclose (res->fileDes);
	      free (res);
	      res = NULL;
	    }
	}
    }
  else
    {
      setError (err, CODE_FILE_ERROR, "Unable to open index file %s",
		index_filename);
      free (res);
      res = NULL;
    }

  free (index_filename);
  return res;
}

static void
closeIndex (IndexFileDesc desc)
{
  fclose (desc->fileDes);
  free (desc);
}

/* getIndex - reads in the entire set of PRs with index entries. */
static PR *
getIndex (IndexDesc indexDesc, ErrorDesc *err)
{
  IndexFileDesc fp = openIndex (indexDesc, err);
  PR *new_chain = NULL;
  PR *end_chain = NULL;

  if (fp != NULL)
    {
      PR *i;

      while ((i = nextIndexEntry (fp)))
	{
	  if (new_chain == NULL)
	    {
	      new_chain = i;
	      end_chain = i;
	    }
	  else
	    {
	      FilePRInfo i_pi = (FilePRInfo) i->ds_private;
	      FilePRInfo e_pi = (FilePRInfo) end_chain->ds_private;
	      i_pi->index->prevPR = end_chain;
	      e_pi->index->nextPR = i;
	      end_chain = i;
	    }
	}

      closeIndex (fp);
    }
  indexDesc->indexRead = 1;
  return new_chain;
}

static PR *
getNewIndexIfChanged (DatabaseInfo database, ErrorDesc *err)
{
  int rereadIndex;
  IndexDesc indexDesc = getIndexDesc (database);

  if (indexDesc->indexRead)
    {
      time_t t;
      char *index_filename = gnats_adm_dir (database, indexDesc->path);

      t = statIndex (index_filename);
      free (index_filename);

      if (t == (time_t)-1)
	{
	  setError (err, CODE_FILE_ERROR,
		   "GNATS cannot stat the index: %s.", strerror (errno));
	  return 0;
	}
      if (t > indexDesc->mtime)
	{
	  rereadIndex = 1;
	}
      else
	{
	  rereadIndex = 0;
	}
    }
  else
    {
      rereadIndex = 1;
    }

  if (rereadIndex)
    {
      return getIndex (indexDesc, err);
    }
  else
    {
      return NULL;
    }
}

void
freePRIndex (DatabaseInfo database, FilePRInfo pi)
{
  if (pi->index != NULL && pi->index->fields != NULL)
    {
      if (pi->index->buf != NULL)
	{
	  free (pi->index->buf);
	  pi->index->buf = NULL;
	}
      else
	{
	  int x;
	  
	  for (x = get_num_fields (database) - 1; x >= 0; x--)
	    {
	      if (pi->index->fields[x] != NULL)
		{
		  free (pi->index->fields[x]);
		  pi->index->fields[x] = NULL;
		}
	    }
	}
      free (pi->index->fields);
      pi->index->fields = NULL;
    }
}

void
allocIndex (FilePRInfo pi)
{
  pi->index = (Index *) xmalloc (sizeof (Index));
  pi->index->buf = NULL;
  pi->index->fields = NULL;
  pi->index->prevPR = NULL;
  pi->index->nextPR = NULL;
}
 
void
buildIndexEntry (PR *pr)
{
  int x;
  int num_fields = get_num_fields (pr->database);
  Index *newIndex;
  FilePRInfo pi;

  if (pr->ds_private == NULL)
    {
      pr_init (pr);
      pi = (FilePRInfo) pr->ds_private;
    }
  else
    {
      pi = (FilePRInfo) pr->ds_private;
      if (pi->index != NULL)
	{
	  freePRIndex (pr->database, pi);
	}
    }
  newIndex = pi->index;

  newIndex->fields = (char **) xmalloc (sizeof (char *) * num_fields);
  memset (newIndex->fields, 0, sizeof (char *) * num_fields);
  newIndex->buf = NULL;
  for (x = 0; x < num_fields; x++)
    {
      FieldIndex field = getNthField (pr->database, x);
      if (isIndexedFieldIndex (field))
	{
	  const char *fc = field_value (pr, field);

	  if (fieldDefForIndex (field)->datatype == Date)
	    {
	      if (fc == NULL 
		  || fc[0] == '\0' 
		  || (strcmp (fc, "-1") == 0))
		{
		  newIndex->fields[x] = xstrdup ("0");
		}
	      else
		{
		  time_t t = get_any_date (fc);
		  asprintf (&(newIndex->fields[x]), "%d", (int) t);
		}
	    }
	  else
	    {
	      char *fcCopy;

	      if (fc == NULL)
		{
		  fcCopy = xstrdup ("");
		}
	      else
		{
		  fcCopy = xstrdup (fc);
		}

	      /* ??? XXX Should we do this here?  It's already being
	         done in the "write out the index" code.  */
	      if (fieldDefForIndex (field)->nospaces)
		{
		  /* Try to minimize the leakage.  */
		  char *p = strchr (fcCopy, ' ');
		  if (p != NULL)
		    {
		      *p = '\0';
		    }
		}
	      newIndex->fields[x] = fcCopy;
	    }
	}
    }
}

int
isIndexedFieldIndex (FieldIndex index)
{
  IndexDesc indexDesc;
  if (index == InvalidFieldIndex)
    {
      return 0;
    }
  else
    {
      indexDesc = getIndexDesc (fieldDefForIndex (index)->database);
      return indexDesc->fieldInIndex[fieldDefForIndex (index)->number];
    }
}

void
finishIndexDesc (DatabaseInfo database, IndexDesc new)
{
  int numFields = get_num_fields (database);

  new->fieldInIndex = (int *) xmalloc (sizeof (int) * numFields);

  {
    int x;

    for (x = 0; x < numFields; x++)
      {
	new->fieldInIndex[x] = 0;
      }
  }

  {
    FieldList f;

    for (f = new->fields; f != NULL; f = f->next)
      {
	FieldIndex i = simpleFieldIndexValue (f->ent);
	if (i != InvalidFieldIndex)
	  {
	    new->fieldInIndex[fieldNumber (i)] = 1;
	  }
      }
  }
  new->fieldInIndex[fieldNumber (NUMBER (database))] = 1;
  new->fieldInIndex[fieldNumber (CATEGORY (database))] = 1;
}

int
isIndexedField (ComplexFieldIndex ent)
{
  if (parseComplexFieldIndex (ent) != 0)
    {
      return 0;
    }
  return isIndexedFieldIndex (simpleFieldIndexValue (ent));
}

/* Write out the index for DATABASE's PR chain.  Returns 0 on success,
   a non-zero value otherwise (and ERR will be filled in). */
int
writeIndex (const DatabaseInfo database, ErrorDesc *err)
{
  FILE *fp;
  char *path, *workfile;
  PR *pr;
  IndexDesc indexDesc = getIndexDesc (database);
  struct stat buf;

  workfile = gnats_adm_dir (database, "indXXXXXX");
  fp = fopen_temporary_file (workfile, "w", 0644);
  if (fp == NULL)
    {
      setError (err, CODE_FILE_ERROR,
		"writeIndex () can't open the temporary file %s",
		workfile);
      free (workfile);
      return -1;
    }

  if (indexIsBinary (database))
    {
      unsigned char fieldCount = indexFieldCount (database);
      fwrite (&fieldCount, 1, 1, fp);
    }

  for (pr = getFirstPR (database, err); pr != NULL ; pr = getNextPR (pr))
    {
      size_t length;
      char *line = createIndexEntry (pr, &length);

      if (line != NULL)
	{
	  fwrite (line, 1, length, fp);
	  free (line);
	}
    }

  fclose (fp);

  block_signals ();

  path = gnats_adm_dir (database, indexDesc->path);

  /* sanity check -- make sure nobody has changed the index since we
     last read it */
  if (stat (path, &buf) < 0)
    {
      /* if the index ain't there, no problem.  if it's there, but
	 unstattable, we're going to declare it a serious problem.
	 not sure if this is the right thing to do... */
      if (errno != ENOENT)
	{
	  setError (err, CODE_FILE_ERROR,
		    "GNATS cannot stat the index: %s.", strerror (errno));
	  log_msg (LOG_ERR, 1, "error in writeIndex:",
		   getErrorMessage (*err));
	  return -1;
	}
    }

  if (buf.st_mtime > indexDesc->mtime)
    {
      setError (err, CODE_INVALID_INDEX,
		"Index has been modified since last read");
      log_msg (LOG_ERR, 1, "error in writeIndex:",
	       getErrorMessage (*err));
      /* the index is corrupt: alert the administrator */
      punt (database, 0, "Attempting to write index file %s,\n\
but index has been modified since last read.  A PR has been modified,\n\
but the change has not been recorded in the index.  You must manually\n\
regenerate the index.", path);
      return -1;
    }

  if ((rename (workfile, path)) < 0)
    {
      if (errno != EXDEV)
	{
	  free (path);
	  setError (err, CODE_FILE_ERROR,
		    "could not rename temporary index %s into gnats-adm: %s",
		    workfile, strerror (errno));
	  return -1;
	}

      if (copy_file (workfile, path))
	{
	  setError (err, CODE_FILE_ERROR, 
		   "could not copy temporary index %s into %s: %s",
		   workfile, path, strerror (errno));
	  return -1;
	}

      unlink (workfile);
    }

  free (path);
  free (workfile);
  unblock_signals ();
  return 0;
}

/* Add the PR to the GNATS index file.  */
int
addToIndex (PR *pr, ErrorDesc *err)
{
  FILE *fp;
  char *path;
  IndexDesc indexDesc = getIndexDesc (pr->database);

  buildIndexEntry (pr);

  block_signals ();

  path = gnats_adm_dir (pr->database, indexDesc->path);

  if (indexIsBinary (pr->database) && ! fileExists (path))
    {
      unsigned char fieldCount = indexFieldCount (pr->database);
      fp = fopen (path, "w");
      if (fp != NULL)
	{
	  fwrite (&fieldCount, 1, 1, fp);
	}
    }
  else
    {
      fp = fopen (path, "a+");
    }
  if (fp == NULL)
    {
      setError (err, CODE_FILE_ERROR, 
		"Can't append to the GNATS index file (%s).", path);
      unblock_signals ();
      return -1;
    }

  {
    size_t reclen;

    char *line = createIndexEntry (pr, &reclen);
    fwrite (line, 1, reclen, fp);
    free (line);
  }

  fclose (fp);
  free (path);
  unblock_signals ();
  return 0;
}

char *
indexValue (PR *pr, FieldIndex field)
{
  FilePRInfo pi = (FilePRInfo) pr->ds_private;
  if (pi->index != NULL && pi->index->fields != NULL)
    {
      return pi->index->fields[field->number];
    }
  else
    {
      return NULL;
    }
}

IndexDesc
getIndexDesc (const DatabaseInfo database)
{
  FileDBInfo dbi = (FileDBInfo) getDatastorePrivate(database);
  if (dbi == NULL)
    {
      return NULL;
    }
  return dbi->indexDesc;
}

void
setIndexDesc (DatabaseInfo database, IndexDesc new)
{
  if (databaseValid (database))
    {
      FileDBInfo dbi = (FileDBInfo) getDatastorePrivate(database);
      finishIndexDesc (database, new);
      dbi->indexDesc = new;
    }
}

void
freeIndexDesc (IndexDesc p)
{
  free (p->path);
  freeFieldList (p->fields);
  free (p->separator);
  if (p->fieldInIndex != NULL)
    {
      free (p->fieldInIndex);
    }
  free (p);
}

IndexDesc
newIndexDesc (const DatabaseInfo database)
{
  IndexDesc res = xmalloc (sizeof (struct index_desc));

  res->database = database;
  res->prChain = NULL;
  res->path = NULL;
  res->fields = NULL;
  res->separator = xstrdup ("|");
  res->isBinary = 0;
  res->numberOfFields = 0;
  res->fieldInIndex = NULL;
  res->mtime = 0;
  res->indexRead = 0;

  return res;
}

void
addFieldToIndex (IndexDesc desc, FieldList ent)
{
  if (desc->fields == NULL)
    {
      desc->fields = ent;
    }
  else
    {
      FieldList f = desc->fields;
      while (f->next != NULL)
	{
	  f = f->next;
	}
      f->next = ent;
    }
  desc->numberOfFields++;
}

void
setIndexDescPath (IndexDesc desc, const char *path)
{
  desc->path = xstrdup (path);
}

void
setIndexDescSeparator (IndexDesc desc, const char *separator)
{
  free (desc->separator);
  desc->separator = xstrdup (separator);
}

void
setIndexDescBinary (IndexDesc desc, int flag)
{
  desc->isBinary = flag;
}

static void
freePRChain (PR *start)
{
  while (start != NULL)
    {
      PR *next = getNextPR (start);
      free_pr (start);
      start = next;
    }
}

int
checkPRChain (const DatabaseInfo database, ErrorDesc *err)
{
  IndexDesc desc = getIndexDesc (database);
  PR *new_chain = getNewIndexIfChanged (database, err);

  if (new_chain != NULL)
    {
      freePRChain (desc->prChain);
      desc->prChain = new_chain;
      return 1;
    }
  else
    {
      return 0;
    }
}

void
initIndex (DatabaseInfo database)
{
  clearPRChain (database);
}

PR *
getFirstPR (const DatabaseInfo database, ErrorDesc *err)
{
  IndexDesc desc = getIndexDesc (database);

  /* first check the PRChain to make sure that the index isn't stale */
  checkPRChain (database, err);

  return desc->prChain;
}

void
clearPRChain (const DatabaseInfo database)
{
  IndexDesc desc = getIndexDesc (database);
  if (desc != NULL)
    {
      if (desc->prChain != NULL)
	{
	  freePRChain (desc->prChain);
	  desc->prChain = NULL;
	}
      desc->indexRead = 0;
    }
}

/* Replace a PR in the index with a copy of a new PR.
   By generating a copy of the new PR, we allow for the new PR
   to be free'd without harming the index pr chain. */
int
replaceCurrentPRInIndex (PR *curr_pr, PR *new_pr, ErrorDesc *err)
{
  PR *pr_replace, *pr_copy;
  const DatabaseInfo database = curr_pr->database;
  const char *new_pr_num = field_value (new_pr, NUMBER (database));
  const char *curr_pr_num = field_value (curr_pr, NUMBER (database));
  FilePRInfo curr_pi = (FilePRInfo) curr_pr->ds_private;

  if (strcmp (curr_pr_num, new_pr_num) != 0)
    {
      return 0;
    }

  /* create a copy of the new pr to be put into the index pr chain */
  pr_copy = allocPR (database);
  set_field (pr_copy, NUMBER (curr_pr->database), new_pr_num, err);
  set_field (pr_copy, CATEGORY (database),
	     field_value (new_pr, CATEGORY (database)), err);
  setPrevPR (pr_copy, curr_pi->index->prevPR);
  setNextPR (pr_copy, curr_pi->index->nextPR);
  pr_load (pr_copy, err);

  /* put the copy of new_pr into place in the index pr chain... */

  if (curr_pi->index->prevPR == NULL)
    {
      /* the pr to be replaced is at the head of the pr chain */
      IndexDesc indexDesc = getIndexDesc (database);
      free_pr (indexDesc->prChain);
      indexDesc->prChain = pr_copy;
      return 1;
    }

  /* save a pointer to the pr to be replaced so that it can later be free'd */
  pr_replace = getNextPR (curr_pi->index->prevPR);

  setNextPR (curr_pi->index->prevPR, pr_copy);

  if (curr_pi->index->nextPR != NULL)
    {
      /* the pr to be replaced is not at the end of the pr chain */
      setPrevPR (curr_pi->index->nextPR, pr_copy);
    }

  free_pr (pr_replace);

  return 1;
}

/* Remove PR PRNUM from the index for the current database, and rewrite the
   database file.  The database must have been locked before calling this
   function.  */
int
removePRFromIndex (const DatabaseInfo database, const char *prNum,
		   ErrorDesc *err)
{
  PR *list;
  PR *prev = NULL;
  IndexDesc indexDesc = getIndexDesc (database);
  FilePRInfo list_pi;

  if (! is_gnats_locked (database))
    {
      setError (err, CODE_GNATS_NOT_LOCKED,
		"Must lock the database before removing a PR from the index");
      return -1;
    }

  list = getFirstPR (database, err);
  while (list != NULL
	 && strcmp (field_value (list, NUMBER (database)), prNum) != 0)
    {
      prev = list;
      list = getNextPR (list);
    }

  if (list == NULL)
    {
      setError (err, CODE_NONEXISTENT_PR,
		"PR %s not in index", prNum);
      return -2;
    }

  list_pi = (FilePRInfo) list->ds_private;
  if (prev != NULL)
    {
      FilePRInfo prev_pi = (FilePRInfo) prev->ds_private;
      prev_pi->index->nextPR = list_pi->index->nextPR;
    }
  else
    {
      indexDesc->prChain = list_pi->index->nextPR;
    }

  return writeIndex (database, err);
}

PR *
getNextPR (PR *pr)
{
  FilePRInfo pi = (FilePRInfo) pr->ds_private;
  return pi->index->nextPR;
}

void
setNextPR (PR *pr, PR *next_pr)
{
  FilePRInfo pi = (FilePRInfo) pr->ds_private;
  pi->index->nextPR = next_pr;
}

PR *
getPrevPR (PR *pr)
{
  FilePRInfo pi = (FilePRInfo) pr->ds_private;
  return pi->index->prevPR;
}

void
setPrevPR (PR *pr, PR *prev_pr)
{
  FilePRInfo pi = (FilePRInfo) pr->ds_private;
  pi->index->prevPR = prev_pr;
}

int
indexIsBinary (const DatabaseInfo database)
{
  IndexDesc indexDesc = getIndexDesc (database);
  if (indexDesc != NULL)
    {
      return indexDesc->isBinary;
    }
  else
    {
      return 0;
    }
}

int
indexFieldCount (const DatabaseInfo database)
{
  IndexDesc indexDesc = getIndexDesc (database);
  if (indexDesc != NULL)
    {
      return indexDesc->numberOfFields;
    }
  else
    {
      return 0;
    }
}
