//
// WordReference.cc
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordReference.cc,v 1.10 2001/06/29 14:14:08 loic Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "WordReference.h"

int WordReference::Merge(const WordReference& other)
{
  int ret = key.Merge(other.Key());
  record = other.record;

  return ret;
}

//
// Set the structure from an ascii representation
//
int
WordReference::Set(const String& buffer)
{
  StringList fields(buffer, "\t ");
  return SetList(fields);
}

//
// Set the structure from list of fields
//
int
WordReference::SetList(StringList& fields)
{
  Clear();
  if(key.SetList(fields) != OK ||
     record.SetList(fields) != OK)
    return NOTOK;
  else
    return OK;
}      

//
// Convert the whole structure to an ascii string description
//
int
WordReference::Get(String& buffer) const
{
  String tmp;
  buffer.trunc();

  if(key.Get(tmp) != OK) return NOTOK;
  //
  // If a readable word is available, print it instead of the numerical
  // serial number.
  //
  if(word.empty()) {
    buffer.append(tmp);
  } else {
    buffer.append(word);
    buffer.append(tmp.sub(tmp.indexOf('\t')));
  }

  if(record.Get(tmp) != OK) return NOTOK;
  buffer.append(tmp);

  return OK;
}

String
WordReference::Get() const
{
  String tmp;
  Get(tmp);
  return tmp;
}

int WordReference::Write(FILE* f) const
{
  String tmp;
  Get(tmp);
  fprintf(f, "%s", (char*)tmp);
  return 0;
}

void WordReference::Print() const
{
  Write(stderr);
}
