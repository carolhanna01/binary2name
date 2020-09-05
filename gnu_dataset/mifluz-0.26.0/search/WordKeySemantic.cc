//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordKeySemantic.cc,v 1.8 2008/06/02 19:24:41 sebdiaz Exp $
//
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <clib.h>
#include <WordKey.h>

#include <WordKeySemantic.h>

WordKeySemantic::WordKeySemantic(WordContext *ncontext)
{
  context = ncontext;
  int nfields = context->GetKeyInfo().nfields;
  document = new int[nfields];
  document_length = 0;
  location = -1;
  verbose = 0;
}

WordKeySemantic::~WordKeySemantic()
{
  if(document) delete [] document;
}

int WordKeySemantic::Initialize(int* document_arg, int document_length_arg, int location_arg, int uniq_arg)
{
  memcpy((char*)document, (char*)document_arg, document_length_arg * sizeof(int));
  document_length = document_length_arg;
  location = location_arg;
  uniq = uniq_arg;

  for(int i = 0; i < document_length; i++) {
    if(i + 1 < document_length) {
      if(document[i] >= document[i+1]) {
	fprintf(stderr, "WordKeySemantic::Initialize: the document list must be a list of strictly increasing numbers (%d >= %d)\n", document[i], document[i+1]);
	return NOTOK;
      }
    }
  }
  return OK;
}

void WordKeySemantic::RealmSet(const WordKey& from, WordKey& to)
{
  to.Clear();
  RealmCopy(from, to);
}

void WordKeySemantic::RealmCopy(const WordKey& from, WordKey& to)
{
  for(int i = WORD_KEY_WORD + 1; i < document[i]; i++)
    to.Set(i, from.Get(i));
}

void WordKeySemantic::RealmUndefined(WordKey& key)
{
  for(int i = WORD_KEY_WORD + 1; i < document[i]; i++)
    key.Undefined(i);
}

void WordKeySemantic::RealmClear(WordKey& key)
{
  key.Clear();
  for(int i = WORD_KEY_WORD + 1; i < document[i]; i++)
    key.Set(i, 0);
}

void WordKeySemantic::DocumentSet(const WordKey& from, WordKey& to)
{
  to.Clear();
  DocumentCopy(from, to);
}

void WordKeySemantic::DocumentCopy(const WordKey& from, WordKey& to)
{
  for(int i = 0; i < document_length; i++)
    to.Set(document[i], from.Get(document[i]));
}

int WordKeySemantic::DocumentCompare(const WordKey& a, const WordKey& b)
{
  int ret = 1;
  for(int i = 0; i < document_length; i++) {
    int idx = document[i];
    if((a.IsDefined(idx) && b.IsDefined(idx)) &&
       (ret = a.Get(idx) - b.Get(idx)) != 0) return ret;
  }
  return ret;
}

int WordKeySemantic::DocumentClear(WordKey& key)
{
  for(int i = 0; i < document_length; i++)
    key.Set(document[i], 0);
  return 0;
}

int WordKeySemantic::DocumentUndefined(WordKey& key)
{
  for(int i = 0; i < document_length; i++)
    key.Undefined(document[i]);
  return 0;
}

void WordKeySemantic::DocumentNext(WordKey& key, int use_uniq)
{
  if(use_uniq)
    key.SetToFollowing(uniq);
  else
    key.SetToFollowing(document[document_length-1]);
}


void WordKeySemantic::LocationSet(const WordKey& from, WordKey& to)
{
  to = from;
  to.Undefined(WORD_KEY_WORD);

  for(int i = location + 1; i < to.NFields(); i++) {
    to.Undefined(i);    
  }
}

int WordKeySemantic::LocationCompare(const WordKey& expected, const WordKey& actual, int proximity )
{
  int ret = 1;

  for(int i = 1; i < document[0]; i++) {
    if((expected.IsDefined(i) && actual.IsDefined(i)) &&
       (ret = expected.Get(i) - actual.Get(i)) != 0) return ret;
  }

  if((ret = DocumentCompare(expected, actual)) != 0) return ret;
  //
  // Only compare location if defined.
  //
  if((expected.IsDefined(location) && actual.IsDefined(location)) &&
     (ret = expected.Get(location) - actual.Get(location))) {
    if(proximity < 0) {
      //
      // -N means ok if in range [-N +N]
      //
      proximity *= 2;
      if(ret < 0 && ret >= proximity)
	ret = 0;
    } else {
      //
      // N means ok if in range [0 +N]
      //
      if(ret < 0 && ret >= -proximity)
	ret = 0;
    }
  }
  return ret;
}

void WordKeySemantic::LocationNext(WordKey& key)
{
  key.SetToFollowing(location);
}

void WordKeySemantic::LocationNearLowest(WordKey& key, int proximity)
{
  if(proximity < 0) {
    if(key.Underflow(location, proximity))
      key.Get(location) = 0;
    else
      key.Get(location) += proximity;
  }
}

void WordKeySemantic::Location2Document(WordKey& key)
{
  key.Undefined(location);
}

