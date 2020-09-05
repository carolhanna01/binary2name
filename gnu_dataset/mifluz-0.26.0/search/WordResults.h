//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordResults.h,v 1.4 2001/08/31 14:06:04 loic Exp $
//

#ifndef _WordResults_h_
#define _WordResults_h_

#include <stdio.h>
#include <stdlib.h>

#include "db.h"
#include <htString.h>
#include <WordKey.h>
#include <WordKeySemantic.h>
#include <WordMatch.h>

class WordContext;

class WordResults 
{
 public:
  WordResults(WordContext* ncontext) :
    key_semantic(ncontext) {
    context = ncontext;
    variables = 0;
    variables_cursor = 0;
    sorted = 0;
    sorted_cursor = 0;
    ranked = 0;
    ranked_cursor = 0;
    uniq = 0;
    uniq_cursor = 0;
    dbenv = 0;
    document_length = 0;
    document_offset = 0;
    uniq_offset = 0;
    name = 0;
    verbose = 0;
   }
   ~WordResults() {
     Close();
  }

  void Verbose(int nverbose) { verbose = nverbose; }

  int Open(char* name);
  int Close();
    
  int KeySemantic(const WordKeySemantic& nkey_semantic);

  int Filled() const;
  int Exists(const WordKey& key) const;
  int UniqExists(const WordKey& key) const;
  int Put(const WordMatch& match, unsigned int position);
  int Get(WordMatches* matches, unsigned int length, unsigned int position) const;

  int PutContext(const String& context_out);
  int GetContext(String& context_in) const;

  int PutMatchesTotal(unsigned int matches_total);
  int GetMatchesTotal(unsigned int& matches_total) const;

  int Count(unsigned int& count) const;
  
 private:
  WordContext*			context;
  WordKeySemantic		key_semantic;
  DB*		            	variables;
  DBC*		            	variables_cursor;
  DB*		            	sorted;
  DBC*		            	sorted_cursor;
  DB*		            	ranked;
  DBC*		            	ranked_cursor;
  DB*		            	uniq;
  DBC*		            	uniq_cursor;
  DB*		            	info;
  DBC*		            	info_cursor;
  DB_ENV*			dbenv;
  int				document_length;
  int				document_offset;
  int				uniq_offset;
  char*				name;
  int				verbose;
};
#endif /* _WordResults_h_ */
