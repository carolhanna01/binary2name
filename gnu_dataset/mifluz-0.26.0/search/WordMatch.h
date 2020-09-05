//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordMatch.h,v 1.3 2001/06/29 14:14:08 loic Exp $
//

#ifndef _WordMatch_h
#define _WordMatch_h

#include <WordKey.h>
#include <WordContext.h>

//
// Return value of the Search method, tells us which document
// matched and why.
//
class WordMatch {
 public:
  WordMatch(WordContext* context) : match(context) { }

  //-
  // Return a textual representation of the object.
  //
  String Get() const;

  //-
  // The document that matched
  //
  WordKey match;
  //-
  // An ascii description of why it matched.
  //
  String info;
  //-
  // 1 if match is valid, 0 if match is not valid
  int valid;
};

class WordMatches {
 public:
  WordMatches(WordContext* ncontext) {
    size = 0;
    length = 0;
    matches = 0;
    context = ncontext;
  }

  ~WordMatches() {
    if(matches) {
      for(unsigned int i = 0; i < size; i++)
	delete matches[i];
      delete matches;
    }
  }

  int Allocate(unsigned int nsize) {
    size = nsize;
    matches = new WordMatch* [size];
    memset((char*)matches, '\0', sizeof(WordMatch*) * size);
    length = 0;
    
    for(unsigned int i = 0; i < size; i++) {
      matches[i] = new WordMatch(context);
      matches[i]->valid = 0;
    }

    return OK;
  }

  WordContext* context;
  WordMatch** matches;
  //
  // Effective allocated size of the array
  //
  unsigned int size;
  //
  // Number of valid elements in the matches array.
  //
  unsigned int length;
  
};

#endif /* _WordMatch_h */
