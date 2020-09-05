//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordPermute.h,v 1.3 2001/06/29 14:14:08 loic Exp $
//
//
// NAME
//
// WordExclude specialization with proximity toggle
//
// SYNOPSIS
//
// #include <WordPermute.h>
//
// #define BITS 5
//
// WordPermute permute;
// permute.Initialize(BITS);
// while(permute.Next() == WORD_EXCLUDE_OK)
//    if(permute.UseProximity()) ...
//
// DESCRIPTION
//
// Each WordExclude permutation is used twice by Next. Once with
// the proximity flag set and once with the proximity flag cleared.
// If the length of the bit field (length argument of Initialize) is
// lower or equal to 1, then the proximity flag is always false.
//
//
// END
//

#ifndef _WordPermute_h
#define _WordPermute_h

#include <WordExcludeMask.h>

// WordPermute methods return values
//
#define WORD_PERMUTE_OK		WORD_EXCLUDE_OK
#define WORD_PERMUTE_END	WORD_EXCLUDE_END

//
// Use or don't use proximity flag
//
#define WORD_PERMUTE_PROXIMITY_NO	0
#define WORD_PERMUTE_PROXIMITY_TOGGLE	1
#define WORD_PERMUTE_PROXIMITY_ONLY	2

//
// Deals with word exclusion and proximity permutations for
// the implementation of the Optional retrieval model.
//
class WordPermute : public WordExcludeMask {
public:
  //-
  // The <b>nuse_proximity</b> may be set to the following:
  //
  // WORD_PERMUTE_PROXIMITY_NO so that the object behaves as
  // WordExcludeMask and Proximity() always return false.
  //
  // WORD_PERMUTE_PROXIMITY_TOGGLE so that each permutation is issued twice: 
  // once with the proximity flag set (Proximity() method) and once with
  // the proximity flag cleared. 
  //
  // WORD_PERMUTE_PROXIMITY_ONLY so that the object behaves as
  // WordExcludeMask and Proximity() always return true.
  //
  virtual inline int Initialize(unsigned int length, unsigned int ignore, unsigned int ignore_mask_arg, int nuse_proximity) {
    if(WordExcludeMask::Initialize(length, ignore, ignore_mask_arg, 0) != OK)
      return NOTOK;

    use_proximity = nuse_proximity;
    switch(use_proximity) {
    case WORD_PERMUTE_PROXIMITY_NO:
      proximity = 0;
      break;
    case WORD_PERMUTE_PROXIMITY_TOGGLE:
      //
      // Don't bother to try proximity search if only one word
      // is involved.
      //
      proximity = (WordExcludeMask::Maxi() - WordExcludeMask::ExcludedCount()) > 1;
      break;
    case WORD_PERMUTE_PROXIMITY_ONLY:
      proximity = 1;
      break;
    default:
      fprintf(stderr, "WordPermute::Initialize: unexpected use_proximity = %d\n", use_proximity);
      return NOTOK;
    }
    return OK;
  }

  //-
  // Return true if the proximity flag is set, false if it is 
  // cleared.
  //
  inline int Proximity() { return proximity; }

  //-
  // Return WORD_PERMUTE_PROXIMITY_NO, WORD_PERMUTE_PROXIMITY_TOGGLE or
  // WORD_PERMUTE_PROXIMITY_ONLY.
  //
  inline int UseProximity() { return use_proximity; }

  //-
  // Find the next permutation. If <b>WORD_PERMUTE_PROXIMITY_TOGGLE<b> was
  // specified in Initialize each permutation is issued twice (see
  // Proximity() to differentiate them), except when the mask 
  // only contains one non exluded bit (NotExcludeCount() <= 1).
  // In both case the last permutation with all bits excluded
  // (i.e. when NotExcludedCount() <= 0) is never returned because 
  // it is useless.
  // 
  virtual int Next() {
    if(WordExcludeMask::Maxi() <= 1)
      return WORD_PERMUTE_END;

    int ret = WORD_PERMUTE_OK;
    int check_useless = 0;
    if(use_proximity == WORD_PERMUTE_PROXIMITY_TOGGLE) {
      //
      // Move to next permutation as follows: 
      // exclude mask 1 + use proximity
      // exclude mask 1 + don't use proximity
      // exclude mask 2 + use proximity 
      // exclude mask 2 + don't use proximity
      // and so on.
      // If only one word is involved never use proximity.
      //
      if(proximity) {
	proximity = 0;
      } else {
	proximity = 1;
	if((ret = WordExcludeMask::Next()) == WORD_PERMUTE_OK) {
	  //
	  // Do not toggle proximity for only one non excluded word
	  //
	  if(NotExcludedCount() <= 1)
	    proximity = 0;
	  check_useless = 1;
	} else if(ret == WORD_PERMUTE_END)
	  proximity = 0;
      }
    } else {
      ret = WordExcludeMask::Next();
      check_useless = 1;
    }

    if(check_useless && ret == WORD_PERMUTE_OK) {
      //
      // If no bits are ignored or all ignore_mask bits are set to
      // one, the last permutation has all exclude bits set, which
      // is useless. Just skip it and expect to be at the end of
      // all permutations.
      //
      if(NotExcludedCount() <= 0) {
	ret = WordExcludeMask::Next();
	if(ret != WORD_PERMUTE_END) {
	  fprintf(stderr, "WordPermute::Next: expected WORD_PERMUTE_END\n");
	  ret = NOTOK;
	}
      }
    }

    return ret;
  }

  //-
  // The semantic is the same as the Get method of Wordexclude
  // but a letter T is appended to the string if the proximity
  // flag is set, or F is appended to the string if the proximity
  // is clear.
  //
  virtual inline void Get(String& buffer) const {
    WordExcludeMask::Get(buffer);
    if(use_proximity == WORD_PERMUTE_PROXIMITY_TOGGLE)
      buffer << (proximity ? 'T' : 'F');
  }

  //-
  // The semantic is the same as the Get method of Wordexclude
  // but if the string end with a T the proximity flag is set
  // and if the string end with a F the proximity flag is cleared.
  //
  virtual inline int Set(const String& buffer) {
    if(buffer.length() < 1) {
      fprintf(stderr, "WordPermute::Set: buffer length < 1\n");
      return NOTOK;
    }
    int ret = OK;
    if(use_proximity == WORD_PERMUTE_PROXIMITY_TOGGLE) {
      if((ret = WordExcludeMask::Set(buffer.sub(0, buffer.length() - 1))) == OK)
	proximity = buffer.last() == 'T';
    } else {
      ret = WordExcludeMask::Set(buffer);
    }

    return ret;
  }

protected:
  int use_proximity;
  int proximity;
};

#endif /* _WordPermute_h */
