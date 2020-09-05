//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordExcludeMask.h,v 1.4 2001/06/29 14:14:08 loic Exp $
//
// NAME
//
// WordExclude specialization that ignore some bits
//
// SYNOPSIS
//
// #include <WordExcludeMask.h>
//
// #define BITS              9
// #define IGNORE        0x0f0
// #define IGNORE_MASK   0x050
//
// WordExcludeMask permute;
// permute.Initialize(BITS, IGNORE, IGNORE_MASK);
// while(permute.Next() == WORD_EXCLUDE_OK)
//    ...
//
// DESCRIPTION
//
// Only perform WordExclude operations on the bits that are not set in
// <i>ignore.</i> The bits of <i>ignore_mask</i> that are set in
// <i>ignore</i> are untouched. In the synopsis section, for instance,
// bits 1,2,3,4 and 9 will be permuted and the bits 5,6,7,8 will be
// left untouched.
// 
//
// END
//

#ifndef _WordExcludeMask_h
#define _WordExcludeMask_h

#include <stdlib.h>

#include <WordExclude.h>

#define WORD_EXCLUDE_IGNORED	(-1)

class WordExcludeMask : public WordExclude {
public:
  //-
  // <b>ignore</b> gives the mask of bits to ignore. The actual WordExclude
  // operations are made on a number of bits that is <b>length</b> - (the number
  // of bits set in <b>ignore).</b>
  // The <b>ignore_mask_arg</b> contains the actual values of the bits ignored by 
  // the <b>ignore</b> argument.
  //
  virtual inline int Initialize(unsigned int length, unsigned int ignore, unsigned int ignore_mask_arg, int) {
    ignore_mask = ignore_mask_arg;
    ignore_maxi = length;
    unsigned int maxi = 0;
    unsigned int i;
    for(i = 0, ignore_bits = 0; i < length; i++) {
      if(ignore & (1 << i)) {
	bit2bit[i] = WORD_EXCLUDE_IGNORED;
	if(ignore_mask & (1 << i)) ignore_bits++;
      } else {
	bit2bit[i] = maxi++;
      }
    }

    return WordExclude::Initialize(maxi, 0, 0, 0);
  }

  virtual inline unsigned int Excluded(int position) const {
    position = WORD_EXCLUDE_POSITION2BIT(ignore_maxi, position);
    if(bit2bit[position] == WORD_EXCLUDE_IGNORED)
      return ignore_mask & (1 << position);
    else
      return WordExclude::Mask() & (1 << bit2bit[position]);
  }

  //-
  // Return true if bit at <b>position</b> is ignored by permutations,
  // i.e. has a fixed value.
  //
  inline int Ignored(int position) const {
    position = WORD_EXCLUDE_POSITION2BIT(ignore_maxi, position);
    return bit2bit[position] == WORD_EXCLUDE_IGNORED;
  }

  virtual inline int NotExcludedCount() const {
    return ignore_maxi - ignore_bits - WordExclude::Bits();
  }

  virtual inline int ExcludedCount() const {
    return ignore_bits - WordExclude::Bits();
  }

  //-
  // The semantic is the same as the Get method of Wordexclude
  // except that ignored bits are assigned 3 and 2 instead of 1 and 0
  // respectively.
  //
  virtual void Get(String& buffer) const;
  //-
  // The semantic is the same as the Get method of Wordexclude
  // except that ignored bits are assigned 3 and 2 instead of 1 and 0
  // respectively.
  //
  virtual int Set(const String& buffer);

  virtual inline unsigned int& Mask() { static unsigned int dummy; fprintf(stderr, "WordExcludeMask::Mask\n"); abort(); return dummy; }
  virtual inline unsigned int Mask() const {
    unsigned int ret = ignore_mask;
    unsigned int i;
    for(i = 0; i < ignore_maxi; i++) {
      if(bit2bit[i] != WORD_EXCLUDE_IGNORED) {
	if(WordExclude::Mask() & (1 << bit2bit[i]))
	  ret |= (1 << i);
      }
    }
    return ret;
  }

  virtual inline unsigned int Maxi() const { return ignore_maxi; }

  virtual inline unsigned int Bits() const { return ignore_bits + WordExclude::Bits(); }

private:
  unsigned int ignore_mask;
  unsigned int ignore_maxi;
  unsigned int ignore_bits;
  int bit2bit[WORD_EXCLUDE_MAX];
};

#endif /* _WordExcludeMask_h */
