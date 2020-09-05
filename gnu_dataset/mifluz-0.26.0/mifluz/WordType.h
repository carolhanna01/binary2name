//
// WordType.h
//
// NAME
// defines a word in term of allowed characters, length etc.
//
// SYNOPSIS
//
// Only called thru WordContext::Initialize()
// 
// DESCRIPTION
// 
// WordType defines an indexed word and operations to validate
// a word to be indexed. All words inserted into the <i>mifluz</i> index
// are <b>Normalize</b>d before insertion. The configuration options
// give some control over the definition of a word.
//
// CONFIGURATION
// 
// wordlist_locale <locale> (default C)
//   Set the locale of the program to <b>locale</b>. See setlocale(3)
//   for more information.
//
// wordlist_allow_numbers {true|false} (default false)
//   If <b>true</b> a word may contain digits. If <b>false</b> digits
//   are not considered to be part of a word and an attempt to insert
//   a word containing digits will result in an error. 
//   See the <b>Normalize</b> method for more information.
// 
// wordlist_mimimun_word_length <number> (default 3)
//   The minimum length of a word.
//   See the <b>Normalize</b> method for more information.
//
// wordlist_maximum_word_length <number> (default 25)
//   The maximum length of a word.
//   See the <b>Normalize</b> method for more information.
//
// wordlist_allow_numbers {true|false} <number> (default false)
//   A digit is considered a valid character within a word if
//   this configuration parameter is set to <i>true</i> otherwise
//   it is an error to insert a word containing digits.
//   See the <b>Normalize</b> method for more information.
//
// wordlist_truncate {true|false} <number> (default true)
//   If a word is too long according to
//   the <i>wordlist_maximum_word_length</i> it is truncated
//   if this configuration parameter is <i>true</i> otherwise it
//   is considered an invalid word.
//
// wordlist_lowercase {true|false} <number> (default true)
//   If a word contains upper case letters it is converted to lowercase
//   if this configuration parameter is true, otherwise it is left
//   untouched.
//
// wordlist_valid_punctuation [characters] (default none)
//   A list of punctuation characters that may appear in a word. 
//   These characters will be removed from the word before insertion
//   in the index.
//
// wordlist_extra_word_characters [characters] (default none)
//   A list of characters that may appear in a word. These characters
//   are left untouched.
//
// END
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later 
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordType.h,v 1.10 2001/06/29 14:14:08 loic Exp $
//

#ifndef _WordType_h
#define _WordType_h

#include "htString.h"
#include "Configuration.h"

//
// Return values of Normalize, to get them in string form use NormalizeStatus
//
#define WORD_NORMALIZE_TOOLONG		0x0001
#define WORD_NORMALIZE_TOOSHORT		0x0002
#define WORD_NORMALIZE_CAPITAL		0x0004
#define WORD_NORMALIZE_NUMBER		0x0008
#define WORD_NORMALIZE_CONTROL		0x0010
#define WORD_NORMALIZE_BAD		0x0020
#define WORD_NORMALIZE_NULL		0x0040
#define WORD_NORMALIZE_PUNCTUATION	0x0080
#define WORD_NORMALIZE_NOALPHA		0x0100
#define WORD_NORMALIZE_OK		0x4000
#define WORD_NORMALIZE_NOTOK		0x8000

class WordType
{
public:
  //
  // Constructors
  //
  WordType(const Configuration& config);

  //
  // Predicates
  // 
  int IsChar(int c) const;
  int IsStrictChar(int c) const;
  int IsDigit(int c) const;
  int IsControl(int c) const;

  //
  // Transformations
  //
  int StripPunctuation(String &s) const;

  //-
  // Normalize a word according to configuration specifications and 
  // builtin transformations. <b>Every</b> word inserted in the inverted
  // index goes thru this function. If
  // a word is rejected (return value has WORD_NORMALIZE_NOTOK bit set) it will not 
  // be inserted in the index. If a word is accepted (return value has 
  // WORD_NORMALIZE_OK bit set) it will be inserted in the index. In
  // addition to these two bits, informational values are stored that
  // give information on the processing done on the word.
  // The bit field values and their meanings are
  // as follows:
  //
  // <dl>
  // <dt>WORD_NORMALIZE_TOOLONG
  // <dd>the word length exceeds the value of 
  //     the <i>wordlist_maximum_word_length</i> configuration parameter.
  // <dt>WORD_NORMALIZE_TOOSHORT
  // <dd>the word length is smaller than the value of 
  //     the <i>wordlist_minimum_word_length</i> configuration parameter.
  // <dt>WORD_NORMALIZE_CAPITAL
  // <dd>the word contained capital letters and has been converted 
  //     to lowercase. This bit is only set
  //     if the <i>wordlist_lowercase</i> configuration parameter
  //     is true.
  // <dt>WORD_NORMALIZE_NUMBER
  // <dd>the word contains digits and the configuration 
  //     parameter <i>wordlist_allow_numbers</i> is set to false.
  // <dt>WORD_NORMALIZE_CONTROL
  // <dd>the word contains control characters.
  // <dt>WORD_NORMALIZE_BAD
  // <dd>the word is listed in the file pointed by 
  //     the <i>wordlist_bad_word_list</i> configuration parameter.
  // <dt>WORD_NORMALIZE_NULL
  // <dd>the word is a zero length string.
  // <dt>WORD_NORMALIZE_PUNCTUATION
  // <dd>at least one character listed in 
  //     the <i>wordlist_valid_punctuation</i> attribute was removed
  //     from the word.
  // <dt>WORD_NORMALIZE_NOALPHA
  // <dd>the word does not contain any alphanumerical character.
  // </dl>
  // 
  int Normalize(String &s) const;

  //
  // Error handling
  //
  //-
  // Returns a string explaining the return flags of the Normalize
  // method.
  //
  static String NormalizeStatus(int flags);

private:

  String		valid_punctuation;      // The same as the attribute.
  String		extra_word_characters;  // The same as the attribute.
  char			chrtypes[256];          // quick lookup table for types
  int			minimum_length;		// Minimum word length
  int			maximum_length;		// Maximum word length
  int			allow_numbers;		// True if a word may contain numbers
  int			lowercase;		// True words converted to lowercase
  int			truncate;		// True if word too long are truncated
  Dictionary		badwords;		// List of excluded words
};

// Bits to set in chrtypes[]:
#define WORD_TYPE_ALPHA	0x01
#define WORD_TYPE_DIGIT	0x02
#define WORD_TYPE_EXTRA	0x04
#define WORD_TYPE_VALIDPUNCT	0x08
#define WORD_TYPE_CONTROL	0x10

// One for characters that when put together are a word
// (including punctuation).
inline int
WordType::IsChar(int c) const
{
  return (chrtypes[(unsigned char)c] & (WORD_TYPE_ALPHA|WORD_TYPE_DIGIT|WORD_TYPE_EXTRA|WORD_TYPE_VALIDPUNCT)) != 0;
}

// Similar, but no punctuation characters.
inline int
WordType::IsStrictChar(int c) const
{
  return (chrtypes[(unsigned char)c] & (WORD_TYPE_ALPHA|WORD_TYPE_EXTRA)) != 0;
}

// Reimplementation of isdigit() using the lookup table chrtypes[] 
inline int
WordType::IsDigit(int c) const
{
  return (chrtypes[(unsigned char)c] & WORD_TYPE_DIGIT) != 0;
}

// Similar to IsDigit, but for iscntrl()
inline int
WordType::IsControl(int c) const
{
  return (chrtypes[(unsigned char)c] & WORD_TYPE_CONTROL) != 0;
}

// Let caller get rid of getting and holding a configuration parameter.
inline int
WordType::StripPunctuation(String &s) const
{
  return s.remove(valid_punctuation);
}


#endif /* __WordType_h */
