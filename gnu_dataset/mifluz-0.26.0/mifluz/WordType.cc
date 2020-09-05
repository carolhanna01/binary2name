//
// WordType.cc
//
// WordType:  Wrap some attributes to make is...() type
//            functions and other common functions without having to manage
//            the attributes or the exact attribute combination semantics.
//	      Configuration parameter used:
//            valid_punctuation,extra_word_characters,minimum_word_length,
//            maximum_word_length,allow_numbers,bad_word_list
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later 
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordType.cc,v 1.11 2001/06/29 14:14:08 loic Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <ctype.h>
#include <stdio.h>
#include <locale.h>

#include "WordType.h"

WordType::WordType(const Configuration &config)
{
  const String valid_punct = config["wordlist_valid_punctuation"];
  const String extra_word_chars = config["wordlist_extra_word_characters"];

  String locale = config.Find("wordlist_locale");
  if(locale.empty())
    locale = "C";
  if(setlocale(LC_ALL, (char*)locale) == 0) {
    fprintf(stderr, "WordType::WordType: cannot set locale: ");
    perror("");
  }

  minimum_length = config.Value("wordlist_minimum_word_length", 3);
  maximum_length = config.Value("wordlist_maximum_word_length", 25);
  allow_numbers = config.Boolean("wordlist_allow_numbers", 0);
  lowercase = config.Boolean("wordlist_lowercase", 1);
  truncate = config.Boolean("wordlist_truncate", 1);

  extra_word_characters = extra_word_chars;
  valid_punctuation = valid_punct;

  chrtypes[0] = 0;
  for (int i = 1; i < 256; i++)
  {
    chrtypes[i] = 0;
    if (isalpha(i))
	chrtypes[i] |= WORD_TYPE_ALPHA;
    if (isdigit(i))
	chrtypes[i] |= WORD_TYPE_DIGIT;
    if (iscntrl(i))
	chrtypes[i] |= WORD_TYPE_CONTROL;
    if (strchr(extra_word_chars, i))
	chrtypes[i] |= WORD_TYPE_EXTRA;
    if (strchr(valid_punct, i))
	chrtypes[i] |= WORD_TYPE_VALIDPUNCT;
  }

  {
    const String filename = config["wordlist_bad_word_list"];
    FILE	*fl = fopen(filename, "r");
    char	buffer[1000];
    char	*word;
    String      new_word;

    // Read in the badwords file (it's just a text file)
    while (fl && fgets(buffer, sizeof(buffer), fl))
      {
	word = strtok(buffer, "\r\n \t");
	if (word && *word)
	  {
	    int flags;
	    new_word = word;
	    if((flags = Normalize(new_word)) & WORD_NORMALIZE_NOTOK) {
	      fprintf(stderr, "WordType::WordType: reading bad words from %s found %s, ignored because %s\n", (const char*)filename, word, (char*)NormalizeStatus(flags & WORD_NORMALIZE_NOTOK));
	    } else {
	      badwords.Add(new_word, 0);
	    }
	  }
    }

    if (fl)
	fclose(fl);
  }
}

int
WordType::Normalize(String& word) const
{
  int status = 0;

  //
  // Reject empty strings, always
  //
  if(word.empty())
    return status | WORD_NORMALIZE_NULL | WORD_NORMALIZE_NOTOK;

  //
  // Always convert to lowercase
  //
  if(lowercase && word.lowercase())
    status |= WORD_NORMALIZE_CAPITAL;

  //
  // Remove punctuation characters according to configuration
  //
  if(StripPunctuation(word))
    status |= WORD_NORMALIZE_PUNCTUATION;

  //
  // Truncate words too long 
  //
  if(word.length() > maximum_length) {
    status |= WORD_NORMALIZE_TOOLONG;
    if(truncate)
      word.chop(word.length() - maximum_length);
    else
      return status | WORD_NORMALIZE_NOTOK;
  }

  //
  // Reject words too short according to configuration
  //
  if(word.length() < minimum_length)
    return status | WORD_NORMALIZE_TOOSHORT | WORD_NORMALIZE_NOTOK;

  //
  // Reject if contains control characters
  //
  int alpha = 0;
  for(const unsigned char *p = (const unsigned char*)(const char*)(char *)word; *p; p++) {
    if(IsStrictChar(*p) || (allow_numbers && IsDigit(*p))) {
      alpha = 1;
    } else if(IsControl(*p)) {
      return status | WORD_NORMALIZE_CONTROL | WORD_NORMALIZE_NOTOK;
    } else if(IsDigit(*p)) {
      return status | WORD_NORMALIZE_NUMBER | WORD_NORMALIZE_NOTOK;
    }
  }

  //
  // Reject if contains no alpha characters (according to configuration)
  //
  if(!alpha) return status | WORD_NORMALIZE_NOALPHA | WORD_NORMALIZE_NOTOK;

  //
  // Reject if listed in config[bad_word_list]
  //
  if(badwords.Exists(word))
    return status | WORD_NORMALIZE_BAD | WORD_NORMALIZE_NOTOK;

  //
  // Accept and report the transformations that occured
  //
  return status | WORD_NORMALIZE_OK;
}

//
// Convert the integer status into a readable string
//
String
WordType::NormalizeStatus(int flags)
{
  String tmp;

  if(flags & WORD_NORMALIZE_TOOLONG) tmp << "TOOLONG ";
  if(flags & WORD_NORMALIZE_TOOSHORT) tmp << "TOOSHORT ";
  if(flags & WORD_NORMALIZE_CAPITAL) tmp << "CAPITAL ";
  if(flags & WORD_NORMALIZE_NUMBER) tmp << "NUMBER ";
  if(flags & WORD_NORMALIZE_CONTROL) tmp << "CONTROL ";
  if(flags & WORD_NORMALIZE_BAD) tmp << "BAD ";
  if(flags & WORD_NORMALIZE_NULL) tmp << "NULL ";
  if(flags & WORD_NORMALIZE_PUNCTUATION) tmp << "PUNCTUATION ";
  if(flags & WORD_NORMALIZE_NOALPHA) tmp << "NOALPHA ";

  if(tmp.empty()) tmp << "GOOD";

  return tmp;
}
