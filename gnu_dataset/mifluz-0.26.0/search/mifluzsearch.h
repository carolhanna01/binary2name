//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: mifluzsearch.h,v 1.4 2001/06/29 14:14:08 loic Exp $
//
#ifndef _mifluzsearch_h
#define _mifluzsearch_h

#include <stdlib.h>

#include <htString.h>
#include <WordList.h>
#include <StringList.h>

#include <WordTree.h>

#define WORD_SEARCH_OR		0x01
#define WORD_SEARCH_OPTIONAL	0x02

class MifluzSearchInput {
 public:
  inline MifluzSearchInput() {
    buffer = 0;
    buffer_length = 0;
    or_method = WORD_SEARCH_OPTIONAL;

    pointer = 0;

    charset = "ISO-8859-1";
    words = 0;
    verbose = 0;
  }
  inline ~MifluzSearchInput() {
    if(buffer) free(buffer);
  }

  inline int BufferSet(char* bufferp, int bufferp_length) {
    buffer_length = bufferp_length;
    buffer = (char*)malloc(buffer_length + 1);
    memcpy(buffer, bufferp, buffer_length);
    buffer[buffer_length] = '\0';
    return 0;
  }

  inline int Verbose(int verbosity) { return verbose = verbosity; }
 
  char* buffer;
  int buffer_length;
  int buffer_size;
  int or_method;

  int pointer;

  StringList words_verbatim;
  StringList words_unaccent;

  WordTree* query;

  char* charset;
  int maximum_word_length;
  WordList* words;
  int verbose;
};

int search_parse(void* inputp);

#endif /* _mifluzsearch_h */
