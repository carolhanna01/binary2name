//
// mifluz.h
//
// NAME
//   C++ library to use and manage inverted indexes
//
// SYNOPSIS
// #include <mifluz.h>
// 
// main()
// {
//    Configuration* config = WordContext::Initialize();
// 
//    WordList* words = new WordList(*config);
// 
//    ...
// 
//    delete words;
// 
//    WordContext::Finish();
// }
//
// DESCRIPTION
//
// The purpose of <i>mifluz</i> is to provide a C++ library to build and query a
// full text inverted index. It is dynamically updatable, scalable (up to
// 1Tb indexes), uses a controlled amount of memory, shares index files
// and memory cache among processes or threads and compresses index files
// to 50% of the raw data. The structure of the index is configurable at
// runtime and allows inclusion of relevance ranking information. The
// query functions do not require loading all the occurrences of a
// searched term.  They consume very few resources and many searches can
// be run in parallel.
//
// The file management library used in mifluz is a modified Berkeley DB 
// (www.sleepycat.com) version 3.1.14.
// 
// CONFIGURATION
// 
// wordlist_wordkey_document [field ...] (default none)
//   A white space separated list of field numbers that define a document.
//   The field number list must not contain gaps. For instance 1 2 3 is 
//   valid but 1 3 4 is not valid.
//   This configuration parameter is not used by the mifluz library
//   but may be used by a query application to define the semantic of 
//   a document. In response to a query, the application will return a
//   list of results in which only distinct documents will be shown.
//
// wordlist_wordkey_location field (default none)
//   A single field number that contains the position of a word in a
//   given document.
//   This configuration parameter is not used by the mifluz library
//   but may be used by a query application.
//
// wordlist_wordkey_uniq field (default none)
//   A single field number on which a uniq sort of a document list
//   will be done. This field is used by mifluzsearch(1) to reduce
//   the list of matching URLs to one URL per server, for instance.
//
// LOCALE SUPPORT
//
// Words are compared using the strcoll(3) function when inserted and
// searched in the index. See setlocale(3) for more information on
// locale support. Only 8 bits character sets are supported.
//
// ENVIRONMENT
//
// <b>MIFLUZ_CONFIG</b> file name of configuration file read by
// WordContext(3). Defaults to <b>~/.mifluz.</b> or <b>/usr/etc/mifluz.conf</b>
//
// END
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: mifluz.h,v 1.17 2001/06/29 14:14:08 loic Exp $
//

#ifndef _mifluz_h_
#define _mifluz_h_

#include <mifluz/config.h>
#include <mifluz/WordContext.h>
#include <mifluz/WordList.h>

#endif /* _mifluz_h */
