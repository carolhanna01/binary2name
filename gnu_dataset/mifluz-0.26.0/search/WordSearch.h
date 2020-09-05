//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordSearch.h,v 1.4 2001/06/29 14:14:08 loic Exp $
//
// ************************* WordSearch implementation ********************
//
// NAME
//
// Solve a query from a WordTree syntax tree
//
// SYNOPSIS
//
// #include <WordSearch.h>
//
// WordTree* expr = get_query();
// WordSearch search;
// search.limit_count = NUMBER_OF_RESULTS;
// WordMatch* search.Search(expr);
// ...
//  
// DESCRIPTION
//
// The WordSearch class is a wrapper to query an inverted index
// using a WordTree syntax tree. 
// 
// END
//

#ifndef _WordSearch_h
#define _WordSearch_h

#include <WordList.h>
#include <WordMatch.h>
#include <WordTree.h>

class WordSearch {
public:
  WordSearch(WordList* words);

  ~WordSearch() {
    if(expr) delete expr;
  }

  //-
  // Set the list of documents that must be ignored.
  //
  inline int SetResults(WordResults* nresults) { results = nresults; return OK; }
  //-
  // Get the list of documents that must be ignored.
  //
  inline WordResults* GetResults() { return results; }

  //-
  // Save the context of the last document retrieved in the context_out
  // data member.
  //
  int ContextSave(int status);
  //-
  // Restore a search context from the context_in data member.
  //
  int ContextRestore();

  //-
  // First call SearchFromCache and then SearchFromIndex if SearchFromCache
  // returned 0.
  //
  WordMatches *Search();
  //-
  // Attempt to retrieve the results from the cache. Returns 0 if
  // the search cannot be resolved from the cache. Returns the list
  // of matches if the search can be resolved from the cache. If the
  // cache only contains part of the desired results, call SearchFromIndex
  // to get the others. If the cache does not contain any of the desired
  // results, return 0.
  //
  int SearchFromCache();
  //-
  // Perform a search from the <b>expr</b> specifications.
  // Restore the context from <i>context_in</i> on <b>expr</b>.
  // Then skip (using WalkNext) <i>limit_bottom</i> entries.
  // Then collect in a WordMatch array of size <i>limit_count</i>
  // each match returned by WalkNext. When finished store
  // the context (ContextSave) in <i>context_out</i>.
  // It is the responsibility of the caller to free the WordMatch
  // array. If no match are found a null pointer is returned.
  //
  int SearchFromIndex(unsigned int length);

  //-
  // Search backend, only run the WalkNext loop but does not
  // allocate/deallocate data. If limit_bottom is above all matches
  // return the last valid limit_count range and reset limit_bottom 
  // accordingly.
  //
  int SearchLoop(WordTree *expr, unsigned int length);

  inline int Verbose(int verbosity) { return verbose = verbosity; }

  //
  // Internal
  //
  WordList* words;
  //-
  // A list of documents that must be ignored
  //
  WordResults *results;
  int verbose;

  //
  // Input/Output
  //
  //
  // Input: Absolute position of the document pointed by context_in. The
  //        limit_bottom position is relative to limit_base.
  // Output: Absolute position of the first document returned.
  //
  unsigned int limit_base;

  //
  // Input
  //
  //
  // Maximum number of matches returned
  //
  unsigned int limit_count;
  //
  // Query tree
  //
  WordTree* expr;
  
  //
  // Output
  //
  //
  // Array of at most limit_count matches. The number of valid elements in
  // the array is matches_length;
  //
  WordMatches* matches;
  //
  // Estimated number of matches.
  //
  unsigned int matches_total;
};

#endif /* _WordSearch_h */
