//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordSearch.cc,v 1.6 2008/06/08 08:29:35 sebdiaz Exp $
//
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <WordSearch.h>
#include <WordResults.h>

WordSearch::WordSearch(WordList* nwords)
{
  //
  // Internal
  //
  words = nwords;
  verbose = 0;

  //
  // Input/Output
  //
  limit_base = 0;

  //
  // Input
  //
  limit_count = 0;
  expr = 0;

  //
  // Output
  //
  matches = 0;
}

int WordSearch::ContextRestore()
{
  String context_in;
  if(results->GetContext(context_in) != OK)
    return NOTOK;

  return expr->ContextRestore(context_in);
}

int WordSearch::ContextSave(int status)
{
  String tmp;

  if(status != WORD_WALK_ATEND) {
    if(expr->ContextSave(tmp) != OK)
      return NOTOK;
  }

  results->PutContext(tmp);

  return OK;
}

WordMatches *WordSearch::Search()
{
  //
  // Build space for results
  //
  matches = new WordMatches(words->GetContext());
  matches->Allocate(limit_count + 1);

  int ret;
  //
  // Call SearchFromIndex if returned value is neither OK (got them all)
  // or WORD_WALK_ATEND (did not get them all but at end of search anyway).
  //
  if((ret = SearchFromCache()) == WORD_WALK_END_CACHE) {
    unsigned int count;
    if(results->Count(count) == NOTOK)
      return 0;
    ret = SearchFromIndex(limit_base - count + limit_count);
  }

  //
  // Discard results if nothing was found or error occured
  //
  if(ret == NOTOK || matches->length <= 0) {
    delete matches;
    matches = 0;
  }

  return matches;
}

int WordSearch::SearchFromCache()
{
  int filled = results->Filled();
  unsigned int available;
  unsigned int base = limit_base;

  if(results->Count(available) != OK)
    return NOTOK;

  if(available <= limit_base) {
    base = (available / limit_count) * limit_count;
    //
    // If the cache is not filled, it is the responsibility of
    // SearchFromIndex to set the limit_base according to what is
    // found when searching.
    //
    if(filled) {
      limit_base = base;
    } 
  }

  if(results->GetMatchesTotal(matches_total) != OK)
    return NOTOK;

  return results->Get(matches, limit_count, base);
}

int WordSearch::SearchFromIndex(unsigned int length)
{
  int ret = 0;

  if(WordTree::TopLevelOptimize(expr) != OK)
    return NOTOK;

  /*
   * This happens when the optimization decided that the
   * expression was meaningless.
   */
  if(this==0 || expr == 0)
    return NOTOK;

  if(expr->Count(matches_total) != OK)
    return NOTOK;
  
  //
  // Move before first possible position. 
  //
  if((ret = expr->WalkInit()) != OK)
    goto end;

  if((ret = ContextRestore()) == NOTOK)
    goto end;

  //
  // Set the result list only after the context was restored otherwise
  // it will interfere with the re-initialization of the context because
  // WalkNext will skip previously seen documents.
  //
  if((ret = expr->SetResults(results)) != OK)
    goto end;

  ret = SearchLoop(expr, length);

  //
  // Don't bother saving the context if at end of 
  // search (WORD_WALK_ATEND) or error (NOTOK)
  //
  if(ret != NOTOK && (ret = ContextSave(ret)) == NOTOK)
    goto end;

end:
  expr->WalkFinish();

  if(results) {
    if(results->PutMatchesTotal(matches_total) != OK)
      return NOTOK;
  }

  return ret;
}

int WordSearch::SearchLoop(WordTree *expr, unsigned int length)
{
  int ret = OK;
  unsigned int i;
  WordResults* results = expr->GetResults();
  unsigned int count;

  if(results->Count(count) != OK)
    return NOTOK;

  for(i = 0; i < length; i++) {
    if((ret = expr->WalkNext()) != OK) {
      if(ret != WORD_WALK_ATEND)
	return ret;
      break;
    } else {
      WordMatch* match = matches->matches[matches->length];
      match->match = expr->GetDocument();
      if(expr->IsA() != WORD_TREE_LITERAL)
	match->info = ((WordTreeOperand*)expr)->GetInfo();
      if((ret = results->Put(*match, count + i)) != OK)
      	return ret;
      if(verbose) fprintf(stderr, "WordSearch::SearchLoop: match %s\n", (char*)match->Get());
      matches->length = (matches->length + 1) % limit_count;
    }
  }

  if(i == 0) {
    ;
  } else {
    if(matches->length == 0) matches->length = limit_count;
    limit_base = ((count + i - 1) / limit_count) * limit_count;
  }

  //
  // Invalidate matches that are above the list of valid matches
  //
  for(i = 0; i < matches->size; i++)
    matches->matches[i]->valid = i < matches->length;

  return ret;
}
