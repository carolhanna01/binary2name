//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordTree.cc,v 1.13 2008/06/08 08:29:35 sebdiaz Exp $
//
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <HtMaxMin.h>

#include <WordTree.h>

static char* operator_name[WORD_TREE_OP_SIZE] = {
  "",
  "or",
  "and",
  "near",
  "optional",
  "literal",
  "mandatory",
  "not",
  0
};

// ************************* WordTree implementation ****************

int WordTree::WalkNextExclude(const WordKey& key)
{
  int exclude = 0;
  if(results) {
    if(has_uniq) {
      exclude = results->UniqExists(key);
    }
    if(!exclude) {
      exclude = results->Exists(key);
    }
  }
  return exclude;
}

int WordTree::TopLevelOptimize(WordTree*& expr)
{
  int verbose = expr->verbose;
  printf("Bef01\n");
  if(verbose) fprintf(stderr, "WordSearch::Optimize: non optimized expression %s\n", (char*)expr->Get());
  if(expr->Optimize() != OK)
    return NOTOK;
  printf("Bef02\n");
  /*
   * Single mandatory word equivalent to single word. 
   */
  if(expr->IsA() == WORD_TREE_MANDATORY) {
    WordTreeMandatory* mandatory = (WordTreeMandatory*)expr;
    if(mandatory->cursors_length != 1) {
      fprintf(stderr, "WordTree::TopLevelOptimize: unexpected cursor length == %d instead of 1\n", mandatory->cursors_length);
      return NOTOK;
    }
    expr = mandatory->cursors[0];
    mandatory->cursors_length = 0;
    delete mandatory;
  }
  printf("Bef03\n");

  /* 
   * Single forbiden word is something we don't want to handle.
   */
  if(expr->IsA() == WORD_TREE_NOT) {
    delete expr;
    expr = 0;
  }
  
  printf("Bef04\n");
  if(verbose) {
    if(expr) {
      fprintf(stderr, "WordTree::TopLevelOptimize: optimized expression %s\n", (char*)expr->Get());
    } else {
      fprintf(stderr, "WordTree::TopLevelOptimize: search expression is meaningless, discarded\n");
    }
  }

  printf("Bef05\n");

  if(expr && expr->IsA() != WORD_TREE_LITERAL && expr->key_semantic.HasRealm()) {
    if(expr->ClearRealm() != OK)
      return NOTOK;
  }

  printf("Bef06\n");
  return OK;
}

// ************************* WordTreeLiteral implementation ****************

int WordTreeLiteral::WalkInit()
{
  if(WordCursorOne::WalkInit() != OK)
    return NOTOK;
  if(!bottom_document.Empty())
    return Seek(bottom_document);
  else
    return OK;
}

int WordTreeLiteral::WalkRewind()
{
  if(WordCursorOne::WalkRewind() != OK) return NOTOK;
  if(!bottom_document.Empty())
    return Seek(bottom_document);
  return OK;
}

int WordTreeLiteral::WalkNext()
{
  int ret;
  int too_low = 0;

  do {
    ret = WordCursorOne::WalkNext();
    //
    // If the returned document is before the lowest document allowed,
    // Seek and try again.
    //
    if(ret == OK &&
       !bottom_document.Empty() &&
       key_semantic.DocumentCompare(bottom_document, GetDocument()) > 0) {
      WordKey patch = bottom_document;
      key_semantic.RealmCopy(GetFound().Key(), patch);
      ret = Seek(patch);
      too_low = 1;
    } else {
      too_low = 0;
    }
  } while(ret == OK && too_low);

  if(verbose > 3) fprintf(stderr, "WordTreeLiteral::WalkNext: reached %s\n", (char*)GetDocument().Get());

  if(ret == OK && !top_document.Empty()) {
    if(key_semantic.DocumentCompare(top_document, GetDocument()) <= 0)
      ret = WORD_WALK_ATEND|WORD_WALK_ATEND_NOMATCH;
  }

  return ret;
}

int WordTreeLiteral::Seek(const WordKey& position)
{
  return WordCursorOne::Seek((bottom_document.Empty() || key_semantic.DocumentCompare(bottom_document, position) <= 0) ? position : bottom_document);
}


int WordTreeLiteral::Get(String& bufferout) const
{

  if(scope.empty())
    bufferout << search;
  else
    bufferout << "( " << operator_name[IsA()] << " \"" << scope << "\" " << search << " )";
  return OK;
}


// ************************* WordTreeOperand implementation ****************

static char* ret2str(int ret)
{
  if(ret == WORD_WALK_REDO)
    return "REDO";

  if(ret == WORD_WALK_RESTART)
    return "RESTART";

  if(ret == WORD_WALK_NEXT)
    return "NEXT";

  if(ret == OK)
    return "OK";

  if(ret == NOTOK)
    return "NOTOK";

  if(ret == WORD_WALK_ATEND)
    return "ATEND";

  if(ret == WORD_WALK_ATEND|WORD_WALK_ATEND_NOMATCH)
    return "ATEND|NOMATCH";

  return "???";
}

WordTreeOperand::~WordTreeOperand()
{
  if(cursors) {
    unsigned int i;
    for(i = 0; i < cursors_length; i++)
      delete cursors[i];
    free(cursors);
  }
}

int 
WordTreeOperand::Optimize()
{
  //
  // Apply to each cursor
  //
  unsigned int i;
  for(i = 0; i < cursors_length; i++)
    if(cursors[i]->Optimize() == NOTOK)
      return NOTOK;
  return OK;
} 

int WordTreeOperand::OptimizeOr(int proximity)
{
  unsigned int ignore = 0;
  unsigned int ignore_mask = 0;
  unsigned int i;
  for(i = 0; i < cursors_length; i++) {
    int reduce;
    //
    // Set ignore & ignore_mask if cursor is NOT or MANDATORY
    //
    switch(cursors[i]->IsA()) {
    case WORD_TREE_MANDATORY:
      ignore |= (1 << WORD_EXCLUDE_POSITION2BIT(cursors_length, i));
      reduce = 1;
      break;
    case WORD_TREE_NOT:
      ignore |= (1 << WORD_EXCLUDE_POSITION2BIT(cursors_length, i));
      ignore_mask |= (1 << WORD_EXCLUDE_POSITION2BIT(cursors_length, i));
      reduce = 1;
      break;
    default:
      reduce = 0;
      break;
    }
    //
    // Replace the NOT or MANDATORY node by its only child
    //
    if(reduce) {
      WordTreeOperand* old = (WordTreeOperand*)cursors[i];
      cursors[i] = old->cursors[0];
      old->cursors[0] = 0;
      old->cursors_length--;
      if(old->cursors_length > 0) {
	fprintf(stderr, "WordTreeOperand::OptimizeOr: too many cursors\n");
	return NOTOK;
      }
      delete old;
    }
  }
  return permutation.Initialize(cursors_length, ignore, ignore_mask, proximity);
}

int 
WordTreeOperand::WalkInit()
{
  unsigned int i;
  int ret = WORD_WALK_ATEND;
  for(i = 0; i < cursors_length; i++)
    if((ret = cursors[i]->WalkInit()) != OK)
      return ret;
  return (status = ret);
}

int 
WordTreeOperand::WalkRewind()
{
  unsigned int i;
  int ret = OK;
  for(i = 0; i < cursors_length; i++)
    if((ret = cursors[i]->WalkRewind()) != OK)
      return ret;
  status = OK;
  key_semantic.DocumentClear(pos);
  cursor_get_flags = DB_SET_RANGE;
  found.Clear();
  return ret;
}

int 
WordTreeOperand::WalkFinish()
{
  unsigned int i;
  int ret = OK;
  for(i = 0; i < cursors_length; i++)
    if((ret = cursors[i]->WalkFinish()) != OK)
      return ret;
  return ret;
}

int 
WordTreeOperand::Seek(const WordKey& patch)
{
  pos = patch;
  cursor_get_flags = DB_SET_RANGE;

  unsigned int i;
  int ret = OK;
  for(i = 0; i < cursors_length; i++)
    if((ret = cursors[i]->Seek(patch)) != OK &&
       !cursors[i]->IsAtEnd())
      return ret;
  status = OK;
  return OK;
}

int WordTreeOperand::Get(String& bufferout) const
{
  bufferout << "( " << operator_name[IsA()] << " \"" << scope << "\" ";
  unsigned int i;
  for(i = 0; i < cursors_length; i++)
    bufferout << cursors[i]->Get() << " ";
  bufferout << " )";
  return OK;
}

// ************************* WordTreeOptional implementation ****************

int WordTreeOptional::Optimize()
{
  int ret;
  if((ret = WordTreeOperand::Optimize()) != OK)
    return ret;

  if(UseProximity() != WORD_PERMUTE_PROXIMITY_ONLY) {
    if((ret = AscendingFrequency()) != OK)
      return ret;
  }

  unsigned int stripped;
  unsigned int killed_mandatory;
  if((ret = StripNonExistent(stripped, killed_mandatory)) != OK)
    return ret;

  if(killed_mandatory || (AllOrNothing() && stripped)) {
    //
    // One word is missing and everything is lost,
    // Just kill the remaining cursors.
    //
    unsigned int i;
    for(i = 0; i < cursors_length; i++)
      delete cursors[i];
    cursors_length = 0;

    return OK;
  } else {
    return OptimizeOr(UseProximity());
  }
}

int WordTreeOptional::ContextSaveList(StringList& list) const
{
  int ret;
  if((ret = WordTreeOperand::ContextSaveList(list)) != OK)
    return ret;

  if(UsePermutation()) {
    String* buffer = new String();
    permutation.Get(*buffer);
    
    list.Add(buffer);
  }
   
  {
    String* buffer = new String();
    if((ret = WordCursorOne::ContextSave(*buffer)) != OK)
      return ret;

    list.Add(buffer);
  }

  return OK;
}

int WordTreeOptional::ContextRestoreList(StringList& list)
{
  int ret;
  if((ret = WordTreeOperand::ContextRestoreList(list)) != OK)
    return ret;

  if(UsePermutation()) {
    char* buffer = list[0];
    if((ret = permutation.Set(buffer)) != OK)
      return ret;
    list.Remove(0);
  }

  {
    char* buffer = list[0];
    if(!buffer) return NOTOK;
    WordKey key(words->GetContext(), buffer);
    if((ret = Seek(key)) != OK)
      return ret;
    cursor_get_flags = DB_NEXT;

    list.Remove(0);
  }

  return OK;
}

int WordTreeOptional::WalkNext()
{
  WordKey& constraint = pos;
  //
  // Set constraint with all 0
  //
  if(constraint.Empty()) {
    if(verbose) fprintf(stderr, "  WordTreeOptional::WalkNext: required position or document is reset, i.e. before first possible entry\n");
    key_semantic.DocumentClear(constraint);
  }

  int ret = OK;
  //
  // Advance cursors so that next call fetches another constraint
  //
  if(cursor_get_flags == DB_NEXT)
    key_semantic.DocumentNext(constraint, has_uniq);
    
  if((ret = Seek(constraint)) != OK)
    return ret;

  int near = permutation.Proximity();
  WordTree* first = 0;
  for(unsigned int i = 0; i < cursors_length;) {
    int excluded = permutation.Excluded(i);
    WordTree& cursor = *(cursors[i]);

    if(excluded && !permutation.Ignored(i)) {
      if(verbose) fprintf(stderr, "\n  WordTreeOptional::WalkNext: looking for %s (ignored)\n", (char*)cursor.search);
      i++;
      continue;
    }

    near = permutation.Proximity();
    if(verbose) fprintf(stderr, "\n  WordTreeOptional::WalkNext: looking for %s (excluded = %s, proximity = %s)\n", (char*)cursor.search, (excluded ? "yes" : "no"), (near ? "yes" : "no" ));

    int ret;
    if(excluded) {
      ret = SearchCursorNot(cursor, constraint);
      if(verbose) fprintf(stderr, "  WordTreeOptional::WalkNext: Not -> %s\n", ret2str(ret));
    } else {
      if(near) {
	ret = SearchCursorNear(cursor, first, constraint, proximity);
	if(verbose) fprintf(stderr, "  WordTreeOptional::WalkNext: Near -> %s\n", ret2str(ret));
      } else {
	ret = SearchCursorAnd(cursor, constraint, permutation);
	if(verbose) fprintf(stderr, "  WordTreeOptional::WalkNext: And -> %s\n", ret2str(ret));
      }
    }

    switch(ret & WORD_WALK_RESULT_MASK) {
    case WORD_WALK_ATEND:
      //
      // If a realm exists between the word and the document and the
      // cursor did not hit the end of the index.
      //
      if(key_semantic.HasRealm()) {
	if(ret & WORD_WALK_ATEND_NOMATCH) {
	  //
	  // If the cursor is still on the same word
	  //
	  if(cursor.GetFound().Key().Get(WORD_KEY_WORD) == cursor.GetSearch().Get(WORD_KEY_WORD)) {
	    //
	    // Change the realm of all cursors
	    //
	    if(SetRealm(cursor.GetFound().Key()) != OK)
	      return NOTOK;
	    //
	    // And restart the search in this realm
	    //
	    if(WalkRewind() != OK)
	      return NOTOK;
	    first = 0;
	    i = 0;
	    break;
	  }
	}
      }
      if(UsePermutation()) {
	//
	// The search is over with this permutation, try another one.
	//
	if(verbose) fprintf(stderr, "  WordTreeOptional::WalkNext: try next proximity/exclusion permutation\n");
	switch(permutation.Next()) {
	  //
	  // No permutations left, the end
	  //
	case WORD_PERMUTE_END:
	  if(verbose) fprintf(stderr, "\nWordTreeOptional::WalkNext: ATEND\n");
	  return (status = WORD_WALK_ATEND);
	  break;

	  //
	  // Sart over with this permutation
	  //
	case WORD_PERMUTE_OK:
	  if(key_semantic.HasRealm() && ClearRealm() != OK)
	    return NOTOK;
	  if(WalkRewind() != OK)
	    return NOTOK;
	  break;
	}
	first = 0;
	i = 0;
      } else {
	if(verbose) fprintf(stderr, "\nWordTreeOptional::WalkNext: ATEND\n");
	return (status = WORD_WALK_ATEND);
      }
      break;
    case WORD_WALK_REDO:
      break;
    case WORD_WALK_RESTART:
      first = 0;
      i = 0;
      break;
    case WORD_WALK_NEXT:
      i++;
      break;
    case NOTOK:
    default:
      if(verbose) fprintf(stderr, "\nWordTreeOptional::WalkNext: %s\n", ret2str(ret));
      return ret;
      break;
    }
  }

  cursor_get_flags = DB_NEXT;

  SetInfo();
    
  //
  // Save possible result, i.e. first non excluded cursor
  //
  for(unsigned int i = 0; i < cursors_length; i++) {
    WordTree& cursor = *(cursors[i]);
    if(!permutation.Excluded(i)) {
      found.Key() = cursor.GetFound().Key();
      break;
    }
  }

  if(verbose) fprintf(stderr, "\nWordTreeOptional::WalkNext: OK\n");
  return ret;
}

int WordTreeOptional::Seek(const WordKey& position)
{
  pos = position;
  cursor_get_flags = DB_SET_RANGE;
  status = OK;

  unsigned int i;
  for(i = 0; i < cursors_length; i++) {
    if(!permutation.Excluded(i)) {
      WordTree& cursor = *(cursors[i]);
      return cursor.Seek(position);
    }
  }

  fprintf(stderr, "  WordTreeOptional::Seek: failed\n");
  return NOTOK;
}  


void WordTreeOptional::SetInfo()
{
  unsigned int i;
  for(i = 0; i < cursors_length; i++)
    cursors[i]->SetInfo();

  info.trunc();

  for(i = 0; i < cursors_length; i++) {
    WordTree& cursor = *(cursors[i]);

    if(!permutation.Excluded(i))
      info << cursor.info << " ";
  }

  info << (permutation.Proximity() ? "proximity" : "");
}

int WordTreeOptional::Count(unsigned int& count) const
{
  unsigned int count_and = 0;

  count = 0;
  for(unsigned int i = 0; i < cursors_length; i++) {
    unsigned int cursor_count;
    cursors[i]->Count(cursor_count);
    if(permutation.Ignored(i) && !permutation.Excluded(i)) {
      count_and = count_and ? HtMIN(count_and, cursor_count) : cursor_count;
    } else {
      count += cursor_count;
    }
  }

  count = count_and ? count_and : count;

  return OK;
}

int WordTreeOptional::SearchCursorNear(WordTree& cursor, WordTree*& master, WordKey& constraint, int proximity)
{
  int is_master = master == 0 || master == &cursor;
  if(master == 0) master = &cursor;
  const WordKey& masterKey = master->GetFound().Key();

  int direction = key_semantic.LocationCompare(constraint, cursor.GetFound().Key(), proximity);
  if(verbose) {
    fprintf(stderr, "  WordTreeOptional::SearchCursorNear: required position is %s\n", (char*)(constraint.Get()));
    fprintf(stderr, "  WordTreeOptional::SearchCursorNear: cursor %s is %s\n",
	    (cursor.IsAtEnd() ? "" : (char*)(cursor.GetFound().Key().Get())),
	    (cursor.IsAtEnd() ? "at end" : ((direction == 0 ? "ok" : (direction < 0 ? "after required position" : "before required position")))));
  }

  //
  // If the cursor is in the authorized locations, consider
  // next cursor
  //
  if(direction == 0) {
    //
    // master cursor makes the rules for location : its location
    // is the base to calculate other words mandatory loacations.
    //
    if(is_master)
      key_semantic.LocationSet(cursor.GetFound().Key(), constraint);
    //
    // Fix location constraint to accomodate proximity tolerance.
    //
    key_semantic.LocationNearLowest(constraint, proximity);
    return WORD_WALK_NEXT;

    //
    // If current location is after cursor location
    //
  } else if(direction > 0) {
    //
    // Move the cursor up to the location.
    //
    cursor.Seek(constraint);
    if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNear: seek to required position\n");
    int ret;
    if((ret = cursor.WalkNext()) == OK) {
      //
      // Remove the location constraint for the master word
      // so that it matches and then enforce location for other
      // keys.
      //
      if(is_master) {
	if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNear: required position is copied from master cursor\n");
	key_semantic.Location2Document(constraint);
      }
      //
      // Reconsider the situation for this cursor
      //
      return WORD_WALK_REDO;
    } else {
      return ret;
    }

    //
    // If current location is lower than cursor location,
    // meaning that the cursor found no match for the current
    // location.
    //
  } else if(direction < 0) {
    //
    // The cursor document becomes the current document. 
    // The master cursor is forced to catch up.
    //
    if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNear: required position is copied from cursor\n");
    key_semantic.DocumentSet(cursor.GetDocument(), constraint);
    //
    // It is possible that this cursor document is the same
    // as the master cursor document (if this cursor hit in the
    // same document but a higher location). In this case we must
    // increase the location of the master cursor otherwise it will
    // match without moving and loop forever.
    //
    if(!is_master && key_semantic.DocumentCompare(masterKey, constraint) == 0) {
      key_semantic.LocationSet(masterKey, constraint);
      key_semantic.LocationNext(constraint);
    }
    //
    // Since the current location changed, start over.
    //
    return WORD_WALK_RESTART;
  } else {
    fprintf(stderr, "  WordTreeOptional::WordCursorNear: reached unreachable statement\n");
    return NOTOK;
  }
  return NOTOK;
}

int WordTreeOptional::SearchCursorNot(WordTree& cursor, WordKey& document)
{
  if(key_semantic.HasRealm()) {
    //
    // Since there is a realm we cannot rely on the sequential ordering
    // of the entries for a given document and we have to start over
    // each time.
    //
    WordKey search = cursor.GetSearch();
    key_semantic.DocumentCopy(document, cursor.GetSearch());
    cursor.UndefinedRealm();
    cursor.WalkRewind();
    cursor.Seek(document);
    if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNot: seek to required document\n");
    if(cursor.WalkNext() != OK && !cursor.IsAtEnd())
      return NOTOK;
    int ret;
    if(cursor.IsAtEnd()) {
      ret = WORD_WALK_NEXT;
    } else {
      if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNot: required document incremented\n");
      //
      // The cursor does not give any hint on a possible
      // next document, just go to the next possible one.
      //
      key_semantic.DocumentNext(document, has_uniq);
      //
      // Since the current document changed, start over.
      //
      ret = WORD_WALK_RESTART;
    }
    cursor.GetSearch() = search;
    cursor.WalkRewind();
    return ret;
  } else {
    int direction = key_semantic.DocumentCompare(document, cursor.GetFound().Key());
    if(verbose) {
      fprintf(stderr, "  WordTreeOptional::SearchCursorNot: required document is %s\n", (char*)(document.Get()));
      fprintf(stderr, "  WordTreeOptional::SearchCursorNot: cursor %s is %s\n",
	      (cursor.IsAtEnd() ? "" : (char*)(cursor.GetFound().Key().Get())),
	      (cursor.IsAtEnd() ? "at end" : ((direction == 0 ? "ok" : (direction < 0 ? "after required document" : "before required document")))));
    }

    //
    // If the cursor is after the current document
    // (being at the end of walk is being after all documents).
    //
    // Means that the cursor is positioned in an acceptable document
    // and proceed to the next cursor.
    //
    if(direction < 0 || cursor.IsAtEnd()) {
      if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNot: cursor does not match in required document\n");
      return WORD_WALK_NEXT;

      //
      // If the cursor is before current document
      //
    } else if(direction > 0) {
      //
      // Move the cursor up to the document
      //
      cursor.Seek(document);
      if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNot: seek to required document\n");
      if(cursor.WalkNext() != OK && !cursor.IsAtEnd())
	return NOTOK;
      //
      // It is expected in this case that the cursor has moved after
      // the current document and another visit in the loop will
      // tell us.
      //
      return WORD_WALK_REDO;

      //
      // If the cursor matches the current document.
      //
      // Means that the current document is not a possible match
      // since it is pointed by this cursor.
      //
    } else if(direction == 0) {
      if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorNot: required document incremented\n");
      //
      // The cursor does not give any hint on a possible
      // next document, just go to the next possible one.
      //
      key_semantic.DocumentNext(document, has_uniq);
      //
      // Since the current document changed, start over.
      //
      return WORD_WALK_RESTART;
    } else {
      fprintf(stderr, "  WordTreeOptional::WordCursorNot: reached unreachable statement\n");
      return NOTOK;
    }
  }
  return NOTOK;
}

int WordTreeOptional::SearchCursorAnd(WordTree& cursor, WordKey& document, WordExclude& permutation)
{
  int direction = key_semantic.DocumentCompare(document, cursor.GetFound().Key());
  if(verbose) {
    fprintf(stderr, "  WordTreeOptional::SearchCursorAnd: required document is %s\n", (char*)(document.Get()));
    fprintf(stderr, "  WordTreeOptional::SearchCursorAnd: cursor %s is %s\n",
	    (cursor.IsAtEnd() ? "" : (char*)(cursor.GetFound().Key().Get())),
	    (cursor.IsAtEnd() ? "at end" : ((direction == 0 ? "ok" : (direction < 0 ? "after required document" : "before required document")))));
  }

  //
  // If the cursor is in the current document.
  //
  // Means that the cursor is positioned in an acceptable document
  // and proceed to the next cursor.
  //
  if(direction == 0) {
    return WORD_WALK_NEXT;

    //
    // If the cursor is before current document
    //
  } else if(direction > 0) {
    //
    // Move the cursor up to the document
    //
    if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorAnd: seek to required document\n");
    cursor.Seek(document);
    int ret;
    if((ret = cursor.WalkNext()) == OK)
      return WORD_WALK_REDO;
    else
      return ret;

    //
    // If the cursor is after current document.
    //
    // Means the the current document is not a possible match
    // since it will never reach it because it's already
    // after it.
    //
  } else if(direction < 0) {
    //
    // The cursor document becomes the current document.
    //
    if(verbose) fprintf(stderr, "  WordTreeOptional::SearchCursorAnd: required document is copied from cursor\n");
    key_semantic.DocumentSet(cursor.GetDocument(), document);

    //
    // Since the current document changed, start over.
    //
    return WORD_WALK_RESTART;
  } else {
    fprintf(stderr, "  WordTreeOptional::WordCursorAnd: reached unreachable statement\n");
    return NOTOK;
  }
  return NOTOK;
}

//
// Helper class for AscendingFrequency method
//
class WordSort {
public:
  unsigned int frequency;
  WordTree *cursor;
};

//
// Helper function for AscendingFrequency method
//
static int ascending_frequency(const void *a, const void *b)
{
  const WordSort& a_cursor = *(WordSort*)a;
  const WordSort& b_cursor = *(WordSort*)b;

  return a_cursor.frequency - b_cursor.frequency;
}

int WordTreeOptional::AscendingFrequency()
{
  //
  // Reorder cursors
  //
  WordSort *tmp = new WordSort[cursors_length];

  memset((char*)tmp, '\0', sizeof(WordSort[cursors_length]));

  unsigned int i;
  for(i = 0; i < cursors_length; i++) {
    unsigned int frequency;
    if(cursors[i]->Noccurrence(frequency) != OK) {
      delete [] tmp;
      return NOTOK;
    }
    if(verbose > 2) fprintf(stderr, "  WordTreeOptional::AscendingFrequency: %s occurs %d times\n", (char*)cursors[i]->search, frequency);
    tmp[i].frequency = frequency;
    tmp[i].cursor = cursors[i];
  }

  memset((char*)cursors, '\0', sizeof(WordTree*) * cursors_length);

  qsort((void *)tmp, cursors_length, sizeof(WordSort), &ascending_frequency);

  for(i = 0; i < cursors_length; i++)
    cursors[i] = tmp[i].cursor;

  delete [] tmp;
  return OK;
}

int WordTreeOptional::StripNonExistent(unsigned int& stripped, unsigned int& killed_mandatory)
{
  stripped = 0;
  killed_mandatory = 0;

  WordTree** tmp = new WordTree*[cursors_length];
  memset((char*)tmp, '\0', sizeof(WordTree*[cursors_length]));

  unsigned int from;
  unsigned int to;

  for(to = from = 0; from < cursors_length; from++) {
    unsigned int frequency;
    if(cursors[from]->Noccurrence(frequency) != OK) {
      delete [] tmp;
      return NOTOK;
    }

    if(verbose > 2) fprintf(stderr, "  WordTreeOptional::StripNonExistent: %s occurs %d times\n", (char*)cursors[from]->search, frequency);
    if(frequency > 0) {
      tmp[to++] = cursors[from];
    } else {
      if(cursors[from]->IsA() == WORD_TREE_MANDATORY)
	killed_mandatory++;
      delete cursors[from];
      stripped++;
    }
  }

  memset((char*)cursors, '\0', sizeof(WordTree*) * cursors_length);
  
  cursors_length = to;
  unsigned int i;
  for(i = 0; i < cursors_length; i++)
    cursors[i] = tmp[i];

  delete [] tmp;

  return OK;
}

// ************************* WordTreeOr implementation ********************

int WordTreeOr::Optimize()
{
  int ret;
  if((ret = WordTreeOperand::Optimize()) != OK)
    return ret;

  if((ret = AscendingFrequency()) != OK)
    return ret;

  unsigned int stripped;
  unsigned int killed_mandatory;
  if((ret = StripNonExistent(stripped, killed_mandatory)) != OK)
    return ret;

  return OptimizeOr(WORD_PERMUTE_PROXIMITY_NO);
}

int WordTreeOr::ContextSaveList(StringList& list) const
{
  int ret;
  if((ret = WordTreeOperand::ContextSaveList(list)) != OK)
    return ret;

  {
    String* buffer = new String();
    permutation.Get(*buffer);
    
    list.Add(buffer);
  }
   
  {
    String* buffer = new String();
    if((ret = WordCursorOne::ContextSave(*buffer)) != OK)
      return ret;

    list.Add(buffer);
  }

  return OK;
}

int WordTreeOr::ContextRestoreList(StringList& list)
{
  int ret;
  if((ret = WordTreeOperand::ContextRestoreList(list)) != OK)
    return ret;

  {
    char* buffer = list[0];
    if((ret = permutation.Set(buffer)) != OK)
      return ret;
    list.Remove(0);
  }

  {
    char* buffer = list[0];
    if(!buffer) return NOTOK;
    WordKey key(words->GetContext(), buffer);
    if((ret = Seek(key)) != OK)
      return ret;
    cursor_get_flags = DB_NEXT;

    list.Remove(0);
  }

  return OK;
}

void WordTreeOr::SetInfo()
{
  unsigned int i;
  for(i = 0; i < cursors_length; i++)
    cursors[i]->SetInfo();

  info.trunc();

  for(i = 0; i < cursors_length; i++) {
    WordTree& cursor = *(cursors[i]);

    if(!permutation.Excluded(i) &&
       !cursor.IsAtEnd() &&
       key_semantic.DocumentCompare(cursor.GetFound().Key(), GetFound().Key()) == 0) {
      info << cursor.info << " ";
    }
  }
}

int WordTreeOr::WalkNext()
{
  WordKey& constraint = pos;
  //
  // Set constraint with all 0
  //
  if(constraint.Empty())
    key_semantic.DocumentClear(constraint);
  
  WordKey candidate(words->GetContext());
  int match_ok;
  do {
      int ret;
      unsigned int i;
      candidate.Clear();
      //
      // Advance cursors so that next call fetches another constraint
      //
      if(cursor_get_flags == DB_NEXT)
	  key_semantic.DocumentNext(constraint, has_uniq);
    
      if((ret = Seek(constraint)) != OK)
	  return ret;

      match_ok = 1;
      //
      // All non excluded cursors are about to move
      // at or beyond constraint. Search for the one (candidate) that
      // is located at the lowest location beyond the constraint.
      //
      for(i = 0; i < cursors_length; i++) {
	  if(permutation.Excluded(i))
	      continue;
	  WordTree& cursor = *(cursors[i]);

	  switch((ret = cursor.WalkNext()) & WORD_WALK_RESULT_MASK) {
	  case WORD_WALK_ATEND:
	      //
	      // Constraint is after all matches for this cursor
	      //
	      break;
	  case OK:
	      //
	      // If candidate is not set or current cursor is before
	      // the current candidate, the curent cursor document becomes
	      // the candidate.
	      //
	      if(candidate.Empty() ||
		 key_semantic.DocumentCompare(candidate, cursor.GetFound().Key()) > 0) {
		  key_semantic.DocumentSet(cursor.GetDocument(), candidate);
	      }
	      break;
	  default:
	      return ret;
	      break;
	  }
      }

      //
      // No candidate ? It's the end of the match list.
      //
      if(candidate.Empty())
	  return WORD_WALK_ATEND;

      found.Key() = candidate;

      SetInfo();

      if(permutation.ExcludedCount() > 0) {
	if((ret = Seek(candidate)) != OK)
	  return ret;

	//
	// Restart loop if candidate matches an excluded cursor.
	//
	for(i = 0; i < cursors_length && match_ok; i++) {
	  if(!permutation.Excluded(i))
	    continue;
	  WordTree& cursor = *(cursors[i]);

	  switch((ret = cursor.WalkNext()) & WORD_WALK_RESULT_MASK) {
	  case WORD_WALK_ATEND:
	    //
	    // This excluded cursor can't match the candidate, fine.
	    //
	    break;
	  case OK:
	    //
	    // This excluded cursor matches candidate therefore it's
	    // not a valid candidate. Restart search with this candidate
	    // as the constraint.
	    //
	    if(key_semantic.DocumentCompare(candidate, cursor.GetFound().Key()) == 0) {
	      constraint = candidate;
	      match_ok = 0;
	    }
	    break;
	  default:
	    return ret;
	    break;
	  }
	  
	}
      }

      cursor_get_flags = DB_NEXT;

  } while(!match_ok);

  constraint = candidate;

  return OK;
}

// ************************* WordTreeAnd implementation ********************

// ************************* WordTreeNear implementation ********************

// ************************* WordTreeMandatory implementation ***************

// ************************* WordTreeNot implementation ***************

