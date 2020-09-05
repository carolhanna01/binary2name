//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordTree.h,v 1.10 2001/07/20 10:37:53 loic Exp $
//
//
// NAME
// 
// Base class for query resolution nodes
//
// SYNOPSIS
//
// #include <WordTree.h>
//
// class WordTreeMethod : public WordTree {
// ...
// };
//
// DESCRIPTION
//
// The WordTree class is derived from the WordCursor class and implement
// the basic operations and data structures needed for query resolution.
// It is the common base class of all the classes that actually implement
// a query resolution. The derived classes must be implemented to follow
// the WordCursor semantic for Walk* operations.
//
// 
// END
// 

#ifndef _WordTree_h
#define _WordTree_h

#include <WordList.h>

#include <WordKeySemantic.h>
#include <WordPermute.h>
#include <WordCursorOne.h>
#include <WordResults.h>

#define WORD_WALK_REDO		0x1000
#define WORD_WALK_RESTART	0x2000
#define WORD_WALK_NEXT		0x4000
#define WORD_WALK_END_CACHE	0x8000

//
// Return values of CursorsObeyProximity method
//
#define WORD_SEARCH_NOPROXIMITY	1

//
// operand values
//
#define WORD_TREE_OR		1
#define WORD_TREE_AND		2
#define WORD_TREE_NEAR		3
#define WORD_TREE_OPTIONAL	4
#define WORD_TREE_LITERAL	5
#define WORD_TREE_MANDATORY	6
#define WORD_TREE_NOT		7

#define WORD_TREE_OP_SIZE	20

//
// Default proximity is to search for adjacent words in order
//
#ifndef WORD_SEARCH_DEFAULT_PROXIMITY
#define WORD_SEARCH_DEFAULT_PROXIMITY (-1)
#endif /* WORD_SEARCH_DEFAULT_PROXIMITY */

class WordTreeArg {
 public:
  WordTreeArg(WordList *words, int uniq, int has_uniq, int nproximity, int *document, int document_length, int location) {
    _words = words;
    _uniq = uniq;
    _has_uniq = has_uniq;
    _nproximity = nproximity;
    _document = document;
    _document_length = document_length;
    _location = location;
    _realm = 0;
  }

  WordList *_words;
  int _uniq;
  int _has_uniq;
  int _nproximity;
  int *_document;
  int _document_length;
  int _location;
  int _realm;
};

class WordTree : public WordCursorOne {
public:
  WordTree(WordList* words) :
    WordCursorOne(words),
    key_semantic(words->GetContext())
  {
    proximity = 0;
    has_uniq = 0;
    results = 0;
    verbose = 0;
  }

  virtual int ContextSaveList(StringList& list) const {
    return OK;
  }

  virtual int ContextRestoreList(StringList& list) {
    return OK;
  }

  //-
  // Initialize the object. <b>words</b> is used to initialize the 
  // WordCursor base class, <b>document, document_length</b> and 
  // <b>location</b> are used to initialize the WordKeySemantic data
  // member. The <b>uniq</b> is the WordKey field position used by
  // the WordKeySemantic::DocumentNext function. The <b>nproximity</b>
  // is the proximity factor used by the WordKeySemantic::LocationCompare
  // method.
  // Return OK on success, NOTOK on failure.
  //
  virtual int Prepare(WordTreeArg& arg) {
    int ret;
    proximity = arg._nproximity;
    has_uniq = arg._has_uniq;
    if((ret = key_semantic.Initialize(arg._document, arg._document_length, arg._location, arg._uniq)) != OK)
      return ret;
    key_semantic.Verbose(verbose);
    WordKey key(arg._words->GetContext());
    //
    // Set scope
    //
    if(!scope.empty()) {
      if(key.Set(scope) != OK) {
	fprintf(stderr, "WordTree::Prepare: setting scope %s failed\n", (char*)scope);
	return NOTOK;
      }
    }
    //
    // Set word
    //
    if(!search.empty()) {
      unsigned int wordid = WORD_DICT_SERIAL_INVALID;
      if(arg._words->Dict()->SerialExists(search, wordid) != OK)
	return NOTOK;
      key.Set(WORD_KEY_WORD, wordid);
    }
    return WordCursorOne::Initialize(arg._words, key, 0, 0, HTDIG_WORDLIST_WALKER);
  }

  virtual int WalkNextExclude(const WordKey& key);

  virtual int Bounds(const WordKey& bottom, const WordKey& top) = 0;

  //-
  // Return a copy of the last document found.
  //
  WordKey GetDocument() {
    WordKey found(words->GetContext());
    key_semantic.DocumentSet(GetFound().Key(), found);
    return found;
  }

  //-
  // Change the search criterion to match realm
  //
  virtual int SetRealm(const WordKey& realm) {
    key_semantic.RealmCopy(realm, GetSearch());
    return OK;
  }
  //-
  // Undefine the realm
  //
  virtual int UndefinedRealm() {
    key_semantic.RealmUndefined(GetSearch());
    return OK;
  }
  //-
  // Set the realm to 0
  //
  int ClearRealm() {
    WordKey realm(words->GetContext());
    key_semantic.RealmClear(realm);
    return SetRealm(realm);
  }

  //-
  // Returns estimate of the match count.
  //
  virtual int Count(unsigned int& count) const { return Noccurrence(count); }

  virtual int Noccurrence(unsigned int& noccurrence) const { return words->Noccurrence(search, noccurrence); }

  //-
  // Store in the <i>info</i> data member textual information about
  // the latest match found.
  //
  virtual void SetInfo() { info = search; }

  //-
  // Return a copy of the <i>info</i> data member. Should be 
  // called after SetInfo().
  //
  String GetInfo() { return info; }

  //-
  // Set the list of documents that must be ignored.
  //
  virtual inline int SetResults(WordResults* nresults) { results = nresults; return OK; }
  virtual inline WordResults* GetResults() { return results; }

  //-
  // Sort WordTree data members (if any) in ascending frequency order.
  // Return OK on success, NOTOK on failure.
  //
  virtual int AscendingFrequency() { return OK; }

  //-
  // Delete WordTree data members (if any) that have a zero frequency.
  // The number of data members deleted is returned in <b>stripped</b>.
  // Return OK on success, NOTOK on failure.
  //
  virtual int StripNonExistent(unsigned int& stripped, unsigned int& killed_mandatory) {
    stripped = 0;
    killed_mandatory = 0;
    return OK;
  }

  //-
  // Optimize the search expression. It first asks the expression
  // (member expr) to optimize itself, then it does some optimization
  // that cannot be done by the expression itself.
  //
  static int TopLevelOptimize(WordTree*& expr);

  inline int Verbose(int verbosity) { return verbose = verbosity; }

  //
  // Input
  //
  //-
  // Proximity factor. See WordKeySemantic::LocationCompare.
  //
  int proximity;
  //-
  // Reduce the result set so that only one document per server 
  // is returned. See WordKeySemantic::DocumentNext.
  //
  int has_uniq;
  //-
  // Semantic of the WordKey object.
  //
  WordKeySemantic key_semantic;
  //-
  // Textual representation of the search scope.
  //
  String scope;
  //-
  // Original search criterion that may be different from the 
  // WordCursor::searchKey data member.
  //
  String search;

  //
  // Internal state
  //
  //-
  // Textual information about the latest match.
  //
  String info;
  //-
  // A list of documents that must be ignored
  //
  WordResults *results;
  int verbose;
};

// ************************* WordTreeLiteral implementation ****************

class WordTreeLiteral : public WordTree {
public:
  //-
  // Constructor. The search criterion is <b>string</b> and the
  // scope is <b>nscope.</b>.
  //
  WordTreeLiteral(WordList* words, const char* string, int string_length, const char* nscope = "") :
    WordTree(words),
    bottom_document(words->GetContext()),
    top_document(words->GetContext())
  {
    search.set(string, string_length);
    scope.set((char*)nscope);
  }

  //-
  // Returns WORD_TREE_LITERAL.
  //
  int IsA() const { return WORD_TREE_LITERAL; }

  virtual int WalkInit();
  virtual int WalkRewind();
  //-
  // Only return a match for each distinct document.
  //
  virtual int WalkNext();
  virtual int Seek(const WordKey& patch);

  //-
  // If scope is set the <b>bufferout</b> is filled with
  // <pre>
  // ( word "scope" )
  // </pre>
  // otherwise the <b>bufferout</b> only contains the word.
  //
  virtual int Get(String& bufferout) const;
 
  virtual int Bounds(const WordKey& bottom, const WordKey& top) {
    bottom_document = bottom;
    top_document = top;
    return OK;
  }

protected:
  WordKey bottom_document;
  WordKey top_document;
};

// ************************* WordTreeOperand implementation ****************
//
// NAME
// 
// Base class for boolean query resolution nodes
//
// SYNOPSIS
//
// #include <WordTree.h>
//
// class WordTreeMethod : public WordTreeOperand {
// ...
// };
//
// DESCRIPTION
//
// The WordTreeOperand class is derived from WordTree and implemet
// the basic operations and data structures needed for query resultion
// of boolean operators. It contains a list of WordTree objects (the
// operands or cursors) and redefine the basic WordCursor methods
// to operate on all of them according to the logic defined by the
// derived class.
//
//
// END
//

//
// Helper for debugging that returns the string representation
// of the return codes.
//

class WordTreeOperand : public WordTree
{
public:
  //-
  // Constructor. The scope is <b>nscope</b>.
  //
  WordTreeOperand(WordList* words, const char* nscope) :
    WordTree(words),
    pos(words->GetContext())
  {
    scope.set((char*)nscope);
  }
  //-
  // Free the objects pointed by <i>cursors</i> with delete as well
  // as the <i>cursors</i> array itself with delete [].
  //
  virtual ~WordTreeOperand();

  virtual void Clear() {
    cursors = 0;
    cursors_length = 0;
    WordCursorOne::Clear();
  }

  //-
  // Recursively call Optimize on each <i>cursors</i>.
  //
  virtual int Optimize();

  //-
  // Change the <i>permutation</i> data member ignore mask according
  // to WORD_TREE_MANDATORY and WORD_TREE_NOT nodes found in 
  // <i>cursors</i>. MANDATORY and NOT nodes are reduced (replaced
  // by their first child cursor. For each MANDATORY and NOT nodes
  // the bit (see WordExcludeMask for information) 
  // corresponding to their position is ignored (set in the <b>ignore</b>
  // argument of the WordExcludeMask::Initialize function. For NOT
  // nodes, the bit corresponding to their position is set in 
  // the <b>ignore_mask</b> of the WordExcludeMask::Initialize function
  // (i.e. implementing a <i>not</i> operation).
  // The <b>proximity</b> argument may be WORD_PERMUTE_PROXIMITY_TOGGLE or 
  // WORD_PERMUTE_PROXIMITY_NO.
  // Returns OK on success, NOTOK on failure.
  //
  int OptimizeOr(int proximity);

  virtual int ContextSave(String& buffer) const {
    StringList list;
    int ret;
    if((ret = ContextSaveList(list)) != OK)
      return ret;

    buffer.trunc();
    String* element;
    list.Start_Get();
    while((element = (String*)list.Get_Next())) {
      buffer << (*element) << ';';
    }
    //
    // Trim last ;
    //
    buffer.chop(1);

    return OK;
  }

  virtual int ContextSaveList(StringList& list) const {
    //
    // Apply to each cursor
    //
    unsigned int i;
    for(i = 0; i < cursors_length; i++)
      if(cursors[i]->ContextSaveList(list) == NOTOK)
	return NOTOK;
    return OK;
  }

  virtual int ContextRestore(const String& buffer) {
    if(!buffer.empty()) {
      StringList list(buffer, ";");
      return ContextRestoreList(list);
    } else {
      return OK;
    }
  }

  virtual int ContextRestoreList(StringList& list) {
    //
    // Apply to each cursor
    //
    unsigned int i;
    for(i = 0; i < cursors_length; i++)
      if(cursors[i]->ContextRestoreList(list) == NOTOK)
	return NOTOK;
    return OK;
  }

  virtual int SetResults(WordResults* nresults) {
    //
    // Apply to each cursor
    //
    unsigned int i;
    for(i = 0; i < cursors_length; i++)
      if(cursors[i]->SetResults(nresults) == NOTOK)
	return NOTOK;
    return WordTree::SetResults(nresults);
  }

  //-
  // Recursively call WalkInit on each <i>cursors</i>.
  //
  virtual int WalkInit();
  //-
  // Recursively call WalkRewind on each <i>cursors</i>.
  // Reset the <i>pos</i> data member with WordKeySemantic::DocumentClear.
  //
  virtual int WalkRewind();
  //-
  // Recursively call WalkFinish on each <i>cursors</i>.
  //
  virtual int WalkFinish();
  //-
  // Recursively call Seek on each <i>cursors</i>.
  // Save the <b>patch</b> argument in the <i>pos</i> data
  // member.
  //
  virtual int Seek(const WordKey& patch);

  //-
  // The number of occurrence of a WordTreeOperand is the sum of the
  // number of occurrence of each term.
  //
  virtual int Noccurrence(unsigned int& noccurrence) const {
    noccurrence = 0;
    unsigned int i;
    for(i = 0; i < cursors_length; i++) {
      unsigned int frequency;
      if(cursors[i]->Noccurrence(frequency) != OK)
	return NOTOK;
      noccurrence += frequency;
    }
    return OK;
  }

  //-
  // The <b>bufferout</b> argument is filled with a lisp like representation
  // of the tree starting at this node.
  //
  virtual int Get(String& bufferout) const;
  //-
  // Call Prepare on each <i>cursors</i>. Set the <i>search</i> member
  // with an textual representation of the tree starting at this node.
  //
  virtual int Prepare(WordTreeArg& arg) {
    int ret;
    if((ret = WordTree::Prepare(arg)) != OK)
      return ret;
    unsigned int i;
    for(i = 0; i < cursors_length; i++) {
      cursors[i]->Verbose(verbose);
      if((ret = cursors[i]->Prepare(arg)) != OK)
	return ret;
    }
    return Get(search);
  }

  virtual int Bounds(const WordKey& bottom, const WordKey& top) {
    int ret;
    unsigned int i;
    for(i = 0; i < cursors_length; i++) {
      if((ret = cursors[i]->Bounds(bottom, top)) != OK)
	return ret;
    }
    return OK;
  }

  //-
  // The current cursor offset (set by Seek for instance). It
  // duplicates the function of the WordCursor <i>key</i> data member
  // because the data type is different (WordKey instead of String).
  //
  WordKey pos;
  //-
  // Sub nodes array.
  //
  WordTree** cursors;
  //-
  // Number of valid entries in the <i>cursors</i> member.
  //
  unsigned int cursors_length;
  //-
  // Permutation generator with proximity toggle
  //
  WordPermute permutation;
};

// ************************* WordTreeOptional implementation ****************

class WordTreeOptional : public WordTreeOperand {
 public:
  WordTreeOptional(WordList* words, const char* nscope) : WordTreeOperand(words, nscope) { }

  //-
  // Return WORD_TREE_OPTIONAL
  //
  virtual int IsA() const { return WORD_TREE_OPTIONAL; }

  virtual int Optimize();

  virtual int ContextSaveList(StringList& list) const;

  virtual int ContextRestoreList(StringList& list);

  //-
  // Multipass walk of the occurrences according to the <i>permutation</i>
  // data member specifications. First search for documents containing
  // all occurrences near to each other. Then documents that
  // contain all occurrences far appart. Then ignore the most frequent 
  // search criterion and search for documents that contain all the others
  // near to each other. The logic goes on until there only remains the
  // most frequent word. 
  //
  virtual int WalkNext();
  //-
  // Only seek the first non excluded cursor. The implementation
  // of WalkNext makes it useless to seek the others.
  //
  virtual int Seek(const WordKey& position);

  virtual int Prepare(WordTreeArg& arg) {
    int ret;
    if((ret = permutation.Initialize(cursors_length, 0, 0, WORD_PERMUTE_PROXIMITY_TOGGLE)) != OK)
      return ret;
    permutation.Verbose(verbose);
    return WordTreeOperand::Prepare(arg);
  }

  virtual void SetInfo();

  virtual int UseProximity() const { return WORD_PERMUTE_PROXIMITY_TOGGLE; }

  virtual int UsePermutation() const { return 1; }

  virtual int SetRealm(const WordKey& realm) {
    unsigned int i;
    int ret = OK;
    for(i = 0; i < cursors_length; i++) {
      if(permutation.Excluded(i)) {
	//
	// An excluded cursor is not restricted to the current realm.
	// The document is excluded no matter where the word appears
	// in the document.
	//
	if((ret = cursors[i]->UndefinedRealm()) != OK)
	  return ret;
      } else {
	if((ret = cursors[i]->SetRealm(realm)) != OK)
	  return ret;
      }
    }
    return OK;
  }

  virtual int Count(unsigned int& count) const;

  //-
  // Returns true if all cursors must have a frequency > 0, false otherwise.
  // 
  virtual int AllOrNothing() const { return 0; }

  //-
  // Comparison between <b>cursor</b> and <b>constraint</b> is made
  // with WordKeySemantic::LocationCompare using the <b>proximity</b>
  // argument. If <b>master</b> is NULL it is set to point to <b>
  // <b>cursor</b>.
  //
  // Return WORD_WALK_NEXT if <b>cursor</b> is at <b>constraint</b> and
  // set <b>constraint</b> if <b>cursor</b> is <b>master</b>.
  //
  // Return WORD_WALK_REDO if <b>cursor</b> is above <b>constraint</b> and
  // call cursor.WalkNext().
  //
  // Return WORD_WALK_RESTART if <b>cursor</b> is below <b>constraint</b> and
  // set <b>constraint</b> from <b>cursor</b> using
  // WordKeySemantic::DocumentSet if <b>cursor</b> is not <b>master</b> 
  // otherwise also set location of <b>constraint</b> using 
  // WordKeySemantic::LocationSet and call WordKeySemantic::LocationNext
  // on <b>constraint.</b>
  //
  // Return WORD_WALK_ATEND if no more match possible.
  //
  // Return NOTOK on failure.
  //
  int SearchCursorNear(WordTree& cursor, WordTree*& master, WordKey& constraint, int proximity);
  //-
  // Comparison between <b>cursor</b> and <b>document</b> is made
  // with WordKeySemantic::DocumentCompare.
  //
  // Return WORD_WALK_NEXT if <b>cursor</b> is above <b>document.</b>
  //
  // Return WORD_WALK_REDO if <b>cursor</b> is below <b>document</b>
  // and call cursor.WalkNext().
  //
  // Return WORD_WALK_RESTART if <b>cursor</b> is at <b>document</b>
  // and call WordKeySemantic::DocumentNext method on <b>document.</b>
  //
  // Return WORD_WALK_ATEND if no more match possible.
  //
  // Return NOTOK on failure.
  //
  int SearchCursorNot(WordTree& cursor, WordKey& document);
  //-
  // Comparison between <b>cursor</b> and <b>document</b> is made
  // with WordKeySemantic::DocumentCompare.
  //
  // Return WORD_WALK_NEXT if <b>cursor</b> is at <b>document.</b>.
  //
  // Return WORD_WALK_REDO if <b>cursor</b> is below <b>document</b>
  //
  // Return WORD_WALK_RESTART if <b>cursor</b> is above <b>document</b>
  // and call WordKeySemantic::DocumentNext method on <b>document.</b>
  //
  // Return WORD_WALK_ATEND if no more match possible.
  //
  // Return NOTOK on failure.
  // 
  //
  int SearchCursorAnd(WordTree& cursor, WordKey& document, WordExclude& permutation);

  //-
  // Sort the <i>cursors</i> in ascending frequency order using the
  // Noccurrence method on each cursor.
  // Return OK on success, NOTOK on failure.
  //
  virtual int AscendingFrequency();
  //-
  // Delete all elements of the <i>cursors</i> array that have a 
  // zero frequency. The <i>cursors</i> array is shrinked and the 
  // <i>cursors_length</i> set accordingly. Returns the number of
  // deletions in the <b>stripped</i> argument. 
  // Return OK on success, NOTOK on failure.
  //
  virtual int StripNonExistent(unsigned int& stripped, unsigned int& killed_mandatory);
};

// ************************* WordTreeOr implementation ********************

class WordTreeOr : public WordTreeOperand {
 public:
  WordTreeOr(WordList* words, const char* nscope) : WordTreeOperand(words, nscope) { }

  //-
  // Return WORD_TREE_OR
  //
  virtual int IsA() const { return WORD_TREE_OR; }

  virtual int Optimize();

  virtual int ContextSaveList(StringList& list) const;

  virtual int ContextRestoreList(StringList& list);

  virtual void SetInfo();

  virtual int WalkNext();

  virtual int UsePermutation() const { return 0; }

  virtual int UseProximity() const { return WORD_PERMUTE_PROXIMITY_NO; }
};

// ************************* WordTreeAnd implementation ********************

class WordTreeAnd : public WordTreeOptional {
 public:
  WordTreeAnd(WordList* words, const char* nscope) : WordTreeOptional(words, nscope) { }

  //-
  // Return WORD_TREE_AND
  //
  virtual int IsA() const { return WORD_TREE_AND; }

  virtual int UsePermutation() const { return 0; }

  virtual int UseProximity() const { return WORD_PERMUTE_PROXIMITY_NO; }

  virtual int AllOrNothing() const { return 1; }
};

// ************************* WordTreeNear implementation ********************

class WordTreeNear : public WordTreeOptional {
 public:
  WordTreeNear(WordList* words, const char* nscope) : WordTreeOptional(words, nscope) { }

  //-
  // Return WORD_TREE_NEAR
  //
  virtual int IsA() const { return WORD_TREE_NEAR; }

  virtual int UsePermutation() const { return 0; }

  virtual int UseProximity() const { return WORD_PERMUTE_PROXIMITY_ONLY; }

  virtual int AllOrNothing() const { return 1; }

  virtual int Prepare(WordTreeArg& arg) {
    int proximity_saved = proximity ? proximity : arg._nproximity;
    int ret = WordTreeOptional::Prepare(arg);
    proximity = proximity_saved;
    return ret;
  }

};

// ************************* WordTreeMandatory implementation ***************

class WordTreeMandatory : public WordTreeOperand {
 public:
  WordTreeMandatory(WordList* words, const char* nscope) : WordTreeOperand(words, nscope) { }

  //-
  // Return WORD_TREE_MANDATORY
  //
  virtual int IsA() const { return WORD_TREE_MANDATORY; }
};

// ************************* WordTreeNot implementation ***************

class WordTreeNot : public WordTreeOperand {
 public:
  WordTreeNot(WordList* words, const char* nscope) : WordTreeOperand(words, nscope) { }

  //-
  // Return WORD_TREE_NOT
  //
  virtual int IsA() const { return WORD_TREE_NOT; }
};

#endif /* _WordTree_h */
