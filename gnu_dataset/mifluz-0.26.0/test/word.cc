//
// word.cc
//
// word: Implement tests for the word database related classes.
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: word.cc,v 1.45 2001/06/29 14:14:08 loic Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

// If we have this, we probably want it.
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include "WordKey.h"
#include "WordList.h"
#include "WordListOne.h"
#include "WordContext.h"
#include "Configuration.h"
#include "StringList.h"
#include "WordDead.h"

static WordContext*	context = 0;

typedef struct
{
    int key;
    int list;
    int skip;
    int compress;
    int env;
    int dead;
} params_t;

static void usage();
static void doword(params_t* params);
static void dolist(params_t* params);
static void dokey(params_t* params);
static void doskip(params_t* params);
static void doenv(params_t* params);
static void dodead(params_t* params);
static void pack_show_wordreference(const WordReference& wordRef);
static void pack_show_key(const String& key);

static int verbose = 0;

// *****************************************************************************
// int main(int ac, char **av)
//

int main(int ac, char **av)
{
  int			c;
  params_t		params;

  params.key = 0;
  params.list = 0;
  params.skip = 0;
  params.env = 0;
  params.compress = 0;
  params.dead = 0;

  while ((c = getopt(ac, av, "ve:klszd")) != -1)
    {
      switch (c)
	{
	case 'v':
	  verbose++;
	  break;
	case 'e':
	  params.env = atoi(optarg);
	  break;
	case 'k':
	  params.key = 1;
	  break;
	case 'l':
	  params.list = 1;
	  break;
	case 's':
	  params.skip = 1;
	  break;
	case 'z':
	  params.compress = 1;
	  break;
	case 'd':
	  params.dead = 1;
	  break;
	case '?':
	  usage();
	  break;
	}
    }

  doword(&params);

  return 0;
}

//
// mifluz.conf structure
//
#define WORD_WORD	0
#define WORD_DOCID	1
#define WORD_FLAGS	2
#define WORD_LOCATION	3

static void doword(params_t* params)
{
  context = new WordContext();

  Configuration &config = context->GetConfiguration();

  if(params->compress) {
    config.Add("wordlist_compress", "true");
  }
  if(verbose > 2) {
    String tmp;
    tmp << (verbose - 2);
    config.Add("wordlist_verbose", tmp);
  }
  if(params->env) {
    config.Add("wordlist_env_share", "true");
    config.Add("wordlist_env_dir", ".");
  }

  context->ReInitialize();

  if(params->key) {
    if(verbose) fprintf(stderr, "Test WordKey class\n");
    dokey(params);
  }

  if(params->list) {
    if(verbose) fprintf(stderr, "Test WordList class\n");
    dolist(params);
  }

  if(params->skip) {
    if(verbose) fprintf(stderr, "Test WordList::SkipUselessSequentialWalking method\n");
    doskip(params);
  }

  if(params->env) {
    if(verbose) fprintf(stderr, "Test WordList with shared env\n");
    doenv(params);
  }

  if(params->dead) {
    if(verbose) fprintf(stderr, "Test WordDead\n");
    dodead(params);
  }
}

#define DOCID_FIRST (1 | 0xf0000000)

static void dolist(params_t*)
{
  static char* word_list[] = {
    "The",	// DocID = 1
    "quick",	// DocID = 2
    "brown",	// DocID = 3
    "fox",	// DocID = 4
    "jumps",	// DocID = 5
    "over",	// DocID = 6
    "the",	// DocID = 7
    "lazy",	// DocID = 8
    "dog",	// DocID = 9
    0
  };

  //
  // Most simple case. Insert a few words and
  // search them, using exact match.
  //
  {

    // setup a new wordlist
    WordList *words = context->List();
    words->Open("test", O_RDWR);


    // create entries from word_list
    WordReference wordRef(context);
    wordRef.Key().Set(WORD_FLAGS, 67);
    unsigned int wordid = 0;
    unsigned int location = 0;
    unsigned int anchor = 0;
    unsigned int docid = DOCID_FIRST;

    if(verbose) fprintf(stderr, "Inserting\n");

    for(char** p = word_list; *p; p++) {
      if(verbose > 4) fprintf(stderr, "inserting word: %s\n", *p);
      wordRef.SetWord(*p);
      words->Dict()->Serial(*p, wordid);
      wordRef.Key().Set(WORD_WORD, wordid);
      wordRef.Key().Set(WORD_DOCID, docid);
      wordRef.Key().Set(WORD_LOCATION, location);
      wordRef.Record().info.data = anchor;
      if(verbose > 1) fprintf(stderr, "%s\n", (char*)wordRef.Get());
      if(verbose > 2) pack_show_wordreference(wordRef);
      words->Override(wordRef);
      location += strlen(*p);
      anchor++;
      docid++;
    }
    words->Close();

    location = anchor = 0;
    docid = DOCID_FIRST;

    if(verbose) fprintf(stderr, "Searching\n");

    // reopen wordlist
    words->Open("test", O_RDONLY);
    //  check if each word (from word_list) is there
    for(char** p = word_list; *p; p++)
    {
      // recreate wordref from each word
      wordRef.SetWord(*p);
      words->Dict()->SerialExists(*p, wordid);
      if(wordid == WORD_DICT_SERIAL_INVALID) {
	fprintf(stderr, "dolist: no wordid found for %s\n", *p);
	exit(1);
      }
      wordRef.Key().Set(WORD_WORD, wordid);
      wordRef.Key().Set(WORD_LOCATION, location);
      wordRef.Record().info.data = anchor;
      wordRef.Key().Set(WORD_DOCID, docid);

      location += strlen(*p);
      anchor++;
      docid++;

      //
      // Skip first word because we don't want to deal with upper/lower case at present.
      //
      if(p == word_list) continue;

      // check if wordref is in wordlist
      if(verbose) fprintf(stderr, "searching for %s ... ", *p);
      if(verbose > 2) pack_show_wordreference(wordRef);
      if(verbose > 1) fprintf(stderr, "%s\n", (char*)wordRef.Get());
      // find matches in wordlist
      List *result = (*words)[wordRef];
      if(!result) {
	fprintf(stderr, "dolist: words[wordRef] returned null pointer\n");
	exit(1);
      }
      result->Start_Get();
      int count = 0;
      WordReference* found;
      // loop through found matches
      while((found = (WordReference*)result->Get_Next()))
      {
	if(wordRef.Key().Get(WORD_WORD) != found->Key().Get(WORD_WORD))
	{
	  fprintf(stderr, "dolist: simple: for word %s expected %d, got %d\n", (char*)wordRef.GetWord(), wordRef.Key().Get(WORD_WORD), found->Key().Get(WORD_WORD));
	  exit(1);
	}
	count++;
      }
      if(count != 1) {
	fprintf(stderr, "dolist: simple: searching %s, got %d matches instead of 1\n", (char*)wordRef.GetWord(), count);
  	exit(1);
      }
      if(verbose) fprintf(stderr, "done\n");

      delete result;

    }
    delete words;
  }
  //
  // Search by prefix
  //
  {
    WordList *words = context->List();
    words->Open("test", O_RDONLY);
    List *result = words->Prefix(words->WordExists("t <UNDEF> <UNDEF> <UNDEF>"));

    if(!result) {
      fprintf(stderr, "dolist: words->Prefix returned null pointer\n");
      exit(1);
    }
    if(result->Count() != 2) {
      fprintf(stderr, "dolist: words->Prefix expected 2 matches got %d\n", result->Count());
      exit(1);
    }
    WordReference* found;
    found = (WordReference*)result->Nth(0);
    if(found->GetWord().nocase_compare("the")) {
      fprintf(stderr, "dolist: expected 'the' and got '%s'\n", (char*)found->GetWord().get());
      exit(1);
    }
    
    delete result;
    delete words;
  }
  exit(0);
  //
  // Print all records as sorted within Berkeley DB with number
  // of occurrences.
  //
  if(verbose) {
    WordList *words = context->List();
    words->Open("test", O_RDWR);

    List *result = words->Words();
    if(result == 0) {
      fprintf(stderr, "dolist: getting all words failed\n");
      exit(1);
    }
    result->Start_Get();
    int count = 0;
    String* found;
    while((found = (String*)result->Get_Next())) {
      unsigned int noccurrence;
      words->Noccurrence(*found, noccurrence);
      fprintf(stderr, "%s (%d)\n", (char*)(*found), noccurrence);
      count++;
    }
    if(count != 8) {
      fprintf(stderr, "dolist: getting all words, got %d matches instead of 8\n", count);
      exit(1);
    }

    delete result;
    delete words;
  }
  //
  // Search all occurrences of 'the'
  //
  {
    WordList *words = context->List();
    words->Open("test", O_RDWR);

    WordReference wordRef(context);
    wordRef.SetWord("the");
    unsigned int wordid;
    words->Dict()->SerialExists("the", wordid);
    wordRef.Key().Set(WORD_WORD, wordid);

    unsigned int noccurrence;
    if(words->Noccurrence(wordRef.GetWord(), noccurrence) != OK) {
      fprintf(stderr, "dolist: get ref count of 'the' failed\n");
      exit(1);
    } else if(noccurrence != 2) {
      fprintf(stderr, "dolist: get ref count of 'the' failed, got %d instead of 2\n", noccurrence);
      exit(1);
    }
    List *result = (*words)[wordRef];
    result->Start_Get();
    int count = 0;
    WordReference* found;
    while((found = (WordReference*)result->Get_Next())) {
	if(wordRef.Key().Get(WORD_WORD) != found->Key().Get(WORD_WORD)) {
	  fprintf(stderr, "dolist: for word %s expected %d, got %d\n", (char*)wordRef.GetWord(), wordRef.Key().Get(WORD_WORD), found->Key().Get(WORD_WORD));
	  exit(1);
	}
	if(verbose) fprintf(stderr, "%s\n", (char*)found->Get());
	count++;
    }
    if(count != 2) {
      fprintf(stderr, "dolist: searching occurrences of '%s', got %d matches instead of 2\n", (char*)wordRef.GetWord(), count);
      exit(1);
    }

    delete result;
    delete words;
  }
  //
  // Delete all occurrences of 'the'
  //
  {
    WordList *words = context->List();
    words->Open("test", O_RDWR);

    WordReference wordRef(context, "the");
    unsigned int wordid;
    words->Dict()->SerialExists("the", wordid);
    wordRef.Key().Set(WORD_WORD, wordid);

    int count;
    if((count = words->WalkDelete(wordRef)) != 2) {
      fprintf(stderr, "dolist: delete occurrences of 'the', got %d deletion instead of 2\n", count);
      exit(1);
    }

    List* result = (*words)[wordRef];
    if(result->Count() != 0) {
      fprintf(stderr, "dolist: unexpectedly found 'the' \n");
      exit(1);
    }
    delete result;

    unsigned int noccurrence;
    if(words->Noccurrence(wordRef.GetWord(), noccurrence) != OK) {
      fprintf(stderr, "dolist: get ref count of 'the' failed\n");
      exit(1);
    } else if(noccurrence != 0) {
      fprintf(stderr, "dolist: get ref count of 'the' failed, got %d instead of 0\n", noccurrence);
      exit(1);
    }
    delete words;
  }
  //
  // Delete all words in document 5 (only one word : jumps)
  //
  {
    WordList *words = context->List();
    words->Open("test", O_RDWR);

    WordReference wordRef(context);
    wordRef.Key().Set(WORD_DOCID, 5);
    int count;
    if((count = words->WalkDelete(wordRef)) != 1) {
      fprintf(stderr, "dolist: delete occurrences in DocID 5, %d deletion instead of 1\n", count);
      exit(1);
    }

    wordRef.Clear();
    wordRef.SetWord("jumps");
    unsigned int wordid;
    words->Dict()->SerialExists("jumps", wordid);
    wordRef.Key().Set(WORD_WORD, wordid);
    List* result = (*words)[wordRef];
    if(result->Count() != 0) {
      fprintf(stderr, "dolist: unexpectedly found 'jumps' \n");
      exit(1);
    }
    delete result;

    unsigned int noccurrence;
    if(words->Noccurrence(wordRef.GetWord(), noccurrence) != OK) {
      fprintf(stderr, "dolist: get ref count of 'jumps' failed\n");
      exit(1);
    } else if(noccurrence != 0) {
      fprintf(stderr, "dolist: get ref count of 'jumps' failed, got %d instead of 0\n", noccurrence);
      exit(1);
    }
    delete words;
  }
}

//
// See WordKey.h
// Tested: Pack, Unpack, Compare (both forms), accessors, meta information
//
static void
dokey(params_t* params)
{
  static char *key_descs[] = {
    "Word 16/DocID 5/Flags 8/Location 19",
    "Word 16/DocID 3/Location 2/Flags 11",
    "Word 16/DocID 3/Flags 8/Location 5",
    "Word 16/DocID 3/Flags 14/Location 7",
    "Word 16/DocID 3/Flags 9/Location 7/Foo1 13/Foo2 16",
    "Word 32/DocID 32/Flags 32/Location 32",
    0,
  };
  char** key_desc;

  for(key_desc = key_descs; *key_desc; key_desc++) {
    context->GetConfiguration().Add("wordlist_wordkey_description", *key_desc);
    context->ReInitialize();

    WordKey word(context);
    int j;
    for(j = 0; j < word.NFields(); j++) {
      WordKeyNum value = (0xfededede & word.MaxValue(j));
      word.Set(j, value);
    }
    if(verbose > 1) fprintf(stderr, "WORD: %s\n", (char*)word.Get());

    String packed;
    word.Pack(packed);

    WordKey other_word(context);
    other_word.Unpack(packed);
    if(verbose > 1) fprintf(stderr, "OTHER_WORD: %s\n", (char*)other_word.Get());

    int failed = 0 ;
    for(j = 0; j < word.NFields(); j++) {
      if(word.Get(j) != other_word.Get(j)) {
	failed = 1;
	break;
      }
    }

    if(failed) {
      fprintf(stderr, "Original and packed/unpacked not equal\n");
      fprintf(stderr, "WORD: %s\n", (char*)word.Get());
      pack_show_key(packed);
      fprintf(stderr, "OTHER_WORD: %s\n", (char*)other_word.Get());
      exit(1);
    }

    //
    // Compare in packed form
    //
    if(!word.PackEqual(other_word))
      {
	fprintf(stderr, "dokey: %s not equal (object compare)\n", *key_desc);
	exit(1);
      }

    //
    // Pack the other_word
    //
    String other_packed;

    other_word.Pack(other_packed);
    //
    // The two (word and other_word) must compare equal
    // using the alternate comparison (fast) interface.
    //
    if(WordKey::Compare(context, packed, other_packed) != 0) {
      fprintf(stderr, "dokey: %s not equal (fast compare)\n", *key_desc);
      exit(1);
    }

    word.Set(WORD_WORD,50);
    word.Set(WORD_DOCID,1);
    other_word.Set(WORD_WORD,50);
    word.Pack(packed);
    //
    // Add one to the word, they must not compare equal and
    // the difference must be minus one.
    //
    other_word.Set(WORD_WORD,51);
    other_word.Pack(other_packed);
    {
      int ret;
      if((ret = WordKey::Compare(context, packed, other_packed)) != -1)
	{
	  fprintf(stderr, "%s\n", (char*)word.Get());
	  fprintf(stderr, "%s\n", (char*)other_word.Get());
	  fprintf(stderr, "dokey: %s different length, expected -1 got %d\n", *key_desc, ret);
	  exit(1);
	}
    }
    other_word.Set(WORD_WORD,50);

    //
    // Substract one to the first numeric field
    // The difference must be one.
    //
    other_word.Set(WORD_DOCID,word.Get(WORD_DOCID) - 1);
    other_word.Pack(other_packed);
    {
      int ret;
      if((ret = WordKey::Compare(context, packed, other_packed)) != 1)
	{
	  fprintf(stderr, "%s\n", (char*)word.Get());
	  fprintf(stderr, "%s\n", (char*)other_word.Get());
	  fprintf(stderr, "dokey: %s different numeric field, expected 1 got %d\n", *key_desc, ret);
	  exit(1);
	}
    }
  }
  //
  // WordKey::Diff function
  //
  {
    WordKey word(context, "50 1 2 3 4 5");
    WordKey other_word(context, "51 1 2 3 4 5");
    //
    // Diff must say that field 0 differ and that word is lower than other_word
    //
    {
      int position = 0;
      int lower = 0;
      if(!word.Diff(other_word, position, lower)) {
	fprintf(stderr, "%s\n", (char*)word.Get());
	fprintf(stderr, "%s\n", (char*)other_word.Get());
	fprintf(stderr, "dokey: diff failed\n");
	exit(1);
      }
      if(position != 0 || lower != 1) {
	fprintf(stderr, "%s\n", (char*)word.Get());
	fprintf(stderr, "%s\n", (char*)other_word.Get());
	fprintf(stderr, "dokey: diff expected position = 0 and lower = 1 but got position = %d and lower = %d\n", position, lower);
	exit(1);
      }
    }
    //
    // Same key have no diff
    //
    {
      int position = 0;
      int lower = 0;
      if(word.Diff(word, position, lower)) {
	fprintf(stderr, "dokey: diff found when comparing %s with itself\n", (char*)word.Get());
	exit(1);
      }
    }
  }
}

static void pack_show_key(const String& key)
{
  int i;
  char c;

  for(i=0; i < key.length(); i++) {
    c = (isprint(key[i]) ? key[i] : '#');
    fprintf(stderr, "%c-%2x ", c, key[i]);
  }
  fprintf(stderr, "\n");

  for(i = 0; i < key.length(); i++) {
    int j;
    for(j = 0; j < 8; j++)
      fprintf(stderr, "%c", (key[j] & (1<<(j))) ? '1' : '0');
  }

  fprintf(stderr, "\n");
  fprintf(stderr, "^0      ^1      ^2      ^3      ^4      ^5      ^6      ^7\n");
  fprintf(stderr, "0123456701234567012345670123456701234567012345670123456701234567\n");
}

static void pack_show_wordreference(const WordReference& wordRef)
{
  String key;
  String record;

  wordRef.Pack(key, record);

  fprintf(stderr, "key = ");
  for(int i = 0; i < key.length(); i++) {
    fprintf(stderr, "0x%02x(%c) ", key[i] & 0xff, key[i]);
  }
  fprintf(stderr, " record = ");
  for(int i = 0; i < record.length(); i++) {
    fprintf(stderr, "0x%02x(%c) ", record[i] & 0xff, record[i]);
  }
  fprintf(stderr, "\n");
}



//*****************************************************************************
// void doskip()
//   Test SkipUselessSequentialWalking in WordList class
//
static void doskip_normal(params_t*);
static void doskip_harness(params_t*);
static void doskip_overflow(params_t*);
static void doskip_try(WordList& words, WordCursorOne& search, char* found_string, char* expected_string);

static void doskip(params_t* params)
{
  if(verbose) fprintf(stderr, "Test WordList::SkipUselessSequentialWalking normal\n");
  doskip_normal(params);
  if(verbose) fprintf(stderr, "Test WordList::SkipUselessSequentialWalking harness\n");
  doskip_harness(params);
  if(verbose) fprintf(stderr, "Test WordList::SkipUselessSequentialWalking overflow\n");
  doskip_overflow(params);
}

static void doskip_try(WordList& words, WordCursorOne& search, char* found_string, char* expected_string)
{
  const WordKey& found = search.GetFound().Key();
  ((WordKey&)found).Set(found_string);
  ((WordKey&)search.GetSearch()).Set(WORD_KEY_WORD, found.Get(WORD_KEY_WORD));
  if(search.SkipUselessSequentialWalking() == NOTOK) {
    fprintf(stderr, "doskip_try: SkipUselessSequentialWalking NOTOK searching %s at step %s expecting %s\n", (char*)search.GetSearch().Get(), (char*)found.Get(), (char*)expected_string);
    exit(1);
  }

  WordKey expected(context, expected_string);
  if(!found.ExactEqual(expected)) {
    fprintf(stderr, "doskip_try: expected %s but got %s\n", (char*)expected.Get(), (char*)found.Get());
    exit(1);
  }
}

//
// Create artificial WordCursor context
// in which SkipUselessSequentialWalking calls SetToFollowing
// that triggers overflow condition.
//
static void doskip_overflow(params_t*)
{
#define WORD_FIELD1	1
#define WORD_FIELD2	2
#define WORD_FIELD3	3

  static ConfigDefaults config_defaults[] = {
    { "wordlist_wordkey_description", "Word 8/FIELD1 32/FIELD2 8/FIELD3 16", 0 },
    { 0 }
  };
  Configuration config;
  config.Defaults(config_defaults);
  if(verbose > 2) config.Add("wordlist_verbose", "3");
  context->Initialize(config);
  {
    WordListOne *words = new WordListOne(context);

    words->Open("test", O_RDWR);
    //
    // Looking for 11 at location 3
    //
    WordKey key(context, "11 <UNDEF> <UNDEF> 3");
    WordCursorOne *search = (WordCursorOne*)words->Cursor(key);

    {
      //
      // Pretend we found 11 3 <MAX> 7
      // That is a valid candidate for SkipUselessSequentialWalking
      //
      String found;
      found << "11 3 " << context->GetKeyInfo().MaxValue(WORD_FIELD2) << " 7";

      //
      // Overflow on FIELD2 must trigger ++ on FIELD1
      //
      String expected("11	4	0	3");
      doskip_try(*words, *search, found, expected);
    }

    delete search;
    delete words;
  }

  //
  // Restore default configuration
  //
  context->Initialize();

#undef WORD_FIELD1
#undef WORD_FIELD2
#undef WORD_FIELD3
}

//
// Create artificial WordCursor contexts
// that covers all possible behaviour of SkipUselessSequentialWalking.
//
static void doskip_harness(params_t*)
{
#define WORD_FIELD1	1
#define WORD_FIELD2	2
#define WORD_FIELD3	3
#define WORD_FIELD4	4
#define WORD_FIELD5	5

  static ConfigDefaults config_defaults[] = {
    { "wordlist_wordkey_description", "Word 8/FIELD1 8/FIELD2 8/FIELD3 8/FIELD4 8/FIELD5 8", 0 },
    { 0 }
  };
  Configuration config;
  config.Defaults(config_defaults);
  if(verbose > 2) config.Add("wordlist_verbose", "3");
  context->Initialize(config);
  {
    WordList *words = new WordListOne(context);

    //
    // Searching
    //
    // {11,21,31} <UNDEF> <UNDEF> 5 <UNDEF> 4 <UNDEF>
    //
    // in data set
    //
    // DATA                     SEE  STATUS    OPERATION
    // 11 1     5 1      4 3         found     next
    // 11 1     6 1      4 3    a    nomatch   skip to 11 2 5 0 4 0
    // 11 1     6 2      4 3         ignore
    // 11 2     3 1      4 3         ignore
    // 21 0     3 1      4 3         ignore
    // 21 8     5 1      1 3    c    nomatch   skip to 21 8 5 1 4 0
    // 21 8     5 1      2 3         ignore
    // 21 8     5 1      2 5         ignore
    // 21 8     5 1      2 9         ignore
    // 21 8     5 1      3 9         ignore
    // 31 3     5 <MAX>  6 3    d    nomatch   skip to 31 4 5 0 4 0
    // 31 3     5 <MAX>  6 5         ignore
    // 31 3     5 <MAX>  8 5         ignore
    // 31 4     5 2      4 3         found
    //
    // legend: status is what WalkNextStep function says about the key
    //                nomatch means searchKey.Equal(found.Key()) is false
    //                found means searchKey.Equal(found.Key()) is true
    //                ignore means we jump over it
    //         operation is the next operation decided by WalkNextStep
    //                always skip if SkipUselessSequentialWalking is called.
    //                In general SkipUselessSequentialWalking is not always
    //                called on nomatch. But it is always called if the
    //                search key is not a prefix key, which is our case.
    //         see is a reference to the list bellow
    //
    //  a) Needless to search for keys in which the FIELD1 is equal to 1 since
    //     the FIELD2 is greater than the searched value. Any key with the FIELD1
    //     set to 1 that follow this one will have a FIELD2 greater than the searched
    //     value (5) since the keys are sorted in ascending order. 
    //     The next possible key is the one that has FIELD1++.
    //
    //  c) The found key does not match the constraint (FIELD4 is lower than the searched
    //     value). We only need to set FIELD4 to the searched value to jump to the
    //     match. No incrementation in this case.
    //
    //  d) The FIELD4 is greater than the searched value, making this a lot similar
    //     to the b) case since the FIELD3 value is <MAX>. However FIELD2 matches
    //     the search key, it is therefore useless to increment it. We must ignore
    //     it and increment FIELD1.
    //
    // Looking for 11 with flags 5
    //
    WordKey key(context, "11 <UNDEF> 5 <UNDEF> 4 <UNDEF>");
    words->Open("test", O_RDWR);
    WordCursorOne *search = (WordCursorOne*)words->Cursor(key);

#define WORD_NTEST 3

    static char* found_strings[WORD_NTEST];
    static char* expected_strings[WORD_NTEST];
  
    int i = 0;
    char tmp[1024];
  
    //
    // See a) in comment above
    //
    found_strings[i] = strdup("11 1     6 1      4 3");
    expected_strings[i] = strdup("11 2 5 0 4 0");
    i++;

    //
    // See c) in comment above
    //
    found_strings[i] = strdup("21 8     5 1      1 3");
    expected_strings[i] = strdup("21 8 5 1 4 0");
    i++;

    //
    // See d) in comment above
    //
    sprintf(tmp, "31 3     5 %d  6 3", context->GetKeyInfo().MaxValue(WORD_FIELD3));
    found_strings[i] = strdup(tmp);
    expected_strings[i] = strdup("31 4 5 0 4 0");
    i++;

    for(i = 0; i < WORD_NTEST; i++) {
      doskip_try(*words, *search, found_strings[i], expected_strings[i]);
      free(found_strings[i]);
      free(expected_strings[i]);
    }

    delete search;
    delete words;
  }

  //
  // Restore default configuration
  //
  context->Initialize();

#undef WORD_FIELD1
#undef WORD_FIELD2
#undef WORD_FIELD3
#undef WORD_FIELD4
#undef WORD_FIELD5
}

int
get_int_array(char *s,int **plist,int &n)
{
    int i=0;
    for(n=0;s[i];n++)
    {
	for(;s[i] && !isalnum(s[i]);i++);
	if(!s[i]){break;}
	for(;s[i] && isalnum(s[i]);i++);
    }
    if(!n){*plist=NULL;return(NOTOK);}
    int *list=new int[n];
    *plist=list;
    int j;
    i=0;
    for(j=0;s[i];j++)
    {
	for(;s[i] && !isalnum(s[i]);i++);
	if(!s[i]){break;}
	list[j]=atoi(s+i);
	for(;s[i] && isalnum(s[i]);i++);
    }
    return(OK);
}
class SkipTestEntry
{
public:
    char *searchkey;
    char *goodorder;
    int Check(WordList &WList)
    {
	WordReference srchwrd(context);

	srchwrd.Key() = WList.Key(searchkey);
	if(verbose) fprintf(stderr, "GetSearchKey: string: %s got: %s\n", (char*)searchkey, (char*)srchwrd.Key().Get());
	
	Object o;
	if(verbose) fprintf(stderr, "checking SkipUselessSequentialWalking on: %s\n", (char*)srchwrd.Get());
	if(verbose) fprintf(stderr, "walking all:\n");
	List *all = WList.WordRefs();
	if(verbose) fprintf(stderr, "walking search: searching for: %s\n", (char*)srchwrd.Get());

	WordCursor *search = WList.Cursor(srchwrd.Key(), HTDIG_WORDLIST_COLLECTOR);
	search->SetTraces(new List);
	search->Walk();
	List *wresw = search->GetResults();
	List *wres  = search->GetTraces();
	wresw->Start_Get();
	wres->Start_Get();
	WordReference *found;
	WordReference *correct;
	int i;
	int ngoodorder;
	int *goodorder_a;
	get_int_array(goodorder,&goodorder_a,ngoodorder);
	for(i=0;(found = (WordReference*)wres->Get_Next());i++)
	{
  	    if(i>=ngoodorder) {
	      fprintf(stderr, "SkipUselessSequentialWalking test failed! i>=ngoodorder\n");
	      exit(1);
	    }
	    if(verbose) fprintf(stderr, "Check actual %d'th walked: %s\n", i, (char*)found->Get());
	    correct = (WordReference*)all->Nth(goodorder_a[i]);
	    if(verbose) fprintf(stderr, "Check correct %d : %s\n", goodorder_a[i], (char*)correct->Get());
	    if(!correct->Key().Equal(found->Key())) {
	      fprintf(stderr, "SkipUselessSequentialWalking test failed! at position: %d\n", i);
	      exit(1);
	    }
	}
	if(i<ngoodorder) {
	  fprintf(stderr, "SkipUselessSequentialWalking test failed! n<ngoodorder\n");
	  exit(1);
	}

	delete [] goodorder_a;
	delete wresw;
	delete wres;
	delete all;
	delete search;
	return OK;
    }
};

SkipTestEntry SkipTestEntries[]=
{
     {
 	"et  <UNDEF>       0       10      ",
 	"3 4 5 9 10 12 13 14"
     },
     // Test for prefix search. 
     //     {
     // 	"et  20            0       <UNDEF> ",
     // 	"3 4 5 6 7 8 9 14 17",
     //     },
};

static void doskip_normal(params_t *)
{
    if (verbose > 0)
	fprintf(stderr, "doing SkipUselessSequentialWalking test\n");
    // read db into WList from file: skiptest_db.txt
    if (verbose)
	fprintf(stderr, "WList config:minimum_word_length: %d\n",
		context->GetConfiguration().Value("minimum_word_length"));
    WordList *words = context->List();
    words->Open("test", O_RDWR);
    // now check walk order for a few search terms
    int i;
    if (verbose)
	fprintf(stderr, "number of entries: %d\n",
		sizeof(SkipTestEntries) / sizeof(SkipTestEntry));
    for (i = 0;
	 i < (int) (sizeof(SkipTestEntries) / sizeof(SkipTestEntry)); i++) {
	if (SkipTestEntries[i].Check(*words) == NOTOK) {
	    fprintf(stderr,
		    "SkipUselessSequentialWalking test failed on SkipTestEntry number: %d\n",
		    i);
	    exit(1);
	}
    }
    delete words;
}

static void doenv(params_t* params)
{
  WordList *words = context->List();
  words->Open("test", O_RDWR);
  sleep(1);
  WordReference wordRef = words->Word("the 1 2 3");
  WordKey& key = wordRef.Key();
  int i;
  for(i = params->env; i < 10000; i += 2) {
    key.Set(WORD_DOCID, i);
    if(words->Override(wordRef) != OK) {
      fprintf(stderr, "doenv:%d: cannot insert %d\n", params->env, i);
      exit(1);
    }
  }
  for(i = params->env; i < 10000; i += 2) {
    key.Set(WORD_DOCID, i);
    if(words->Exists(wordRef) != OK) {
      fprintf(stderr, "doenv:%d: cannot find %d\n", params->env, i);
      exit(1);
    }
  }
  delete words;
}

static void dodead(params_t* params)
{
  WordList *words = context->List();
  words->Open("test", O_RDWR);

  unsigned int wordid = WORD_DICT_SERIAL_INVALID;
  words->Dict()->SerialExists("et", wordid);
  if(wordid == WORD_DICT_SERIAL_INVALID) {
    fprintf(stderr, "dodead: cannot get serial for 'et'\n");
    exit(1);
  }

  WordKey key(context);
  key.Set(WORD_KEY_WORD, wordid);
  key.Set(WORD_DOCID, 20);
  //
  // Check that we indeed have matches for this search key.
  //
  WordCursor* cursor = words->Cursor(key, HTDIG_WORDLIST_COLLECTOR);
  cursor->Walk();
  if(cursor->GetResults()->Count() != 5) {
    fprintf(stderr, "dodead: searching %s expected 5 results, got %d\n", (char*)key.Get(), cursor->GetResults()->Count());
    exit(1);
  }
  delete cursor->GetResults();

  //
  // Document <UNDEF> 20 <UNDEF> <UNDEF> is dead
  //
  words->Dead()->Mask(WordKey(context, "<UNDEF> 0 <UNDEF> <UNDEF>"));
  words->Dead()->Put(WordKey(context, "<UNDEF> 20 <UNDEF> <UNDEF>"));
  
  cursor->Walk();
  if(cursor->GetResults()->Count() != 0) {
    fprintf(stderr, "dodead: document marked dead searching %s expected 5 results, got %d\n", (char*)key.Get(), cursor->GetResults()->Count());
    exit(1);
  }
  delete cursor->GetResults();

  delete cursor;

  WordReference wordRef(context);
  wordRef.Key() = key;

  if(words->Exists(wordRef) != NOTOK) {
    fprintf(stderr, "dodead: Exists returned true\n");
    exit(1);
  }
  
  delete words;
}

//*****************************************************************************
// void usage()
//   Display program usage information
//
static void usage()
{
    printf("usage: word [options]\n");
    printf("Options:\n");
    printf("\t-v\t\tIncreases the verbosity\n");
    printf("\t-k\t\tTest WordKey\n");
    printf("\t-l\t\tTest WordList\n");
    printf("\t-d\t\tTest WordDead\n");
    printf("\t-e n\t\tTest WordList with shared environnement, process number <n>\n");
    printf("\t-s\t\tTest WordList::SkipUselessSequentialWalking\n");
    printf("\t-z\t\tActivate compression test (use with -s, or -l)\n");
    exit(0);
}
