//
// The goal of this example is to provide a minimal standalone
// program that creates an inverted index and searches in it.
// It is by no mean a tutorial but is commented to allow a fast
// introduction to mifluz.
//
// Compilation:
// gcc -I/usr/local/include/mifluz -L/usr/local/lib -lmifluz example0.cc
//

#include "config.h"
#include "example1.h"
#include <stdlib.h>

#include <htString.h>
#include <WordContext.h>
#include <WordList.h>
#include <Configuration.h>

static ConfigDefaults defaults[] = {
  //
  // Very simple key structure: word+one numerical field
  //
  { "wordlist_wordkey_description","Word 8/Location 32"},
  { 0 }
};

void action(WordContext* context)
{
  //
  // Open a new inverted index (O_RDWR), reseting it
  // to empty if it was not empty (O_TRUNC).
  //
  WordList *words = context->List();
  words->Open("words.db", O_RDWR|O_TRUNC);

  //
  // The WordReference object now holds data that we want to
  // save in the inverted index. The Insert metho ensure that
  // two identical keys are not inserted in the index (no
  // duplicates allowed).
  //
  words->Override(words->Word("computer 1"));
  words->Override(words->Word("computing 16"));
  words->Override(words->Word("computing 10"));
  words->Override(words->Word("hello 20"));

  //
  // We now search for all the words matching computing
  //
  List *results = words->FindWord("computing");
  //
  // And display them
  //
  WordReference *match;
  for(results->Start_Get(); (match = (WordReference*)results->Get_Next());) {
    printf("%s:%s\n", match->GetWord().get(),(char*)match->Get());
    match->Print();
  }

  delete results;

  //
  // It's not so much important to release the file descriptor
  // than to flush the cached information.
  //
  words->Close();

  delete words;
}

int main(void) {
  //
  // WordContext is the central object from which all 
  // mifluz objects are created.
  //
  WordContext *context = new WordContext(defaults);
  action(context);
  delete context;
  
  main2();

  printf("Success\n");
}
