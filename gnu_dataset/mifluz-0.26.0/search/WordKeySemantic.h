//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordKeySemantic.h,v 1.7 2001/07/20 10:37:53 loic Exp $
//
// ************************* Document definition implementation ***********

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifndef _WordKeySemantic_h
#define _WordKeySemantic_h

#include <WordContext.h>

// *********************** WordKeySemantic implementation ********************
//
// NAME
//
// encapsulate WordKey semantic for document and location
//
// SYNOPSIS
//
// #include <WordKeySemantic.h>
//
// #define SERVER 1
// #define URL 2
// #define LOCATION 3
//
// static int document[] = {
//   SERVER,
//   URL
// };
// 
// WordKeySemantic semantic;
// semantic.Initialize(document, sizeof(document)/sizeof(int), LOCATION);
//
// DESCRIPTION
//
// Encapsulate the semantic of a WordKey object fields. It defines
// what a document and a location are. It implements the set of
// operation that a search needs to perform given the fact that it
// implements a search whose purpose is to retrieve a document and
// wants to implement proximity search based on a word location.
//
//
// END
//
// A document is a set of fields in a given order. 
// A location is a field.
// The actual fields used to implement WordKeySemantic methods are
// set with the Initialize method.
//
class WordKeySemantic {
public:
  WordKeySemantic(WordContext *ncontext);
  ~WordKeySemantic();

  //-
  // Set the actual field numbers that define what a document is and
  // what a location is. The <b>document_arg<b> is a list of WordKey field
  // positions of length <b>document_length_arg</b> that must be adjacent.
  // The <b>location_arg</b> is the WordKey field position of the word
  // location within a document.
  // Return OK on success, NOTOK on failure.
  //
  int Initialize(int* document_arg, int document_length_arg, int location_arg, int uniq_arg);

  //
  // A realm exists if there is data between the word and the document.
  //
  //-
  // Return 1 if document is at the beginning of the key, 0 otherwise.
  // For instance a document that is defined as 1 2 3 is at the beginning
  // of a key and a document that is defined as 2 3 is not at the beginning
  // of a key.
  //
  int HasRealm() {
    if(!document) {
      fprintf(stderr, "WordKeySemantic::HasRealm: document is not defined\n");
      return 0;
    }
    return document[0] != 1;
  }
  //-
  // Clear <b>to</b> and copy the realm in <b>from</b> into <b>to.</b>
  //
  void RealmSet(const WordKey& from, WordKey& to);
  //-
  // Undefine the realm fields of <b>key</b>.
  //
  void RealmUndefined(WordKey& key);
  //-
  // Copy the realm in <b>from</b> into <b>to.</b>
  //
  void RealmCopy(const WordKey& from, WordKey& to);
  //-
  // Clear key and set all realm fields to 0.
  //
  void RealmClear(WordKey& key);

  //-
  // Return the index of the uniq field. It must be in the range of
  // indexes defined for the document.
  //
  inline int Uniq() { return uniq; }

  //-
  // Clear <b>to</b> and copy the document in <b>from</b> into <b>to.</b>
  //
  void DocumentSet(const WordKey& from, WordKey& to);
  //-
  // Copy the document in <b>from</b> into <b>to.</b>
  //
  void DocumentCopy(const WordKey& from, WordKey& to);
  //-
  // Increment the document in <b>key</b> using the <i>SetToFollowing</i>
  // method of WordKey. <b>uniq</b> is the WordKey position at which the 
  // increment starts.
  //
  void DocumentNext(WordKey& key, int use_uniq);
  //-
  // Compare the document fields defined in both <b>a</b> and <b>b</b>
  // and return the difference a - b, as in strcmp. If all document
  // fields in <b>a</b> or <b>b</b> are undefined return 1.
  //
  int DocumentCompare(const WordKey& a, const WordKey& b);
  //-
  // Set all document fields to 0.
  //
  int DocumentClear(WordKey& key);
  //-
  // Set all document fields to undefined.
  //
  int DocumentUndefined(WordKey& key);
  //-
  // Return the index of the first document field.
  //
  inline int DocumentOffset() const { return document[0]; }
  //-
  // Return the length of the document fields.
  //
  inline int DocumentLength() const { return document_length; }

  //
  // These functions and only these know what a location is. 
  // This should really be a class containing function pointers and be
  // given as argument to the search algorithm.
  //
  //-
  // Copy the document and location in <b>from</b> into <b>to.</b>
  //
  void LocationSet(const WordKey& from, WordKey& to);
  //-
  // Increment the document and location in <b>key</b> 
  // using the <i>SetToFollowing</i>
  // method of WordKey. 
  //
  void LocationNext(WordKey& key);
  //-
  // Compare <b>expected</b> location to <b>actual</b> location. Compares equal
  // as long as expected location is at a maximum distance of <b>proximity</b>
  // of actual. If <b>actual</b> only has undefined field, return > 0.
  // <b>expected</b> must always be the lowest possible bound.
  // <b>actual</b> is tolerated if it is greater than <b>actual</b> but not
  // greater than <b>proximity</b> if <b>proximity</b> > 0 or abs(<b>proximity</b>) * 2 if
  // <b>proximity</b> < 0.
  // Return the difference expected - actual.
  //
  int  LocationCompare(const WordKey& expected, const WordKey& actual, int proximity = 0);
  //-
  // <b>key</b> is the expected location of a searched key. 
  // LocationNearLowest modifies <b>key</b> to add tolerance accroding to
  // <b>proximity</b>. 
  //
  // The idea is that <b>key</b> will be the lowest possible match for 
  // for the <b>proximity</b> range. If <proxmity> is positive, <b>key</b>
  // is already the lowest possible match since we accept [0 proximity].
  // If <b>proximity</b> is negative, substract it since we accept
  // [-proximity proximity].
  //
  // For better understanding see the functions in which it is used.
  //
  void LocationNearLowest(WordKey& key, int proximity);

  //-
  // Undefine the location field in <b>key.</b>.
  //
  void Location2Document(WordKey& key);

  inline int Verbose(int verbosity) { return verbose = verbosity; }

protected:
  int* document;
  int document_length;
  int location;
  int uniq;
  WordContext *context;

  int verbose;
};

#endif /* _WordKeySemantic_h */
