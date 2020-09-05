//
// NAME
// 
// load the content of an inverted index.
//
// SYNOPSIS
//
// mifluzload file
//
// DESCRIPTION
//
// mifluzload reads from <b>stdout</b> a complete ascii description
// of the <b>file</b> inverted index using the <i>WordList::Read</i>
// method. 
//
// ENVIRONMENT
//
// <b>MIFLUZ_CONFIG</b>
// file name of configuration file read by WordContext(3). Defaults to
// <b>~/.mifluz.</b> 
// 
// 
// END
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#include <unistd.h>
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#include <locale.h>

#include <htString.h>
#include <WordContext.h>
#include <WordList.h>

static void action(WordContext* context, const String& file)
{
  WordList *words = context->List();
  if(words->Open(file, O_RDWR | O_TRUNC) != OK) exit(1);
  if(words->Read(stdin) < 0) exit(1);
  if(words->Close() != OK) exit(1);
  delete words;
}

static void usage()
{
  fprintf(stderr, "usage: mifluzload [-zv] file\n");
  exit(1);
}

int main(int argc, char *argv[])
{
  if(argc < 2) usage();

  setlocale(LC_ALL, "");

  

  //  extern char *optarg;
  extern int optind;
  int ch;
bool isCompress=false;
bool isV=false;
  while ((ch = getopt(argc, argv, "zvM:")) != EOF) {
    switch (ch) {
    case 'z':
      isCompress=true;
      break;
    case 'v':
      {
	isV=true;
	
      }
      break;
    case 'M':
      {
	char *milo= (char*)malloc(strlen(optarg) + 32);
	sprintf(milo, "MIFLUZ_CONFIG=%s", optarg);
	if(putenv(milo) < 0) {
	  perror("putenv");
	  exit(1);
	}
      }
      break;
    default:
      usage();
      break;
    }
  }

//
  // Mandatory to create global data needed for the library.
  //
  WordContext *context = new WordContext();
  if(!context) exit(1);

  Configuration& config = context->GetConfiguration();
  context->ReInitialize();
  if (isCompress)
   config.Add("wordlist_compress", "true");

  if (isV)
  {
	int value = config.Value("wordlist_verbose", 0);
	value++;
	char value_string[64];
	sprintf(value_string, "%d", value);
	config.Add("wordlist_verbose", value_string);
  }
  action(context, argv[optind]);
  delete context;
}

