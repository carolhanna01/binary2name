//
// NAME
// search the content of an inverted index.
//
// SYNOPSIS
//
// mifluzsearch -f words [options]
//
// DESCRIPTION
//
// mifluzsearch searches a mifluz index for documents matching a 
// Alt*Vista expression (simple syntax). 
//
// Debugging information interpretation. A cursor is open in the index
// for every word and they are stored in a list. The list of cursors
// is always processed in the same order, as a single link list. With
// -v, each block is an individual action on behalf of the word shown
// on the first line. The last line of the block is the conclusion of
// the action described in the block. REDO means the same cursor must
// be examined again because the conditions have changed. RESTART means
// we go back to the first cursor in the list because it may not 
// match the new conditions anymore. NEXT means the cursor and all
// the cursors before it match the conditions and we may proceed to
// the next cursor. ATEND means the cursor cannot match the conditions
// because it is at the end of the index.
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
// mifluzsearch: Sample implementation of search algorithms using
//         a mifluz inverted index. 
//
//         Each class is documented in the class definition. Before
//         each method declaration a comment explains the semantic of
//         the method. In the method definition comments in the code
//         may contain additional information.
//
//         Each virtual function is documented in the base class, not
//         in the derived classes except for semantic differences.
//         
//         The class tree is:
//         
//         WordKeySemantic
//
//         WordExclude
//           WordExcludeMask
//             WordPermute
//
//         WordSearch
//
//         WordMatch
//
//         WordTree
//           WordTreeOperand
//             WordTreeOptional
//              WordTreeOr
//              WordTreeAnd
//              WordTreeNear
//             WordTreeMandatory
//             WordTreeNot
//           WordTreeLiteral
//
//         WordParser
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: mifluzsearch.cc,v 1.18 2008/06/08 08:29:35 sebdiaz Exp $
//

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

// If we have this, we probably want it.
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif /* HAVE_MALLOC_H */
#include <stdlib.h>
#include <ctype.h>

#include <htString.h>
#include <WordList.h>
#include <WordContext.h>
#include <WordCursorOne.h>
#include <HtMaxMin.h>
#include <WordListOne.h>
#include <WordDict.h>

#include <mifluzsearch.h>
#include <WordTree.h>
#include <WordSearch.h>

//
// Verbosity level set with -v (++)
// 
static int verbose = 0;

// ************************* main loop implementation ********************

//
// Store all options from the command line
//
class params_t
{
public:
  char* dbfile;
  char* find;
  unsigned int count;
  int uniq_server;
  int proximity;
  unsigned int base;
  int nop;
  int exclude;
  char* low;
  char* high;
  char* occurrences;
  int bounded;
  int xml;
  int http;
  int or_method;
  char* mifluz_config;
  unsigned int restrict;
};

static int dosearch(params_t* params);

//
// Explain options
//
static void usage();

static int urldecode(char *s)
{
  char *p = s;

  while (*s != '\0')
  {
    if (*s == '%')
    {
      s++;
      if (!isxdigit(*s))
        return 0;
      *p = (isalpha(*s) ? (*s & 0xdf) - 'A' + 10 : *s - '0') << 4;
      s++;
      if (!isxdigit(*s))
        return 0;
      *p += isalpha(*s) ? (*s & 0xdf) - 'A' + 10 : *s - '0';
    }
    else if (*s == '+')
      *p = ' ';
    else
      *p = *s;
    s++;
    p++;
  }
  *p = '\0';
  return 1;
}

static void handle_param(params_t* params, char c, const char* optarg) 
{
  switch (c)
    {
    case 'v':
      verbose++;
      break;
    case 'B':
      free(params->dbfile);
      params->dbfile = strdup(optarg);
      break;
    case 'f':
      params->find = strdup(optarg);
      break;
    case 'c':
      params->count = (unsigned int)atoi(optarg);
      break;
    case 'd':
      params->base = (unsigned int)atoi(optarg);
      break;
    case 'P':
      params->proximity = atoi(optarg);
      break;
    case 'S':
      params->uniq_server = 1;
      break;
    case 'n':
      params->nop = 1;
      break;
    case 'l':
      free(params->low);
      params->low = strdup(optarg);
      break;
    case 'h':
      free(params->high);
      params->high = strdup(optarg);
      break;
    case 'o':
      free(params->occurrences);
      params->occurrences = strdup(optarg);
      break;
    case 'O':
      params->or_method = WORD_SEARCH_OR;
      break;
    case 'x':
      params->xml = 1;
      break;
    case 'H':
      params->http = 1;
      break;
    case 'M':
      {
	free(params->mifluz_config);
	params->mifluz_config = (char*)malloc(strlen(optarg) + 32);
	sprintf(params->mifluz_config, "MIFLUZ_CONFIG=%s", optarg);
	if(putenv(params->mifluz_config) < 0) {
	  perror("putenv");
	  exit(1);
	}
      }
      break;
    case 'R':
      params->restrict = (unsigned int)strtoul(optarg, 0, 10);
      break;
    case '?':
      usage();
      break;
    }
}

int main(int ac, char **av)
{
  params_t		params;

  params.dbfile = strdup("");
  params.find = 0;
  params.count = 10;
  params.base = 0;
  params.uniq_server = 0;
  params.proximity = WORD_SEARCH_DEFAULT_PROXIMITY;
  params.nop = 0;
  params.low = strdup("");
  params.high = strdup("");
  params.occurrences = strdup("");
  params.bounded = 0;
  params.xml = 0;
  params.http = 0;
  params.or_method = WORD_SEARCH_OPTIONAL;
  params.mifluz_config = strdup("");
  params.restrict = 0;

  if(getenv("QUERY_STRING")) {
    StringList fields(getenv("QUERY_STRING"), "&");
    for(int i = 0; i < fields.Count(); i++) {
      const char* field = fields[i];
      StringList pair(field, "=");
      if(pair.Count() != 2 && pair.Count() != 1) {
	fprintf(stderr, "%s should match .+=.*\n", field);
	exit(1);
      }
      //
      // name= with no value is silently ignored
      //
      if(pair.Count() == 2) {
	const char* name = pair[0];
	char* value = pair[1];
	if(!urldecode(value)) {
	  fprintf(stderr, "mifluzsearch: unable to decode %s from QUERY_STRING %s\n", value, getenv("QUERY_STRING"));
	  exit(1);
	}
	handle_param(&params, name[0], value);
      }
    }
    params.http = params.xml = 1;
  } else {
    int			c;
    extern char		*optarg;

    while ((c = getopt(ac, av, "vB:f:c:SP:nR:l:h:o:xM:HOd:")) != -1)
      {
	handle_param(&params, c, optarg);
      }
  }
  
  if(dosearch(&params) < 0)
    exit(1);

  if(params.find) free(params.find);
  free(params.dbfile);
  free(params.low);
  free(params.high);
  free(params.occurrences);
  free(params.mifluz_config);
}

static int dosearch(params_t* params)
{
  clock_t start_time;
  clock_t end_time;

  if((start_time = clock()) == (time_t)-1) {
    perror("start clock");
    return -1;
  }

  WordContext* context = new WordContext();
  Configuration& config = context->GetConfiguration();

  if(!context) {
    fprintf(stderr, "search: cannot create context\n");
    return -1;
  }

  //
  // Forward command line verbosity to htword library.
  //
  if(verbose > 1) {
    String tmp;
    tmp << (verbose - 1);
    config.Add("wordlist_verbose", tmp);
    context->ReInitialize();
  }

  //
  // Prepare the index (-B).
  //
  if(params->dbfile[0] == '\0') {
    printf("missing -B option\n");
    usage();
  }

  WordList *words = context->List();
  words->Open(params->dbfile, O_RDONLY);


  //
  // Return the number of occurrences of a given word
  //
  if(params->occurrences[0]) {
    unsigned int occurrences = 0;
    words->Noccurrence(params->occurrences, occurrences);
    printf("%s occurs %d times\n", params->occurrences, occurrences);
    exit(0);
  }

  if(!params->find) {
    printf("missing -f option\n");
    usage();
  }

  WordTree* expr;
  MifluzSearchInput input;
  {
    input.BufferSet(params->find, strlen(params->find));
    input.Verbose(verbose);
    input.or_method = params->or_method;
    input.maximum_word_length = config.Value("wordlist_maximum_word_length", 25);
    input.words = words;
    search_parse(&input);
    expr = input.query;
    expr->Verbose(verbose);
  }


  //
  // Try the query parser alone
  //
  if(params->nop) {
    printf("%s\n", (char*)expr->Get().get());
    exit(0);
  }

  int* document = 0;
  int document_length = 0;

  //
  // Define the semantic of the key
  //
  {
    int location = -1;
    int nfields = words->GetContext()->GetKeyInfo().nfields;

    StringList fields(config.Find("wordlist_wordkey_document"), "\t ");
    document_length = fields.Count();
    if(document_length > 0) {
      if(document_length > nfields - 1) {
	fprintf(stderr, "wordlist_wordkey_document has more fields than the authorized maximum (%d)\n", nfields - 1);
	return -1;
      }

      document = new int[document_length];
      for(int i = 0; i < document_length; i++) {
	if(!fields[i]) {
	  fprintf(stderr, "wordlist_wordkey_document unexpected null field returned at position %d \n", i);
	  return -1;
	}
	document[i] = atoi(fields[i]);
      }

      if((location = config.Value("wordlist_wordkey_location", -1)) == -1) {
	fprintf(stderr, "wordlist_wordkey_location must be set in configuration to define the structure of the key\n");
	exit(0);
      }
      
      int uniq = config.Value("wordlist_wordkey_uniq", 0);
      if(params->uniq_server && !uniq) {
	fprintf(stderr, "wordlist_wordkey_uniq not set or 0, -S is useless\n");
      }

      WordTreeArg arg(words, uniq, params->uniq_server, params->proximity, document, document_length, location);
      if(expr->Prepare(arg) != OK)
	return -1;
    } else {
      fprintf(stderr, "wordlist_wordkey_document and wordlist_wordkey_location must be set in configuration to define the structure of the key\n");
      return -1;
    }
  }

  //
  // Calculate low and high bounds based on -R and wordlist_wordkey_uniq
  //
  if(params->restrict) {
    if(params->low[0] || params->high[0]) {
      fprintf(stderr, "-R number will override -l and -h\n");
    }

    WordKey restrict(context);
    int uniq = config.Value("wordlist_wordkey_uniq", 0);

    restrict.Set(uniq, params->restrict);
    free(params->low);
    params->low = strdup((char*)restrict.Get());

    if(restrict.Overflow(uniq, 1)) {
      if(verbose) fprintf(stderr, "-R %d overflow\n", params->restrict); 
    } else {
      restrict.Get(uniq)++;
      free(params->high);
      params->high = strdup((char*)restrict.Get());
    }

    if(verbose > 1) {
      fprintf(stderr, "-R %d => -l '%s' -h '%s'\n", params->restrict, params->low, params->high);
    }
  }
  
  //
  // Set lower and higher bounds if appropriate
  //
  String low_string;
  String high_string;
  {
    WordKey low(context);
    WordKey high(context);
    if(params->low[0]) {
      low.Set(params->low);
      low_string << "L" << params->low;
    }
    if(params->high[0]) {
      high.Set(params->high);
      high_string << "H" << params->high;
    }
    if(params->low[0] || params->high[0])
      if(expr->Bounds(low, high) != OK)
	return -1;
  }

  WordSearch* search = new WordSearch(words);
  search->Verbose(verbose);

  //
  // Forward query options to WordSearch object
  //
  search->limit_count = params->count;          	// -c
  search->limit_base = params->base;	          	// -d
  if(params->base % params->count) {
    fprintf(stderr, "mifluzsearch: -d number must be a multiple of -c number\n");
    return -1;
  }

  //
  // Create or re-use the list of results
  //
  {
    WordResults* results = new WordResults(context);

    results->Verbose(verbose);

    results->KeySemantic(expr->key_semantic);

    if(search->SetResults(results) != OK)
      return -1;

	String nameFile;
    nameFile=nameFile+String("Cmifluz_")+	expr->search +
		     String(params->uniq_server ? "U" : "") +
		     low_string +
		     high_string;
	nameFile.replace(' ', '1');
	nameFile.replace('(', '1');
	nameFile.replace(')', '1');
	nameFile.replace('"', '1');
	nameFile.replace('<', '1');
	nameFile.replace('>', '1');
	nameFile.replace('\t', '1');
    if(results->Open(nameFile) != OK)
      return -1;
  }
  //
  // Perform the search (-f)
  //
  search->expr = expr;
  WordMatches* matches = search->Search();

  //
  // Close the list of results
  //
  search->GetResults()->Close();

  if((end_time = clock()) == (time_t)-1) {
    perror("end clock");
    return -1;
  }
  //
  // Elapsed time in milli seconds
  //
  clock_t elapsed_time = (end_time - start_time) / 1000;
  
  //
  // Display results, if any.
  //
  if(params->xml) {
    if(params->http) {
      printf("Content-Type: text/html\n\n");
    }
    printf("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>\n");
    printf("<searchresults>\n");
    if(matches) {
      unsigned int i;
      for(i = 0; i < matches->length; i++) {
	const WordMatch& match = *matches->matches[i];
	printf("<match>");
	printf("<document>");
	for(int j = 0; j < document_length; j++) {
	  printf("%u ", match.match[document[j]]);
	}
	printf("</document>");
	if(!match.info.empty()) {
	  printf("<info>");
	  printf("%s", (const char*)match.info);
	  printf("</info>");
	}
	printf("</match>\n");
      }
      printf("<count>%d</count>\n", search->matches_total);
      printf("<time>%lu</time>\n", elapsed_time);
      {
	ListCursor cursor;
	String* word;
	printf("<words>\n");
	printf("\t<verbatim>");
	for(input.words_verbatim.Start_Get(cursor); (word = (String*)input.words_verbatim.Get_Next(cursor)); ) {
	  printf("%s ", (char*)word->get());
	}
	printf("</verbatim>\n");
	printf("\t<unaccent>");
	for(input.words_unaccent.Start_Get(cursor); (word = (String*)input.words_unaccent.Get_Next(cursor)); ) {
	  printf("%s ", (char*)word->get());
	}
	printf("</unaccent>\n");
	printf("</words>\n");
      }
      printf("<base>%d</base>\n", search->limit_base);
      delete matches;
    }
    printf("</searchresults>\n");
  } else {
    if(matches) {
      unsigned int i;
      for(i = 0; i < matches->length; i++) {
	const WordMatch& match = *matches->matches[i];
	printf("match: %s\n", (char*)match.Get());
      }
      printf("count: %d\n", search->matches_total);
      printf("time: %lu\n", elapsed_time);
      printf("base: %d\n", search->limit_base);
      {
	ListCursor cursor;
	String* word;
	printf("words:");
	for(input.words_verbatim.Start_Get(cursor); (word = (String*)input.words_verbatim.Get_Next(cursor)); ) {
	  printf(" %s", (char*)word->get());
	}
	printf("\n");
      }
      delete matches;
    } else {
      printf("match: none\n");
    }
  }

  //
  // Cleanup
  //
  delete search;
  delete words;
  delete context;
  delete [] document;

  return 0;
}

// *****************************************************************************
// void usage()
//   Display program usage information
//
static void usage()
{
    printf("usage:\tmifluzsearch -f words [options]\n");
    printf("\tmifluzsearch -o word [options]\n");
    printf("\tQUERY_STRING='...' mifluzsearch\n");
    printf("Options:\n");
    printf("\t-v\t\tIncreases the verbosity.\n");
    printf("\t-M config_file\tUse <config_file> instead of MIFLUZ_CONFIG env.\n");
    printf("\t-B dbfile\tUse <dbfile> as a db file name (default test).\n");
    printf("\t-f expr\t\tAltavista search expression.\n");
    printf("\t-x\t\tXML output.\n");
    printf("\t-H\t\tHTML headers.\n");
    printf("\t-O\t\tUse WordTreeOr instead of WordTreeOptional.\n");
    printf("\t-o <word>\treturn the number of occurrences of <word>\n");
    printf("\t\t\tSee WordParser comments in source for more information.\n");
    printf("\t-c number\tRetrieve at most this number documents.\n");
    printf("\t-d number\tIndex of the first document.\n");
    printf("\t-n\t\tOnly parse the search expression and print it.\n");
    printf("\t-P proximity\tUse with near/optional, proximity tolerance is <proximity>\n");
    printf("\t\t\tif negative order of terms is not meaningful\n");
    printf("\t\t\t(default 1).\n");
    printf("\t-S\t\tReturn at most one match per server.\n");
    printf("\t-R <value>\tcompute -l and -h according to wordlist_wordkey_uniq.\n");
    printf("\t-l <key>\tlow bound.\n");
    printf("\t-h <key>\thigh bound.\n");
    exit(1);
}
