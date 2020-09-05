//
// WordResults.cc
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordResults.cc,v 1.7 2014/04/17 20:27:43 sebdiaz Exp $
//
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <WordCursor.h>
#include <WordTree.h>
#include <WordResults.h>

#define WORD_RESULT_PAGESIZE 1024

static int wordResults_cmp(const DBT* a, const DBT* b)
{
  long length = (long)a->app_private;

  WordKeyNum* a_values = (WordKeyNum*)a->data;
  WordKeyNum* b_values = (WordKeyNum*)b->data;
  for(int i = 0; i < length; i++) {
    if(a_values[i] != b_values[i]) {
      return a_values[i] > b_values[i] ? 1 : -1;
    }
  }
  return 0;
}

int WordResults::Open(char* namep)
{
  if(document_length == 0) {
    fprintf(stderr, "WordListKey::Open: key semantic must be set (using method KeySemantic) before the Open method is called\n");
    return NOTOK;
  }
  
  const Configuration& config = context->GetConfiguration();
  String tmp_dir = config["wordlist_cache"];
  if(tmp_dir.empty())
    tmp_dir = MIFLUZ_CACHE;
  name = strdup(namep);
  String filename = tmp_dir + String("/") + String(name);
  
  int error;

  if(verbose)
    fprintf(stderr, "WordListKey::Open: %s\n", (char*)filename);

  if((error = CDB_db_env_create(&dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Open db_env_create for %s failed %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Environment
  //
  if((error = dbenv->open(dbenv, (char*)tmp_dir, DB_PRIVATE|DB_INIT_MPOOL|DB_CREATE, 0666)) != 0) {
    fprintf(stderr, "WordResults::Open dbenv->open(%s) failed: %s\n", (char*)tmp_dir, CDB_db_strerror(error));
    return NOTOK;
  }

  dbenv->set_errfile(dbenv, stderr);
  dbenv->set_errpfx(dbenv, "WordResults");

  //
  // Expire old result cache
  //
  {
    struct stat statbuf;
    if(stat((char*)filename, &statbuf) < 0) {
      if(errno != ENOENT) {
	fprintf(stderr, "WordResults::Open cannot stat %s:", (char*)filename);
	perror("");
	return NOTOK;
      }
    } else {
      time_t now;
      if(time(&now) < 0) {
	fprintf(stderr, "WordResults::Open time failed while handling %s:", (char*)filename);
	perror("");
	return NOTOK;
      }
      time_t timeout = config.Value("wordlist_cache_timeout");
      //
      // Minimal value for timeout is 5 seconds
      //
      if(timeout < 5) timeout = 3600;
      if(now - statbuf.st_atime > timeout) {
	if(unlink((char*)filename) < 0) {
	  fprintf(stderr, "WordResults::Open cannot unlink old cache file %s:", (char*)filename);
	  perror("");
	  return NOTOK;
	}
      }
    }
  }
  
  //
  // Variables list
  //
  if((error = CDB_db_create(&variables, dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Open db_create(%s) variables failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = variables->set_pagesize(variables, WORD_RESULT_PAGESIZE)) != 0) {
    fprintf(stderr, "WordResults::Open variables->set_pagesize(%d) failed: %s\n", WORD_RESULT_PAGESIZE, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = variables->open(variables, (char*)filename, "variables", DB_HASH, DB_CREATE, 0666)) != 0) {
    fprintf(stderr, "WordResults::Open variables->open(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = variables->cursor(variables, 0, &variables_cursor, 0)) != 0) {
    fprintf(stderr, "WordResults::Open variables->cursor(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Sorted list
  //
  if((error = CDB_db_create(&sorted, dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Open db_create(%s) sorted failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = sorted->set_pagesize(sorted, WORD_RESULT_PAGESIZE)) != 0) {
    fprintf(stderr, "WordResults::Open sorted->set_pagesize(%d) failed: %s\n", WORD_RESULT_PAGESIZE, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = sorted->set_bt_compare(sorted, wordResults_cmp)) != 0) {
    fprintf(stderr, "WordResults::Open sorted->set_bt_compare(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }
  
  if((error = sorted->open(sorted, (char*)filename, "sorted", DB_BTREE, DB_CREATE, 0666)) != 0) {
    fprintf(stderr, "WordResults::Open sorted->open(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = sorted->cursor(sorted, 0, &sorted_cursor, 0)) != 0) {
    fprintf(stderr, "WordResults::Open sorted->cursor(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Uniq list
  //
  if((error = CDB_db_create(&uniq, dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Open db_create(%s) uniq failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = uniq->set_pagesize(uniq, WORD_RESULT_PAGESIZE)) != 0) {
    fprintf(stderr, "WordResults::Open uniq->set_pagesize(%d) failed: %s\n", WORD_RESULT_PAGESIZE, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = uniq->set_bt_compare(uniq, wordResults_cmp)) != 0) {
    fprintf(stderr, "WordResults::Open uniq->set_bt_compare(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }
  
  if((error = uniq->open(uniq, (char*)filename, "uniq", DB_BTREE, DB_CREATE, 0666)) != 0) {
    fprintf(stderr, "WordResults::Open uniq->open(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = uniq->cursor(uniq, 0, &uniq_cursor, 0)) != 0) {
    fprintf(stderr, "WordResults::Open uniq->cursor(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Ranked list
  //
  if((error = CDB_db_create(&ranked, dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Open db_create(%s) ranked failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = ranked->set_pagesize(ranked, WORD_RESULT_PAGESIZE)) != 0) {
    fprintf(stderr, "WordResults::Open ranked->set_pagesize(%d) failed: %s\n", WORD_RESULT_PAGESIZE, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = ranked->set_re_len(ranked, sizeof(WordKeyNum) * document_length)) != 0) {
    fprintf(stderr, "WordResults::Open ranked->set_re_len(%d) failed: %s\n", sizeof(WordKeyNum) * document_length, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = ranked->open(ranked, (char*)filename, "ranked", DB_RECNO, DB_CREATE, 0666)) != 0) {
    fprintf(stderr, "WordResults::Open ranked->open(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = ranked->cursor(ranked, 0, &ranked_cursor, 0)) != 0) {
    fprintf(stderr, "WordResults::Open ranked->cursor(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Info list
  //
  if((error = CDB_db_create(&info, dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Open db_create(%s) info failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = info->set_pagesize(info, WORD_RESULT_PAGESIZE)) != 0) {
    fprintf(stderr, "WordResults::Open info->set_pagesize(%d) failed: %s\n", WORD_RESULT_PAGESIZE, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = info->open(info, (char*)filename, "info", DB_RECNO, DB_CREATE, 0666)) != 0) {
    fprintf(stderr, "WordResults::Open info->open(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  if((error = info->cursor(info, 0, &info_cursor, 0)) != 0) {
    fprintf(stderr, "WordResults::Open info->cursor(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  return OK;
}

int WordResults::Close()
{
  int error;

  //
  // Variables list
  //
  if((error = variables_cursor->c_close(variables_cursor)) != 0) {
    fprintf(stderr, "WordResults::Close variables_cursor->close(%s) failed: %s\n", name, CDB_db_strerror(error));
  }
  
  if((error = variables->close(variables, 0)) != 0) {
    fprintf(stderr, "WordResults::Close variables->close(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Sorted list
  //
  if((error = sorted_cursor->c_close(sorted_cursor)) != 0) {
    fprintf(stderr, "WordResults::Close sorted_cursor->close(%s) failed: %s\n", name, CDB_db_strerror(error));
  }
  
  if((error = sorted->close(sorted, 0)) != 0) {
    fprintf(stderr, "WordResults::Close sorted->close(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Uniq list
  //
  if((error = uniq_cursor->c_close(uniq_cursor)) != 0) {
    fprintf(stderr, "WordResults::Close uniq_cursor->close(%s) failed: %s\n", name, CDB_db_strerror(error));
  }
  
  if((error = uniq->close(uniq, 0)) != 0) {
    fprintf(stderr, "WordResults::Close uniq->close(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Ranked list
  //
  if((error = ranked_cursor->c_close(ranked_cursor)) != 0) {
    fprintf(stderr, "WordResults::Close ranked_cursor->close(%s) failed: %s\n", name, CDB_db_strerror(error));
  }

  if((error = ranked->close(ranked, 0)) != 0) {
    fprintf(stderr, "WordResults::Close ranked->close(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Info list
  //
  if((error = info_cursor->c_close(info_cursor)) != 0) {
    fprintf(stderr, "WordResults::Close info_cursor->close(%s) failed: %s\n", name, CDB_db_strerror(error));
  }

  if((error = info->close(info, 0)) != 0) {
    fprintf(stderr, "WordResults::Close info->close(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Environment
  //
  if((error = dbenv->close(dbenv, 0)) != 0) {
    fprintf(stderr, "WordResults::Close dbenv->close(%s) failed: %s\n", name, CDB_db_strerror(error));
    return NOTOK;
  }

  free(name);

  name = 0;
  sorted = 0;
  uniq = 0;
  ranked = 0;
  info = 0;
  dbenv = 0;
  return OK;
}

int WordResults::KeySemantic(const WordKeySemantic& nkey_semantic)
{
  key_semantic = nkey_semantic;

  uniq_offset = key_semantic.Uniq();
  document_offset = key_semantic.DocumentOffset();
  document_length = key_semantic.DocumentLength();

  return OK;
}

int WordResults::Filled() const
{
  //
  // If reached the end of result list and no context was saved, it means
  // that this is the end of all possible results.
  //
  String context_in;
  GetContext(context_in);
  return context_in.empty();
}  

int WordResults::Exists(const WordKey& key) const
{
  DBT rkey;
  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)(key.Values() + document_offset);
  rkey.size = sizeof(WordKeyNum) * document_length;
  rkey.app_private = (void*)document_length;

  DBT rdata;
  db_recno_t recno;
  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)&recno;
  rdata.size = sizeof(db_recno_t);

  int error;
  if((error = sorted_cursor->c_get(sorted_cursor, &rkey, &rdata, DB_SET)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::Exists sorted_cursor->c_get(%s) failed: %s\n", (char*)key.Get(), CDB_db_strerror(error));
    }
  }

  if(verbose)
    fprintf(stderr, "WordResults::Exists: %s %s\n", (char*)key.Get(), (error ? "was not found" : "was found"));

  return error == 0;
}

int WordResults::UniqExists(const WordKey& key) const
{
  DBT rkey;
  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)(key.Values() + uniq_offset);
  rkey.size = sizeof(WordKeyNum);
  rkey.app_private = (void*)1;

  DBT rdata;
  db_recno_t recno;
  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)&recno;
  rdata.size = sizeof(db_recno_t);

  int error;
  if((error = uniq_cursor->c_get(uniq_cursor, &rkey, &rdata, DB_SET)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::UniqExists uniq_cursor->c_get(%s) failed: %s\n", (char*)key.Get(), CDB_db_strerror(error));
    }
  }

  if(verbose)
    fprintf(stderr, "WordResults::UniqExists: %s %s\n", (char*)key.Get(), (error ? "was not found" : "was found"));

  return error == 0;
}

int WordResults::Get(WordMatches* matches, unsigned int length, unsigned int position) const
{
  if(verbose)
    fprintf(stderr, "WordResults::Get: %d to %d\n", position, position + length);

  int error;
  db_recno_t recno = position + 1;
  DBT rkey;
  DBT rdata;

  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)&recno;
  rkey.size = sizeof(db_recno_t);
  memset((void*)&rdata, '\0', sizeof(DBT));

  //
  // Ranked list
  //
  if((error = ranked_cursor->c_get(ranked_cursor, &rkey, &rdata, DB_SET)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::Get ranked_cursor->c_get(%d, DB_SET) failed: %s\n", position, CDB_db_strerror(error));
    }
    return WORD_WALK_END_CACHE;
  }
  //
  // Info list
  //
  if((error = info_cursor->c_get(info_cursor, &rkey, &rdata, DB_SET)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::Get info_cursor->c_get(%d, DB_SET) failed: %s\n", position, CDB_db_strerror(error));
    }
    return WORD_WALK_END_CACHE;
  }

  if(verbose)
    fprintf(stderr, "WordResults::Get found match at position %d\n", position);

  for(unsigned int i = 0; i < length && error == 0; i++) {
    WordMatch& match = *matches->matches[i];

    //
    // Ranked list
    //
    if((error = ranked_cursor->c_get(ranked_cursor, &rkey, &rdata, DB_CURRENT)) != 0) {
      fprintf(stderr, "WordResults::Get ranked_cursor->c_get(%d, DB_CURRENT) failed: %s\n", position, CDB_db_strerror(error));
      return NOTOK;
    }

    int* values = (int*)rdata.data;
    for(int j = document_offset; j < document_offset + document_length; j++) {
      match.match.Set(j, values[j - document_offset]);
    }

    recno = *(db_recno_t*)(rkey.data);
    if(recno - 1 != position + i) {
      fprintf(stderr, "WordResults::Get ranked retrieved %s at position %d instead of expected %d\n", (char*)match.Get(), recno - 1, position + i);
    }
    
    //
    // Info list
    //
    if((error = info_cursor->c_get(info_cursor, &rkey, &rdata, DB_CURRENT)) != 0) {
      fprintf(stderr, "WordResults::Get info_cursor->c_get(%d, DB_CURRENT) failed: %s\n", position, CDB_db_strerror(error));
      return NOTOK;
    }

    match.info.set((char*)rdata.data, (int)rdata.size);

    recno = *(db_recno_t*)(rkey.data);
    if(recno - 1 != position + i) {
      fprintf(stderr, "WordResults::Get info retrieved %s at position %d instead of expected %d\n", (char*)match.Get(), recno - 1, position + i);
    }
    
    //
    // Ranked list
    //
    if((error = ranked_cursor->c_get(ranked_cursor, &rkey, &rdata, DB_NEXT)) != 0) {
      if(error != DB_NOTFOUND) {
	fprintf(stderr, "WordResults::Get ranked_cursor->c_get(%d, DB_NEXT) failed: %s\n", recno, CDB_db_strerror(error));
	return NOTOK;
      }
    }

    //
    // Info list
    //
    if((error = info_cursor->c_get(info_cursor, &rkey, &rdata, DB_NEXT)) != 0) {
      if(error != DB_NOTFOUND) {
	fprintf(stderr, "WordResults::Get info_cursor->c_get(%d, DB_NEXT) failed: %s\n", recno, CDB_db_strerror(error));
	return NOTOK;
      }
    }

    if(verbose)
      fprintf(stderr, "WordResults::Get retrieved %s at position %d\n", (char*)match.Get(), position + i);
    
    match.valid = 1;
    matches->length++;
  }

  int ret = OK;
  //
  // If reached the end of result list and no context was saved, it means
  // that this is the end of all possible results.
  //
  if(error == DB_NOTFOUND) {
    ret = Filled() ? WORD_WALK_ATEND :  WORD_WALK_END_CACHE;
  }

  return ret;
}

int WordResults::Put(const WordMatch& match, unsigned int position)
{
  if(verbose)
    fprintf(stderr, "WordResults::Put: %s at %d\n", (char*)match.Get(), position);
  
  int error;
  db_recno_t recno = 0;
  DBT rkey;
  DBT rdata;

  //
  // Ranked list
  //
  memset((void*)&rkey, '\0', sizeof(DBT));
  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)(match.match.Values() + document_offset);
  rdata.size = sizeof(WordKeyNum) * document_length;

  if((error = ranked->put(ranked, 0, &rkey, &rdata, DB_APPEND)) != 0) {
    fprintf(stderr, "WordResults::Put ranked->put(%s) failed: %s\n", (char*)match.Get(), CDB_db_strerror(error));
    return NOTOK;
  }

  recno = *(db_recno_t*)(rkey.data);
  if(recno - 1 != position) {
    fprintf(stderr, "WordResults::Put ranked->put(%s) inserted at position %d instead of expected %d\n", (char*)match.Get(), recno - 1, position);
    return NOTOK;
  }
  
  //
  // Info list
  //
  memset((void*)&rkey, '\0', sizeof(DBT));
  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)match.info.get();
  rdata.size = match.info.length();

  if((error = info->put(info, 0, &rkey, &rdata, DB_APPEND)) != 0) {
    fprintf(stderr, "WordResults::Put info->put(%s) failed: %s\n", (char*)match.Get(), CDB_db_strerror(error));
    return NOTOK;
  }

  recno = *(db_recno_t*)(rkey.data);
  if(recno - 1 != position) {
    fprintf(stderr, "WordResults::Put info->put(%s) inserted at position %d instead of expected %d\n", (char*)match.Get(), recno - 1, position);
    return NOTOK;
  }
  
  //
  // Sorted list
  //
  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)(match.match.Values() + document_offset);
  rkey.size = sizeof(WordKeyNum) * document_length;
  rkey.app_private = (void*)document_length;

  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)&recno;
  rdata.size = sizeof(db_recno_t);

  if((error = sorted_cursor->c_put(sorted_cursor, &rkey, &rdata, DB_KEYLAST)) != 0) {
    fprintf(stderr, "WordResults::Put sorted_cursor->c_put(%s) failed: %s\n", (char*)match.Get(), CDB_db_strerror(error));
    return NOTOK;
  }

  //
  // Uniq list
  //
  if(uniq_offset) {
    memset((void*)&rkey, '\0', sizeof(DBT));
    rkey.data = (void*)(match.match.Values() + uniq_offset);
    rkey.size = sizeof(WordKeyNum);
    rkey.app_private = (void*)1;

    memset((void*)&rdata, '\0', sizeof(DBT));
    rdata.data = (void*)&recno;
    rdata.size = sizeof(db_recno_t);

    if((error = uniq_cursor->c_put(uniq_cursor, &rkey, &rdata, DB_KEYLAST)) != 0) {
      fprintf(stderr, "WordResults::Put uniq_cursor->c_put(%s) failed: %s\n", (char*)match.Get(), CDB_db_strerror(error));
      return NOTOK;
    }
  }

  return OK;
}

int WordResults::PutContext(const String& context_out)
{
  if(verbose)
    fprintf(stderr, "WordResults::PutContext: %s\n", (const char*)context_out);
  
  DBT rkey;
  DBT rdata;
  int error;

  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)"context";
  rkey.size = strlen((char*)rkey.data);

  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)context_out.get();
  rdata.size = context_out.length();

  if((error = variables_cursor->c_put(variables_cursor, &rkey, &rdata, DB_KEYFIRST)) != 0) {
    fprintf(stderr, "WordResults::PutContext variables_cursor->c_put(%s) failed: %s\n", (const char*)context_out, CDB_db_strerror(error));
    return NOTOK;
  }

  return OK;
}

int WordResults::GetContext(String& context_in) const
{
  if(verbose)
    fprintf(stderr, "WordResults::GetContext:\n");

  context_in.trunc();
  
  DBT rkey;
  DBT rdata;

  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)"context";
  rkey.size = strlen((char*)rkey.data);

  memset((void*)&rdata, '\0', sizeof(DBT));

  int error;
  if((error = variables_cursor->c_get(variables_cursor, &rkey, &rdata, DB_SET)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::GetContext variables_cursor->c_get(context) failed: %s\n", CDB_db_strerror(error));
      return NOTOK;
    }
  }

  context_in.set((char*)rdata.data, (unsigned int)rdata.size);

  return OK;
}

int WordResults::PutMatchesTotal(unsigned int matches_total)
{
  if(verbose)
    fprintf(stderr, "WordResults::PutMatchesTotal: %d\n", matches_total);
  
  DBT rkey;
  DBT rdata;
  int error;

  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)"matches_total";
  rkey.size = strlen((char*)rkey.data);

  memset((void*)&rdata, '\0', sizeof(DBT));
  rdata.data = (void*)&matches_total;
  rdata.size = sizeof(unsigned int);

  if((error = variables_cursor->c_put(variables_cursor, &rkey, &rdata, DB_KEYFIRST)) != 0) {
    fprintf(stderr, "WordResults::PutMatchesTotal: variables_cursor->c_put(%d) failed: %s\n", matches_total, CDB_db_strerror(error));
    return NOTOK;
  }

  return OK;
}

int WordResults::GetMatchesTotal(unsigned int& matches_total) const
{
  if(verbose)
    fprintf(stderr, "WordResults::GetMatchesTotal:\n");

  matches_total = 0;

  DBT rkey;
  DBT rdata;

  memset((void*)&rkey, '\0', sizeof(DBT));
  rkey.data = (void*)"matches_total";
  rkey.size = strlen((char*)rkey.data);

  memset((void*)&rdata, '\0', sizeof(DBT));

  int error;
  if((error = variables_cursor->c_get(variables_cursor, &rkey, &rdata, DB_SET)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::GetMatchesTotal variables_cursor->c_get(matches_total) failed: %s\n", CDB_db_strerror(error));
      return NOTOK;
    }
  }

  if(error == 0) {
    memcpy((char*)&matches_total, (char*)rdata.data, sizeof(unsigned int));
  } else {
    matches_total = 0;
  }

  return OK;
}

int WordResults::Count(unsigned int& count) const
{
  int error;
  DBT rkey;
  DBT rdata;

  memset((void*)&rkey, '\0', sizeof(DBT));
  memset((void*)&rdata, '\0', sizeof(DBT));

  if((error = ranked_cursor->c_get(ranked_cursor, &rkey, &rdata, DB_LAST)) != 0) {
    if(error != DB_NOTFOUND) {
      fprintf(stderr, "WordResults::Count ranked_cursor->c_get(DB_LAST) failed: %s\n", CDB_db_strerror(error));
      return NOTOK;
    }
    count = 0;
  } else {
    count = *(unsigned int*)(rkey.data);
  }

  if(verbose)
    fprintf(stderr, "WordResults::Count %d\n", count);

  return OK;
}
