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
#include <stdlib.h>

#include <htString.h>

#include <sys/time.h>
#include <WordContext.h>
#include <WordList.h>
#include <Configuration.h>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <map>

using namespace std;

struct wordCounting{
	string word;
	int nb;
        string filename;
};


char charFilter (char pin)
{
        pin=tolower(pin);
	if (pin == 'é' || pin=='è' || pin=='ê' || pin =='ë' )
		return 'e';
	if (pin == 'à' || pin=='â' || pin =='ä')
		return 'a';
	if (pin == 'î' || pin=='ï'  )
		return 'i';
	if (pin == 'ô' || pin=='ö'  )
		return 'o';
	if (pin == 'ù' || pin=='û'  || pin=='ü')
		return 'u';
	if (pin == ',' || pin=='"'  || pin=='\''|| pin==';'|| pin=='!'|| pin=='?'|| pin=='$'|| pin=='&'|| pin==')'|| pin=='('|| pin=='.'|| pin==':' || pin=='-'|| pin=='_' || pin=='—' || pin==']' || pin=='[' || pin=='/' )
		return ' ';
	return pin;
}

string stringFilter (string &pin)
{
	stringstream ret;
	for (unsigned int i=0;i<pin.length();i++)
	{
		ret<<charFilter (pin.at(i));
	}
	return ret.str();
}

map<string,wordCounting *> parseWords (string &pFile)
{
	cout <<"start parseWords file="<<pFile<<endl;
	map<string,wordCounting *> list;
	char c;
	ifstream is;
	is.open (pFile.c_str());        // open file
	string wordstream;
	map<string,wordCounting *>::iterator it;
  	while (is.good())     // loop while extraction from file is possible
  	{
    		is.get(c);       // get character from file
		
		c=charFilter(c);
		
    		if (c==' '||c=='\n' || c=='\t' || c=='\0')
		{
			if (!wordstream.empty())
			{
				
				wordCounting *res;
				it=list.find(wordstream);
				if( it == list.end() ) {
					res = new wordCounting();
					res->word=wordstream;
					res->nb=0;
					res->filename=pFile;
					list.insert( make_pair( wordstream, res ) ); 
				}
				else
					res=it->second;
				res->nb++;
			}
			
			wordstream="";
		}
		else
		{
			wordstream+=c;
		}
  	}
	cout <<"close file="<<pFile<<endl;
	is.close();
	cout <<"list size="<<list.size()<<endl;
	return list;
}
static map<int,string> file_list; 
void addList(map<string,wordCounting *> &list,WordList *words)
{
	map<int,string>::iterator iteror;
	
	map<string,wordCounting *>::iterator iter;
  	for( iter = list.begin(); iter != list.end(); iter++ ) {
		bool isOnList=false;
		int idFile=-100;
        	for (iteror=file_list.begin();iteror!=file_list.end();iteror++)
		{
			if (iteror->second==iter->second->filename)
			{
				idFile=iteror->first;
				isOnList=true;
				break;
			}
		}
		if (!isOnList)
		{
			idFile=file_list.size()+1;
			file_list.insert(make_pair(idFile,iter->second->filename));
		}
    		stringstream el;
		el<<iter->second->word<<" "<<iter->second->nb<<" "<<idFile;
		
		words->Override(words->Word(el.str().c_str()));
		
  	}
}


static ConfigDefaults defaultsInd[] = {
  //
  // Very simple key structure: word+one numerical field
  //
  { "wordlist_wordkey_description","Word 30/Rank 15/Location 15"},
  { 0 }
};

void action2(WordContext* context)
{
  printf("start action2\n");
  //
  // Open a new inverted index (O_RDWR), reseting it
  // to empty if it was not empty (O_TRUNC).
  //
  WordList *words = context->List();
  words->Open("words2.db", O_RDWR|O_TRUNC);

  //
  // The WordReference object now holds data that we want to
  // save in the inverted index. The Insert metho ensure that
  // two identical keys are not inserted in the index (no
  // duplicates allowed).
  //
  /*
  words->Override(words->Word("computer 1"));
  words->Override(words->Word("computing 16"));
  words->Override(words->Word("computing 10"));
  words->Override(words->Word("hello 20"));
  */
  string file = "file1.test";
  timeval timerFile;
  timeval timerFile2;
  gettimeofday(&timerFile,NULL);
  map<string,wordCounting *> list=parseWords (file);
  addList(list,words);
  file = "file2.test";
  list=parseWords (file);
  addList(list,words);
  file = "file3.test";
  list=parseWords (file);
  addList(list,words);
  file = "file4.test";
  list=parseWords (file);
  addList(list,words);
  file = "file5.test";
  list=parseWords (file);
  addList(list,words);
  file = "file6.test";
  list=parseWords (file);
  addList(list,words);
  file = "file7.test";
  list=parseWords (file);
  addList(list,words);
  file = "file8.test";
  list=parseWords (file);
  addList(list,words);
  file = "file9.test";
  list=parseWords (file);
  addList(list,words);
  file = "file10.test";
  list=parseWords (file);
  addList(list,words);
  file = "file11.test";
  list=parseWords (file);
  addList(list,words);
  gettimeofday(&timerFile2,NULL);
  double timerFile3=timerFile2.tv_usec-timerFile.tv_usec;
  timerFile3=timerFile3/1000000.0;
  cout <<"Time to index 11 files is :"<<timerFile3<< " seconds"<<endl;
  //
  // We now search for all the words matching computing
  //
  gettimeofday(&timerFile,NULL);
  List *results = words->FindWord("which");
  gettimeofday(&timerFile2,NULL);
  timerFile3=timerFile2.tv_usec-timerFile.tv_usec;
  timerFile3=timerFile3/1000000.0;
  cout <<"Time to retrieve the word 'which' is :"<<timerFile3<< " seconds"<<endl;
  //
  // And display them
  //
  WordReference *match;
  for(results->Start_Get(); (match = (WordReference*)results->Get_Next());) {
    //printf("%s\n", match->GetWord().get());
    //cout <<"id="<<match->Key().Get(2)<<endl;
    map<int,string>::iterator iteror;
    iteror = file_list.find(match->Key().Get(2));
    cout <<"on File ="<<iteror->second<<" with number of words="<<match->Key().Get(1) <<endl;
    //match->Print();
  }

  printf("\n");
  delete results;
  gettimeofday(&timerFile,NULL);
  results = words->FindWord("philosophy");
  gettimeofday(&timerFile2,NULL);
  timerFile3=timerFile2.tv_usec-timerFile.tv_usec;
  timerFile3=timerFile3/1000000.0;
  cout <<"Time to retrieve the word 'philosophy' is :"<<timerFile3<< " seconds"<<endl;
  //
  // And display them
  //
  
  for(results->Start_Get(); (match = (WordReference*)results->Get_Next());) {
    //printf("%s\n", match->GetWord().get());
    //cout <<"id="<<match->Key().Get(2)<<endl;
    map<int,string>::iterator iteror;
    iteror = file_list.find(match->Key().Get(2));
    cout <<"on File ="<<iteror->second<<" with number of words ="<<match->Key().Get(1) <<endl;
    //match->Print();
  }

  //
  // It's not so much important to release the file descriptor
  // than to flush the cached information.
  //
  words->Close();

  delete words;
}

int main2(void) {
  //
  // WordContext is the central object from which all 
  // mifluz objects are created.
  //
  printf("Main 2\n");
  WordContext *context = new WordContext(defaultsInd);
  action2(context);
  delete context;

  printf("Success\n");
}
