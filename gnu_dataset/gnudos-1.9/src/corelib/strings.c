/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2016 (c)
 * 
 *    file: strings.c
 *    This file is part of the GnuDOS project.
 *
 *    GnuDOS is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    GnuDOS is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with GnuDOS.  If not, see <http://www.gnu.org/licenses/>.
 */    
#include "strings.h"
#include "stdlib.h"
static str _string;
static str _base_of_string = (str)NULL;

int __do_indexof(str string, char chr, int reverse)
{
  if(!string || string[0] == '\0') return -1;	//make sure it is not NULL
  if(_base_of_string)
  {
	free(_base_of_string);
	_base_of_string = (str)NULL;
  }
  _string = (str) malloc(strlen(string)+1);
  if(!_string) return -1;
  strcpy(_string, string);
  _base_of_string = _string;
  
  str j = reverse ? strrchr(string, chr) : strchr(string, chr);
  if(j) {_string += (++j)-_string; return j-string; }
  return -1;
}

/********************************************
 * returns zero-based index of chr in string.
 * if not found, returns -1;
 * ******************************************/
int indexof(str string, char chr) 
{
	return __do_indexof(string, chr, 0);
}

/********************************************
 * returns zero-based last index of chr in 
 * string. if not found, returns -1;
 * ******************************************/
int lindexof(str string, char chr) 
{
	return __do_indexof(string, chr, 1);
}

/************************************************
 * returns zero-based index of next occurence
 * of chr in string. should be called after
 * a call to indexof(). if not found, returns -1;
 * **********************************************/
int nindexof(char chr) 
{
  if(!_string) return -1;	//make sure it is not NULL
  
  str j = strchr(_string, chr);
  if(j) { return j-_string; _string = ++j; }
  return -1;
}

/************************************************
 * returns substring of string starting at index
 * and running to the end of string.
 * If not found, returns -1;
 * **********************************************/
str substr(str string, int start) 
{
  if(!string) return NULL;	//make sure it is not NULL
  if(start < 0 || start >= strlen(string)) return NULL;
  return string+start;
}

/************************************************
 * returns substring of string starting at index
 * and running through to length chars.
 * If not found, returns -1;
 * **********************************************/
str nsubstr(str string, int start, int length) 
{
  if(!string) return NULL;	//make sure it is not NULL
  if(start < 0 || start >= strlen(string)) return NULL;
  if(length < 0 || length > strlen(string)) return NULL;
  str _s = (str) malloc(length+1);
  if(!_s) return NULL;
  strncpy(_s, string+start, length);
  _s[length] = '\0';
  return _s;
}

/************************************************
 * returns the string in CAPITALS.
 * **********************************************/
str uppercase(str string) 
{
  if(!string) return NULL;
  str _s = (str) malloc(strlen(string)+1);
  if(!_s) return NULL;
  strcpy(_s, string);
  int i;
  for(i = 0; i < strlen(_s); i++)
    if(_s[i] >= 'a' && _s[i] <= 'z')
      _s[i] -= 32;
  return _s;
}

/************************************************
 * returns the string in small letters.
 * **********************************************/
str lowercase(str string) 
{
  if(!string) return NULL;
  str _s = (str) malloc(strlen(string)+1);
  if(!_s) return NULL;
  strcpy(_s, string);
  int i;
  for(i = 0; i < strlen(_s); i++)
    if(_s[i] >= 'A' && _s[i] <= 'Z')
      _s[i] += 32;
  return _s;
}

/************************************************
 * Trims all the whitespace characters from the
 * strings' left end.
 * **********************************************/
str ltrim(str string) 
{
  if(!string) return NULL;
  str _s = (str) malloc(strlen(string)+1);
  if(!_s) return NULL;
  strcpy(_s, string);
  int i;
  for(i = 0; i < strlen(_s); i++)
    if(_s[i] == ' ' || _s[i] == '\t' || _s[i] == '\n') continue;
    else break;
  
  if(i == 0) return _s;
  else return _s+i;
}

/************************************************
 * Trims all the whitespace characters from the
 * strings' right end.
 * **********************************************/
str rtrim(str string) 
{
  if(!string) return NULL;
  str _s = (str) malloc(strlen(string)+1);
  if(!_s) return NULL;
  strcpy(_s, string);
  int i;
  for(i = strlen(_s)-1; i >= 0; i--)
    if(_s[i] == ' ' || _s[i] == '\t' || _s[i] == '\n') continue;
    else break;
  
  if(i == strlen(_s)-1) return _s;
  else { _s[i+1] = '\0'; return _s; }
}

/************************************************
 * Trims all the whitespace characters from the
 * strings' both left and right ends.
 * **********************************************/
str trim(str string) 
{
  if(!string) return NULL;
  str _s = (str) malloc(strlen(string)+1);
  if(!_s) return NULL;
  strcpy(_s, string);
  int i;
  //trim the left side
  for(i = 0; i < strlen(_s); i++)
    if(_s[i] == ' ' || _s[i] == '\t' || _s[i] == '\n') continue;
    else break;
  
  if(i != 0) _s += i;
  //trim the right side
  for(i = strlen(_s)-1; i >= 0; i--)
    if(_s[i] == ' ' || _s[i] == '\t' || _s[i] == '\n') continue;
    else break;
  
  if(i == strlen(_s)-1) return _s;
  else { _s[i+1] = '\0'; return _s; }
}
