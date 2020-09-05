%{
#include "gnats.h"
#include "fconfig.h"

#define YY_INPUT(buf, result, max_size) { \
  extern int (*yyinpfunc)(char *, int); \
  result = (yyinpfunc)((buf), (max_size)); \
}

char *qStrVal (struct qstring *str);
char *takeQString (struct qstring *str);
void cleanLexer (void);

int fconfig_lineno;

static struct qstring {
  char *str;
  struct qstring *prev, *next;
} *qstringList;

char *
qStrVal (struct qstring *str)
{
  return str->str;
}

char *
takeQString (struct qstring *str)
{
  char *res = str->str;
  if (str->prev != NULL)
    {
      str->prev->next = str->next;
    }
  else
    {
      qstringList = str->next;
    }
  if (str->next != NULL)
    {
      str->next->prev = (str->prev);
    }
  free (str);
  return res;
}

void
cleanLexer (void)
{
  struct qstring *p = qstringList;

  while (p != NULL)
    {
      struct qstring *next = p->next;
      free (p->str);
      free (p);
      p = next;
    }
  qstringList = NULL;
}

/* This is pretty crummy. */
static struct qstring *
lexQString (char *str) 
{
  int len = strlen (str) - 1;
  char *dest;
  int z;
  struct qstring *res = (struct qstring *) xmalloc (sizeof (struct qstring));
  res->str = xmalloc (len + 1);
  dest = res->str;

  if (qstringList == NULL)
    {
      res->next = NULL;
      res->prev = NULL;
    }
  else
    {
      res->next = qstringList;
      qstringList->prev = res;
      res->prev = NULL;
    }
  qstringList = res;

  for (z = 0; z < len; z++)
    {
      if (str[z] == '\\')
	{
	  z++;
	  if (z < len)
	    {
	      switch (str[z])
		{
		case '\n':
		  break;
		case 'n':
		  *(dest++) = '\n';
		  break;
		case 't':
		  *(dest++) = '\t';
		  break;
		case 'r':
		  *(dest++) = '\r';
		  break;
		case '"':
		  *(dest++) = '"';
		  break;
		case '\\':
		  *(dest++) = '\\';
		  break;
		default:
		  *(dest++) = '\\';
		  *(dest++) = str[z];
		  break;
		}
	    }
	}
      else
	{
	  *(dest++) = str[z];
	}
    }
  *(dest) = '\0';
  return res;
}

%}

%option noinput
%option never-interactive
%option noyywrap
%option nounput
%x BadTok
%x QuoteLex

%%
field {
  return FIELD;
}

text {
  return STRINGTYPE;
}

multitext {
  return MULTITEXTTYPE;
}

date {
  return DATETYPE;
}

path {
  return PATHTOK;
}

query-default {
  return QDEFAULT;
}

matching {
  return MATCHING;
}

enum {
  return ENUM;
}

multienum {
  return MULTIENUMTOK;
}

enumerated-in-file {
  return ENUM_IN_FILE;
}

multi-enumerated-in-file {
  return MULTI_ENUM_IN_FILE;
}

key {
  return KEYTOK;
}

fields {
  return FIELDSTOK;
}

values {
  return VALUES;
}

default {
  return DEFAULT;
}

input-default {
  return INPUTDEFAULTTOK;
}

exact-regexp {
  return EXACT_REGEXP;
}

inexact-regexp {
  return INEXACT_REGEXP;
}

all {
  return ALL;
}

format {
  return FORMATTOK;
}

textsearch {
  return TEXTSEARCH;
}

query {
  return QUERYTOK;
}

index {
  return INDEXTOK;
}

separator {
  return SEPARATORTOK;
}

restricted {
  return RESTRICTEDTOK;
}

integer {
  return INTEGERTOK;
}

nospaces-in-index {
  return NOSPACESTOK;
}

builtin-name {
   return BUILTINTOK;
}

allow-any-value {
   return ALLOWANYVALUETOK;
}

on-change {
   return CHANGETOK;
}

require {
   return REQUIRETOK;
}

append-to-field {
   return APPENDFIELDTOK;
}

set-field {
   return SETFIELDTOK;
}

description {
   return DESCRIPTIONTOK;
}

initial-entry {
   return INPUTTOK;
}

database-info {
    return DATABASEINFOTOK;
}

debug-mode {
    return DEBUGMODETOK;
}

keep-all-received-headers {
    return KEEPRECTOK;
}

notify-about-expired-prs {
    return NOTIFYEXPTOK;
}

send-submitter-ack {
    return SUBMITTERACKTOK;
}

libexecdir {
    return LIBEXECDIRTOK;
}

business-day-hours {
    return BUSINESSDAYTOK;
}

business-week-days {
    return BUSINESSWEEKTOK;
}

create-category-dirs {
    return CREATECATEGORYDIRSTOK;
}

false {
    return FALSETOK;
}

true {
    return TRUETOK;
}

separators {
    return ENUMSEPARATORSTOK;
}

mail-format {
    return MAILFORMATTOK;
}

to-addresses {
    return TOADDRESSESTOK;
}

from-address {
    return FROMADDRESSTOK;
}

reply-to {
    return REPLYTOTOK;
}

fixed-address {
    return FIXEDTOK;
}

body {
    return BODYTOK;
}

header {
    return HEADERTOK;
}

audit-trail-format {
    return AUDITTRAILFMTTOK;
}

add-audit-trail {
    return ADDAUDITTRAILTOK;
}

require-change-reason {
    return REQUIRECHANGEREASONTOK;
}

read-only {
    return READONLYTOK;
}

binary-index {
    return BINARYINDEXTOK;
}

aux-flags {
    return AUXFLAGSTOK;
}

pr-list {
    return PRLISTTOK;
}

max-prs-in-field {
    return MAXPRSTOK;
}

edit-only {
    return EDITONLYTOK;
}

virtual-format {
    return VIRTUALFORMATTOK;
}

raw {
    return RAWTOK;
}

category-dir-perms {
    return CATPERMSTOK;
}

[|] {
    return '|';
}

"{"|"}" {
  return yytext[0];
}

["] {
  BEGIN(QuoteLex);
}

<QuoteLex>[\\][\n] { fconfig_lineno++; yymore (); }
<QuoteLex>[\\]. { yymore (); }
<QuoteLex>[\n] {
   fprintf (stderr, "Warning: unquoted newline within quoted string at line %d\n",
            fconfig_lineno);
   fconfig_lineno++;
   yymore ();
}
<QuoteLex>[^"\n\\]+ { yymore (); }
<QuoteLex>["] {
   fconflval.qstr = lexQString (yytext);
   BEGIN (0);
   return QSTRING;
}

[#].*  /* A comment */

[0-9]+ {
   fconflval.intval = atoi (yytext);
   return INTVAL;
}

[-] {
   return '-';
}

[ \t]+	     /* The usual "eat up whitespace" */
[\n] {
   fconfig_lineno++;
}

. {
   BEGIN (BadTok);
   yymore ();
}

<BadTok>[^ \t\n#]* {
   fprintf (stderr, "Unrecognized token %s at line %d\n", yytext,
            fconfig_lineno);
   BEGIN (0);
   return BADTOK;
}
