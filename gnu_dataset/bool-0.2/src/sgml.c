/* sgml.c - SGML constructs used in HTML.
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Written by Marc Tardif <intmktg@cam.org>.  */

/*
   The routines in this program support the SGML constructs that are
   used in the HTML 4.01 standard (http://www.w3c.org/TR/html4/).

   An SGML document type definition declares element types that represent
   structures and desired behavior.  For pattern matching, only the
   element types affecting the spacing between display characters are
   considered.  Some common examples include:
   - "<p>" represents a new paragraph.
   - "<br>" represents a new line.
   - "<sup>" represents a space.
   - "<b>" is ignored.

   HTML also supports character references which are interpreted as
   numeric or symbolic names. Numeric names can be represented as a
   decimal value ('&#123;') or as a hexadecimal value ('&#xABC;').
   Symbolic names are a sequence of ASCII letters and numbers
   ('&foobar;').
*/

#include <stdlib.h>

#include "sgml.h"

#define ENTITY_NUM 100
#define MAX_ENTITY_LEN 6

#define is_digit(c) ((c)>0x2F && (c)<0x3A)
#define is_alpha(c) (((c)>0x40 && (c)<0x5B) \
                     || ((c)>0x60 && (c)<0x7B))
#define is_space(c) ((c)==0x09 || (c)==0x0A || (c)==0x0B || (c)==0x0C \
                     || (c)==0x0D || (c)==0x20 || (c)==0x3E)

struct entitylist
{
  char name[MAX_ENTITY_LEN];
  unsigned char val;
};

static unsigned char
get_entity (char **str, char *lim)
{
  static unsigned char entityhash[] = {
       16,  1, 70,  0,  4, 70, 61,123,  0, 70, 25,  0,123,  0, 76, 26,
        0, 61,  0,  1, 51,  1, 76,  0,  4,  0,  4, 85, 47,  0, 51, 87,
       16,  0,101,106, 76, 76, 70,  0, 17,119,119,  5,  0, 47, 17,  0,
        1, 76,  0, 16, 76, 70,123, 83,  0, 16,104,112, 66,101,  0, 76,
  };

  static struct entitylist entitylist[ENTITY_NUM] = {
    {"gt", 62},
    {"raquo", 187},
    {"thorn", 254},
    {"uml", 168},
    {"ordf", 170},
    {"cedil", 184},
    {"micro", 181},
    {"Auml", 196},
    {"Eacute", 201},
    {"aelig", 230},
    {"Iacute", 205},
    {"Ccedil", 199},
    {"ugrave", 249},
    {"ntilde", 241},
    {"atilde", 227},
    {"ordm", 186},
    {"Yacute", 221},
    {"Uacute", 218},
    {"AElig", 198},
    {"curren", 164},
    {"acute", 180},
    {"Ograve", 210},
    {"Iuml", 207},
    {"frac12", 189},
    {"Ecirc", 202},
    {"frac14", 188},
    {"aring", 229},
    {"Acirc", 194},
    {"para", 182},
    {"yen", 165},
    {"icirc", 238},
    {"brvbar", 166},
    {"divide", 247},
    {"szlig", 223},
    {"THORN", 222},
    {"middot", 183},
    {"plusmn", 177},
    {"egrave", 232},
    {"eth", 240},
    {"yuml", 255},
    {"ouml", 246},
    {"uacute", 250},
    {"Agrave", 192},
    {"ccedil", 231},
    {"cent", 162},
    {"yacute", 253},
    {"macr", 175},
    {"not", 172},
    {"Aring", 197},
    {"frac34", 190},
    {"lt", 60},
    {"Oacute", 211},
    {"ecirc", 234},
    {"Otilde", 213},
    {"aacute", 225},
    {"Euml", 203},
    {"sup3", 179},
    {"sup2", 178},
    {"sup1", 185},
    {"times", 215},
    {"Ouml", 214},
    {"pound", 163},
    {"reg", 174},
    {"oacute", 243},
    {"oslash", 248},
    {"Aacute", 193},
    {"quot", 34},
    {"Oslash", 216},
    {"iexcl", 161},
    {"iuml", 239},
    {"Ocirc", 212},
    {"laquo", 171},
    {"amp", 38},
    {"Egrave", 200},
    {"agrave", 224},
    {"iacute", 237},
    {"acirc", 226},
    {"ograve", 242},
    {"uuml", 252},
    {"ocirc", 244},
    {"otilde", 245},
    {"euml", 235},
    {"ETH", 208},
    {"eacute", 233},
    {"shy", 173},
    {"Atilde", 195},
    {"nbsp", 160},
    {"auml", 228},
    {"copy", 169},
    {"sect", 167},
    {"Ugrave", 217},
    {"Icirc", 206},
    {"iquest", 191},
    {"Igrave", 204},
    {"deg", 176},
    {"Ntilde", 209},
    {"ucirc", 251},
    {"Uuml", 220},
    {"igrave", 236},
    {"Ucirc", 219}
  };

  register unsigned long hash = 0xf1bbcdc8;
  register unsigned char c;
  register char *s = *str;

  while ((c = *s++) && (is_digit (c) || is_alpha (c)))
    if (s - *str > 8)
      return '&';
    else
      hash = (c ^ hash) + ((hash<<26) + (hash>>6));

  if (c == 0 && s > lim)
    return 0;
  else if (c != ';')
    return '&';

  *str = s;
  return entitylist[((hash & 0x3F) ^ entityhash[hash >> 26])
    % ENTITY_NUM].val;
}

unsigned char
get_character (char **str, char *lim)
{
  register unsigned int base, acc = 0;
  register unsigned char c;
  register char *s = *str;

  if (*s++ != '#')
    return get_entity (str, lim);

  if (*s=='x' || *s=='X')
    base = 16, s++;
  else
    base = 10;

  while ((c = *s++) != ';')
    {
      if (is_digit (c))
        c -= '0';
      else if (is_alpha (c))
        c -= (c > 'Z') ? 'a' - 10 : 'A' - 10;
      else if (c == '\0' && s > lim)
        return 0;
      else
        return '&';
      if (c >= base)
        return '&';
      acc *= base;
      acc += c;
      if (acc > 256)
        return '&';
    }

  *str = s;
  return (unsigned char)acc;
}

enum element
get_element (char **str, char *lim)
{
  register int state = E_IG;
  register unsigned char c;
  register char *s = *str;

  if (*s=='/')
    s++;

  switch (*s)
    {
    case '!':
      /* comment */
      *str = s + 1;
      return E_IG;
    case 'a':
    case 'A':
      /* address */
      if ((*(++s)=='d' || *s=='D')
          && (*(++s)=='d' || *s=='D')
          && (*(++s)=='r' || *s=='R')
          && (*(++s)=='e' || *s=='E')
          && (*(++s)=='s' || *s=='S')
          && (*(++s)=='s' || *s=='S'))
        state = E_NP;
      break;
    case 'b':
    case 'B':
      switch (*(++s))
        {
        case 'l':
        case 'L':
          /* blockquote */
          if ((*(++s)=='o' || *s=='O')
              && (*(++s)=='c' || *s=='C')
              && (*(++s)=='k' || *s=='K')
              && (*(++s)=='q' || *s=='Q')
              && (*(++s)=='u' || *s=='U')
              && (*(++s)=='o' || *s=='O')
              && (*(++s)=='t' || *s=='T')
              && (*(++s)=='e' || *s=='E'))
            state = E_NP;
          break;
        case 'r':
        case 'R':
          /* br */
          state = E_NL;
          break;
        }
      break;
    case 'c':
    case 'C':
      switch (*(++s))
        {
        case 'a':
        case 'A':
          /* caption */
          if ((*(++s)=='p' || *s=='P')
              && (*(++s)=='t' || *s=='T')
              && (*(++s)=='i' || *s=='I')
              && (*(++s)=='o' || *s=='O')
              && (*(++s)=='n' || *s=='N'))
            state = E_NP;
          break;
        case 'e':
        case 'E':
          /* center */
          if ((*(++s)=='n' || *s=='N')
              && (*(++s)=='t' || *s=='T')
              && (*(++s)=='e' || *s=='E')
              && (*(++s)=='r' || *s=='R'))
            state = E_NP;
          break;
        }
      break;
    case 'd':
    case 'D':
      switch (*(++s))
        {
        case 'i':
        case 'I':
          switch (*(++s))
            {
            case 'r':
            case 'R':
              /* dir */
            case 'v':
            case 'V':
              /* div */
              state = E_NP;
              break;
            }
          break;
        case 'l':
        case 'L':
          /* dl */
          state = E_NP;
          break;
        case 't':
        case 'T':
          /* dt */
          state = E_NL;
          break;
        }
      break;
    case 'e':
    case 'E':
      break;
    case 'f':
    case 'F':
      switch (*(++s))
        {
        case 'o':
        case 'O':
          /* form */
          if ((*(++s)=='r' || *s=='R')
              && (*(++s)=='m' || *s=='M'))
            state = E_NP;
          break;
        case 'r':
        case 'R':
          /* frame */
          if ((*(++s)=='a' || *s=='A')
              && (*(++s)=='m' || *s=='M')
              && (*(++s)=='e' || *s=='E'))
            state = E_NP;
          break;
        }
      break;
    case 'g':
    case 'G':
      break;
    case 'h':
    case 'H':
      switch (*(++s))
        {
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
          /* h# */
        case 'r':
          /* hr */
          state = E_NP;
          break;
        }
      break;
    case 'i':
    case 'I':
      switch (*(++s))
        {
        case 'f':
        case 'F':
          /* iframe */
          if ((*(++s)=='r' || *s=='R')
              && (*(++s)=='a' || *s=='A')
              && (*(++s)=='m' || *s=='M')
              && (*(++s)=='e' || *s=='E'))
            state = E_NP;
          break;
        case 'm':
        case 'M':
          /* img */
          if (*(++s)=='g' || *s=='G')
            state = E_SP;
          break;
        }
      break;
    case 'j':
    case 'J':
    case 'k':
    case 'K':
      break;
    case 'l':
    case 'L':
      /* li */
      if (*(++s)=='i' || *s=='I')
        state = E_NL;
      break;
    case 'm':
    case 'M':
      /* menu */
      if ((*(++s)=='e' || *s=='E')
          && (*(++s)=='n' || *s=='N')
          && (*(++s)=='u' || *s=='U'))
        state = E_NP;
      break;
    case 'n':
    case 'N':
      break;
    case 'o':
    case 'O':
      /* option */
      if ((*(++s)=='p' || *s=='P')
          && (*(++s)=='t' || *s=='T')
          && (*(++s)=='i' || *s=='I')
          && (*(++s)=='o' || *s=='O')
          && (*(++s)=='n' || *s=='N'))
        state = E_SP;
      break;
    case 'p':
    case 'P':
      switch (*(++s))
        {
        case 'r':
        case 'R':
          /* pre */
          if (*(++s)=='e' || *s=='E')
            state = E_NL;
          break;
        case '>':
        case ' ':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case '\v':
          /* p */
          *str = s;
          return E_NP;
        }
      break;
    case 'q':
    case 'Q':
    case 'r':
    case 'R':
      break;
    case 's':
    case 'S':
      switch (*(++s))
        {
        case 'c':
        case 'C':
          /* script */
          if ((*(++s)=='r' || *s=='R')
              && (*(++s)=='i' || *s=='R')
              && (*(++s)=='p' || *s=='R')
              && (*(++s)=='t' || *s=='R'))
            state = E_NP;
          break;
        case 'u':
        case 'U':
          switch (*(++s))
            {
            case 'b':
            case 'B':
              /* sub */
            case 'p':
            case 'P':
              /* sup */
              state = E_SP;
              break;
            }
          break;
        }
      break;
    case 't':
    case 'T':
      switch (*(++s))
        {
        case 'a':
        case 'A':
          /* table */
          if ((*(++s)=='b' || *s=='B')
              && (*(++s)=='l' || *s=='L')
              && (*(++s)=='e' || *s=='E'))
            state = E_NP;
          break;
        case 'i':
        case 'I':
          /* title */
          if ((*(++s)=='t' || *s=='T')
              && (*(++s)=='l' || *s=='L')
              && (*(++s)=='e' || *s=='E'))
            state = E_NP;
          break;
        case 'd':
        case 'D':
          /* td */
        case 'h':
        case 'H':
          /* th */
          state = E_SP;
          break;
        case 'r':
        case 'R':
          /* tr */
          state = E_NL;
          break;
        }
      break;
    case 'u':
    case 'U':
      /* ul */
      if (*(++s)=='l' || *s=='L')
        state = E_NP;
      break;
    case 'v':
    case 'V':
    case 'w':
    case 'W':
    case 'x':
    case 'X':
    case 'y':
    case 'Y':
    case 'z':
    case 'Z':
    case '\0':
      break;
    default:
      return E_NA;
    }

  if (state != E_IG)
    {
      s++;
      if (is_space (*s))
        {
          *str = s;
          return state;
        }
    }

  while ((c = *s++) && !is_space (c))
    if (s - *str > 8 || (!is_digit (c) && !is_alpha (c)))
      return E_NA;

  if (c == '\0' && s > lim)
    return E_NU;

  *str = s - 1;
  return E_IG;
}

