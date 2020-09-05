/* expr.c - Expression tree.
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

/* A boolean expression is composed of operands separated by operators.
   The precedence of operators is evaluated from left to right using a
   Look Ahead Left Recursive (LALR) parser.

   An operand can be another boolean expression isolated between
   parentheses or a pattern.  The latter is typically a keyword delimited
   by spaces or enclosed in quotes.  In a NEAR expression, a pattern is
   composed of the keywords on either side of the operator.  Therefore,
   the same keyword can appear in multiple patterns.  For example:

   (a NEAR b NEAR c) AND a

   To distinguish the keyword "a" from both patterns, an array of unique
   keywords is built.  Each keyword then points to a linked list of
   patterns using this keyword:

   keywords        list of patterns
     +---+                   +---+   +---+
     | a |------------------>| 0 |-->| 1 |
     +---+           +---+   +---+   +---+
     | b |---------->| 0 |
     +---+   +---+   +---+
     | c |-->| 0 |
     +---+   +---+

   Similarly, an array of patterns is built.  Each pattern then points
   to a portion of the overall expression tree:

                            +---+
   patterns        tree    /| c |
     +---+           +---+/ +---+  +---+
     | 0 |---------->| N |        /| b |
     +---+   +---+   +---+\ +---+/ +---+
     | 1 |-->| a |         \| N |
     +---+   +---+          +---+\ +---+
                                  \| a |
                                   +---+

   Note how the first pattern points to a branch whereas the second to a
   leaf.  The reason is that the NEAR operator needs to be evaluated on
   each match to determine the distance between keywords. On the other
   hand, the second pattern simply evaluates to true if the count is
   greater than zero.
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "match.h"
#include "mem.h"
#include "options.h"

#define is_space(c) ((c)==0x09 || (c)==0x0A || (c)==0x0B || (c)==0x0C \
                     || (c)==0x0D || (c)==0x20)

enum token
{
  END = -8, /* end of buffer                   */
  TERM,     /* term in expression              */
  OPEN,     /* open parenthesis                */
  CLOSE,    /* close parenthesis               */
  AND,      /* boolean operator                */
  NOT,      /* boolean operator                */
  OR,       /* boolean operator                */
  NEAR      /* boolean operator                */
            /* this value should be zero       */
};

/* Syntax tree representating a boolean expression.                      */
struct tree
{
  void *left;                    /* Left link or beginning of pattern.   */
  void *right;                   /* Right link or end of pattern.        */
  int value;                     /* Value of boolean operator or index.  */
};

/* Attributes retained for each pattern.                                 */
struct pat
{
  struct tree *tree;             /* Branch representing the pattern.     */
  int count;                     /* Number of occurrences.               */
};

/* Attributes retained for each keyword.                                 */
struct kw
{
  struct list *list;             /* List of keywords near current.       */
  int byte;                      /* Byte offset in input file.           */
  int word;                      /* Word offset in input file.           */
};

/* Relationships between keywords and patterns.                          */
struct list
{
  struct list *next;             /* Linked list of relationships.        */
  int pat;                       /* Pattern number.                      */
};

/* Structure containing everything.                                      */
struct expr
{
  struct list *state;            /* State of list iteration.             */
  struct tree *tree;             /* Top of the syntax tree.              */
  struct pat *pat;               /* Dynamic array of patterns.           */
  struct kw *kw;                 /* Dynamic array of keywords.           */
  int num_pat;                   /* Number of patterns in expression.    */
  int num_kw;                    /* Number of keywords in expression.    */
};

struct expr *expr;

static char sep_keyword[] = "-";

/* Symbiotic functions for building syntax tree.  */
static struct tree *term (char **, int *);
static struct tree *parse (char **, int *);

static struct tree *
get_token (char **str)
{
  register unsigned char c;
  register char *beg, *end;
  struct tree *tree;
  int val;

  beg = *str;
  while (is_space (*beg))
    beg++;

  end = beg;
  switch (*end)
    {
    case '\0':
      return NULL;
    case '\"':
      end = ++beg;
      while ((c = *end++) != '\0')
        if (*end == '\"' && c != '\\')
          break;
      if (*end != '\"')
        return NULL;
      *str = end + 1;
      break;
    case '\'':
      end = ++beg;
      while ((c = *end++) != '\0')
        if (*end == '\'' && c != '\\')
          break;
      if (*end != '\'')
        return NULL;
      *str = end + 1;
      break;
    default:
      do
        c = *end++;
      while ((c == '\\' || !is_space (*end))
             && *end != ')' && *end != '\0');
      *str = end;
      break;
    }

  val = match_incr (beg, end, expr->num_kw);
  if (val == expr->num_kw)
    expr->num_kw++;

  expr->num_pat++;
  tree = mem_alloc (sizeof (struct tree));
  tree->left = beg;
  tree->right = end;
  tree->value = val;

  return tree;
}

static enum token
lookahead (char **str)
{
  while (is_space (**str))
    (*str)++;
  switch (**str)
    {
    case '\0':
      return END;
    case '(':
      (*str)++;
      return OPEN;
    case ')':
      (*str)++;
      return CLOSE;
    case 'a':
    case 'A':
      if (((*str)[1]=='n' || (*str)[1]=='N')
          && ((*str)[2]=='d' || (*str)[2]=='D')
          && (is_space ((*str)[3]) || (*str)[3]=='(' || (*str)[3]=='\0'))
        {
          *str += 3;
          return AND;
        }
      break;
    case 'n':
    case 'N':
      switch ((*str)[1]) {
      case 'o':
      case 'O':
        if (((*str)[2]=='t' || (*str)[2]=='T')
            && (is_space ((*str)[3]) || (*str)[3]=='(' || (*str)[3]=='\0'))
          {
            *str += 3;
            return NOT;
          }
        break;
      case 'e':
      case 'E':
        if (((*str)[2]=='a' || (*str)[2]=='A')
            && ((*str)[3]=='r' || (*str)[3]=='R')
            && (is_space ((*str)[4]) || (*str)[4]=='(' || (*str)[4]=='\0'))
          {
            *str += 4;
            return NEAR;
          }
        break;
      }
      break;
    case 'o':
    case 'O':
      if (((*str)[1]=='r' || (*str)[1]=='R')
          && (is_space ((*str)[2]) || (*str)[2]=='(' || (*str)[2]=='\0'))
        {
          *str += 2;
          return OR;
        }
      break;
    }

  return TERM;
}

static struct tree *
tree_dup (struct tree *tree)
{
  if (tree->value < 0)
    {
      struct tree *p;

      p = mem_alloc (sizeof (struct tree));
      p->left = tree_dup (tree->left);
      p->right = tree_dup (tree->right);
      p->value = tree->value;

      return p;
    }
  else
    return tree;
}

static struct tree *
tree_norm (struct tree *l, struct tree *r)
{
  struct tree *tree;

  if (r->value < NEAR)
    {
      expr->num_pat++;
      tree = tree_dup (l);
      r->left = tree_norm (l, r->left);
      r->right = tree_norm (tree, r->right);

      return r;
    }
  else if (l->value < NEAR)
    {
      expr->num_pat++;
      tree = l;
      tree->left = tree_norm (l->left, r);
      tree->right = tree_norm (l->right, r);

      return tree;
    }
  else
    {
      expr->num_pat--;
      tree = mem_alloc (sizeof (struct tree));
      tree->left = l;
      tree->right = r;
      tree->value = NEAR;

      return tree;
    }
}

static struct tree *
term (char **str, enum token *tok)
{
  struct tree *tree;

  if (*tok == OPEN)
    {
      tree = parse (str, tok);
      if (*tok == END)
        return NULL;
    }
  else if (*tok == TERM)
    tree = get_token (str);
  else
    return NULL;

  *tok = lookahead (str);
  return tree;
}

static struct tree *
parse (char **str, enum token *tok)
{
  struct tree *left, *tree;

  *tok = lookahead (str);
  left = term (str, tok);
  if (!left)
    return NULL;

  for (;;)
    switch (*tok)
      {
      case AND:
      case NOT:
      case OR:
        tree = mem_alloc (sizeof (struct tree));
        tree->value = *tok;
        *tok = lookahead (str);

        tree->left = left;
        tree->right = term (str, tok);
        if (!tree->right)
          return NULL;

        left = tree;
        break;
      case NEAR:
        *tok = lookahead (str);
        tree = term (str, tok);
        if (!tree)
          return NULL;

        left = tree_norm (left, tree);
        break;
      case TERM:
      case OPEN:
        return NULL;
      case END:
      case CLOSE:
        return left;

      default:
        abort ();
      }

  return NULL;
}

static void
list_kw (struct tree *tree, int num_pat)
{
  if (tree->value < 0)
    {
      list_kw (tree->left, num_pat);
      list_kw (tree->right, num_pat);
    }
  else
    {
      struct list *list;
      struct kw *kw;

      kw = expr->kw + tree->value;
      list = kw->list;
      kw->list = mem_alloc (sizeof (struct list));
      kw->list->next = list;
      kw->list->pat = num_pat;
    }
}

static void
list_pat (struct tree *tree, int num_pat)
{
  if (tree->value < NEAR)
    {
      list_pat (tree->left, num_pat);
      list_pat (tree->right, num_pat + 1);
    }
  else
    {
      struct pat *pat;
    
      list_kw (tree, num_pat);
      pat = expr->pat + num_pat;
      if (tree->value == NEAR)
        {
          pat->tree = mem_alloc (sizeof (struct tree));
          *pat->tree = *tree;
        }
      else
        pat->tree = tree;

      tree->value = num_pat;
    }
}

int
expr_init (char *str)
{
  register int i;

  expr = mem_alloc (sizeof (struct expr));
  match_init (opt.ignore_case);

  if (opt.fixed_string)
    {
      expr->tree = mem_alloc (sizeof (struct tree));
      expr->tree->value = 0;
      expr->tree->left = str;
      expr->tree->right = str + strlen (str);
      expr->num_kw = expr->num_pat = 1;
      match_incr (expr->tree->left, expr->tree->right, 0);
    }
  else
    {
      enum token tok;

      expr->num_kw = expr->num_pat = 0;
      expr->tree = parse (&str, &tok);
      if (!expr->tree)
        return 0;
    }

  expr->state = NULL;
  expr->pat = mem_alloc (expr->num_pat * sizeof (struct pat));
  expr->kw  = mem_alloc (expr->num_kw * sizeof (struct kw));

  for (i = 0; i < expr->num_kw; i++)
    expr->kw[i].list = NULL;

  list_pat (expr->tree, 0);
  match_prep ();

  return expr->num_pat;
}

void
expr_prep ()
{
  register int i;

  for (i = 0; i < expr->num_pat; i++)
    expr->pat[i].count = 0;

  for (i = 0; i < expr->num_kw; i++)
    expr->kw[i].byte = expr->kw[i].word = 0;
}

static int
tree_eval (struct tree *tree)
{
  switch (tree->value)
    {
    case AND:
      return tree_eval (tree->left) && tree_eval (tree->right);
    case NEAR:
      abort ();
    case NOT:
      return tree_eval (tree->left) && !tree_eval (tree->right);
    case OR:
      return tree_eval (tree->left) || tree_eval (tree->right);
    default:
      return expr->pat[tree->value].count;
    }
}

int
expr_eval (void)
{
  return tree_eval (expr->tree);
}

int
expr_count (int pattern)
{
  return expr->pat[pattern].count;
}

static void
tree_print (struct tree *tree, int *num)
{
  if (tree->value < 0)
    {
      tree_print (tree->left, num);
      tree_print (tree->right, num);
    }
  else
    {
      if ((*num)++)
        printf ("%s", sep_keyword);
      printf ("%.*s", tree->right - tree->left, (char *)tree->left);
    }
}

void
expr_print (int pattern)
{
  int num;

  num = 0;
  tree_print (expr->pat[pattern].tree, &num);
}

static int
tree_distance (struct tree *tree)
{
  if (tree->value < 0)
    {
      int d, l, r;

      l = tree_distance (tree->left);
      r = tree_distance (tree->right);
      if (!l || !r)
        return 0;

      d = l - r < 0 ? r - l : l - r;
      return d <= opt.distance ? r : 0;
    }
  return expr->kw[tree->value].word;
}

int
expr_next (int keyword, int byte, int *word)
{
  struct list *list;
  struct pat *pat;
  struct kw *kw;

  kw = expr->kw + keyword;
  if (!expr->state)
    {
      if (kw->byte == byte)
        return -1;
      list = kw->list;
    }
  else
    list = expr->state;

  kw->byte = byte;
  kw->word = *word;
  pat = expr->pat + list->pat;
  if (pat->tree->value < 0)
    {
      *word = tree_distance (pat->tree);
      if (!*word)
        *word = -1;
    }
  else
    *word = 0;

  if (*word >= 0)
    pat->count++;

  expr->state = list->next;

  return list->pat;
}

