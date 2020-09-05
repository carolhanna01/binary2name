/* match.c - Keyword matching automaton.
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

/* The keyword matching algorithm in this program is discussed by A.
   V. Aho and M. Corasick in "Efficient String Matching:  An Aid to
   Bibliographic Search," CACM June 1975, Vol. 18, No. 6.

   In summary, this algorithm consists of building a trie in which
   each state corresponds to a prefix of one or more keywords. The
   start state corresponds to the empty string, and a state that
   corresponds to a complete keyword is a final state.

   For each state, a failure function is defined.  This function
   corresponds to the default transition if the next state is not
   defined.  A transition leaving from a state representing the
   prefix x leads to a state representing a prefix y, such that y
   is the longest prefix in the set of keywords which is also a
   proper suffix of x.

   Note that there is no failure function for the start state.
*/

#include <ctype.h>
#include <stdlib.h>

#include "match.h"
#include "mem.h"

#include "event.h"
#include "sgml.h"

#define MAXDEPTH 12

/* Balanced tree of edges and labels leaving a given trie node.          */
struct tree
{
  struct tree *left;             /* Left link; MUST be first field.      */
  struct tree *right;            /* Right link (to larger labels).       */
  struct trie *trie;             /* Trie node pointed to by this edge.   */
  unsigned char label;           /* Label on this edge.                  */
  char balance;                  /* Difference in depths of subtrees.    */
};

/* Node of a trie representing a set of keywords.                        */
struct trie
{
  unsigned accepting;            /* Word index of accepted word, or 0.   */
  struct tree *links;            /* Tree of edges leaving this node.     */
  struct trie *next;             /* List of trie nodes in level order.   */
  struct trie *fail;             /* Aho-Corasick failure function.       */
};

/* Structure containing everything.                                      */
struct ac
{
  struct trie *trie;             /* The trie itself.                     */
  struct trie *cur;              /* Current trie.                        */
  unsigned char *trans;          /* Character translation table.         */
};

static struct ac *ac;

/* Translation table which notably converts all space characters
   to ' '. If icase is set, this table will also be modified to
   convert all upper-case characters to lower-case.
*/
static unsigned char trans[] =
{
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x20, 0x20, 0x20, 0x20, 0x20, 0x0E, 0x0F, 
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
  0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
  0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
  0x20, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
  0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
  0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
  0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
  0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
  0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
  0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
  0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
  0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
  0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
  0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
  0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
};

void
match_init (int icase)
{
  ac = mem_alloc (sizeof (struct ac));
  ac->trie = mem_alloc (sizeof (struct trie));

  ac->trie->accepting = 0;
  ac->trie->links = 0;
  ac->trie->next = 0;
  ac->trie->fail = 0;

  if (icase)
    {
      register int i;

      ac->trans = trans;
      for (i = 0; i < 256; ++i)
        trans[i] = tolower (i);
    }
  else
    ac->trans = NULL;
}

static unsigned char
get_label (char **text, char *lim)
{
  register unsigned char c;

  c = *(*text)++;
  if (c == '\\' && (lim - *text))
    c = *(*text)++;
  else if (c == '&')
    c = get_character (text, lim);

  return ac->trans ? trans[c] : c;
}

int
match_incr (char *text, char *lim, int index)
{
  register struct trie *trie;
  register struct tree *link;
  register unsigned char label;
  register int depth;

  struct tree *links[MAXDEPTH];
  enum { L, R } dirs[MAXDEPTH];

  label = get_label (&text, lim);
  event[label] += event[label] % 2;
  if (ac->trans)
    event[toupper(label)] = event[label];

  trie = ac->trie;
  while (text <= lim)
    {
      link = trie->links;
      links[0] = (struct tree *) &trie->links;
      dirs[0] = L;
      depth = 1;

      while (link && label != link->label)
        {
          links[depth] = link;
          if (label < link->label)
            dirs[depth++] = L, link = link->left;
          else
            dirs[depth++] = R, link = link->right;
        }

      if (!link)
        {
          struct tree *r, *l, *rl, *lr, *t = NULL;

          link = mem_alloc (sizeof (struct tree));
          link->label = label;
          link->balance = 0;
          link->left = 0;
          link->right = 0;

          link->trie = mem_alloc (sizeof (struct trie));
          link->trie->accepting = 0;
          link->trie->links = 0;
          link->trie->next = 0;
          link->trie->fail = 0;

          if (dirs[--depth] == L)
            links[depth]->left = link;
          else
            links[depth]->right = link;

          while (depth && !links[depth]->balance)
            {
              if (dirs[depth] == L)
                --links[depth]->balance;
              else
                ++links[depth]->balance;
              --depth;
            }

          if (depth && ((dirs[depth] == L && --links[depth]->balance)
              || (dirs[depth] == R && ++links[depth]->balance)))
            {
              switch (links[depth]->balance)
                {
                case (char) -2:
                  switch (dirs[depth + 1])
                    {
                    case L:
                      r = links[depth], t = r->left, rl = t->right;
                      t->right = r, r->left = rl;
                      t->balance = r->balance = 0;
                      break;
                    case R:
                      r = links[depth], l = r->left, t = l->right;
                      rl = t->right, lr = t->left;
                      t->left = l, l->right = lr;
                      t->right = r, r->left = rl;
                      l->balance = t->balance != 1 ? 0 : -1;
                      r->balance = t->balance != (char) -1 ? 0 : 1;
                      t->balance = 0;
                      break;
                    default:
                      abort ();
                    }
                  break;
                case 2:
                  switch (dirs[depth + 1])
                    {
                    case R:
                      l = links[depth], t = l->right, lr = t->left;
                      t->left = l, l->right = lr;
                      t->balance = l->balance = 0;
                      break;
                    case L:
                      l = links[depth], r = l->right, t = r->left;
                      lr = t->left, rl = t->right;
                      t->left = l, l->right = lr;
                      t->right = r, r->left = rl;
                      l->balance = t->balance != 1 ? 0 : -1;
                      r->balance = t->balance != (char) -1 ? 0 : 1;
                      t->balance = 0;
                      break;
                    default:
                      abort ();
                    }
                  break;
                default:
                  abort ();
                }

              if (dirs[depth - 1] == L)
                links[depth - 1]->left = t;
              else
                links[depth - 1]->right = t;
            }
        }

      label = get_label (&text, lim);
      label = trans[label];
      trie = link->trie;
    }

  if (!trie->accepting)
    trie->accepting = index + 1;

  return trie->accepting - 1;
}

static void
enqueue (struct tree *tree, struct trie **last)
{
  if (!tree)
    return;
  enqueue (tree->left, last);
  enqueue (tree->right, last);
  (*last) = (*last)->next = tree->trie;
}

static void
tree_fails (register struct tree *tree, struct trie *fail,
           struct trie *recourse)
{
  register struct tree *link;

  if (!tree)
    return;

  tree_fails (tree->left, fail, recourse);
  tree_fails (tree->right, fail, recourse);

  while (fail)
    {
      link = fail->links;
      while (link && tree->label != link->label)
        if (tree->label < link->label)
          link = link->left;
        else
          link = link->right;
      if (link)
        {
          tree->trie->fail = link->trie;
          return;
        }
      fail = fail->fail;
    }

  tree->trie->fail = recourse;
}

void
match_prep ()
{
  register struct trie *curr;
  struct trie *last;

  for (curr = last = ac->trie; curr; curr = curr->next)
    {
      enqueue (curr->links, &last);
      tree_fails (curr->links, curr->fail, ac->trie);
    }
}

#define match_edge(trie)                                                  \
  do                                                                      \
    {                                                                     \
      tree = trie->links;                                                 \
      while (tree && c != tree->label)                                    \
        if (c < tree->label)                                              \
          tree = tree->left;                                              \
        else                                                              \
          tree = tree->right;                                             \
     }                                                                    \
  while (0)

int
match_first (unsigned char c)
{
  struct tree *tree;

  c = ac->trans ? trans[c] : c;
  match_edge (ac->trie);
  ac->cur = tree->trie;
  return ac->cur->accepting;
}

int
match_next (unsigned char c)
{
  struct tree *tree;
  struct trie *trie;

  c = ac->trans ? trans[c] : c;
  trie = ac->cur;
  for (;;)
    {
      match_edge (trie);
      if (tree)
        {
          ac->cur = tree->trie;
          return ac->cur->accepting;
        }
      else if ((trie = trie->fail) == NULL)
        return -1;
    }
}

int
match_maybe (unsigned char c)
{
  struct tree *tree;

  match_edge (ac->cur);
  if (tree)
    ac->cur = tree->trie;

  return ac->cur->accepting;
}

