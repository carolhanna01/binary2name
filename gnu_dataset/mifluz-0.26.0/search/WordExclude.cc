//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordExclude.cc,v 1.2 2001/06/29 14:14:08 loic Exp $
//
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <htString.h>

#include <WordExclude.h>

//
// Helper that displays an unsigned int in binary/hexa/decimal
//
static inline void show_bits(unsigned int result)
{
  int i;
  for(i = 0; i < 10; i++) {
    fprintf(stderr, "%c", (result & (1 << i)) ? '1' : '0');
  }
  fprintf(stderr, " (0x%08x - %15d)\n", result, result);
}

int WordExclude::Initialize(unsigned int length, unsigned int, unsigned int, int)
{
  if(length > WORD_EXCLUDE_MAX) {
    fprintf(stderr, "WordExclude::Initialize: length must be < %d\n", WORD_EXCLUDE_MAX);
    return NOTOK;
  }

  mask = 0;
  bits = 0;
  maxi = length;

  return OK;
}

inline unsigned int WordExclude::Permute(unsigned int mask, unsigned int bits)
{
  unsigned int bits_cleared = 0;
  unsigned int j;
  for(j = 0; j < bits; j++) {
    if(mask & (1 << j)) {
      bits_cleared++;
      mask &= ~(1 << j);
    } else {
      if(bits_cleared) {
	bits_cleared--;
	mask |= (1 << j);
	break;
      }
    }
  }
    
  if(j >= bits)
    return 0;

  for(j = 0; j < bits_cleared; j++)
    mask |= (1 << j);

  return mask;
}

int WordExclude::Next()
{
  mask = Permute(mask, maxi);

  int ret = WORD_EXCLUDE_OK;

  if(mask == 0) {
    bits++;
    if(bits > maxi)
      ret = WORD_EXCLUDE_END;
    else {
      unsigned int i;
      for(i = 0; i < bits; i++)
	mask |= (1 << i);
      ret = WORD_EXCLUDE_OK;
    }
  }
  
  if(verbose > 2) show_bits(mask);

  return ret;
}

void WordExclude::Get(String& buffer) const
{
  buffer.trunc();
  unsigned int i;
  for(i = 0; i < maxi; i++) {
    buffer << ((mask & (1 << i)) ? '1' : '0');
  }
}

int WordExclude::Set(const String& buffer)
{
  if(Initialize(buffer.length(), 0, 0, 0) == NOTOK)
    return NOTOK;
  unsigned int i;
  for(i = 0; i < maxi; i++) {
    if(buffer[i] == '1') {
      mask |= (1 << i);
      bits++;
    }
  }
  return OK;
}

