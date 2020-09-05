//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordExcludeMask.cc,v 1.2 2001/06/29 14:14:08 loic Exp $
//
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <htString.h>

#include <WordExcludeMask.h>

void WordExcludeMask::Get(String& buffer) const
{
  buffer.trunc();
  unsigned int i;
  for(i = 0; i < ignore_maxi; i++) {
    if(bit2bit[i] == WORD_EXCLUDE_IGNORED)
      buffer << ((ignore_mask & (1 << i)) ? '3' : '2');
    else
      buffer << ((WordExclude::Mask() & (1 << bit2bit[i])) ? '1' : '0');
  }
}

int WordExcludeMask::Set(const String& buffer)
{
  WordExclude::Initialize(0, 0, 0, 0);

  ignore_mask = 0;
  ignore_bits = 0;
  ignore_maxi = buffer.length();

  unsigned int i;
  for(i = 0; i < ignore_maxi; i++) {
    if(buffer[i] == '1' || buffer[i] == '0') {
      if(buffer[i] == '1') {
	WordExclude::mask |= (1 << WordExclude::maxi);
	WordExclude::bits++;
      }
      bit2bit[i] = WordExclude::maxi;
      WordExclude::maxi++;
    } else if(buffer[i] == '3' || buffer[i] == '2') {
      if(buffer[i] == '3') {
	ignore_mask |= (1 << i);
	ignore_bits++;
      }
      bit2bit[i] = WORD_EXCLUDE_IGNORED;
    }
  }

  return OK;
}

#ifdef MAIN

static void exclude_test()
{
  static unsigned int expected[] = {
    0x00000001,
    0x00000002,
    0x00000004,
    0x00000008,
    0x00000010,
    0x00000003,
    0x00000005,
    0x00000006,
    0x00000009,
    0x0000000a,
    0x0000000c,
    0x00000011,
    0x00000012,
    0x00000014,
    0x00000018,
    0x00000007,
    0x0000000b,
    0x0000000d,
    0x0000000e,
    0x00000013,
    0x00000015,
    0x00000016,
    0x00000019,
    0x0000001a,
    0x0000001c,
    0x0000000f,
    0x00000017,
    0x0000001b,
    0x0000001d,
    0x0000001e,
    0x0000001f
  };

  //
  // WordExclude
  //
  if(verbose) fprintf(stderr, "exclude_test: testing WordExclude\n");
  {
    WordExclude exclude;
    exclude.Initialize(5);
    int count = 0;
    while(exclude.Next() == WORD_EXCLUDE_OK) {
      if(expected[count] != exclude.Mask()) {
	fprintf(stderr, "exclude_test: WordExclude iteration %d expected 0x%08x but got 0x%08x\n", count, expected[count], exclude.Mask());
	exit(1);
      }
      count++;
    }
    if(count != sizeof(expected)/sizeof(unsigned int)) {
      fprintf(stderr, "exclude_test: WordExclude expected %d iterations but got %d\n", sizeof(expected)/sizeof(unsigned int), count);
      exit(1);
    }
  }

  //
  // WordExcludeMask without ignore bits behaves exactly the same
  // as WordExclude.
  //
  if(verbose) fprintf(stderr, "exclude_test: testing WordExcludeMask behaving like WordExclude\n");
  {
    WordExcludeMask exclude;
    exclude.Initialize(5, 0, 0);
    int count = 0;
    while(exclude.Next() == WORD_EXCLUDE_OK) {
      if(expected[count] != exclude.Mask()) {
	fprintf(stderr, "exclude_test: WordExcludeMask 1 iteration %d expected 0x%08x but got 0x%08x\n", count, expected[count], exclude.Mask());
	exit(1);
      }
      count++;
    }
    if(count != sizeof(expected)/sizeof(unsigned int)) {
      fprintf(stderr, "exclude_test: WordExcludeMask 1 expected %d iterations but got %d\n", sizeof(expected)/sizeof(unsigned int), count);
      exit(1);
    }
  }

  //
  // WordExcludeMask 
  //
  if(verbose) fprintf(stderr, "exclude_test: testing WordExcludeMask\n");
  {
    static unsigned int expected[] = {
      0x00000102,
      0x00000108,
      0x00000120,
      0x00000180,
      0x0000010a,
      0x00000122,
      0x00000128,
      0x00000182,
      0x00000188,
      0x000001a0,
      0x0000012a,
      0x0000018a,
      0x000001a2,
      0x000001a8,
      0x000001aa
    };
    static unsigned int excluded[] = {
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      1
    };

    WordExcludeMask exclude;
    unsigned int ignore = 0x155;
    unsigned int ignore_mask = 0x100;
    exclude.Initialize(9, ignore, ignore_mask);
    if(verbose) {
      fprintf(stderr, "exclude_test: ignore\n");
      show_bits(ignore);
      fprintf(stderr, "exclude_test: ignore_mask\n");
      show_bits(ignore_mask);
    }
    if(exclude.NotExcludedCount() != 8) {
	fprintf(stderr, "exclude_test: WordExcludeMask 2 expected NoExcludedCount = 8 but got %d\n", exclude.NotExcludedCount());
	exit(1);
    }
    int count = 0;
    while(exclude.Next() == WORD_EXCLUDE_OK) {
      if(expected[count] != exclude.Mask()) {
	fprintf(stderr, "exclude_test: WordExcludeMask 2 iteration %d expected 0x%08x but got 0x%08x\n", count, expected[count], exclude.Mask());
	exit(1);
      }
      //
      // Test Excluded() method on ignored bit
      // Is bit 5 set ? (9 - 4) = 5 (counting from 1)
      //
      if(exclude.Excluded(4)) {
	fprintf(stderr, "exclude_test: WordExcludeMask 2 iteration %d bit 5 was set 0x%08x\n", count, exclude.Mask());
	exit(1);
      }
      //
      // Test Excluded() method on variable bit
      // Is bit 2 set ? (9 - 2) = 7 (counting from 1)
      //
      if((exclude.Excluded(7) && !excluded[count]) ||
	 (!exclude.Excluded(7) && excluded[count])) {
	fprintf(stderr, "exclude_test: WordExcludeMask 2 iteration %d expected bit 2 %s but was %s in 0x%08x\n", count, (excluded[count] ? "set" : "not set"), (excluded[count] ? "not set" : "set"), expected[count]);
	exit(1);
      }
      count++;
    }
    if(count != sizeof(expected)/sizeof(unsigned int)) {
      fprintf(stderr, "exclude_test: WordExcludeMask 2 expected %d iterations but got %d\n", sizeof(expected)/sizeof(unsigned int), count);
      exit(1);
    }
  }

  {
    WordExclude exclude;
    String ascii("110101");
    String tmp;
    exclude.Set(ascii);
    exclude.Get(tmp);
    if(tmp != ascii) {
      fprintf(stderr, "exclude_test: WordExclude::Get/Set expected %s but got %s\n", (char*)ascii, (char*)tmp);
      exit(1);
    }
    if(exclude.Mask() != 0x2b) {
      fprintf(stderr, "exclude_test: WordExclude::Mask expected 0x2b but got 0x%02x\n", exclude.Mask());
      exit(1);
    }
  }
  {
    WordExcludeMask exclude;
    String ascii("12031");
    String tmp;
    exclude.Set(ascii);
    exclude.Get(tmp);
    if(tmp != ascii) {
      fprintf(stderr, "exclude_test: WordExcludeMask::Get/Set expected %s but got %s\n", (char*)ascii, (char*)tmp);
      exit(1);
    }
    if(exclude.Mask() != 0x19) {
      fprintf(stderr, "exclude_test: WordExcludeMask::Mask expected 0x19 but got 0x%02x\n", exclude.Mask());
      exit(1);
    }
  }
}

main() {
  exclude_test();
  exit(0);
}

#endif /* MAIN */
