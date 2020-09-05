//
// cmpr.cc
//
// cmpr: Implement tests for the word database compression related classes.
//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: cmpr.cc,v 1.4 2001/06/29 14:14:08 loic Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

// If we have this, we probably want it.
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include "WordBitCompress.h"
#include "WordContext.h"

static WordContext*	context = 0;

typedef struct
{
    int bit;
    int cmpr;
} params_t;

static void usage();
static void dobit(params_t* params);
static void docmpr(params_t* params);

static int verbose = 0;

// *****************************************************************************
// int main(int ac, char **av)
//

int main(int ac, char **av)
{
  int			c;
  params_t		params;

  params.bit = 0;
  params.cmpr = 0;

  while ((c = getopt(ac, av, "bc")) != -1)
    {
      switch (c)
	{
	case 'v':
	  verbose++;
	  break;
	case 'b':
	  params.bit = 1;
	  break;
	case 'c':
	  params.cmpr = 1;
	  break;
	case '?':
	  usage();
	  break;
	}
    }

  if(params.bit) dobit(&params);
  if(params.cmpr) docmpr(&params);

  return 0;
}

#define VALUE 0xfabcafe5

static void dobit(params_t* params)
{
  int i;
  context = new WordContext();

  //
  // Init/Check/Allocate
  //
  {
    WordBitStream stream(1);
    if(stream.BuffLength() != 1) {
      fprintf(stderr, "dobit: Init expected buffer length of 1 byte but is %d byte(s)\n", stream.BuffLength());
      exit(1);
    }
    stream.PutUint(0x00ffffff, 24);
    if(stream.BuffLength() != 4) {
      fprintf(stderr, "dobit: Check expected buffer length of 4 byte but is %d byte(s)\n", stream.BuffLength());
      exit(1);
    }
    stream.Rewind();
    unsigned int value;
    if((value = stream.GetUint(24)) != 0x00ffffff) {
      fprintf(stderr, "dobit: GetUint after Check expected 0x00ffffff but got 0x%08x\n", value);
      exit(1);
    }
  }

  WordBitStream stream(10240);

  //
  // Also tested Init/Clear/Rewind
  //
  //
  // Put/Get
  //
  unsigned int value = VALUE;
  while(value) {
    stream.Put(value & 1);
    value >>= 1;
  }
  stream.Rewind();
  value = 0;
  for(i = 0; i < (int)(sizeof(unsigned int) * 8); i++) {
    if(stream.Get())
      value |= (1 << i);
  }
  if(value != VALUE) {
    fprintf(stderr, "dobit: stream Put/Get failed got 0x%08x instead of 0x%08x\n", value, VALUE);
    exit(1);
  }
  stream.Clear();

  //
  // PutUint/GetUint
  //
  // Make things more difficult by breaking allignement
  stream.Put(1);
  stream.PutUint(VALUE, sizeof(unsigned int) * 8);
  stream.Put(1);
  stream.Rewind();
  if(!stream.Get()) {
    fprintf(stderr, "dobit: stream PutUint/GetUint: fail to recover first bit\n");
    exit(1);
  }

  if((value = stream.GetUint(sizeof(unsigned int) * 8)) != VALUE) {
    fprintf(stderr, "dobit: stream PutUint/GetUint failed got 0x%08x instead of 0x%08x\n", value, VALUE);
    exit(1);
  }
  stream.Clear();

  //
  // PutZone/GetZone + Length/BuffLength/BuffCopy/BuffSet
  //
  // Make things more difficult by breaking allignement
  char *in = "The quick brown fox";
  stream.Put(1);
  stream.PutZone((unsigned char*)in, (strlen(in) + 1) * 8);
  stream.Put(1);
  int expected_length = (int)((strlen(in) + 1) * 8) + 2;
  if(stream.Length() != expected_length) {
    fprintf(stderr, "dobit: stream PutZone/GetZone: has length %d bits instead of %d bits\n", stream.Length(), expected_length);
    exit(1);
  }
  int copy_length = (int)(strlen(in) + 1 + 1);
  if(stream.BuffLength() != copy_length) {
    fprintf(stderr, "dobit: stream PutZone/GetZone: has length %d bytes instead of %d bytes\n", stream.BuffLength(), copy_length);
    exit(1);
  }
  unsigned char* copy = stream.BuffCopy();
  stream.BuffSet(copy, copy_length);
  free(copy);
  stream.Rewind();
  char *out = (char*)malloc(strlen(in) + 1);
  if(!stream.Get()) {
    fprintf(stderr, "dobit: stream PutZone/GetZone: fail to recover first bit\n");
    exit(1);
  }
  stream.GetZone((unsigned char*)out, (strlen(in) + 1) * 8);
  if(memcmp(in, out, strlen(in) + 1)) {
    fprintf(stderr, "dobit: stream PutZone/GetZone: got %.*s instead of %s\n", (int)strlen(in), out, in);
    exit(1);
  }
  free(out);
  stream.Clear();

  //
  // Freeze/UnFreeze
  //
  stream.Freeze();
  stream.Put(1);
  if(stream.Length() != 1) {
    fprintf(stderr, "dobit: Freeze length is %d bit(s) instead of 1 bit", stream.Length());
    exit(1);
  }
  stream.UnFreeze();
  if(stream.Length() != 0) {  
    fprintf(stderr, "dobit: UnFreeze length is %d bit(s) instead of 0 bit", stream.Length());
    exit(1);
  }

  delete context;
}

static void docmpr(params_t* params)
{
  int i;
  context = new WordContext();

  WordBitCompress stream(10240);

  unsigned int value;
  //
  // PutUint/GetUint
  //
  // Make things more difficult by breaking allignement
  stream.Put(1);
  stream.PutUint(VALUE, sizeof(unsigned int) * 8);
  stream.Put(1);
  stream.Rewind();
  if(!stream.Get()) {
    fprintf(stderr, "docmpr: stream PutUint/GetUint: fail to recover first bit\n");
    exit(1);
  }

  if((value = stream.GetUint(sizeof(unsigned int) * 8)) != VALUE) {
    fprintf(stderr, "docmpr: stream PutUint/GetUint failed got 0x%08x instead of 0x%08x\n", value, VALUE);
    exit(1);
  }
  stream.Clear();

  {
    //
    // PutUints/GetUints
    //
    static unsigned int in[] = {
      0xcafe,
      100,
      0xbaffe + 100,
      200,
      0xbaba + 200,
      300,
      0x5afebebe + 300
    };
    int in_length = sizeof(in) / sizeof(unsigned int);
    stream.PutUints(in, in_length);
    stream.Rewind();
    unsigned int* out = 0;
    int out_length = 0;
    out_length = stream.GetUints(&out);
    if(out_length != in_length) {
      fprintf(stderr, "docmpr: GetUints expected %d return value but got %d\n", in_length, out_length);
      exit(1);
    }
    for(i = 0; i < out_length; i++) {
      if(in[i] != out[i]) {
	fprintf(stderr, "docmpr: GetUints expected out[%d] == %d but got %d\n", i, in[i], out[i]);
	exit(1);
      }
    }
    stream.Clear();

    //
    // PutUintsDecr/GetUintsDecr
    //
    stream.PutUintsDecr(in, in_length);
    stream.Rewind();
    stream.GetUintsDecr(out, out_length);
    for(i = 0; i < out_length; i++) {
      if(in[i] != out[i]) {
	fprintf(stderr, "docmpr: GetUintsDecr expected out[%d] == %d but got %d\n", i, in[i], out[i]);
	exit(1);
      }
    }
    stream.Clear();

    //
    // PutUintsFixed/GetUintsFixed
    //
    stream.PutUintsFixed(in, in_length);
    stream.Rewind();
    stream.GetUintsFixed(out, out_length);
    for(i = 0; i < out_length; i++) {
      if(in[i] != out[i]) {
	fprintf(stderr, "docmpr: GetUintsFixed expected out[%d] == %d but got %d\n", i, in[i], out[i]);
	exit(1);
      }
    }
    stream.Clear();
    delete [] out;
  }

  //
  // PutUchars/GetUchars
  //
  {
    char *in = "The quick brown fox";
    int in_length = (int)(strlen(in) + 1);
    stream.PutUchars((unsigned char*)in, in_length);
    stream.Rewind();
    char *out = 0;
    int out_length = 0;
    out_length = stream.GetUchars((unsigned char**)&out);
    if(in_length != out_length) {
      fprintf(stderr, "docmpr: GetUchars expected length = %d bytes but got %d bytes instead\n", in_length, out_length);
      exit(1);
    }
    if(memcmp(in, out, in_length)) {
      fprintf(stderr, "docmpr: stream GetUchars: got %.*s instead of %s\n", in_length, out, in);
      exit(1);
    }
    stream.Clear();
    delete [] out;
  }
  
  delete context;
}

//*****************************************************************************
// void usage()
//   Display program usage information
//
static void usage()
{
    printf("usage: cmpr [options]\n");
    printf("Options:\n");
    printf("\t-v\t\tIncreases the verbosity\n");
    printf("\t-b\t\tTest WordBitStream\n");
    printf("\t-c\t\tTest WordBitCompress\n");
    exit(0);
}
