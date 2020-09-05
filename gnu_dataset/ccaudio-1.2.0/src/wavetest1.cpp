// Copyright (C) 2006 Free Software Foundation.
//  
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software 
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
// 
// As a special exception to the GNU General Public License, permission is 
// granted for additional uses of the text contained in its release 
// of APE.
// 
// The exception is that, if you link the APE library with other files
// to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the APE library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released under the 
// name APE.  If you copy code from other releases into a copy of
// APE, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for APE, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.

// This is a test program to verify that the wave headers are being
// written correctly upon a call to AudioFile::close().  Jason Spence
// <thalakan@ostel.com> wrote it so he could do dumps of audio data
// from the Voicetronix driver in Bayonne to debug a customer problem.

#include <cerrno>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "audio.h"

static unsigned char correct[] = {
0x52, 0x49, 0x46, 0x46, 0x64, 0xe2, 0x01, 0x00, 0x57, 0x41, 0x56, 0x45, 0x66,
0x6d, 0x74, 0x20, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x44, 0xac,
0x00, 0x00, 0x10, 0xb1, 0x02, 0x00, 0x04, 0x00, 0x10, 0x00, 0x64, 0x61, 0x74,
0x61, 0x40, 0xe2, 0x01,
};

#define CORRECT_BUF_SIZE (43)
#define BUF_SIZE (123456)

#ifdef	CCXX_NAMESPACES
namespace ost {
using namespace std;
#endif

int do_test1(void);

extern "C" int main(void) {
  int rc, rc2; // rc stands for "return code"

  rc = do_test1();
  if(rc == -1)
    cerr << ">>> Test 1 FAILED! <<<" << endl;
  else
    cout << "Test 1 passed." << endl;

  // These young'uns never learn, always forgetting to omit checks on
  // the return values of system calls.  Grumble.  In *MY* day, we
  // checked the return value of every system call, including
  // unlink(2), yessiree...
  if(rc == 0) {
    rc2 = unlink("test.wav");
    if(rc2 == -1)
      cerr << "Deleting test wave file failed: " << strerror(errno) << endl;
  }

  return rc;
}

int do_test1(void) {
  unsigned char * buf;
  int i;
  int rc;
  int ret = 0;
  Audio::Info ai;

  cout << "Beginning test 1" << endl; 
  buf = new unsigned char[BUF_SIZE];
  memset(buf, 0, BUF_SIZE * sizeof(unsigned char));
  
  /* Create the file via ccAudio */
  ai.format = Audio::riff;
  ai.encoding = Audio::pcm16Stereo;
  ai.rate = 44100;
  ai.order = 0;
  ai.annotation = "Groo";
  AudioFile out("test.wav", &ai);
  rc = out.putSamples(buf, BUF_SIZE / out.getFrame(Audio::pcm16Stereo));
  if(rc != Audio::errSuccess) {
    cout << "error writing samples to file: " << rc;
  }

  delete buf;
  out.close();
  buf = new unsigned char [CORRECT_BUF_SIZE];

  /* Open the file and read the header */
  ifstream file("test.wav");
  for(i = 0; i < CORRECT_BUF_SIZE; ++i)
    file.read((char *)(buf + i), 1);
  file.close();

  /* Compare the header that was read against the "correct" header above */
  for(i = 0; i < CORRECT_BUF_SIZE; ++i)
    if(correct[i] != buf[i]) {
      cerr.fill('0');
      cerr << "Test failed at offset " << dec << i << ".  Saw 0x" << setw(2) << hex << (int) buf[i] << " should have been 0x" << setw(2) << hex << (int) correct[i] << endl;
      ret = -1;
    }

  return ret;
  delete buf;
}

#ifdef	CCXX_NAMESPACES
};
#endif

