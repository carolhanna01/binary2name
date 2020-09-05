/*
    iv_gen.h -- efficiently generate IV values using AES
    Copyright (C) 2013      Marc Lehmann <gvpe@schmorp.de>
 
    This file is part of GVPE.

    GVPE is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 3 of the License, or (at your
    option) any later version.
   
    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
    Public License for more details.
   
    You should have received a copy of the GNU General Public License along
    with this program; if not, see <http://www.gnu.org/licenses/>.
   
    Additional permission under GNU GPL version 3 section 7
   
    If you modify this Program, or any covered work, by linking or
    combining it with the OpenSSL project's OpenSSL library (or a modified
    version of that library), containing parts covered by the terms of the
    OpenSSL or SSLeay licenses, the licensors of this Program grant you
    additional permission to convey the resulting work.  Corresponding
    Source for a non-source form of such a combination shall include the
    source code for the parts of OpenSSL used as well as that of the
    covered work.
*/

#ifndef IV_GEN_H__
#define IV_GEN_H__

#include "global.h"

#include <openssl/aes.h>

#if 0
// only good for up to 2**31 sequence numbers
struct iv_gen
{
  AES_KEY ctx;
  u32 count; // low 32 bit of counter value

  void reset ();
  void get (void *buf, int len);
};
#endif

#endif

