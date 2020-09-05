/*
    curve25519.C -- diffie hellman key exchange
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

#include "config.h"

#include <cstring>
#include <openssl/rand.h>

#include "util.h"
#include "curve25519.h"

#if __GNUC__ >= 4 && __SIZEOF_LONG__ == 8
#include "curve25519-donna-c64.c"
#else
#include "curve25519-donna.c"
#endif

static void
curve25519_derive (const curve25519_key &a, curve25519_key &b)
{
  static const curve25519_key basepoint = { 9 };
  curve25519_donna (b, a, basepoint);
}

void curve25519_generate (curve25519_key &a, curve25519_key &b)
{
  rand_fill (a);

#if 0
  a [ 0] &= 0xf8;
  a [31] &= 0x7f;
  a [31] |= 0x40;
#endif

  curve25519_derive (a, b);
}

void curve25519_combine (const curve25519_key &a, const curve25519_key &b, curve25519_key &s)
{
  curve25519_donna (s, a, b);
}

void curve25519_verify ()
{
  // NaCl test vector

  static const curve25519_key alice_private  = { 0x77, 0x07, 0x6d, 0x0a, 0x73, 0x18, 0xa5, 0x7d, 0x3c, 0x16, 0xc1, 0x72, 0x51, 0xb2, 0x66, 0x45, 0xdf, 0x4c, 0x2f, 0x87, 0xeb, 0xc0, 0x99, 0x2a, 0xb1, 0x77, 0xfb, 0xa5, 0x1d, 0xb9, 0x2c, 0x2a };
  static const curve25519_key alice_public   = { 0x85, 0x20, 0xf0, 0x09, 0x89, 0x30, 0xa7, 0x54, 0x74, 0x8b, 0x7d, 0xdc, 0xb4, 0x3e, 0xf7, 0x5a, 0x0d, 0xbf, 0x3a, 0x0d, 0x26, 0x38, 0x1a, 0xf4, 0xeb, 0xa4, 0xa9, 0x8e, 0xaa, 0x9b, 0x4e, 0x6a };
  static const curve25519_key bob_private    = { 0x5d, 0xab, 0x08, 0x7e, 0x62, 0x4a, 0x8a, 0x4b, 0x79, 0xe1, 0x7f, 0x8b, 0x83, 0x80, 0x0e, 0xe6, 0x6f, 0x3b, 0xb1, 0x29, 0x26, 0x18, 0xb6, 0xfd, 0x1c, 0x2f, 0x8b, 0x27, 0xff, 0x88, 0xe0, 0xeb };
  static const curve25519_key bob_public     = { 0xde, 0x9e, 0xdb, 0x7d, 0x7b, 0x7d, 0xc1, 0xb4, 0xd3, 0x5b, 0x61, 0xc2, 0xec, 0xe4, 0x35, 0x37, 0x3f, 0x83, 0x43, 0xc8, 0x5b, 0x78, 0x67, 0x4d, 0xad, 0xfc, 0x7e, 0x14, 0x6f, 0x88, 0x2b, 0x4f };
  static const curve25519_key alice_mult_bob = { 0x4a, 0x5d, 0x9d, 0x5b, 0xa4, 0xce, 0x2d, 0xe1, 0x72, 0x8e, 0x3b, 0xf4, 0x80, 0x35, 0x0f, 0x25, 0xe0, 0x7e, 0x21, 0xc9, 0x47, 0xd1, 0x9e, 0x33, 0x76, 0xf0, 0x9b, 0x3c, 0x1e, 0x16, 0x17, 0x42 };

  curve25519_key a, b, s1, s2;

  curve25519_derive (alice_private, a);
  curve25519_derive (bob_private  , b);
  curve25519_combine (alice_private, b, s1);
  curve25519_combine (bob_private  , a, s2);

  require (!memcmp (alice_public  , a , sizeof a ));
  require (!memcmp (bob_public    , b , sizeof b ));
  require (!memcmp (alice_mult_bob, s1, sizeof s1));
  require (!memcmp (alice_mult_bob, s2, sizeof s2));
}

