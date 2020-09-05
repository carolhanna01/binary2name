/*
    hkdf.h -- RFC 5869 HKDF implementation
    Copyright (C) 2013,2016 Marc Lehmann <gvpe@schmorp.de>
 
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

#ifndef HKDF_H__
#define HKDF_H__

#include "global.h"

#include "crypto.h"

// see RFC5869
struct hkdf
{
  hmac ctx;
  u8 prk[EVP_MAX_MD_SIZE];
  const void *salt;
  int salt_len;

  hkdf (const void *salt = 0, int len = 0, const EVP_MD *xtr_hash = EVP_sha512 ());

  void extract (const void *ikm, int len);
  void extract_done (const EVP_MD *prf_hash = 0);

  void expand (void *okm, int len, const void *info = 0, int infolen = 0);

  static void verify ();
};

#endif

