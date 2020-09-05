/*
    crypto.h -- openssl crypto wrappers
    Copyright (C) 2016      Marc Lehmann <gvpe@schmorp.de>
 
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

#ifndef CRYPTO_H__
#define CRYPTO_H__

#include <slog.h>

#include <openssl/opensslv.h>
#include <openssl/hmac.h>
#include <openssl/evp.h>

// openssl 0.9.8/1.0.0 compatibility
#if OPENSSL_VERSION_NUMBER < 0x10001000
  #define require101(exp) exp
#else
  #define require101(exp) require (exp)
#endif

/* this pretty much wraps the slightly weird openssl api */
class hmac
{
  HMAC_CTX *ctx;

public:

  hmac ();
  ~hmac ();

  void init (const void *key, int key_len, const EVP_MD *hash = 0)
  {
    require101 (HMAC_Init_ex (ctx, key, key_len, hash, 0));
  }

  void init ()
  {
    require101 (HMAC_Init_ex (ctx, 0, 0, 0, 0));
  }

  void
  add (const void *data, int len)
  {
    require101 (HMAC_Update (ctx, (const unsigned char *)data, len));
  }

  void
  digest (void *dgst)
  {
    require101 (HMAC_Final (ctx, (unsigned char *)dgst, 0));
  }

  int
  size ()
  {
    return HMAC_size (ctx);
  }
};

/* cheap alloc/free wrapper only atm. */
class cipher
{
  EVP_CIPHER_CTX *ctx;

public:

  cipher ();
  ~cipher ();

  operator EVP_CIPHER_CTX *()
  {
    return ctx;
  }
};

#endif

