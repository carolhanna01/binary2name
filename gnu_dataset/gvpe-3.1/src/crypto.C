/*
    crypto.C -- openssl crypto wrappers
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

#include <openssl/crypto.h>

#include "crypto.h"

hmac::hmac ()
{
#if OPENSSL_VERSION_NUMBER < 0x10100000
  require (ctx = (HMAC_CTX *)OPENSSL_malloc (sizeof (*ctx)));
  HMAC_CTX_init (ctx);
#else
  require (ctx = HMAC_CTX_new ());
#endif
}

hmac::~hmac ()
{
#if OPENSSL_VERSION_NUMBER < 0x10100000
  HMAC_CTX_cleanup (ctx);
  OPENSSL_free (ctx);
#else
  HMAC_CTX_free (ctx);
#endif
}

cipher::cipher ()
{
#if OPENSSL_VERSION_NUMBER < 0x10100000
  require (ctx = (EVP_CIPHER_CTX *)OPENSSL_malloc (sizeof (*ctx)));
  EVP_CIPHER_CTX_init (ctx);
#else
  require (ctx = EVP_CIPHER_CTX_new ());
#endif
}

cipher::~cipher ()
{
#if OPENSSL_VERSION_NUMBER < 0x10100000
  EVP_CIPHER_CTX_cleanup (ctx);
  OPENSSL_free (ctx);
#else
  EVP_CIPHER_CTX_free (ctx);
#endif
}


