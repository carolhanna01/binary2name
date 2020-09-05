/*
    hkdf.C -- RFC 5869 HKDF implementation
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

#include "config.h"

#include <cstring>

#include <openssl/opensslv.h>
#include <openssl/rand.h>
#include <openssl/hmac.h>

#include "crypto.h"
#include "util.h"
#include "hkdf.h"

hkdf::hkdf (const void *salt, int len, const EVP_MD *xtr_hash)
: salt (salt), salt_len (len)
{
  ctx.init (salt, salt_len, xtr_hash);
}

void
hkdf::extract (const void *ikm, int len)
{
  ctx.add (ikm, len);
}

void
hkdf::extract_done (const EVP_MD *prf_hash)
{
  ctx.digest (prk);
  ctx.init (salt, salt_len, prf_hash);
}

void
hkdf::expand (void *okm, int len, const void *info, int infolen)
{
  u8 tn[sizeof prk];
  u8 iter = 0;
  int md_size = ctx.size ();

  while (len)
    {
      ctx.init (prk, md_size);

      if (iter)
        ctx.add (tn, md_size);

      ctx.add (info, infolen);

      ++iter;
      require (iter);

      ctx.add (&iter, 1);
      ctx.digest (tn);

      int ol = len > md_size ? md_size : len;

      memcpy (okm, tn, ol);

      okm = (void *)(ol + (char *)okm);
      len -= ol;
    }
}

// try to verify all test vectors from the RFC
// since I implemented the hkdf myself, and I am no crypto expert,
// we run verification on every startup.
void
hkdf::verify ()
{
  struct unhex
  {
    u8 *p;
    int l;

    u8 s[256];

    unhex (const char *hs)
    {
      l = 0;
      p = s;

      if (!hs)
        return;

      while (*hs)
        {
          int d1 = *hs >= '0' && *hs <= '9' ? *hs - '0' : *hs - 'a' + 10; ++hs;
          int d2 = *hs >= '0' && *hs <= '9' ? *hs - '0' : *hs - 'a' + 10; ++hs;

          *p++ = d1 * 16 + d2;
          ++l;
        }

      p = s;
    }
  };

  const struct hkdf_test
  {
     int hash;
     const char *IKM, *salt, *info;
     const char *PRK, *OKM;
  } tests[] = {
    { // 0
      256,
      "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b",
      "000102030405060708090a0b0c",
      "f0f1f2f3f4f5f6f7f8f9",
      "077709362c2e32df0ddc3f0dc47bba63"
      "90b6c73bb50f9c3122ec844ad7c2b3e5",
      "3cb25f25faacd57a90434f64d0362f2a"
      "2d2d0a90cf1a5a4c5db02d56ecc4c5bf"
      "34007208d5b887185865"
    }, { // 1
      256,
      "000102030405060708090a0b0c0d0e0f"
      "101112131415161718191a1b1c1d1e1f"
      "202122232425262728292a2b2c2d2e2f"
      "303132333435363738393a3b3c3d3e3f"
      "404142434445464748494a4b4c4d4e4f",
      "606162636465666768696a6b6c6d6e6f"
      "707172737475767778797a7b7c7d7e7f"
      "808182838485868788898a8b8c8d8e8f"
      "909192939495969798999a9b9c9d9e9f"
      "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf",
      "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
      "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"
      "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
      "e0e1e2e3e4e5e6e7e8e9eaebecedeeef"
      "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",
      "06a6b88c5853361a06104c9ceb35b45c"
      "ef760014904671014a193f40c15fc244",
      "b11e398dc80327a1c8e7f78c596a4934"
      "4f012eda2d4efad8a050cc4c19afa97c"
      "59045a99cac7827271cb41c65e590e09"
      "da3275600c2f09b8367793a9aca3db71"
      "cc30c58179ec3e87c14c01d5c1f3434f"
      "1d87"
    }, { // 2
      256,
      "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b",
      "",
      "",
      "19ef24a32c717b167f33a91d6f648bdf"
      "96596776afdb6377ac434c1c293ccb04",
      "8da4e775a563c18f715f802a063c5a31"
      "b8a11f5c5ee1879ec3454e5f3c738d2d"
      "9d201395faa4b61a96c8"
    }, { // 3
      1,
      "0b0b0b0b0b0b0b0b0b0b0b",
      "000102030405060708090a0b0c",
      "f0f1f2f3f4f5f6f7f8f9",
      "9b6c18c432a7bf8f0e71c8eb88f4b30baa2ba243",
      "085a01ea1b10f36933068b56efa5ad81"
      "a4f14b822f5b091568a9cdd4f155fda2"
      "c22e422478d305f3f896"
    }, { // 4
      1,
      "000102030405060708090a0b0c0d0e0f"
      "101112131415161718191a1b1c1d1e1f"
      "202122232425262728292a2b2c2d2e2f"
      "303132333435363738393a3b3c3d3e3f"
      "404142434445464748494a4b4c4d4e4f",
      "606162636465666768696a6b6c6d6e6f"
      "707172737475767778797a7b7c7d7e7f"
      "808182838485868788898a8b8c8d8e8f"
      "909192939495969798999a9b9c9d9e9f"
      "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf",
      "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
      "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"
      "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
      "e0e1e2e3e4e5e6e7e8e9eaebecedeeef"
      "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",
      "8adae09a2a307059478d309b26c4115a224cfaf6",
      "0bd770a74d1160f7c9f12cd5912a06eb"
      "ff6adcae899d92191fe4305673ba2ffe"
      "8fa3f1a4e5ad79f3f334b3b202b2173c"
      "486ea37ce3d397ed034c7f9dfeb15c5e"
      "927336d0441f4c4300e2cff0d0900b52"
      "d3b4"
    }, { // 5
      1,
      "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b",
      "",
      "",
      "da8c8a73c7fa77288ec6f5e7c297786aa0d32d01",
      "0ac1af7002b3d761d1e55298da9d0506"
      "b9ae52057220a306e07b6b87e8df21d0"
      "ea00033de03984d34918"
    }, { // 6
      1,
      "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c",
      0,
      "",
      "2adccada18779e7c2077ad2eb19d3f3e731385dd",
      "2c91117204d745f3500d636a62f64f0a"
      "b3bae548aa53d423b0d1f27ebba6f5e5"
      "673a081d70cce7acfc48"
    }
  };

  for (int i = 0; i < sizeof (tests) / sizeof (tests[0]); ++i)
    {
      const hkdf_test &test = tests[i];

      unhex salt (test.salt);
      unhex ikm (test.IKM);
      unhex info (test.info);
      unhex prk_correct (test.PRK);
      unhex okm_correct (test.OKM);

      char okm[256];

      hkdf h (salt.p, salt.l, test.hash == 1 ? EVP_sha1 () : EVP_sha256 ());
      h.extract (ikm.p, ikm.l);
      h.extract_done ();
      h.expand (okm, okm_correct.l, info.p, info.l);

      require (!memcmp (h.prk, prk_correct.p, prk_correct.l));
      require (!memcmp (okm  , okm_correct.p, okm_correct.l));
    }
}

