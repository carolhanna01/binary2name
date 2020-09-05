/*
    global.h -- global variables and constants
    Copyright (C) 2003-2013 Marc Lehmann <gvpe@schmorp.de>
 
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

#ifndef GLOBAL_H__
#define GLOBAL_H__

#include "config.h"

#include <time.h>

#define HASH_BITS(hash) hashbits_ ## hash
#define HASH_SIZE(hash) (HASH_BITS (hash) >> 3)
#define hashbits_EVP_ripemd160    160
#define hashbits_EVP_sha1         160
#define hashbits_EVP_sha224       224
#define hashbits_EVP_sha256       256
#define hashbits_EVP_sha384       384
#define hashbits_EVP_sha512       512
#define hashbits_EVP_whirlpool    512

#define KEY_BITS(cipher) keybits_ ## cipher
#define KEY_SIZE(cipher) (KEY_BITS (cipher) >> 3)
//#define keybits_EVP_bf_ctr        128 // actually 32-448
#define keybits_EVP_aes_128_ctr   128
#define keybits_EVP_aes_192_ctr   192
#define keybits_EVP_aes_256_ctr   256

#define BLOCK_BITS(cipher) blockbits_ ## cipher
#define BLOCK_SIZE(cipher) (BLOCK_BITS (cipher) >> 3)
//#define blockbits_EVP_bf_ctr       64
#define blockbits_EVP_aes_128_ctr   8
#define blockbits_EVP_aes_192_ctr   8
#define blockbits_EVP_aes_256_ctr   8

#define IV_BITS(cipher) ivbits_ ## cipher
#define IV_SIZE(cipher) (IV_BITS (cipher) >> 3)
//#define ivbits_EVP_bf_ctr         64
#define ivbits_EVP_aes_128_ctr    128
#define ivbits_EVP_aes_192_ctr    128
#define ivbits_EVP_aes_256_ctr    128

/* Protocol version. Different major versions are incompatible,
 * different minor versions probably are compatible ;)
 */

#define PROTOCOL_MAJOR 1
#define PROTOCOL_MINOR 0

#define SERIAL_SIZE	16

#define SEED_SIZE	64 // how many octets to seed rng with

#define RSA_OAEP_SIZE	41

#define HKDF_XTR_HASH	EVP_sha512
#define HKDF_PRF_HASH	EVP_sha256

#define HKDF_SALT	24 // how many bytes for the hkdf salt

#define RSA_KEYLEN	(RSABITS >> 3)

#define AUTH_DIGEST	ENABLE_AUTH
#define AUTH_SIZE	(HASH_SIZE (AUTH_DIGEST))
#define AUTH_TTL	12		// challenge bytes timeout after n seconds of non-use

#define CIPHER		ENABLE_CIPHER
#define CIPHER_KEYSIZE	(KEY_SIZE (CIPHER))
#define CIPHER_IKMSIZE	(CIPHER_KEYSIZE * 3 / 2) // randomness in rsa challenge

#define MAC_DIGEST	ENABLE_HMAC
#define MAC_KEYSIZE	HASH_SIZE (ENABLE_HMAC)	// number of bits used for the HMAC key
#define MAC_IKMSIZE	(MAC_KEYSIZE    * 3 / 2) // randomness in rsa challenge

#define WINDOWSIZE	65536		// sliding window size
#define MAX_SEQNO	(0xfffffff0U - WINDOWSIZE * 8)

//                    hdr seq len  hmac        MAC MAC
#define VPE_OVERHEAD  (4 + 4 + 4 + HMACLENGTH - 6 - 6)
#define IP_OVERHEAD   20			// size of a (normal) ip header
#define GRE_OVERHEAD  (IP_OVERHEAD +  4)
#define ICMP_OVERHEAD (IP_OVERHEAD +  4)
#define UDP_OVERHEAD  (IP_OVERHEAD + 20)	// size of a (normal) ip + udp header (wrong, but don't care)
#define TCP_OVERHEAD  (IP_OVERHEAD + 22)	// size of a (normal) ip + tcp header + packetlength
#define MAX_OVERHEAD  UDP_OVERHEAD		// the max. overhead of any protocol (ok, tcp doesn't count)
#define ETH_OVERHEAD  14			// the size of an ethernet header
#define MAXSIZE       (MAX_MTU + IP_OVERHEAD)	// slightly too large, but who cares

#define PKTCACHESIZE	128	// the size of the memory pool for packets

extern char *confbase;		// directory in which all config files are
extern char *thisnode;		// config for current node (TODO: remove)

template<typename T, typename U> static inline T    min    (T  a, U b) { return a < (T)b ? a : (T)b; }
template<typename T, typename U> static inline void min_it (T &a, U b) {    a = a < (T)b ? a : (T)b; }
template<typename T, typename U> static inline T    max    (T  a, U b) { return a > (T)b ? a : (T)b; }
template<typename T, typename U> static inline void max_it (T &a, U b) {    a = a > (T)b ? a : (T)b; }

template<typename T, typename U, typename V> static inline T clamp (T  v, U a, V b) { return v < (T)a ? a : v >(T)b ? b : v; }

template<typename T, typename U> static inline void swap (T& a, U& b) { T t=a; a=(T)b; b=(U)t; }

#endif

