/* Copyright (C) 1999 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
  to anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU General Public License for full details.
  
  Everyone is granted permission to copy, modify and redistribute GNU
  Ghostscript, but only under the conditions described in the GNU General
  Public License.  A copy of this license is supposed to have been given
  to you along with GNU Ghostscript so you can know your rights and
  responsibilities.  It should be in a file named COPYING.  Among other
  things, the copyright notice and this notice must be preserved on all
  copies.
*/

/*$RCSfile: gscrypt1.c,v $ $Revision: 1.2.2.1 $ */
/* Adobe Type 1 encryption/decryption. */
#include "stdpre.h"
#include "gstypes.h"
#include "gscrypt1.h"

/* Encrypt a string. */
int
gs_type1_encrypt(byte * dest, const byte * src, uint len, crypt_state * pstate)
{
    crypt_state state = *pstate;
    const byte *from = src;
    byte *to = dest;
    uint count = len;

    while (count) {
	encrypt_next(*from, state, *to);
	from++, to++, count--;
    }
    *pstate = state;
    return 0;
}
/* Decrypt a string. */
int
gs_type1_decrypt(byte * dest, const byte * src, uint len, crypt_state * pstate)
{
    crypt_state state = *pstate;
    const byte *from = src;
    byte *to = dest;
    uint count = len;

    while (count) {
	/* If from == to, we can't use the obvious */
	/*      decrypt_next(*from, state, *to);        */
	byte ch = *from++;

	decrypt_next(ch, state, *to);
	to++, count--;
    }
    *pstate = state;
    return 0;
}
