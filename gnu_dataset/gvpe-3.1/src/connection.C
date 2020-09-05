/*
    connection.C -- manage a single connection
    Copyright (C) 2003-2008,2010,2011,2013,2016 Marc Lehmann <gvpe@schmorp.de>
 
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

#include <list>
#include <queue>
#include <utility>

#include <openssl/opensslv.h>
#include <openssl/rand.h>
#include <openssl/evp.h>
#include <openssl/rsa.h>
#include <openssl/err.h>

#include "conf.h"
#include "slog.h"
#include "crypto.h"
#include "device.h"
#include "vpn.h"
#include "connection.h"
#include "hkdf.h"

#include "netcompat.h"

#define MAGIC     "gvpe\xbd\xc6\xdb\x82"	// 8 bytes of magic

#define ULTRA_FAST 1
#define HLOG 15
#include "lzf/lzf.h"
#include "lzf/lzf_c.c"
#include "lzf/lzf_d.c"

//////////////////////////////////////////////////////////////////////////////

static std::queue< std::pair<run_script_cb *, const char *> > rs_queue;
static ev::child rs_child_ev;

namespace
{
  void // c++ requires external linkage here, apparently :(
  rs_child_cb (ev::child &w, int revents)
  {
    w.stop ();

    if (rs_queue.empty ())
      return;

    pid_t pid = run_script (*rs_queue.front ().first, false);
    if (pid)
      {
        w.set (pid);
        w.start ();
      }
    else
      slog (L_WARN, rs_queue.front ().second);

    delete rs_queue.front ().first;
    rs_queue.pop ();
  }
};

// despite the fancy name, this is quite a hack
static void
run_script_queued (run_script_cb *cb, const char *warnmsg)
{
  rs_queue.push (std::make_pair (cb, warnmsg));

  if (!rs_child_ev.is_active ())
    {
      rs_child_ev.set<rs_child_cb> ();
      rs_child_ev ();
    }
}

//////////////////////////////////////////////////////////////////////////////

struct crypto_ctx
{
  cipher cctx;
  hmac hctx;

  crypto_ctx (const auth_data &auth1, const auth_data &auth2, const ecdh_key &a, const ecdh_key &b, int enc);
  ~crypto_ctx ();
};

crypto_ctx::crypto_ctx (const auth_data &auth1, const auth_data &auth2, const ecdh_key &a, const ecdh_key &b, int enc)
{
  ecdh_key s;

  curve25519_combine (a, b, s);

  {
    u8 mac_key[MAC_KEYSIZE];
    static const unsigned char mac_info[] = "gvpe mac key";

    hkdf kdf (auth2.rsa.hkdf_salt, sizeof (auth2.rsa.hkdf_salt), HKDF_XTR_HASH ());
    kdf.extract (auth1.rsa.mac_key, sizeof (auth1.rsa.mac_key));
    kdf.extract (s, sizeof (s));
    kdf.extract_done (HKDF_PRF_HASH ());
    kdf.expand (mac_key, sizeof (mac_key), mac_info, sizeof (mac_info));

    hctx.init (mac_key, MAC_KEYSIZE, MAC_DIGEST ());
  }

  {
    u8 cipher_key[CIPHER_KEYSIZE];
    static const unsigned char cipher_info[] = "gvpe cipher key";

    hkdf kdf (auth2.rsa.hkdf_salt, sizeof (auth2.rsa.hkdf_salt), HKDF_XTR_HASH ());
    kdf.extract (auth1.rsa.cipher_key, sizeof (auth1.rsa.cipher_key));
    kdf.extract (s, sizeof (s));
    kdf.extract_done (HKDF_PRF_HASH ());
    kdf.expand (cipher_key, sizeof (cipher_key), cipher_info, sizeof (cipher_info));

    EVP_CIPHER_CTX_init (cctx);
    require (EVP_CipherInit_ex (cctx, CIPHER (), 0, cipher_key, 0, enc));
  }
}

crypto_ctx::~crypto_ctx ()
{
  require (EVP_CIPHER_CTX_cleanup (cctx));
}

static inline void
auth_encrypt (RSA *key, const auth_data &auth, auth_encr &encr)
{
  if (RSA_public_encrypt (sizeof (auth.rsa),
                          (unsigned char *)&auth.rsa, (unsigned char *)&encr.rsa,
                          key, RSA_PKCS1_OAEP_PADDING) < 0)
    fatal ("RSA_public_encrypt error");

  memcpy (&encr.ecdh, &auth.ecdh, sizeof (encr.ecdh));
}

static inline bool
auth_decrypt (RSA *key, const auth_encr &encr, auth_data &auth)
{
  u8 rsa_decrypt[RSA_KEYLEN];

  if (RSA_private_decrypt (sizeof (encr.rsa),
                           (const unsigned char *)&encr.rsa, (unsigned char *)rsa_decrypt,
                           key, RSA_PKCS1_OAEP_PADDING) != sizeof (auth.rsa))
    return 0;

  memcpy (&auth.rsa, rsa_decrypt, sizeof (auth.rsa));
  memcpy (&auth.ecdh, &encr.ecdh, sizeof (auth.ecdh));

  return 1;
}

static void
auth_hash (const auth_data &auth, const ecdh_key &b, auth_mac &mac)
{
  hkdf kdf (b, sizeof b, AUTH_DIGEST ()); // use response ecdh b as salt
  kdf.extract (&auth.rsa, sizeof (auth.rsa));
  kdf.extract_done ();
  kdf.expand (mac, sizeof mac, auth.ecdh, sizeof (auth.ecdh)); // use challenge ecdh b as info
}

void
connection::generate_auth_data ()
{
  if (auth_expire < ev_now ())
    {
      // request data
      rand_fill (snd_auth.rsa);
      curve25519_generate (snd_ecdh_a, snd_auth.ecdh);

      // eventual response data
      curve25519_generate (rcv_ecdh_a, rcv_ecdh_b);
    }

  // every use prolongs the expiry
  auth_expire = ev_now () + AUTH_TTL;
}

//////////////////////////////////////////////////////////////////////////////

pkt_queue::pkt_queue (double max_ttl, int max_queue)
: max_ttl (max_ttl), max_queue (max_queue)
{
  queue = new pkt [max_queue];

  i = 0;
  j = 0;

  expire.set<pkt_queue, &pkt_queue::expire_cb> (this);
}

pkt_queue::~pkt_queue ()
{
  while (net_packet *p = get ())
    delete p;

  delete [] queue;
}

void
pkt_queue::expire_cb (ev::timer &w, int revents)
{
  ev_tstamp expire = ev_now () - max_ttl;

  for (;;)
    {
      if (empty ())
        break;

      double diff = queue[j].tstamp - expire;

      if (diff >= 0.)
        {
          w.start (diff > 0.5 ? diff : 0.5);
          break;
        }

      delete get ();
    }
}

void
pkt_queue::put (net_packet *p)
{
  ev_tstamp now = ev_now ();

  // start expiry timer
  if (empty ())
    expire.start (max_ttl);

  int ni = i + 1 == max_queue ? 0 : i + 1;

  if (ni == j)
    delete get ();

  queue[i].pkt    = p;
  queue[i].tstamp = now;

  i = ni;
}

net_packet *
pkt_queue::get ()
{
  if (empty ())
    return 0;

  net_packet *p = queue[j].pkt;
  queue[j].pkt = 0;

  j = j + 1 == max_queue ? 0 : j + 1;

  return p;
}

struct net_rateinfo
{
  u32    host;
  double pcnt, diff;
  tstamp last;
};

// only do action once every x seconds per host whole allowing bursts.
// this implementation ("splay list" ;) is inefficient,
// but low on resources.
struct net_rate_limiter : list<net_rateinfo>
{
# define NRL_ALPHA  (1. - 1. / 600.)     // allow bursts
# define NRL_CUTOFF 10.                  // one event every CUTOFF seconds
# define NRL_EXPIRE (NRL_CUTOFF * 30.)   // expire entries after this time
# define NRL_MAXDIF (NRL_CUTOFF * (1. / (1. - NRL_ALPHA))) // maximum diff /count value

  bool can (const sockinfo &si) { return can((u32)si.host); }
  bool can (u32 host);
};

static net_rate_limiter auth_rate_limiter, reset_rate_limiter;

bool
net_rate_limiter::can (u32 host)
{
  iterator i;

  for (i = begin (); i != end (); )
    if (i->host == host)
      break;
    else if (i->last < ev_now () - NRL_EXPIRE)
      i = erase (i);
    else
      i++;

  if (i == end ())
    {
      net_rateinfo ri;

      ri.host = host;
      ri.pcnt = 1.;
      ri.diff = NRL_MAXDIF;
      ri.last = ev_now ();

      push_front (ri);

      return true;
    }
  else
    {
      net_rateinfo ri (*i);
      erase (i);

      ri.pcnt = ri.pcnt * NRL_ALPHA;
      ri.diff = ri.diff * NRL_ALPHA + (ev_now () - ri.last);

      ri.last = ev_now ();

      double dif = ri.diff / ri.pcnt;

      bool send = dif > NRL_CUTOFF;

      if (dif > NRL_MAXDIF)
        {
          ri.pcnt = 1.;
          ri.diff = NRL_MAXDIF;
        }
      else if (send)
        ri.pcnt++;

      push_front (ri);

      return send;
    }
}

/////////////////////////////////////////////////////////////////////////////

void
hmac_packet::hmac_gen (crypto_ctx *ctx, u8 *hmac_digest)
{
  ctx->hctx.init ();
  ctx->hctx.add (((unsigned char *) this) + sizeof (hmac_packet), len - sizeof (hmac_packet));
  ctx->hctx.digest (hmac_digest);
}

void
hmac_packet::hmac_set (crypto_ctx *ctx)
{
  unsigned char hmac_digest[EVP_MAX_MD_SIZE];
  hmac_gen (ctx, hmac_digest);
  memcpy (hmac, hmac_digest, HMACLENGTH);
}

bool
hmac_packet::hmac_chk (crypto_ctx *ctx)
{
  unsigned char hmac_digest[EVP_MAX_MD_SIZE];
  hmac_gen (ctx, hmac_digest);
  return slow_memeq (hmac, hmac_digest, HMACLENGTH);
}

void
vpn_packet::set_hdr (ptype type_, unsigned int dst)
{
  type = type_;

  int src = THISNODE->id;

  src1 = src;
  srcdst = ((src >> 8) << 4) | (dst >> 8);
  dst1 = dst;
}

#define MAXVPNDATA (MAX_MTU - 6 - 6)

struct vpndata_packet : vpn_packet
{
  u32 ctr; // seqno
  u8 data[MAXVPNDATA];

  void setup (connection *conn, int dst, u8 *d, u32 len, u32 seqno);
  tap_packet *unpack (connection *conn, u32 &seqno);

private:
  const u32 data_hdr_size () const
  {
    // the distance from beginning of packet to data member
    return data - at (0);
  }
};

// expands packet counter (unlike seqno, in network byte order) to counter mode IV
static unsigned char *
expand_iv (u32 ctr)
{
  static u32 iv[IV_SIZE (CIPHER) / 4];

  require (sizeof (iv) == 4 * 4);
  require (IV_SIZE (CIPHER) % 4 == 0);

  iv[0] =
  iv[1] =
  iv[2] = ctr;

  // I would reuse ctr here to to avoid potential endianness issues,
  // but it seems openssl wraps around. While this would be still ok,
  // and I don't even know if its true, let's play safe and initialise
  // to 0.
  iv[3] = 0;

  return (unsigned char *)iv;
}

void
vpndata_packet::setup (connection *conn, int dst, u8 *d, u32 l, u32 seqno)
{
  EVP_CIPHER_CTX *cctx = conn->octx->cctx;
  int outl = 0, outl2;
  ptype type = PT_DATA_UNCOMPRESSED;

#if ENABLE_COMPRESSION
  u8 cdata[MAX_MTU];

  if (conn->features & FEATURE_COMPRESSION)
    {
      u32 cl = lzf_compress (d, l, cdata + 2, (l - 2) & ~7);

      if (cl)
        {
          type = PT_DATA_COMPRESSED;
          d = cdata;
          l = cl + 2;

          d[0] = cl >> 8;
          d[1] = cl;
        }
    }
#endif

  ctr = htonl (seqno);

  require (EVP_EncryptInit_ex (cctx, 0, 0, 0, expand_iv (ctr)));

  require (EVP_EncryptUpdate (cctx,
                     (unsigned char *)data + outl, &outl2,
                     (unsigned char *)d, l));
  outl += outl2;

  // it seems this is a nop for us, but we do it anyways
  require (EVP_EncryptFinal_ex (cctx, (unsigned char *)data + outl, &outl2));
  outl += outl2;

  len = data_hdr_size () + outl;

  set_hdr (type, dst);

  hmac_set (conn->octx);
}

tap_packet *
vpndata_packet::unpack (connection *conn, u32 &seqno)
{
  EVP_CIPHER_CTX *cctx = conn->ictx->cctx;
  int outl = 0, outl2;
  tap_packet *p = new tap_packet;
  u8 *d;

  seqno = ntohl (ctr);

  require (EVP_DecryptInit_ex (cctx, 0, 0, 0, expand_iv (ctr)));

#if ENABLE_COMPRESSION
  u8 cdata[MAX_MTU];

  if (type == PT_DATA_COMPRESSED)
    d = cdata;
  else
#endif
    d = &(*p)[6 + 6];

  // this can overwrite the len/dst/src fields
  require (EVP_DecryptUpdate (cctx,
                     d, &outl2,
                     (unsigned char *)&data, len - data_hdr_size ()));
  outl += outl2;

  // it seems this is a nop for us, but we do it anyways
  require (EVP_DecryptFinal_ex (cctx, (unsigned char *)d + outl, &outl2));
  outl += outl2;
  
  id2mac (dst () ? dst() : THISNODE->id, p->dst);
  id2mac (src (),                        p->src);

#if ENABLE_COMPRESSION
  if (type == PT_DATA_COMPRESSED)
    {
      u32 cl = (d[0] << 8) | d[1];

      p->len = lzf_decompress (d + 2, cl < MAX_MTU - 2 ? cl : 0,
                               &(*p)[6 + 6], MAX_MTU)
               + 6 + 6;
    }
  else
    p->len = outl + (6 + 6);
#endif

  return p;
}

struct ping_packet : vpn_packet
{
  void setup (int dst, ptype type)
  {
    set_hdr (type, dst);
    len = sizeof (*this) - sizeof (net_packet);
  }
};

struct config_packet : vpn_packet
{
  u8 serial[SERIAL_SIZE];
  u8 prot_major, prot_minor, randsize;
  u8 flags, features, pad6, pad7, pad8;
  u32 cipher_nid, mac_nid, auth_nid;

  void setup (ptype type, int dst);
  bool chk_config (const conf_node *conf, const sockinfo &rsi) const;

  static u8 get_features ()
  {
    u8 f = 0;
#if ENABLE_COMPRESSION
    f |= FEATURE_COMPRESSION;
#endif
#if ENABLE_ROHC
    f |= FEATURE_ROHC;
#endif
#if ENABLE_BRIDGING
    f |= FEATURE_BRIDGING;
#endif
    return f;
  }
};

void
config_packet::setup (ptype type, int dst)
{
  prot_major = PROTOCOL_MAJOR;
  prot_minor = PROTOCOL_MINOR;
  flags = 0;
  features = get_features ();

  strncpy ((char *)serial, conf.serial, sizeof (serial));

  cipher_nid = htonl (EVP_CIPHER_nid (CIPHER ()));
  mac_nid    = htonl (EVP_MD_type (MAC_DIGEST ()));
  auth_nid   = htonl (EVP_MD_type (AUTH_DIGEST ()));

  len = sizeof (*this) - sizeof (net_packet);
  set_hdr (type, dst);
}

bool
config_packet::chk_config (const conf_node *conf, const sockinfo &rsi) const
{
  if (prot_major != PROTOCOL_MAJOR)
    slog (L_WARN, _("%s(%s): major version mismatch (remote %d <=> local %d)"),
          conf->nodename, (const char *)rsi, prot_major, PROTOCOL_MAJOR);
  else if (cipher_nid != htonl (EVP_CIPHER_nid (CIPHER ())))
    slog (L_WARN, _("%s(%s): cipher algo mismatch (remote %x <=> local %x)"),
          conf->nodename, (const char *)rsi, ntohl (cipher_nid), EVP_CIPHER_nid (CIPHER ()));
  else if (mac_nid != htonl (EVP_MD_type (MAC_DIGEST ())))
    slog (L_WARN, _("%s(%s): mac algo mismatch (remote %x <=> local %x)"),
          conf->nodename, (const char *)rsi, ntohl (mac_nid), EVP_MD_type (MAC_DIGEST ()));
  else if (auth_nid != htonl (EVP_MD_type (AUTH_DIGEST ())))
    slog (L_WARN, _("%s(%s): auth algo mismatch (remote %x <=> local %x)"),
          conf->nodename, (const char *)rsi, ntohl (auth_nid), EVP_MD_type (AUTH_DIGEST ()));
  else
    {
      int cmp = memcmp (serial, ::conf.serial, sizeof (serial));

      if (cmp > 0)
        slog (L_WARN, _("%s(%s): remote serial newer than local serial - outdated config?"),
              conf->nodename, (const char *)rsi);
      else if (cmp == 0)
        return true;
    }

  return false;
}

struct auth_req_packet : config_packet // UNPROTECTED
{
  char magic[8];
  u8 initiate; // false if this is just an automatic reply
  u8 protocols; // supported protocols (will be patched on forward)
  u8 pad2, pad3;
  auth_encr encr;

  auth_req_packet (int dst, bool initiate_, u8 protocols_)
  {
    config_packet::setup (PT_AUTH_REQ, dst);
    memcpy (magic, MAGIC, 8);
    initiate = !!initiate_;
    protocols = protocols_;

    len = sizeof (*this) - sizeof (net_packet);
  }
};

struct auth_res_packet : vpn_packet // UNPROTECTED
{
  auth_response response;

  auth_res_packet (int dst)
  {
    set_hdr (PT_AUTH_RES, dst);

    len = sizeof (*this) - sizeof (net_packet);
  }
};

struct connect_req_packet : vpn_packet
{
  u8 id, protocols;
  u8 pad1, pad2;

  connect_req_packet (int dst, int id_, u8 protocols_)
  : id(id_)
  , protocols(protocols_)
  {
    set_hdr (PT_CONNECT_REQ, dst);
    len = sizeof (*this) - sizeof (net_packet);
  }
};

struct connect_info_packet : vpn_packet
{
  u8 id, protocols;
  u8 pad1, pad2;
  sockinfo si;

  connect_info_packet (int dst, int id_, const sockinfo &si_, u8 protocols_)
  : id(id_)
  , protocols(protocols_)
  , si(si_)
  {
    set_hdr (PT_CONNECT_INFO, dst);

    len = sizeof (*this) - sizeof (net_packet);
  }
};

/////////////////////////////////////////////////////////////////////////////

void
connection::connection_established (const sockinfo &rsi)
{
  if (!have_snd_auth || !have_rcv_auth)
    return;

  si = rsi;
  protocol = rsi.prot;

  slog (L_INFO, _("%s(%s): connection established (%s), protocol version %d.%d."),
        conf->nodename, (const char *)rsi,
        vpn->can_direct (THISNODE, conf) ? "direct" : "forwarded",
        PROTOCOL_MAJOR, prot_minor);

  if (::conf.script_node_up)
    {
      run_script_cb *cb = new run_script_cb;
      cb->set<connection, &connection::script_node_up> (this);
      run_script_queued (cb, _("node-up command execution failed, continuing."));
    }
  
  delete ictx; ictx = new crypto_ctx (rcv_auth, snd_auth, rcv_ecdh_a, rcv_auth.ecdh, 0);
  iseqno.reset (ntohl (rcv_auth.rsa.seqno) & 0x7fffffff);

  delete octx; octx = new crypto_ctx (snd_auth, rcv_auth, snd_ecdh_a, snd_ecdh_b   , 1);
  oseqno = ntohl (snd_auth.rsa.seqno) & 0x7fffffff;

  // make sure rekeying timeouts are slightly asymmetric
  ev::tstamp rekey_interval = ::conf.rekey + (conf->id > THISNODE->id ? 10 : 0);
  rekey.start (rekey_interval, rekey_interval);

  hmac_error = 0.;

  keepalive.start (::conf.keepalive);

  // send queued packets
  while (tap_packet *p = (tap_packet *)data_queue.get ())
    {
      if (p->len) send_data_packet (p);
      delete p;
    }

  while (vpn_packet *p = (vpn_packet *)vpn_queue.get ())
    {
      if (p->len) send_vpn_packet (p, si, IPTOS_RELIABILITY);
      delete p;
    }

  vpn->connection_established (this);
}

void
connection::reset_si ()
{
  if (vpn->can_direct (THISNODE, conf))
    protocol = best_protocol (THISNODE->protocols & conf->connectable_protocols ());
  else
    {
      slog (L_TRACE, _("%s: direct connection denied by config."), conf->nodename);
      protocol = 0;
    }

  si.set (conf, protocol);
}

// ensure sockinfo is valid, forward if necessary
const sockinfo &
connection::forward_si (const sockinfo &si) const
{
  if (!si.valid ())
    {
      connection *r = vpn->find_router_for (this);

      if (r)
        {
          slog (L_DEBUG, _("%s: no common protocol, trying to route through %s."),
                conf->nodename, r->conf->nodename);
          return r->si;
        }
      else
        slog (L_DEBUG, _("%s: node unreachable, no common protocol or no router available."),
              conf->nodename);
    }

  return si;
}

void
connection::send_vpn_packet (vpn_packet *pkt, const sockinfo &si, int tos)
{
  if (!vpn->send_vpn_packet (pkt, si, tos))
    reset_connection ("packet send error");
}

void
connection::send_ping (const sockinfo &si, u8 pong)
{
  ping_packet *pkt = new ping_packet;

  pkt->setup (conf->id, pong ? ping_packet::PT_PONG : ping_packet::PT_PING);

  slog (L_TRACE, "%s << %s [%s]", conf->nodename, pong ? "PT_PONG" : "PT_PING", (const char *)si);
  send_vpn_packet (pkt, si, IPTOS_LOWDELAY);

  delete pkt;
}

void
connection::send_reset (const sockinfo &si)
{
  if (reset_rate_limiter.can (si) && connectmode != conf_node::C_DISABLED)
    {
      config_packet *pkt = new config_packet;

      pkt->setup (vpn_packet::PT_RESET, conf->id);
      send_vpn_packet (pkt, si, IPTOS_MINCOST);

      delete pkt;
    }
}

void
connection::send_auth_request (const sockinfo &si, bool initiate)
{
  auth_req_packet *pkt = new auth_req_packet (conf->id, initiate, THISNODE->protocols);

  generate_auth_data ();
  auth_encrypt (conf->rsa_key, snd_auth, pkt->encr);

  slog (L_TRACE, "%s << PT_AUTH_REQ [%s]", conf->nodename, (const char *)si);
  send_vpn_packet (pkt, si, IPTOS_RELIABILITY | IPTOS_LOWDELAY); // rsa is very very costly

  delete pkt;
}

void
connection::send_auth_response (const sockinfo &si)
{
  auth_res_packet *pkt = new auth_res_packet (conf->id);

  memcpy (pkt->response.ecdh, rcv_ecdh_b, sizeof rcv_ecdh_b);
  auth_hash (rcv_auth, rcv_ecdh_b, pkt->response.mac);

  slog (L_TRACE, "%s << PT_AUTH_RES [%s]", conf->nodename, (const char *)si);
  send_vpn_packet (pkt, si, IPTOS_RELIABILITY); // rsa is very very costly

  delete pkt;
}

void
connection::send_connect_info (int rid, const sockinfo &rsi, u8 rprotocols)
{
  slog (L_TRACE, "%s << PT_CONNECT_INFO(%s,%s,p%02x)", conf->nodename,
                 vpn->conns[rid - 1]->conf->nodename, (const char *)rsi,
                 conf->protocols);

  connect_info_packet *r = new connect_info_packet (conf->id, rid, rsi, rprotocols);

  r->hmac_set (octx);
  send_vpn_packet (r, si);

  delete r;
}

inline void
connection::establish_connection_cb (ev::timer &w, int revents)
{
  if (!(ictx && octx)
      && conf != THISNODE
      && connectmode != conf_node::C_NEVER
      && connectmode != conf_node::C_DISABLED
      && !w.is_active ())
    {
      // a bit hacky, if ondemand, and packets are no longer queued, then reset the connection
      // and stop trying. should probably be handled by a per-connection expire handler.
      if (connectmode == conf_node::C_ONDEMAND && vpn_queue.empty () && data_queue.empty ())
        {
          reset_connection ("no demand");
          return;
        }

      last_establish_attempt = ev_now ();

      ev::tstamp retry_int = ev::tstamp (retry_cnt & 3
                                         ? (retry_cnt & 3) + 1
                                         : 1 << (retry_cnt >> 2));

      reset_si ();

      bool slow = (si.prot & PROT_SLOW) || (conf->low_power || THISNODE->low_power);

      if (si.prot && !si.host && vpn->can_direct (THISNODE, conf))
        {
          /*TODO*/ /* start the timer so we don't recurse endlessly */
          w.start (1);
          vpn->send_connect_request (this);
        }
      else
        {
          if (si.valid ())
            slog (L_DEBUG, _("%s: sending direct connection request to %s."),
                  conf->nodename, (const char *)si);

          const sockinfo &dsi = forward_si (si);

          slow = slow || (dsi.prot & PROT_SLOW);

          if (dsi.valid () && auth_rate_limiter.can (dsi))
            {
              // use ping after the first few retries
              // TODO: on rekeys, the other node might not interpret ping correctly,
              // TODO: as it will still have a valid connection
              if (retry_cnt < 4 && (!conf->low_power || THISNODE->low_power))
                send_auth_request (dsi, true);
              else
                send_ping (dsi, 0);
            }
        }

      retry_int *= slow ? 4. : 0.9;

      if (retry_int < conf->max_retry)
        retry_cnt++;
      else
        retry_int = conf->max_retry;

      w.start (retry_int);
    }
}

void
connection::reset_connection (const char *reason)
{
  if (ictx && octx)
    {
      slog (L_INFO, _("%s(%s): connection lost (%s)"),
            conf->nodename, (const char *)si, reason);

      if (::conf.script_node_down)
        {
          run_script_cb *cb = new run_script_cb;
          cb->set<connection, &connection::script_node_down> (this);
          run_script_queued (cb, _("node-down command execution failed, continuing."));
        }
    }

  delete ictx; ictx = 0;
  delete octx; octx = 0;

  si.host = 0;

  have_snd_auth = false;
  have_rcv_auth = false;
  auth_expire = 0.;

  last_activity = 0.;
  //last_si_change = 0.;
  retry_cnt = 0;

  rekey.stop ();
  keepalive.stop ();
  establish_connection.stop ();
}

void
connection::shutdown ()
{
  if (ictx && octx)
    send_reset (si);

  reset_connection ("shutdown");
}

// poor-man's rekeying
inline void
connection::rekey_cb (ev::timer &w, int revents)
{
  reset_connection ("rekeying");
  establish_connection ();
}

void
connection::send_data_packet (tap_packet *pkt)
{
  vpndata_packet *p = new vpndata_packet;
  int tos = 0;

  // I am not hilarious about peeking into packets, but so be it.
  if (conf->inherit_tos && pkt->is_ipv4 ())
    tos = (*pkt)[15] & IPTOS_TOS_MASK;

  p->setup (this, conf->id, &((*pkt)[6 + 6]), pkt->len - 6 - 6, ++oseqno); // skip 2 macs
  send_vpn_packet (p, si, tos);

  delete p;

  if (oseqno > MAX_SEQNO)
    rekey ();
}

void
connection::post_inject_queue ()
{
  // force a connection every now and when when packets are sent (max 1/s)
  if (ev_now () - last_establish_attempt >= (conf->low_power || THISNODE->low_power ? 2.95 : 0.95)) // arbitrary
    establish_connection.stop ();

  establish_connection ();
}

void
connection::inject_data_packet (tap_packet *pkt)
{
  if (ictx && octx)
    send_data_packet (pkt);
  else
    {
      data_queue.put (new tap_packet (*pkt));
      post_inject_queue ();
    }
}

void
connection::inject_vpn_packet (vpn_packet *pkt, int tos)
{
  if (ictx && octx)
    send_vpn_packet (pkt, si, tos);
  else
    {
      vpn_queue.put ((vpn_packet *)new data_packet (*(data_packet *)pkt));
      post_inject_queue ();
    }
}

void
connection::recv_vpn_packet (vpn_packet *pkt, const sockinfo &rsi)
{
  last_activity = ev_now ();

  slog (L_NOISE, "%s >> received packet type %d from %d to %d.",
        conf->nodename, pkt->typ (), pkt->src (), pkt->dst ());

  if (connectmode == conf_node::C_DISABLED)
    return;

  switch (pkt->typ ())
    {
      case vpn_packet::PT_PING:
        slog (L_TRACE, "%s >> PT_PING", conf->nodename);

        // we send pings instead of auth packets after some retries,
        // so reset the retry counter and establish a connection
        // when we receive a ping.
        if (!ictx)
          {
            if (auth_rate_limiter.can (rsi))
              send_auth_request (rsi, true);
          }
        else
          // we would love to change the socket address here, but ping's aren't
          // authenticated, so we best ignore it.
          send_ping (rsi, 1); // pong

        break;

      case vpn_packet::PT_PONG:
        slog (L_TRACE, "%s >> PT_PONG", conf->nodename);

        // a PONG might mean that the other side doesn't really know
        // about our desire for communication.
        establish_connection ();
        break;

      case vpn_packet::PT_RESET:
          slog (L_TRACE, "%s >> PT_RESET", conf->nodename);

        if (ictx && octx)
          {
            reset_connection ("remote reset");

            config_packet *p = (config_packet *) pkt;

            if (p->chk_config (conf, rsi) && connectmode == conf_node::C_ALWAYS)
              establish_connection ();
          }

        break;

      case vpn_packet::PT_AUTH_REQ:
        if (auth_rate_limiter.can (rsi))
          {
            auth_req_packet *p = (auth_req_packet *)pkt;

            slog (L_TRACE, "%s >> PT_AUTH_REQ(%s,p%02x,f%02x)",
                  conf->nodename, p->initiate ? "initiate" : "reply",
                  p->protocols, p->features);

            if (memcmp (p->magic, MAGIC, 8))
              {
                slog (L_WARN, _("%s(%s): protocol magic mismatch - stray packet?"),
                      conf->nodename, (const char *)rsi);
              }
            else if (p->chk_config (conf, rsi))
              {
                if (p->prot_minor != PROTOCOL_MINOR)
                  slog (L_INFO, _("%s(%s): protocol minor version mismatch: ours is %d, %s's is %d."),
                        conf->nodename, (const char *)rsi,
                        PROTOCOL_MINOR, conf->nodename, p->prot_minor);

                if (p->initiate)
                  {
                    send_auth_request (rsi, false);

                    if (ictx && octx)
                      reset_connection ("reconnect");
                  }

                auth_data auth;

                if (!auth_decrypt (::conf.rsa_key, p->encr, auth))
                  {
                    slog (L_ERR, _("%s(%s): challenge illegal or corrupted (%s). mismatched key or config file?"),
                          conf->nodename, (const char *)rsi, ERR_error_string (ERR_get_error (), 0));
                  }
                else
                  {
                    bool chg = !have_rcv_auth || !slow_memeq (&rcv_auth, &auth, sizeof auth);

                    rcv_auth = auth;
                    have_rcv_auth = true;

                    send_auth_response (rsi);

                    if (chg)
                      {
                        conf->protocols = p->protocols;
                        features = p->features & config_packet::get_features ();

                        connection_established (rsi);
                      }
                  }

                break;
              }

            send_reset (rsi);
          }

        break;

      case vpn_packet::PT_AUTH_RES:
        {
          auth_res_packet *p = (auth_res_packet *)pkt;

          slog (L_TRACE, "%s >> PT_AUTH_RES", conf->nodename);

          auth_mac local_mac;
          auth_hash (snd_auth, p->response.ecdh, local_mac);

          if (!slow_memeq (&p->response.mac, local_mac, sizeof local_mac))
            {
              slog (L_ERR, _("%s(%s): unrequested or outdated auth response, ignoring."),
                    conf->nodename, (const char *)rsi);
            }
          else if (!have_snd_auth)
            {
              memcpy (snd_ecdh_b, p->response.ecdh, sizeof snd_ecdh_b);

              have_snd_auth = true;
              connection_established (rsi);
            }
        }
        break;

      case vpn_packet::PT_DATA_COMPRESSED:
#if !ENABLE_COMPRESSION
        send_reset (rsi);
        break;
#endif

      case vpn_packet::PT_DATA_UNCOMPRESSED:

        if (ictx && octx)
          {
            vpndata_packet *p = (vpndata_packet *)pkt;

            if (!p->hmac_chk (ictx))
              {
                // rekeying often creates temporary hmac auth floods
                // we assume they don't take longer than a few seconds normally,
                // and suppress messages and resets during that time.
                //TODO: should be done per source address
                if (!hmac_error)
                  {
                    hmac_error = ev_now () + 3;
                    break;
                  }
                else if (hmac_error >= ev_now ())
                  break; // silently suppress
                else
                  {
                    slog (L_ERR, _("%s(%s): hmac authentication error, received invalid packet\n"
                                   "could be an attack, or just corruption or a synchronization error."),
                          conf->nodename, (const char *)rsi);
                    // reset
                  }
              }
            else
              {
                u32 seqno;
                tap_packet *d = p->unpack (this, seqno);
                int seqclass = iseqno.seqno_classify (seqno);

                hmac_error = 0;

                if (seqclass == 0) // ok
                  {
                    vpn->tap->send (d);

                    if (si != rsi)
                      {
                        // fast re-sync on source address changes, useful especially for tcp/ip
                        //if (last_si_change < ev_now () + 5.)
                        //  {
                            slog (L_INFO, _("%s(%s): changing socket address to %s."),
                                  conf->nodename, (const char *)si, (const char *)rsi);

                            si = rsi;

                            if (::conf.script_node_change)
                              {
                                run_script_cb *cb = new run_script_cb;
                                cb->set<connection, &connection::script_node_change> (this);
                                run_script_queued (cb, _("node-change command execution failed, continuing."));
                              }

                        //  }
                        //else
                        //  slog (L_INFO, _("%s(%s): accepted packet from %s, not (yet) redirecting traffic."),
                        //        conf->nodename, (const char *)si, (const char *)rsi);
                      }
                  }
                else if (seqclass == 1) // far history
                  slog (L_ERR, _("received very old packet (received %08lx, expected %08lx). "
                                 "possible replay attack, or just packet duplication/delay, ignoring."), seqno, iseqno.seq + 1);
                else if (seqclass == 2) // in-window duplicate, happens often on wireless
                  slog (L_DEBUG, _("received recent duplicated packet (received %08lx, expected %08lx). "
                                   "possible replay attack, or just packet duplication, ignoring."), seqno, iseqno.seq + 1);
                else if (seqclass == 3) // reset
                  {
                    slog (L_ERR, _("received out-of-sync (far future) packet (received %08lx, expected %08lx). "
                                   "probably just massive packet loss, sending reset."), seqno, iseqno.seq + 1);
                    send_reset (rsi);
                  }

                delete d;
                break;
              }
          }

        send_reset (rsi);
        break;

      case vpn_packet::PT_CONNECT_REQ:
        if (ictx && octx && rsi == si && pkt->hmac_chk (ictx))
          {
            connect_req_packet *p = (connect_req_packet *)pkt;

            if (p->id > 0 && p->id <= vpn->conns.size ())
              {
                connection *c = vpn->conns[p->id - 1];
                conf->protocols = p->protocols;

                slog (L_TRACE, "%s >> PT_CONNECT_REQ(%s,p%02x) [%d]",
                               conf->nodename, vpn->conns[p->id - 1]->conf->nodename,
                               p->protocols,
                               c->ictx && c->octx);

                if (c->ictx && c->octx)
                  {
                    // send connect_info packets to both sides, in case one is
                    // behind a nat firewall (or both ;)
                    c->send_connect_info (conf->id, si, conf->protocols);
                    send_connect_info (c->conf->id, c->si, c->conf->protocols);
                  }
                else
                  c->establish_connection ();
              }
            else
              slog (L_WARN,
                    _("received authenticated connection request from unknown node #%d, config file mismatch?"),
                    p->id);
          }

        break;

      case vpn_packet::PT_CONNECT_INFO:
        if (ictx && octx && rsi == si && pkt->hmac_chk (ictx))
          {
            connect_info_packet *p = (connect_info_packet *)pkt;

            if (p->id > 0 && p->id <= vpn->conns.size ())
              {
                connection *c = vpn->conns[p->id - 1];

                c->conf->protocols = p->protocols;
                protocol = best_protocol (c->conf->protocols & THISNODE->protocols & p->si.supported_protocols (c->conf));
                p->si.upgrade_protocol (protocol, c->conf);

                slog (L_TRACE, "%s >> PT_CONNECT_INFO(%s,%s,protocols=%02x,protocol=%02x,upgradable=%02x) [%d]",
                               conf->nodename,
                               vpn->conns[p->id - 1]->conf->nodename,
                               (const char *)p->si,
                               p->protocols,
                               protocol,
                               p->si.supported_protocols (c->conf),
                               !c->ictx && !c->octx);

                const sockinfo &dsi = forward_si (p->si);

                if (dsi.valid ())
                  c->send_auth_request (dsi, true);
                else
                  slog (L_INFO, "connect info for %s received (%s), but still unable to contact.",
                                 vpn->conns[p->id - 1]->conf->nodename,
                                 (const char *)p->si);
              }
            else
              slog (L_WARN,
                    _("received authenticated connection request from unknown node #%d, config file mismatch?"),
                    p->id);
          }

        break;

      default:
        send_reset (rsi);
        break;
    }
}

inline void
connection::keepalive_cb (ev::timer &w, int revents)
{
  ev_tstamp when = last_activity + ::conf.keepalive - ev::now ();

  if (when >= 0)
    w.start (when);
  else if (when < -15)
    {
      reset_connection ("keepalive overdue");
      establish_connection ();
    }
  else if (conf->connectmode != conf_node::C_ONDEMAND
           || THISNODE->connectmode != conf_node::C_ONDEMAND)
    {
      w.start (3);
      send_ping (si);
    }
  else if (when >= -10)
    // hold ondemand connections implicitly a few seconds longer
    // should delete octx, though, or something like that ;)
    w.start (when + 10);
  else
    reset_connection ("keepalive timeout");
}

void
connection::send_connect_request (int id)
{
  connect_req_packet *p = new connect_req_packet (conf->id, id, THISNODE->protocols);

  slog (L_TRACE, "%s << PT_CONNECT_REQ(%s,p%02x)",
                 conf->nodename, vpn->conns[id - 1]->conf->nodename,
                 THISNODE->protocols);
  p->hmac_set (octx);
  send_vpn_packet (p, si);

  delete p;
}

void
connection::script_init_env (const char *ext)
{
  char *env;
  asprintf (&env, "IFUPDATA%s=%s", ext, conf->if_up_data); putenv (env);
  asprintf (&env, "NODENAME%s=%s", ext, conf->nodename);   putenv (env);
  asprintf (&env, "MAC%s=%02x:%02x:%02x:%02x:%02x:%02x", ext,
            0xfe, 0xfd, 0x80, 0x00, conf->id >> 8,
            conf->id & 0xff);                              putenv (env);
}

void
connection::script_init_connect_env ()
{
  vpn->script_init_env ();

  char *env;
  asprintf (&env, "DESTID=%d",   conf->id);         putenv (env);
  asprintf (&env, "DESTSI=%s",   (const char *)si); putenv (env);
  asprintf (&env, "DESTNODE=%s", conf->nodename);   putenv (env);
  asprintf (&env, "DESTIP=%s",   si.ntoa ());       putenv (env);
  asprintf (&env, "DESTPORT=%d", ntohs (si.port));  putenv (env);
}

inline const char *
connection::script_node_up ()
{
  script_init_connect_env ();

  putenv ((char *)"STATE=up");

  char *filename;
  asprintf (&filename,
            "%s/%s",
            confbase,
            ::conf.script_node_up ? ::conf.script_node_up : "node-up");

  return filename;
}

inline const char *
connection::script_node_change ()
{
  script_init_connect_env ();

  putenv ((char *)"STATE=change");

  char *filename;
  asprintf (&filename,
            "%s/%s",
            confbase,
            ::conf.script_node_change ? ::conf.script_node_change : "node-change");

  return filename;
}

inline const char *
connection::script_node_down ()
{
  script_init_connect_env ();

  putenv ((char *)"STATE=down");

  char *filename;
  asprintf (&filename,
            "%s/%s",
            confbase,
            ::conf.script_node_down ? ::conf.script_node_down : "node-down");

  return filename;
}

connection::connection (struct vpn *vpn, conf_node *conf)
: vpn(vpn), conf(conf),
#if ENABLE_DNS
  dns (0),
#endif
  data_queue(conf->max_ttl, conf->max_queue + 1),
  vpn_queue(conf->max_ttl, conf->max_queue + 1)
{
  rekey               .set<connection, &connection::rekey_cb               > (this);
  keepalive           .set<connection, &connection::keepalive_cb           > (this);
  establish_connection.set<connection, &connection::establish_connection_cb> (this);

  last_establish_attempt = 0.;
  octx = ictx = 0;

  connectmode = conf->connectmode;

  // queue a dummy packet to force an initial connection attempt
  if (connectmode != conf_node::C_ALWAYS && connectmode != conf_node::C_DISABLED)
    vpn_queue.put (new net_packet);

  reset_connection ("startup");
}

connection::~connection ()
{
  shutdown ();
}

void
connection_init ()
{
  auth_rate_limiter.clear ();
  reset_rate_limiter.clear ();
}

