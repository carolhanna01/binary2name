/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996-1997, Riley Rainey (rainey@netcom.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <rpc/types.h>
#include <rpc/xdr.h>
#include <dis/dis.h>
#ifndef WIN32
#ifdef HAVE_RECVMSG
#include <sys/uio.h>
#endif
#include <sys/socket.h>
#define BSD_COMP				/* keeps Solaris happy */
#include <sys/ioctl.h>
#include <net/if.h>
#include <netdb.h>
#endif							/* ifndef WIN32 */
#include <stdio.h>
#include <stdlib.h>

#ifndef WIN32
#define INVALID_SOCKET	-1
#define SOCKET_ERROR	-1
#endif

#if SYSCALL_PROTO
extern int recvmsg(int, struct msghdr *, int);
extern int sendmsg(int, struct msghdr *, int);
extern int socket(int domain, int type, int protocol);
extern int setsockopt(int s, int level, int optname, char *optval, int optlen);
extern int bind(int s, struct sockaddr *name, int namelen);
extern int ioctl(int, int,...);
extern void perror(const char *);
extern void bcopy(const void *, void *, int);

#endif

extern bool_t xdr_dis_pdu(XDR * xdrs, dis_pdu * objp);
extern void
xdrumem_create(register XDR * xdrs, caddr_t addr, u_int size, enum xdr_op op);

int
DISReadPDU(DISTransceiver * xcvr, dis_pdu * pdu)
{
	char      buffer[2048];
	int       size;
	XDR       xdr;

#ifdef HAVE_RECVMSG
	struct sockaddr from;
	struct msghdr msg;
	struct iovec vec;

	msg.msg_name = (caddr_t) & from;
	msg.msg_namelen = sizeof(from);
	msg.msg_iov = &vec;
	msg.msg_iovlen = 1;
#ifdef HAVE_MSG_ACCRIGHTS 
	msg.msg_accrights = (caddr_t) NULL;
	msg.msg_accrightslen = 0;
#endif    
#ifdef HAVE_MSG_CONTROL
	msg.msg_control = (caddr_t) NULL;
	msg.msg_controllen = 0;
#endif    
	vec.iov_base = (caddr_t) & buffer;
	vec.iov_len = sizeof(buffer);

	size = recvmsg(xcvr->s, &msg, 0);
#else
	size = recv(xcvr->s, buffer, sizeof(buffer), 0);
#endif

	if (size == -1) {
		return size;
	}

/*
 *  XDR man pages say size must be a multiple of 4.
 */

	size = ((size + 3) / 4) * 4;

	xdrumem_create(&xdr, buffer, size, XDR_DECODE);

	xdr_dis_pdu(&xdr, pdu);

	return 0;
}

int
DISWritePDU(DISTransceiver * xcvr, dis_pdu * pdu)
{
	char      buffer[2048], *p;

#ifdef HAVE_RECVMSG
	struct msghdr msg;
	struct iovec vec;

#endif
	XDR       xdr;
	int       i, result, len;

/*
 *  Fill-out any length fields internal to the PDU (other than the length
 *  field in the header.
 */

	DISAddPDUSizes(pdu);

/*
 *  Now normalize the packet.
 */

	xdrumem_create(&xdr, (char *) &buffer, sizeof(buffer), XDR_ENCODE);

	xdr_dis_pdu(&xdr, pdu);

	len = xdr_getpos(&xdr);

/*
 *  Now for a hack. We need to insert the correct packet length into
 *  the PDU header.  The header is somewhat stable from one protocol release
 *  to the next, so I've just hard-coded it here.
 */

	p = buffer + 8;
	*((u_short *) p) = htons(len);

#ifdef HAVE_RECVMSG

	msg.msg_namelen = sizeof(struct sockaddr);

	msg.msg_iov = &vec;
	msg.msg_iovlen = 1;
#ifdef HAVE_MSG_ACCRIGHTS
	msg.msg_accrights = (caddr_t) NULL;
	msg.msg_accrightslen = 0;
#endif
#ifdef HAVE_MSG_CONTROL
	msg.msg_control = (caddr_t) NULL;
	msg.msg_controllen = 0;
#endif    
	vec.iov_base = (caddr_t) & buffer;
	vec.iov_len = len;

	for (i = 0; i < xcvr->num_dest; ++i) {
		msg.msg_name = (caddr_t) & xcvr->dest[i].addr;
		if ((result = sendmsg(xcvr->s, &msg, 0)) == -1) {
			perror("on sendmsg");
		}
	};

#else

	for (i = 0; i < xcvr->num_dest; ++i) {
		if ((result = sendto(xcvr->s, buffer, xdr_getpos(&xdr), 0,
							 (struct sockaddr *) &xcvr->dest[i].addr,
							 sizeof(struct sockaddr))) == -1) {
#ifdef WIN32
			result = WSAGetLastError();
#else
			perror("on sendto");
#endif
		}
	};

#endif

	return 0;
}

DISTransceiver *
DISOpenTransceiver(int port)
{
#ifdef WIN32
	char      Hostname[100];
	HOSTENT  *pHostEnt;
	char     *ad;

#else
	struct ifconf ifc;
	struct ifreq *ifr;

#endif
	struct sockaddr_in sin;
	char      buf[BUFSIZ];
	int       s, n, i = 0;
	int       on = 1;
	DISTransceiver *xcvr;
	struct hostent *relay_hp;
	char     *relay;

/*
 *  User requested the "default" UDP port?
 */

	if (port == -1) {
		port = 3000;
	}

/*
 *  Allocate and initialize the DISTransceiver structure.
 */

	xcvr = (DISTransceiver *) malloc(sizeof(DISTransceiver));
	xcvr->s = 0;
	xcvr->num_dest = 0;

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) == INVALID_SOCKET) {
#ifdef WIN32
		errno = WSAGetLastError();
#endif
		perror("position update socket");
		exit(1);
	}

	if (setsockopt(s, SOL_SOCKET, SO_BROADCAST, (char *) &on,
				   sizeof(on)) == SOCKET_ERROR) {
#ifdef WIN32
		errno = WSAGetLastError();
#endif
		perror("can't set broadcast flag");
		exit(1);
	}

	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) &on,
				   sizeof(on)) == SOCKET_ERROR) {
		perror("can't reuse broadcast port");
	}

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = htonl(INADDR_ANY);
	sin.sin_port = htons(port);
	bind(s, (struct sockaddr *) &sin, sizeof(sin));

	xcvr->s = s;

/*
 *  If the user has a DIS_RELAY environment variable set, then use that
 *  host as the only destination.  This assumes a point-to-point connection
 *  to a relay site.
 */

	if ((relay = getenv("DIS_RELAY")) != (char *) NULL) {
		if ((relay_hp = gethostbyname(relay)) == (struct hostent *) NULL) {
			close(s);
			return 0;
		}
		if (relay_hp->h_addrtype != AF_INET) {
			close(s);
			return 0;
		}
		xcvr->dest[0].addr.sin_family = relay_hp->h_addrtype;
		bcopy((char *) relay_hp->h_addr,
			  (char *) &xcvr->dest[0].addr.sin_addr,
			  sizeof(relay_hp->h_addr));
		xcvr->dest[0].addr.sin_port = htons(port);
		xcvr->dest[0].type = 1;
		xcvr->num_dest = 1;
		return xcvr;
	}

/*
 *  Determine how many interfaces are configured on the local system.
 */

#ifdef WIN32
#ifdef notdef
	gethostname(Hostname, sizeof(Hostname));
	pHostEnt = gethostbyname(Hostname);

	i = 0;
	while (pHostEnt->h_addr_list[i]) {
		// pHostEnt->h_addr_list[i] - the current address in host order
		ad = pHostEnt->h_addr_list[i];
		bcopy((char *) ad,
			  (char *) &xcvr->dest[i].addr.sin_addr,
			  sizeof(xcvr->dest[i].addr));
		bcopy((char *) ad,
			  (char *) x,
			  4);
		xcvr->dest[i].addr.sin_family = AF_INET;
		xcvr->dest[i].addr.sin_port = htons(port);
		xcvr->dest[i].type = 0;
		i++;
	}
#endif
	xcvr->dest[0].addr.sin_family = AF_INET;
	xcvr->dest[0].addr.sin_addr.S_un.S_addr = INADDR_BROADCAST;
	xcvr->dest[0].addr.sin_port = htons(port);
	xcvr->dest[0].type = 0;
	i = 1;

#else

	ifc.ifc_len = BUFSIZ;
	ifc.ifc_buf = buf;
	if (ioctl(s, SIOCGIFCONF, (char *) &ifc) < 0) {
		perror("error getting interface configuration");
		close(s);
		return NULL;
	}

	n = ifc.ifc_len / sizeof(struct ifreq);

/*
 *  Insure that there are enough elements in blist to accomodate all interfaces.
 */

	if (n > 32) {
		return NULL;
	}

	for (ifr = ifc.ifc_req; --n >= 0; ifr++) {

/*
 *  We're only intrested in Internet domain interfaces
 */

		if (ifr->ifr_addr.sa_family != AF_INET)
			continue;

		if (ioctl(s, SIOCGIFFLAGS, (char *) ifr) < 0) {
			perror("error getting interface flags");
			close(s);
			return NULL;
		}

/*
 *  Skip boring cases ...
 */

		if ((ifr->ifr_flags & IFF_UP) == 0 ||
			(ifr->ifr_flags & IFF_LOOPBACK) ||
			(ifr->ifr_flags & (IFF_BROADCAST | IFF_POINTOPOINT)) == 0)
			continue;

/*
 *  Get the appropriate broadcast address based on the interface type.
 */

		if (ifr->ifr_flags & IFF_POINTOPOINT) {
			if (ioctl(s, SIOCGIFDSTADDR, (char *) ifr) < 0) {
				close(s);
				perror("error getting address");
				return NULL;
			}

			bcopy((char *) &ifr->ifr_dstaddr,
				  (char *) &xcvr->dest[i].addr,
				  sizeof(ifr->ifr_dstaddr));

		}
		else if (ifr->ifr_flags & IFF_BROADCAST) {
			if (ioctl(s, SIOCGIFBRDADDR, (char *) ifr) < 0) {
				close(s);
				perror("error getting broadcast address");
				return NULL;
			}

			bcopy((char *) &ifr->ifr_broadaddr,
				  (char *) &xcvr->dest[i].addr,
				  sizeof(ifr->ifr_broadaddr));

		}

		xcvr->dest[i].addr.sin_port = htons(port);
		xcvr->dest[i].type = 0;
		i++;

	}

#endif							/* not WIN32 */

	xcvr->num_dest = i;

	return xcvr;
}

void
DISCloseTransceiver(DISTransceiver * xcvr)
{
#ifdef WIN32
	shutdown(xcvr->s, SD_BOTH);
	closesocket(xcvr->s);
#else
	close(xcvr->s);
#endif
	free(xcvr);
}

int
DISSetNBIOState(DISTransceiver * xcvr, int state)
{
	int       i;

	i = (state) ? 1 : 0;

#ifdef WIN32
	if (ioctlsocket(xcvr->s, FIONBIO, &i) != 0) {
#else
	if (ioctl(xcvr->s, FIONBIO, &i) != 0) {
#endif
		return -1;
	}
	return 0;
}

#ifdef notdef
int
DISSetMulticastMode(DISTransceiver * xcvr, unsigned long maddress, unsigned long ointerface)
{
	int       ttl = 8;
	unsigned long addr = ointerface;
	struct ip_mreq mreq;

	mreq.imr_multiaddr.s_addr = maddress;
	mreq.imr_interface.s_addr = ointerface;

	setsockopt(xcvr->s, IPPROTO_IP, IP_MULTICAST_TTL, (char *) &ttl, sizeof(ttl));
	setsockopt(xcvr->s, IPPROTO_IP, IP_MULTICAST_IF, (char *) &addr, sizeof(addr));
	setsockopt(xcvr->s, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char *) &mreq, sizeof(mreq));

	return 0;
}
#endif
