/* tcp.h -- A Quick & Dirty TCP protocol. */

/* Brian J. Fox (bfox@ai.mit.edu): Wed Mar  8 12:15:38 1995 */

#if !defined (_TCP_H_)
#define _TCP_H_

#if defined (__cplusplus)
extern "C"
{
#endif

/* Set to non-zero to override the standard connect() timeout with a
   one of length TIME_OUT. This is useful for large networks where
   otherwise wwwproxy could wait for long periods while trying to
   connect to non-responding hosts. */
#define TCP_ALLOW_TIMEOUTS 1

/* Default number of seconds before timing out on connect call. Only
 used if ALLOW_TIMEOUTS is non-zero.*/
#define TCP_TIME_OUT 30

extern int tcp_to_host (char *host, char *service);
extern unsigned char *hostname_or_ip_to_address (char *hostname_spec);

#if defined (__cplusplus)
}
#endif

#endif /* _TCP_H_ */
