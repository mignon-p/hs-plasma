#ifndef ZE_HS_SYSLOG_H_B0D81412
#define ZE_HS_SYSLOG_H_B0D81412

#ifdef _WIN32
#define	LOG_EMERG	0
#define	LOG_ALERT	1
#define	LOG_CRIT	2
#define	LOG_ERR		3
#define	LOG_WARNING	4
#define	LOG_NOTICE	5
#define	LOG_INFO	6
#define	LOG_DEBUG	7
#else
#include <syslog.h>
#endif

#ifndef LOG_PRIMASK
#define LOG_PRIMASK 7
#endif

#ifndef LOG_FACMASK
#define LOG_FACMASK 0x03f8
#endif

#endif /* ZE_HS_SYSLOG_H_B0D81412 */
