#ifndef ZE_HS_SYSLOG_H_B0D81412
#define ZE_HS_SYSLOG_H_B0D81412

#ifdef _WIN32

/* priorities */
#define LOG_EMERG      0
#define LOG_ALERT      1
#define LOG_CRIT       2
#define LOG_ERR        3
#define LOG_WARNING    4
#define LOG_NOTICE     5
#define LOG_INFO       6
#define LOG_DEBUG      7

/* facilities */
#define LOG_KERN     ( 0 << 3)
#define LOG_USER     ( 1 << 3)
#define LOG_MAIL     ( 2 << 3)
#define LOG_NEWS     ( 3 << 3)
#define LOG_UUCP     ( 4 << 3)
#define LOG_DAEMON   ( 5 << 3)
#define LOG_AUTH     ( 6 << 3)
#define LOG_CRON     ( 7 << 3)
#define LOG_LPR      ( 8 << 3)
#define LOG_LOCAL0   ( 9 << 3)
#define LOG_LOCAL1   (10 << 3)
#define LOG_LOCAL2   (11 << 3)
#define LOG_LOCAL3   (12 << 3)
#define LOG_LOCAL4   (13 << 3)
#define LOG_LOCAL5   (14 << 3)
#define LOG_LOCAL6   (15 << 3)
#define LOG_LOCAL7   (16 << 3)

#else  /* _WIN32 */

#include <syslog.h>

#endif  /* _WIN32 */

/* define a few things if they are not already defined */

#ifndef LOG_PRIMASK
#define LOG_PRIMASK 7
#endif

#ifndef LOG_FACMASK
#define LOG_FACMASK 0x03f8
#endif

#ifndef LOG_PID
#define LOG_PID      0
#endif
#ifndef LOG_CONS
#define LOG_CONS     0
#endif
#ifndef LOG_ODELAY
#define LOG_ODELAY   0
#endif
#ifndef LOG_NDELAY
#define LOG_NDELAY   0
#endif
#ifndef LOG_NOWAIT
#define LOG_NOWAIT   0
#endif
#ifndef LOG_PERROR
#define LOG_PERROR   0
#endif

#endif /* ZE_HS_SYSLOG_H_B0D81412 */
