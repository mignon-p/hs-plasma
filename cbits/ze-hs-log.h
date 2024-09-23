#ifndef ZE_HS_LOG_H_7E3A0D14
#define ZE_HS_LOG_H_7E3A0D14

#include "libLoam/c/ob-log.h"
#include "libLoam/c/ob-types.h"

/* Bug, Error, Deprecation, Warn, Info, debuG */
ob_log_level *ze_hs_log_level (char c);

void ze_hs_log_loc (const char   *file,
                    int64         lineno,
                    ob_log_level *level,
                    unt64         code,
                    char         *msg,
                    const char   *thread,
                    const char   *backtrace);

#endif /* ZE_HS_LOG_H_7E3A0D14 */
