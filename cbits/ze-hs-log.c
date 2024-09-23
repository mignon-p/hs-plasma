#include "ze-hs-log.h"

/* Bug, Error, Deprecation, Warn, Info, debuG */
ob_log_level *ze_hs_log_level (char c)
{
    switch (c) {
    case 'B': return OBLV_BUG;
    case 'E': return OBLV_ERROR;
    case 'D': return OBLV_DEPRECATION;
    case 'W': return OBLV_WARN;
    case 'I': return OBLV_INFO;
    case 'G': return OBLV_DEBUG;
    default:  return NULL;
    }
}

void ze_hs_log_loc (const char   *file,
                    int64         lineno,
                    ob_log_level *level,
                    unt64         code,
                    char         *msg,
                    const char   *thread,
                    const char   *backtrace)
{
    ob_log_loc (file, lineno, level, code, "%s", msg);
}
