#include "ze-hs-log.h"

#include <syslog.h>

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

ob_log_level *ze_hs_log_level_alloc (void)
{
    ob_log_level *lev = (ob_log_level *) calloc (1, sizeof (*lev));

    if (lev) {
        lev->sl_priority = LOG_INFO;
        lev->fd          = 2;
    }

    return lev;
}

static void my_free_rules (ob_log_rule *rule)
{
    while (rule) {
        ob_log_rule *next = rule->next;
        my_free_rules (rule->uniques);
        free (rule);
        rule = next;
    }
}

void ze_hs_log_level_free (ob_log_level *level)
{
    if (level) {
        my_free_rules (level->rules);

        /* FIXME: currently no way to free file rules! */

        free (level);
    }
}

void ze_hs_log_level_set_flags (ob_log_level *level,
                                unt32         flags,
                                unt32         mask)
{
    unt32 x = level->flags;
    x &= ~mask;
    x ^= flags;
    level->flags = x;
}

unt32 ze_hs_log_level_get_flags (ob_log_level *level)
{
    return level->flags;
}

static void my_copy_string (char       *dst,
                            size_t      dst_len,
                            const char *src,
                            size_t      src_len)
{
    size_t i;

    for (i = 0; i < dst_len; i++) {
        if (i < src_len && (i + 1 < dst_len)) {
            dst[i] = src[i];
        } else {
            dst[i] = 0;
        }
    }
}

void ze_hs_log_level_set_prefix (ob_log_level *level,
                                 const char   *pfx,
                                 size_t        pfx_len)
{
    my_copy_string (level->prefix, sizeof (level->prefix), pfx, pfx_len);
}

void ze_hs_log_level_get_prefix (ob_log_level *level,
                                 char         *dst,
                                 size_t        dst_len)
{
    my_copy_string (dst, dst_len, level->prefix, sizeof (level->prefix));
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
