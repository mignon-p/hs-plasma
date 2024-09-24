#include "ze-hs-log.h"
#include "ze-hs-retorts.h"

#include "libLoam/c/ob-file.h"
#include "libLoam/c/ob-sys.h"

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

static int set_output_fd (ob_log_level *lev, int fd)
{
  int r;
  if (lev->fd <= 2)
    r = lev->fd = ob_dup_cloexec (fd);
  else
    r = ob_dup2_cloexec (fd, lev->fd);
  return r;
}

#ifdef _WIN32
static const char devNull[] = "NUL";
#else   /* _WIN32 */
static const char devNull[] = "/dev/null";
#endif  /* _WIN32 */

ob_retort ze_hs_log_level_set_dest (ob_log_level *lev,
                                    char          op,
                                    const char   *name)
{
    int oflag = 0;
    int fd    = -1;
    int r     = 0;

    switch (op) {
    case 'O': fd = 1;                                break;
    case 'E': fd = 2;                                break;
    case 'F': oflag = O_WRONLY            | O_CREAT; break;
    case 'A': oflag = O_WRONLY | O_APPEND | O_CREAT; break;
    case 'N':
        lev->flags &= ~OB_DST_FD;
        oflag = O_WRONLY | O_APPEND;
        name  = devNull;
        break;
    default:
        return ZE_HS_INTERNAL_ERROR;
    }

    if (fd > -1) {
        r = set_output_fd (lev, fd);
    } else if (oflag != 0) {
        fd = ob_open_cloexec (name, oflag, 0666);
        if (fd < 0) {
            r = fd;
            goto done;
        }

        r = set_output_fd (lev, fd);
        if (r < 0) {
            const int save = errno;
            close (fd);
            errno = save;
            goto done;
        }

        r = close (fd);
    }

 done:
    if (r < 0) {
        return ob_errno_to_retort (errno);
    }

    if (op != 'N') {
        lev->flags |= OB_DST_FD;
    }

    return OB_OK;
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
