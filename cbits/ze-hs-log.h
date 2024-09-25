#ifndef ZE_HS_LOG_H_7E3A0D14
#define ZE_HS_LOG_H_7E3A0D14

#include "libLoam/c/ob-log.h"
#include "libLoam/c/ob-types.h"

#include <stddef.h>

/* Bug, Error, Deprecation, Warn, Info, debuG */
ob_log_level *ze_hs_log_level (char c);

ob_log_level *ze_hs_log_level_alloc (void);

void ze_hs_log_level_free (ob_log_level *level);

void ze_hs_log_level_set_flags (ob_log_level *level,
                                unt32         flags,
                                unt32         mask);

unt32 ze_hs_log_level_get_flags (ob_log_level *level);

void ze_hs_log_level_set_prefix (ob_log_level *level,
                                 const char   *pfx,
                                 size_t        pfx_len);

void ze_hs_log_level_get_prefix (ob_log_level *level,
                                 char         *dst,
                                 size_t        dst_len);

/* None, stdOut, stdErr, File, Append */
ob_retort ze_hs_log_level_set_dest (ob_log_level *level,
                                    char          op,
                                    const char   *name);

void ze_hs_log_level_set_sl_priority (ob_log_level *level,
                                      int32         pri);

int32 ze_hs_log_level_get_sl_priority (ob_log_level *level);

void ze_hs_log_level_set_sl_facility (ob_log_level *level,
                                      int32         fac);

int32 ze_hs_log_level_get_sl_facility (ob_log_level *level);

const char *ze_hs_facility_name (size_t idx, int32 *fac_out);

int32 ze_hs_default_facility (void);

void ze_hs_log_loc (const char   *file,
                    int64         lineno,
                    ob_log_level *level,
                    unt64         code,
                    char         *msg,
                    const char   *thread,
                    const char   *backtrace);

#endif /* ZE_HS_LOG_H_7E3A0D14 */
