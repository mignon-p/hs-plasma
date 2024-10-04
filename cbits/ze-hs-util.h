#ifndef ZE_HS_UTIL_H_2995A248
#define ZE_HS_UTIL_H_2995A248

#include "libLoam/c/ob-types.h"
#include "libPlasma/c/slaw.h"

static inline slaw ze_hs_ret_slaw_len (slaw s, int64 *len)
{
    *len = slaw_len (s);
    return s;
}

#endif /* ZE_HS_UTIL_H_2995A248 */
