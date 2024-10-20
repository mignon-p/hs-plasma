#ifndef ZE_HS_UTIL_H_2995A248
#define ZE_HS_UTIL_H_2995A248

#include "libLoam/c/ob-types.h"
#include "libPlasma/c/slaw.h"

static inline slaw ze_hs_ret_slaw_len (slaw s, int64 *len)
{
    *len = slaw_len (s);
    return s;
}

#define U_2248_ALMOST_EQUAL_TO           0x2248 /* ≈ */
#define U_2264_LESS_THAN_OR_EQUAL_TO     0x2264 /* ≤ */
#define U_2265_GREATER_THAN_OR_EQUAL_TO  0x2265 /* ≥ */

#endif /* ZE_HS_UTIL_H_2995A248 */
