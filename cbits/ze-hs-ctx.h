#ifndef ZE_HS_CTX_H_4F9128D7
#define ZE_HS_CTX_H_4F9128D7

#include "libLoam/c/ob-retorts.h"
#include "libLoam/c/ob-types.h"
#include "libPlasma/c/pool.h"
#include "libPlasma/c/slaw.h"

pool_context ze_hs_new_context (bslaw opts, ob_retort *tort_out);

void ze_hs_free_context (pool_context ctx);

slaw ze_hs_ctx_get_options (pool_context ctx, int64 *len_out);

#endif /* ZE_HS_CTX_H_4F9128D7 */
