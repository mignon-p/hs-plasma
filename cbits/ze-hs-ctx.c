#include "ze-hs-cleanup.h"
#include "ze-hs-ctx.h"

pool_context ze_hs_new_context (bslaw opts, ob_retort *tort_out)
{
    pool_context ctx = NULL;

    ze_hs_check_cleanup ();

    ob_retort tort = pool_new_context (&ctx);
    if (tort < OB_OK) {
        *tort_out = tort;
        return NULL;
    }

    tort = pool_ctx_set_options (ctx, opts);
    if (tort < OB_OK) {
        pool_free_context (ctx);
        *tort_out = tort;
        return NULL;
    }

    *tort_out = tort;
    return ctx;
}

void ze_hs_free_context (pool_context ctx)
{
    ze_hs_submit_finalizer ((ze_hs_cleanup_func) pool_free_context, ctx);
}

static inline slaw ret_slaw_len (slaw s, int64 *len)
{
    *len = slaw_len (s);
    return s;
}

slaw ze_hs_ctx_get_options (pool_context ctx, int64 *len_out)
{
    slaw ret = NULL;

    bslaw opts = pool_ctx_get_options (ctx);
    if (opts) {
        ret = slaw_dup (opts);
    } else {
        ret = slaw_map_empty ();
    }

    return ret_slaw_len (ret, len_out);
}
