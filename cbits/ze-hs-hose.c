#include <stdlib.h>

#include "ze-hs-cleanup.h"
#include "ze-hs-hose.h"
#include "ze-hs-util.h"

ze_hs_hose *ze_hs_make_hose (pool_hose   hose,
                             HsStablePtr ctx,
                             const char *name,
                             ob_retort  *tort_out)
{
    ob_retort tort = OB_OK;

    ze_hs_check_cleanup();

    ze_hs_hose *zHose = (ze_hs_hose *) calloc (1, sizeof (*zHose));
    if (zHose == NULL) {
        tort = OB_NO_MEM;
        goto fail;
    }

    zHose->hose = hose;
    zHose->ctx  = ctx;

    tort = pool_set_hose_name (hose, name);
    if (tort < OB_OK) {
        free (zHose);
        zHose = NULL;
    fail:
        pool_withdraw (hose);
        hs_free_stable_ptr (ctx);
    }

    *tort_out = tort;
    return zHose;
}

ob_retort ze_hs_withdraw (ze_hs_hose *zHose)
{
    ob_retort tort = OB_OK;

    if (zHose->hose) {
        tort = pool_withdraw (zHose->hose);
        hs_free_stable_ptr (zHose->ctx);
        zHose->hose = NULL;
        zHose->ctx  = NULL;
    }

    return tort;
}

static void hose_finalizer (void *v)
{
    ze_hs_hose *zHose = (ze_hs_hose *) v;
    ze_hs_withdraw (zHose);
    free (zHose);
}

void ze_hs_finalize_hose (ze_hs_hose *zHose)
{
    ze_hs_submit_finalizer (hose_finalizer, zHose);
}

HsStablePtr ze_hs_get_context (ze_hs_hose *zHose)
{
    return zHose->ctx;
}

pool_hose ze_hs_hose_clone (ze_hs_hose *orig,
                            ob_retort  *tort_out)
{
    pool_hose new_hose  = NULL;

    *tort_out = pool_hose_clone (orig->hose, &new_hose);
    return new_hose;
}
