#include <stdlib.h>

#include "ze-hs-cleanup.h"
#include "ze-hs-hose.h"
#include "ze-hs-util.h"

ze_hs_hose *ze_hs_make_hose (pool_hose   hose,
                             HsStablePtr ctx,
                             ob_retort  *tort_out)
{
    ze_hs_check_cleanup();

    ze_hs_hose *zHose = (ze_hs_hose *) calloc (1, sizeof (*zHose));
    if (zHose == NULL) {
        *tort_out = OB_NO_MEM;
        return NULL;
    }

    zHose->hose = hose;
    zHose->ctx  = ctx;

    *tort_out = OB_OK;
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
