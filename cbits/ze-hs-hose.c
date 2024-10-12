#include <stdlib.h>

#include "ze-hs-cleanup.h"
#include "ze-hs-hose.h"
#include "ze-hs-retorts.h"
#include "ze-hs-util.h"

static pool_hose get_hose (ze_hs_hose *zHose, ob_retort *tort_out)
{
    ze_hs_check_cleanup();

    if (zHose->magic[0] != ZE_HS_HOSE_MAGIC) {
        *tort_out = ZE_HS_INTERNAL_ERROR;
        return NULL;
    }

    pool_hose h = zHose->hose;
    *tort_out = (h ? OB_OK : ZE_HS_ALREADY_CLOSED);
    return h;
}

ze_hs_hose *ze_hs_make_hose (pool_hose   hose,
                             HsStablePtr ctx,
                             const char *name,
                             ob_retort  *tort_out)
{
    ob_retort tort = OB_OK;
    size_t    i;

    ze_hs_check_cleanup();

    ze_hs_hose *zHose = (ze_hs_hose *) calloc (1, sizeof (*zHose));
    if (zHose == NULL) {
        tort = OB_NO_MEM;
        goto fail;
    }

    for (i = 0; i < ZE_HS_N_MAGIC; i++) {
        zHose->magic[i] = ZE_HS_HOSE_MAGIC;
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

ob_retort ze_hs_deposit (ze_hs_hose     *zHose,
                         bprotein        p,
                         int64          *idx_out,
                         pool_timestamp *ts_out)
{
    ob_retort tort = OB_OK;
    pool_hose h    = get_hose (zHose, &tort);

    if (!h) {
        return tort;
    }

    return pool_deposit_ex (h, p, idx_out, ts_out);
}

protein ze_hs_nth_protein (ze_hs_hose     *zHose,
                           int64           idx,
                           pool_timestamp *ts_out,
                           ob_retort      *tort_out,
                           int64          *len_out)
{
    pool_hose h = get_hose (zHose, tort_out);

    if (!h) {
        return NULL;
    }

    protein p = NULL;
    ob_retort tort = pool_nth_protein (h, idx, &p, ts_out);
    *tort_out = tort;
    if (tort < OB_OK) {
        return NULL;
    }

    return ze_hs_ret_slaw_len (p, len_out);
}

protein ze_hs_protein_op (char            op,
                          ze_hs_hose     *zHose,
                          bslaw           search,
                          pool_timestamp  timeout,
                          pool_timestamp *ts_out,
                          int64          *idx_out,
                          ob_retort      *tort_out,
                          int64          *len_out)
{
    pool_hose h = get_hose (zHose, tort_out);

    if (!h) {
        return NULL;
    }

    protein   p    = NULL;
    ob_retort tort = ZE_HS_INTERNAL_ERROR;

    switch (op) {
    case 'n':
        tort = pool_next (h, &p, ts_out, idx_out);
        break;
    case 'a':
        tort = pool_await_next (h, timeout, &p, ts_out, idx_out);
        break;
    case 'c':
        tort = pool_index (h, idx_out);
        if (tort >= OB_OK) {
            tort = pool_curr (h, &p, ts_out);
        }
        break;
    case 'p':
        tort = pool_prev (h, &p, ts_out, idx_out);
        break;
    case 'f':
        tort = pool_probe_frwd (h, search, &p, ts_out, idx_out);
        break;
    case 'w':
        tort = pool_await_probe_frwd (h, search, timeout,
                                      &p, ts_out, idx_out);
        break;
    case 'b':
        tort = pool_probe_back (h, search, &p, ts_out, idx_out);
        break;
    }

    *tort_out = tort;
    if (tort < OB_OK) {
        return NULL;
    }

    return ze_hs_ret_slaw_len (p, len_out);
}
