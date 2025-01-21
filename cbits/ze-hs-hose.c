#include <stdlib.h>

#include "ze-hs-cleanup.h"
#include "ze-hs-fetch.h"
#include "ze-hs-hose.h"
#include "ze-hs-retorts.h"
#include "ze-hs-util.h"

static pool_hose get_hose (ze_hs_hose *zHose, ob_retort *tort_out)
{
    ze_hs_check_cleanup();
    return ze_hs_get_hose (zHose, tort_out);
}

pool_hose ze_hs_get_hose (ze_hs_hose *zHose, ob_retort *tort_out)
{
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

int64 ze_hs_get_index (char        op,
                       ze_hs_hose *zHose,
                       ob_retort  *tort_out)
{
    pool_hose h = get_hose (zHose, tort_out);

    if (!h) {
        return -1;
    }

    int64 idx      = -1;
    ob_retort tort = ZE_HS_INTERNAL_ERROR;

    switch (op) {
    case 'n':
        tort = pool_newest_index (h, &idx);
        break;
    case 'o':
        tort = pool_oldest_index (h, &idx);
        break;
    case 'i':
        tort = pool_index (h, &idx);
        break;
    }

    *tort_out = tort;
    return idx;
}

ob_retort ze_hs_seek_op (char        op,
                         ze_hs_hose *zHose,
                         int64       idx)
{
    ob_retort tort = ZE_HS_INTERNAL_ERROR;
    pool_hose h    = get_hose (zHose, &tort);

    if (!h) {
        return tort;
    }

    switch (op) {
    case 'r':
        return pool_rewind (h);
    case 'l':
        return pool_tolast (h);
    case 'o':
        return pool_runout (h);
    case 'f':
        return pool_frwdby (h, idx);
    case 'b':
        return pool_backby (h, idx);
    case 's':
        return pool_seekto (h, idx);
    case 'a':
        return pool_advance_oldest (h, idx);
    }

    return ZE_HS_INTERNAL_ERROR;
}

void ze_hs_fetch (ze_hs_hose     *zHose,
                  bool            clamp,
                  int64          *ops,
                  size_t          nops,
                  int64          *oldest_idx_out,
                  int64          *newest_idx_out)
{
    ob_retort tort = ZE_HS_INTERNAL_ERROR;
    pool_hose h    = get_hose (zHose, &tort);
    size_t    i;

    if (!h) {
    splatter_retort:
        *oldest_idx_out = tort;
        *newest_idx_out = tort;

        for (i = 0; i < nops; i++) {
            const size_t j = i * ZE_HS_FETCH_MAX;
            ops[j + ZE_HS_FETCH_TORT]  = tort;
            ops[j + ZE_HS_FETCH_P]     = 0;
            ops[j + ZE_HS_FETCH_P_LEN] = -1;
        }

        return;
    }

    pool_fetch_op *fops = (pool_fetch_op *)
        calloc (nops * ZE_HS_FETCH_MAX, sizeof (int64));

    if (!fops) {
        tort = OB_NO_MEM;
        goto splatter_retort;
    }

    for (i = 0; i < nops; i++) {
        const size_t j = i * ZE_HS_FETCH_MAX;
        ze_hs_encode_fetch_op (fops + i, ops + j);
    }

    pool_fetch_ex (h, fops, nops, oldest_idx_out, newest_idx_out, clamp);

    for (i = 0; i < nops; i++) {
        const size_t j = i * ZE_HS_FETCH_MAX;
        ze_hs_decode_fetch_op (ops + j, fops + i);
    }

    free (fops);
}

protein ze_hs_get_info (ze_hs_hose *zHose,
                        int64       hops,
                        ob_retort  *tort_out,
                        int64      *len_out)
{
    pool_hose h = get_hose (zHose, tort_out);

    if (!h) {
        return NULL;
    }

    protein   p    = NULL;
    ob_retort tort = pool_get_info (h, hops, &p);

    *tort_out = tort;
    if (tort < OB_OK) {
        return NULL;
    }

    return ze_hs_ret_slaw_len (p, len_out);
}

ob_retort ze_hs_seek_time_op (char        op,
                              ze_hs_hose *zHose,
                              float64     timestamp,
                              HsChar      timeCmp)
{
    time_comparison tc;
    ob_retort       tort = OB_UNKNOWN_ERR;
    pool_hose       h    = get_hose (zHose, &tort);

    if (!h) {
        return tort;
    }

    switch (timeCmp) {
    case U_2248_ALMOST_EQUAL_TO:          /* ≈ */
        tc = OB_CLOSEST;
        break;
    case U_2264_LESS_THAN_OR_EQUAL_TO:    /* ≤ */
        tc = OB_CLOSEST_LOWER;
        break;
    case U_2265_GREATER_THAN_OR_EQUAL_TO: /* ≥ */
        tc = OB_CLOSEST_HIGHER;
        break;
    default:
        return ZE_HS_INTERNAL_ERROR;
    }

    switch (op) {
    case 't':
        return pool_seekto_time (h, timestamp, tc);
    case 'b':
        return pool_seekby_time (h, timestamp, tc);
    }

    return ZE_HS_INTERNAL_ERROR;
}

ob_retort ze_hs_change_options (ze_hs_hose *zHose,
                                bslaw       opts)
{
    ob_retort       tort = OB_UNKNOWN_ERR;
    pool_hose       h    = get_hose (zHose, &tort);

    if (!h) {
        return tort;
    }

    return pool_change_options (h, opts);
}

ob_retort ze_hs_wakeup_op (char        op,
                           ze_hs_hose *zHose)
{
    ob_retort       tort = OB_UNKNOWN_ERR;
    pool_hose       h    = get_hose (zHose, &tort);

    if (!h) {
        return tort;
    }

    switch (op) {
    case 'e':
        return pool_hose_enable_wakeup (h);
    case 'w':
        return pool_hose_wake_up (h);
    }

    return ZE_HS_INTERNAL_ERROR;
}
