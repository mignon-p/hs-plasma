#include <stdlib.h>

#include "ze-hs-cleanup.h"
#include "ze-hs-gang.h"
#include "ze-hs-hose.h"
#include "ze-hs-retorts.h"
#include "ze-hs-util.h"

static pool_gang get_gang (ze_hs_gang *zGang,
                           ob_retort  *tort_out)
{
    ze_hs_check_cleanup();

    if (zGang->magic[0] != ZE_HS_GANG_MAGIC) {
        *tort_out = ZE_HS_INTERNAL_ERROR;
        return NULL;
    }

    /* Allocate pool_gang on demand */
    if (zGang->gang == NULL) {
        *tort_out = pool_new_gang (&zGang->gang);
    } else {
        *tort_out = OB_OK;
    }

    return zGang->gang;
}

ze_hs_gang *ze_hs_new_gang (HsStablePtr hoses,
                            ob_retort  *tort_out)
{
    ze_hs_check_cleanup();

    ze_hs_gang *zGang = (ze_hs_gang *) calloc (1, sizeof (*zGang));
    if (zGang == NULL) {
        hs_free_stable_ptr (hoses);
        *tort_out = OB_NO_MEM;
        return NULL;
    }

    size_t i;
    for (i = 0; i < ZE_HS_N_MAGIC; i++) {
        zGang->magic[i] = ZE_HS_GANG_MAGIC;
    }

    zGang->gang  = NULL;
    zGang->hoses = hoses;

    *tort_out = OB_OK;
    return zGang;
}

static void gang_finalizer (void *v)
{
    ze_hs_gang *zGang = (ze_hs_gang *) v;

    /* disband gang */
    if (zGang->gang) {
        pool_disband_gang (zGang->gang, false);
        hs_free_stable_ptr (zGang->hoses);
        zGang->gang  = NULL;
        zGang->hoses = NULL;
    }

    free (zGang);
}

void ze_hs_finalize_gang (ze_hs_gang *zGang)
{
    ze_hs_submit_finalizer (gang_finalizer, zGang);
}

HsStablePtr ze_hs_get_gang_hoses (ze_hs_gang *zGang)
{
    return zGang->hoses;
}

ob_retort ze_hs_gang_op (char        op,
                         ze_hs_gang *zGang,
                         ze_hs_hose *zHose)
{
    ob_retort tort = OB_UNKNOWN_ERR;
    pool_gang g    = get_gang (zGang, &tort);

    if (!g) {
        return tort;
    }

    pool_hose h = ze_hs_get_hose (zHose, &tort);

    if (!h) {
        return tort;
    }

    switch (op) {
    case 'j':
        return pool_join_gang (g, h);
    case 'l':
        return pool_leave_gang (g, h);
    default:
        return ZE_HS_INTERNAL_ERROR;
    }
}

protein ze_hs_gang_next_op (char            op,
                            ze_hs_gang     *zGang,
                            pool_timestamp  timeout,
                            pool_hose      *hose_out,
                            pool_timestamp *ts_out,
                            int64          *index_out,
                            ob_retort      *tort_out,
                            int64          *len_out)
{
    pool_gang g = get_gang (zGang, tort_out);

    if (!g) {
        return NULL;
    }

    protein   p    = NULL;
    ob_retort tort = ZE_HS_INTERNAL_ERROR;

    switch (op) {
    case 'n':
        tort = pool_next_multi (g, hose_out, &p, ts_out, index_out);
        break;
    case 'a':
        tort = pool_await_next_multi (g, timeout,
                                      hose_out, &p, ts_out, index_out);
        break;
    }

    *tort_out = tort;
    if (tort < OB_OK) {
        return NULL;
    }

    return ze_hs_ret_slaw_len (p, len_out);
}

ob_retort ze_hs_gang_misc_op (char            op,
                              ze_hs_gang     *zGang,
                              pool_timestamp  timeout)
{
    if (op == 'd' && zGang->gang == NULL) {
        /* Don't bother allocating gang if we're just going
         * to deallocate it immediately. */
        return OB_OK;
    }

    ob_retort tort = OB_UNKNOWN_ERR;
    pool_gang g    = get_gang (zGang, &tort);

    if (!g) {
        return tort;
    }

    switch (op) {
    case 'a':
        return pool_await_multi (g, timeout);
    case 'w':
        return pool_gang_wake_up (g);
    case 'd':
        tort = pool_disband_gang (g, false);
        zGang->gang = NULL;
       return tort;
    default:
        return ZE_HS_INTERNAL_ERROR;
    }
}
