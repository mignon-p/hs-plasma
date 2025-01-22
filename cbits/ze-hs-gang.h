#ifndef ZE_HS_GANG_H_D162907C
#define ZE_HS_GANG_H_D162907C

#include <stddef.h>
#include <HsFFI.h>
#include "libPlasma/c/pool.h"
#include "ze-hs-hose.h"
#include "ze-hs-util.h"

typedef struct ze_hs_gang {
    int32       magic[ZE_HS_N_MAGIC];
    pool_gang   gang;
    HsStablePtr hoses;
} ze_hs_gang;

/* Create a new pool_gang and wrap it in a ze_hs_gang. */
ze_hs_gang *ze_hs_new_gang (HsStablePtr hoses,
                            ob_retort  *tort_out);

void ze_hs_finalize_gang (ze_hs_gang *zGang);

HsStablePtr ze_hs_get_gang_hoses (ze_hs_gang *zGang);

/* "op" character:
 *   pool_(j)oin_gang
 *   pool_(l)eave_gang
 */
ob_retort ze_hs_gang_op (char        op,
                         ze_hs_gang *zGang,
                         ze_hs_hose *hose);

/* "op" character:
 *   pool_(n)ext_multi
 *   pool_(a)wait_next_multi
 */
protein ze_hs_gang_next_op (char            op,
                            ze_hs_gang     *zGang,
                            pool_timestamp  timeout,
                            pool_hose      *hose_out,
                            pool_timestamp *ts_out,
                            int64          *index_out,
                            ob_retort      *tort_out,
                            int64          *len_out);

/* "op" character:
 *   pool_(a)wait_multi
 *   pool_gang_(w)ake_up
 *   pool_(d)isband_gang
 */
ob_retort ze_hs_gang_misc_op (char            op,
                              ze_hs_gang     *zGang,
                              pool_timestamp  timeout);

#endif /* ZE_HS_GANG_H_D162907C */
