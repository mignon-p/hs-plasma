#ifndef ZE_HS_HOSE_H_57ED2E1F
#define ZE_HS_HOSE_H_57ED2E1F

#include <HsFFI.h>
#include "libPlasma/c/pool.h"

#define ZE_HS_N_MAGIC    4
#define ZE_HS_HOSE_MAGIC 666

typedef struct ze_hs_hose {
    int32       magic[ZE_HS_N_MAGIC];
    pool_hose   hose;
    HsStablePtr ctx;
} ze_hs_hose;

ze_hs_hose *ze_hs_make_hose (pool_hose   hose,
                             HsStablePtr ctx,
                             const char *name,
                             ob_retort  *tort_out);

ob_retort ze_hs_withdraw (ze_hs_hose *zHose);

void ze_hs_finalize_hose (ze_hs_hose *zHose);

HsStablePtr ze_hs_get_context (ze_hs_hose *zHose);

pool_hose ze_hs_hose_clone (ze_hs_hose *orig,
                            ob_retort  *tort_out);

ob_retort ze_hs_deposit (ze_hs_hose     *zHose,
                         bprotein        p,
                         int64          *idx_out,
                         pool_timestamp *ts_out);

protein ze_hs_nth_protein (ze_hs_hose     *zHose,
                           int64           idx,
                           pool_timestamp *ts_out,
                           ob_retort      *tort_out,
                           int64          *len_out);

#endif /* ZE_HS_HOSE_H_57ED2E1F */
