#ifndef ZE_HS_HOSE_H_57ED2E1F
#define ZE_HS_HOSE_H_57ED2E1F

#include <HsFFI.h>
#include "libPlasma/c/pool.h"

typedef struct ze_hs_hose {
    pool_hose   hose;
    HsStablePtr ctx;
} ze_hs_hose;

ze_hs_hose *ze_hs_make_hose (pool_hose   hose,
                             HsStablePtr ctx,
                             const char *name,
                             ob_retort  *tort_out);

ob_retort ze_hs_withdraw (ze_hs_hose *zHose);

void ze_hs_finalize_hose (ze_hs_hose *zHose);

#endif /* ZE_HS_HOSE_H_57ED2E1F */
