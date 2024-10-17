#ifndef ZE_HS_HOSE_H_57ED2E1F
#define ZE_HS_HOSE_H_57ED2E1F

#include <stddef.h>
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

/* "op" character:
 *   pool_(n)ext
 *   pool_(a)await_next
 *   pool_(c)urr
 *   pool_(p)rev
 *   pool_probe_(f)rwd
 *   pool_a(w)ait_probe_frwd
 *   pool_probe_(b)ack
 */
protein ze_hs_protein_op (char            op,
                          ze_hs_hose     *zHose,
                          bslaw           search,
                          pool_timestamp  timeout,
                          pool_timestamp *ts_out,
                          int64          *idx_out,
                          ob_retort      *tort_out,
                          int64          *len_out);

/* "op" character:
 *   pool_(n)ewest_index
 *   pool_(o)ldest_index
 *   pool_(i)ndex
 */
int64 ze_hs_get_index (char        op,
                       ze_hs_hose *zHose,
                       ob_retort  *tort_out);

/* "op" character:
 *   pool_re(w)ind
 *   pool_to(l)ast
 *   pool_run(o)ut
 *   pool_(f)rwdby
 *   pool_(b)ackby
 *   pool_(s)eekto
 */
ob_retort ze_hs_seek_op (char        op,
                         ze_hs_hose *zHose,
                         int64       idx);

void ze_hs_fetch (ze_hs_hose     *zHose,
                  bool            clamp,
                  int64          *ops,
                  size_t          nops,
                  int64          *oldest_idx_out,
                  int64          *newest_idx_out);

#endif /* ZE_HS_HOSE_H_57ED2E1F */
