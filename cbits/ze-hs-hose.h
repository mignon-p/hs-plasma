#ifndef ZE_HS_HOSE_H_57ED2E1F
#define ZE_HS_HOSE_H_57ED2E1F

#include <stddef.h>
#include <HsFFI.h>
#include "libPlasma/c/pool.h"
#include "ze-hs-util.h"

typedef struct ze_hs_hose {
    int32       magic[ZE_HS_N_MAGIC];
    pool_hose   hose;
    HsStablePtr ctx;
} ze_hs_hose;

/* Unwrap the pool_hose inside of a ze_hs_hose. */
pool_hose ze_hs_get_hose (ze_hs_hose *zHose, ob_retort *tort_out);

/* Wrap a pool_hose in a ze_hs_hose. */
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
 *   pool_(a)dvance_oldest
 *
 * Note: although pool_advance_oldest() isn't a "seek" operation,
 * it takes the same arguments, so we're shoving it in there.
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

protein ze_hs_get_info (ze_hs_hose *zHose,
                        int64       hops,
                        ob_retort  *tort_out,
                        int64      *len_out);

/* "op" character:
 *   pool_seek(t)o_time
 *   pool_seek(b)y_time
 *
 * "timeCmp" character:
 *   '≈' (U+2248) OB_CLOSEST
 *   '≤' (U+2264) OB_CLOSEST_LOWER
 *   '≥' (U+2265) OB_CLOSEST_HIGHER
 */
ob_retort ze_hs_seek_time_op (char        op,
                              ze_hs_hose *zHose,
                              float64     timestamp,
                              HsChar      timeCmp);

ob_retort ze_hs_change_options (ze_hs_hose *zHose,
                                bslaw       opts);

/* "op" character:
 *   pool_hose_(e)nable_wakeup
 *   pool_hose_(w)ake_up
 */
ob_retort ze_hs_wakeup_op (char        op,
                           ze_hs_hose *zHose);

#endif /* ZE_HS_HOSE_H_57ED2E1F */
