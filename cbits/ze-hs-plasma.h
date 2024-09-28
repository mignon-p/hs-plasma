#ifndef ZE_HS_PLASMA_H_EF95EBA1
#define ZE_HS_PLASMA_H_EF95EBA1

#include "libLoam/c/ob-dirs.h"
#include "libLoam/c/ob-rand.h"
#include "libLoam/c/ob-retorts.h"
#include "libLoam/c/ob-types.h"
#include "libPlasma/c/slaw.h"

slaw ze_hs_plasma_search_standard_path (ob_standard_dir dir,
                                        const char     *filename,
                                        const char     *searchspec,
                                        int64           max_to_return,
                                        ob_retort      *retort_ptr,
                                        int64          *len_ptr);

slaw ze_hs_plasma_spew_overview_to_string (bslaw s, int64 *len_ptr);

OB_HOT unt64 ze_hs_jenkins_hash64 (const void *key,
                                   size_t      length,
                                   unt64       seed);

void ze_hs_rand_free_state (ob_rand_t *rand_state);

#endif /* ZE_HS_PLASMA_H_EF95EBA1 */
