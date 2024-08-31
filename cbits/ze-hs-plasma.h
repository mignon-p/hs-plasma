#ifndef ZE_HS_PLASMA_H_EF95EBA1
#define ZE_HS_PLASMA_H_EF95EBA1

#include "libLoam/c/ob-dirs.h"
#include "libLoam/c/ob-retorts.h"
#include "libLoam/c/ob-types.h"
#include "libPlasma/c/slaw.h"

slaw ze_hs_plasma_search_standard_path (ob_standard_dir dir,
                                        const char     *filename,
                                        const char     *searchspec,
                                        int64           max_to_return,
                                        ob_retort      *retort_ptr,
                                        int64          *len_ptr);

#endif /* ZE_HS_PLASMA_H_EF95EBA1 */
