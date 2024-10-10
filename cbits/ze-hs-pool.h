#ifndef ZE_HS_POOL_H_5BD9401B
#define ZE_HS_POOL_H_5BD9401B

#include <stdbool.h>
#include "libPlasma/c/pool.h"

pool_hose ze_hs_participate (bool         creatingly,
                             pool_context ctx,
                             const char  *pool_name,
                             bslaw        create_opts,
                             ob_retort   *tort_out);

ob_retort ze_hs_create (pool_context ctx,
                        const char  *pool_name,
                        bslaw        create_opts);

slaw ze_hs_list (pool_context ctx,
                 const char  *uri,
                 ob_retort   *tort_out,
                 int64       *len_out);

#endif /* ZE_HS_POOL_H_5BD9401B */
