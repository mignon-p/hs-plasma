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

#endif /* ZE_HS_POOL_H_5BD9401B */
