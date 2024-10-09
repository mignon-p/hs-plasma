#include "ze-hs-cleanup.h"
#include "ze-hs-pool.h"
#include "libPlasma/c/protein.h"
#include "libPlasma/c/slaw.h"
#include "libPlasma/c/slaw-path.h"

static const char kType[] = "type";
static const char kMmap[] = "mmap";

pool_hose ze_hs_participate (bool         creatingly,
                             pool_context ctx,
                             const char  *pool_name,
                             bslaw        create_opts,
                             ob_retort   *tort_out)
{
    pool_hose h    = NULL;
    ob_retort tort = OB_OK;

    ze_hs_check_cleanup();

    if (creatingly) {
        bslaw opts = create_opts;
        while (slaw_is_protein (opts)) {
            opts = protein_ingests (opts);
        }

        const char *pool_type = slaw_path_get_string (opts,
                                                      kType,
                                                      kMmap);

        /* Remove the "type" key, because otherwise it
         * causes a warning. */
        slaw freeme = NULL;
        if (slaw_is_list_or_map (opts)) {
            slabu *sb = slabu_from_slaw (opts);
            if (sb) {
                slabu_map_remove_c (sb, kType);
                freeme = slaw_map_f (sb);
                if (freeme) {
                    opts = freeme;
                }
            }
        }

        tort = pool_participate_creatingly_ctx (pool_name,
                                                pool_type,
                                                &h,
                                                opts,
                                                ctx);
        slaw_free (freeme);
    } else {
        tort = pool_participate_ctx (pool_name, &h, ctx);
    }

    *tort_out = tort;
    return h;
}
