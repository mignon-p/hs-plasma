#include <stdarg.h>
#include "ze-hs-cleanup.h"
#include "ze-hs-misc.h"
#include "libLoam/c/ob-hash.h"

static inline slaw ret_slaw_len (slaw s, int64 *len)
{
    *len = slaw_len (s);
    return s;
}

static ob_retort my_callback (const char *file, va_list vargies)
{
    slabu *sb            = va_arg (vargies, slabu *);
    int64  max_to_return = va_arg (vargies, int64);
    slaw   s             = slaw_string (file);

    if (s == NULL) {
        return OB_NO_MEM;
    }

    if (slabu_list_add_x (sb, s) < 0) {
        slaw_free (s);
        return OB_NO_MEM;
    }

    if (slabu_count (sb) >= max_to_return) {
        return OB_STOP;
    } else {
        return OB_OK;
    }
}

slaw ze_hs_search_standard_path (ob_standard_dir dir,
                                 const char     *filename,
                                 const char     *searchspec,
                                 int64           max_to_return,
                                 ob_retort      *retort_ptr,
                                 int64          *len_ptr)
{
    ob_retort tort = OB_UNKNOWN_ERR;
    slabu    *sb   = slabu_new ();
    slaw      ret  = NULL;

    if (sb == NULL) {
        tort = OB_NO_MEM;
        goto done;
    }

    tort = ob_search_standard_path (dir, filename, searchspec,
                                    my_callback, sb, max_to_return);
    if (tort < 0) {
        goto done;
    } else if (tort == OB_STOP) {
        tort = OB_OK;
    }

    ret = slaw_list (sb);
    if (ret == NULL) {
        tort = OB_NO_MEM;
    }

 done:
    slabu_free (sb);
    *retort_ptr = tort;
    return ret_slaw_len (ret, len_ptr);
}

slaw ze_hs_spew_overview_to_string (bslaw s, int64 *len_ptr)
{
    slaw ret = slaw_spew_overview_to_string (s);
    return ret_slaw_len (ret, len_ptr);
}

unt64 ze_hs_jenkins_hash64 (const void *key,
                            size_t      length,
                            unt64       seed)
{
    unt32 io1, io2;
    unt64 ret;

    io1 = (unt32)(seed >> 32);
    io2 = (unt32) seed;

    ob_jenkins_hash2 (key, length, &io1, &io2);

    ret = io1;
    ret <<= 32;
    ret |= io2;

    return ret;
}

void ze_hs_rand_free_state (ob_rand_t *rand_state)
{
    /* Queue the finalizer to run later, because ob_rand_free_state()
     * might call ob_log().  (At least on Windows, anyway.) */
    ze_hs_submit_finalizer ((ze_hs_cleanup_func) ob_rand_free_state,
                            rand_state);
}
