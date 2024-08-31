#include <stdarg.h>
#include "ze-hs-plasma.h"

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

slaw ze_hs_plasma_search_standard_path (ob_standard_dir dir,
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
