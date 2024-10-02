#include "ze-hs-cleanup.h"
#include "ze-hs-rand.h"

ob_rand_t *ze_hs_rand_allocate_state (int32 seedval)
{
    ze_hs_check_cleanup ();
    return ob_rand_allocate_state (seedval);
}

ob_retort ze_hs_truly_random (void *dst, size_t len)
{
    ze_hs_check_cleanup ();
    return ob_truly_random (dst, len);
}

void ze_hs_rand_free_state (ob_rand_t *rand_state)
{
    /* Queue the finalizer to run later, because ob_rand_free_state()
     * might call ob_log().  (At least on Windows, anyway.) */
    ze_hs_submit_finalizer ((ze_hs_cleanup_func) ob_rand_free_state,
                            rand_state);
}
