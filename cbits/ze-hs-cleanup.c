#include <stdlib.h>

#include "libLoam/c/ob-atomic.h"
#include "libLoam/c/ob-util.h"

#include "ze-hs-cleanup.h"
#include "ze-hs-retorts.h"

static ze_hs_cleanup *cleanup_head;

void ze_hs_check_cleanup (void)
{
    ze_hs_cleanup *cu;

    do {
        cu = ob_atomic_pointer_ref (&cleanup_head);
    } while (! ob_atomic_pointer_compare_and_swap (&cleanup_head,
                                                   cu, NULL));

    while (cu) {
        ze_hs_cleanup *next = cu->next;
        cu->func ((void *) cu);
        free (cu);
        cu = next;
    }
}

void ze_hs_submit_cleanup (ze_hs_cleanup *cu)
{
    if (cu->arg) {
        ze_hs_cleanup *head;

        do {
            head = ob_atomic_pointer_ref (&cleanup_head);
            ob_atomic_pointer_set (&cu->next, head);
        } while (! ob_atomic_pointer_compare_and_swap (&cleanup_head,
                                                       head, cu));
    } else {
        free (cu);
    }
}

void ze_hs_submit_finalizer (ze_hs_cleanup_func f, void *v)
{
    ze_hs_cleanup *cu = (ze_hs_cleanup *) calloc (1, sizeof (*cu));

    /* If we're out of memory, we'll leak a finalizer */
    if (cu) {
        cu->arg  = v;
        cu->func = f;

        ze_hs_submit_cleanup (cu);
    }
}

/* We need to arrange for libLoam to be told about our error codes... */
OB_PRE_POST (ob_add_error_names (ze_hs_ioe_error_string), ob_nop ());
