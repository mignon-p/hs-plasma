#ifndef ZE_HS_CLEANUP_H_DA8FD815
#define ZE_HS_CLEANUP_H_DA8FD815

typedef void (*ze_hs_cleanup_func) (void *v);

typedef struct ze_hs_cleanup {
    void                 *arg;
    ze_hs_cleanup_func    func;
    struct ze_hs_cleanup *next;
} ze_hs_cleanup;

void ze_hs_check_cleanup (void);
void ze_hs_submit_cleanup (ze_hs_cleanup *cu);
void ze_hs_submit_finalizer (ze_hs_cleanup_func f, void *v);

#endif /* ZE_HS_CLEANUP_H_DA8FD815 */
