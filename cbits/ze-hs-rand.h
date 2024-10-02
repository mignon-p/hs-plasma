#ifndef ZE_HS_RAND_H_250EE80D
#define ZE_HS_RAND_H_250EE80D

#include "libLoam/c/ob-rand.h"
#include "libLoam/c/ob-retorts.h"
#include "libLoam/c/ob-types.h"

ob_rand_t *ze_hs_rand_allocate_state (int32 seedval);

ob_retort ze_hs_truly_random (void *dst, size_t len);

void ze_hs_rand_free_state (ob_rand_t *rand_state);

#endif /* ZE_HS_RAND_H_250EE80D */
