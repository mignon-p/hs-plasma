#ifndef ZE_HS_FETCH_H_CF378F93
#define ZE_HS_FETCH_H_CF378F93

#include "libPlasma/c/pool.h"

typedef enum ze_hs_fetch_op_idx {
    ZE_HS_FETCH_IDX,
    ZE_HS_FETCH_WANT_DESCRIPS,
    ZE_HS_FETCH_WANT_INGESTS,
    ZE_HS_FETCH_RUDE_OFFSET,
    ZE_HS_FETCH_RUDE_LENGTH,
    ZE_HS_FETCH_TORT,
    ZE_HS_FETCH_TS,
    ZE_HS_FETCH_TOTAL_BYTES,
    ZE_HS_FETCH_DESCRIP_BYTES,
    ZE_HS_FETCH_INGEST_BYTES,
    ZE_HS_FETCH_RUDE_BYTES,
    ZE_HS_FETCH_NUM_DESCRIPS,
    ZE_HS_FETCH_NUM_INGESTS,
    ZE_HS_FETCH_P,
    ZE_HS_FETCH_P_LEN,
    ZE_HS_FETCH_MAX,            /* must be last */
} ze_hs_fetch_op_idx;

void ze_hs_encode_fetch_op (pool_fetch_op *dst,
                            const int64   *src);

void ze_hs_decode_fetch_op (int64               *dst,
                            const pool_fetch_op *src);

#endif /* ZE_HS_FETCH_H_CF378F93 */
