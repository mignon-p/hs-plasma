#include <stdint.h>
#include "ze-hs-fetch.h"
#include "libPlasma/c/slaw.h"

typedef union ze_hs_cast64 {
    int64   i64;
    float64 f64;
} ze_hs_cast64;

void ze_hs_encode_fetch_op (pool_fetch_op *dst,
                            const int64   *src)
{
    ze_hs_cast64 u;
    intptr_t     iptr;

    dst->idx           = src[ZE_HS_FETCH_IDX];
    dst->want_descrips = (src[ZE_HS_FETCH_WANT_DESCRIPS] != 0);
    dst->want_ingests  = (src[ZE_HS_FETCH_WANT_INGESTS]  != 0);
    dst->rude_offset   = src[ZE_HS_FETCH_RUDE_OFFSET];
    dst->rude_length   = src[ZE_HS_FETCH_RUDE_LENGTH];
    dst->tort          = src[ZE_HS_FETCH_TORT];

    u.i64              = src[ZE_HS_FETCH_TS];
    dst->ts            = u.f64;

    dst->total_bytes   = src[ZE_HS_FETCH_TOTAL_BYTES];
    dst->descrip_bytes = src[ZE_HS_FETCH_DESCRIP_BYTES];
    dst->ingest_bytes  = src[ZE_HS_FETCH_INGEST_BYTES];
    dst->rude_bytes    = src[ZE_HS_FETCH_RUDE_BYTES];
    dst->num_descrips  = src[ZE_HS_FETCH_NUM_DESCRIPS];
    dst->num_ingests   = src[ZE_HS_FETCH_NUM_INGESTS];

    iptr               = (intptr_t) src[ZE_HS_FETCH_P];
    dst->p             = (protein)  iptr;
}

void ze_hs_decode_fetch_op (int64               *dst,
                            const pool_fetch_op *src)
{
    ze_hs_cast64 u;
    intptr_t     iptr;

    dst[ZE_HS_FETCH_IDX]           = src->idx;
    dst[ZE_HS_FETCH_WANT_DESCRIPS] = (src->want_descrips ? 1 : 0);
    dst[ZE_HS_FETCH_WANT_INGESTS]  = (src->want_ingests  ? 1 : 0);
    dst[ZE_HS_FETCH_RUDE_OFFSET]   = src->rude_offset;
    dst[ZE_HS_FETCH_RUDE_LENGTH]   = src->rude_length;
    dst[ZE_HS_FETCH_TORT]          = src->tort;

    u.f64                          = src->ts;
    dst[ZE_HS_FETCH_TS]            = u.i64;

    dst[ZE_HS_FETCH_TOTAL_BYTES]   = src->total_bytes;
    dst[ZE_HS_FETCH_DESCRIP_BYTES] = src->descrip_bytes;
    dst[ZE_HS_FETCH_INGEST_BYTES]  = src->ingest_bytes;
    dst[ZE_HS_FETCH_RUDE_BYTES]    = src->rude_bytes;
    dst[ZE_HS_FETCH_NUM_DESCRIPS]  = src->num_descrips;
    dst[ZE_HS_FETCH_NUM_INGESTS]   = src->num_ingests;

    iptr                           = (intptr_t) src->p;
    dst[ZE_HS_FETCH_P]             = (int64)    iptr;
    dst[ZE_HS_FETCH_P_LEN]         = slaw_len (src->p);
}
