#ifndef ZE_HS_SLAWIO_H_9466ABB5
#define ZE_HS_SLAWIO_H_9466ABB5

#include "ze-hs-cleanup.h"

#include "libLoam/c/ob-retorts.h"
#include "libLoam/c/ob-types.h"
#include "libPlasma/c/slaw-io.h"

/* op is 'r' (read) or 'c' (close) */
typedef ob_retort (*ze_hs_read_func) (char    op,
                                      byte   *buffer,
                                      size_t  size,
                                      size_t *size_read);

/* op is 'w' (write), 'f' (flush) or 'c' (close) */
typedef ob_retort (*ze_hs_write_func) (char        op,
                                       const byte *buffer,
                                       size_t      size);

typedef union {
    ze_hs_cleanup cu;
    slaw_input    si;
} ze_hs_input;

typedef union {
    ze_hs_cleanup cu;
    slaw_output   so;
} ze_hs_output;

ze_hs_input *ze_hs_open_yaml_input (ze_hs_read_func rf,
                                    ob_retort      *tort_out);

ze_hs_output *ze_hs_open_yaml_output (ze_hs_write_func wf,
                                      bslaw            options,
                                      ob_retort       *tort_out);

slaw ze_hs_read_input (ze_hs_input *inp,
                       ob_retort   *tort_out,
                       int64       *len_out);

ob_retort ze_hs_close_input (ze_hs_input *inp);

void ze_hs_finalize_input (ze_hs_input *inp);

ob_retort ze_hs_write_output (ze_hs_output *out, bslaw s);

ob_retort ze_hs_close_output (ze_hs_output *out);

void ze_hs_finalize_output (ze_hs_output *out);

#endif /* ZE_HS_SLAWIO_H_9466ABB5 */
