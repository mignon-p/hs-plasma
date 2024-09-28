#include <stdlib.h>
#include <HsFFI.h>

#include "libLoam/c/ob-atomic.h"
#include "libLoam/c/ob-attrs.h"
#include "libLoam/c/ob-util.h"

#include "ze-hs-cleanup.h"
#include "ze-hs-slawio.h"
#include "ze-hs-retorts.h"

static ob_retort read_handle_read (void   *cookie,
                                   byte   *buffer,
                                   size_t  size,
                                   size_t *size_read)
{
    ze_hs_read_func f = (ze_hs_read_func) cookie;
    return f ('r', buffer, size, size_read);
}

static ob_retort read_handle_close (void *cookie)
{
    ze_hs_read_func f = (ze_hs_read_func) cookie;
    ob_retort tort    = f ('c', NULL, 0, NULL);
    hs_free_fun_ptr ((HsFunPtr) cookie);
    return tort;
}

static ob_retort write_handle_write (void       *cookie,
                                     const byte *buffer,
                                     size_t      size)
{
    ze_hs_write_func f = (ze_hs_write_func) cookie;
    return f ('w', buffer, size);
}

static ob_retort write_handle_flush (void *cookie)
{
    ze_hs_write_func f = (ze_hs_write_func) cookie;
    return f ('f', NULL, 0);
}

static ob_retort write_handle_close (void *cookie)
{
    ze_hs_write_func f = (ze_hs_write_func) cookie;
    ob_retort tort     = f ('c', NULL, 0);
    hs_free_fun_ptr ((HsFunPtr) cookie);
    return tort;
}

ze_hs_input *ze_hs_open_yaml_input (ze_hs_read_func rf,
                                    ob_retort      *tort_out)
{
    ze_hs_input      *inp  = NULL;
    ob_retort         tort = OB_UNKNOWN_ERR;
    slaw_read_handler h;

    ze_hs_check_cleanup();

    OB_CLEAR (h);
    h.read   = read_handle_read;
    h.close  = read_handle_close;
    h.cookie = (void*) rf;

    inp = (ze_hs_input *) calloc (1, sizeof (*inp));
    if (inp == NULL) {
        tort = OB_NO_MEM;
        goto fail;
    }

    tort = slaw_input_open_text_handler (h, &inp->si);
    if (tort < OB_OK) {
        goto fail;
    }

    goto done;

 fail:
    hs_free_fun_ptr ((HsFunPtr) rf);
    free (inp);
    inp = NULL;

 done:
    *tort_out = tort;
    return inp;
}

ze_hs_output *ze_hs_open_yaml_output (ze_hs_write_func wf,
                                      bslaw            options,
                                      ob_retort       *tort_out)
{
    ze_hs_output     *out  = NULL;
    ob_retort         tort = OB_UNKNOWN_ERR;
    slaw_write_handler h;

    ze_hs_check_cleanup();

    OB_CLEAR (h);
    h.write  = write_handle_write;
    h.flush  = write_handle_flush;
    h.close  = write_handle_close;
    h.cookie = (void*) wf;

    out = (ze_hs_output *) calloc (1, sizeof (*out));
    if (out == NULL) {
        tort = OB_NO_MEM;
        goto fail;
    }

    tort = slaw_output_open_text_handler (h, options, &out->so);
    if (tort < OB_OK) {
        goto fail;
    }

    goto done;

 fail:
    hs_free_fun_ptr ((HsFunPtr) wf);
    free (out);
    out = NULL;

 done:
    *tort_out = tort;
    return out;
}

slaw ze_hs_read_input (ze_hs_input *inp,
                       ob_retort   *tort_out,
                       int64       *len_out)
{
    ob_retort  tort = ZE_HS_ALREADY_CLOSED;
    slaw_input si   = inp->si;
    slaw       ret  = NULL;

    ze_hs_check_cleanup();

    if (si) {
        tort = slaw_input_read (si, &ret);
    }

    *tort_out = tort;
    *len_out  = slaw_len (ret);
    return ret;
}

ob_retort ze_hs_close_input (ze_hs_input *inp)
{
    ob_retort  tort = OB_OK;
    slaw_input si   = inp->si;

    ze_hs_check_cleanup();

    if (si) {
        tort    = slaw_input_close (si);
        inp->si = NULL;
    }

    return tort;
}

void ze_hs_finalize_input (ze_hs_input *inp)
{
    inp->cu.func = (ze_hs_cleanup_func) ze_hs_close_input;
    ze_hs_submit_cleanup (&inp->cu);
}

ob_retort ze_hs_write_output (ze_hs_output *out, bslaw s)
{
    ob_retort   tort = ZE_HS_ALREADY_CLOSED;
    slaw_output so   = out->so;

    ze_hs_check_cleanup();

    if (so) {
        tort = slaw_output_write (so, s);
    }

    return tort;
}

ob_retort ze_hs_close_output (ze_hs_output *out)
{
    ob_retort   tort = OB_OK;
    slaw_output so   = out->so;

    ze_hs_check_cleanup();

    if (so) {
        tort    = slaw_output_close (so);
        out->so = NULL;
    }

    return tort;
}

void ze_hs_finalize_output (ze_hs_output *out)
{
    out->cu.func = (ze_hs_cleanup_func) ze_hs_close_output;
    ze_hs_submit_cleanup (&out->cu);
}
