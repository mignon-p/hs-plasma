#include <HsFFI.h>

#include "libLoam/c/ob-attrs.h"
#include "libLoam/c/ob-util.h"

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

/* We need to arrange for libLoam to be told about our error codes... */
OB_PRE_POST (ob_add_error_names (ze_hs_ioe_error_string), ob_nop ());
