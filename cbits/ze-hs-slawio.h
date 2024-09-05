#ifndef ZE_HS_SLAWIO_H_9466ABB5
#define ZE_HS_SLAWIO_H_9466ABB5

#include "libLoam/c/ob-retorts.h"
#include "libLoam/c/ob-types.h"

/* op is 'r' (read) or 'c' (close) */
typedef ob_retort (*ze_hs_read_func) (char    op,
                                      byte   *buffer,
                                      size_t  size,
                                      size_t *size_read);

/* op is 'w' (write), 'f' (flush) or 'c' (close) */
typedef ob_retort (*ze_hs_write_func) (char        op,
                                       const byte *buffer,
                                       size_t      size);


#endif /* ZE_HS_SLAWIO_H_9466ABB5 */
