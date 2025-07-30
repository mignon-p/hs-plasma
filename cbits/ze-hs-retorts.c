#include <stddef.h>

#include "ze-hs-retorts.h"

const char *ze_hs_ioe_error_string (ob_retort err)
{
#define E(x,y)                                                          \
    case x:                                                             \
        return #x

    switch (err) {
        E (HSPLASMA_ALREADY_CLOSED,       "already closed");
        E (HSPLASMA_INTERNAL_ERROR,       "internal error");
        E (IOERR_ALREADY_EXISTS,          "already exists");
        E (IOERR_ALREADY_IN_USE,          "resource busy");
        E (IOERR_DOES_NOT_EXIST,          "does not exist");
        E (IOERR_EOF,                     "end of file");
        E (IOERR_FULL,                    "resource exhausted");
        E (IOERR_HARDWARE_FAULT,          "hardware fault");
        E (IOERR_ILLEGAL_OPERATION,       "illegal operation");
        E (IOERR_INAPPROPRIATE_TYPE,      "inappropriate type");
        E (IOERR_INTERRUPTED,             "interrupted");
        E (IOERR_INVALID_ARGUMENT,        "invalid argument");
        E (IOERR_PERMISSION,              "permission denied");
        E (IOERR_PROTOCOL_ERROR,          "protocol error");
        E (IOERR_RESOURCE_VANISHED,       "resource vanished");
        E (IOERR_SYSTEM_ERROR,            "system error");
        E (IOERR_TIMEOUT,                 "timeout");
        E (IOERR_UNKNOWN,                 "unspecified error");
        E (IOERR_UNSATISFIED_CONSTRAINTS, "unsatisfied constraints");
        E (IOERR_UNSUPPORTED_OPERATION,   "unsupported operation");
        E (IOERR_USER,                    "user error");
    default:
        return NULL;
    }

#undef E
}
