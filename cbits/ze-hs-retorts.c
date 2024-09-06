#include <stddef.h>

#include "ze-hs-retorts.h"

const char *ze_hs_ioe_error_string (ob_retort err)
{
#define E(x,y)                                                          \
    case x:                                                             \
        return #x

    switch (err) {
        E (ZE_HS_ALREADY_CLOSED,              "already closed");
        E (ZE_HS_IOE_ALREADY_EXISTS,          "already exists");
        E (ZE_HS_IOE_ALREADY_IN_USE,          "resource busy");
        E (ZE_HS_IOE_DOES_NOT_EXIST,          "does not exist");
        E (ZE_HS_IOE_EOF,                     "end of file");
        E (ZE_HS_IOE_FULL,                    "resource exhausted");
        E (ZE_HS_IOE_HARDWARE_FAULT,          "hardware fault");
        E (ZE_HS_IOE_ILLEGAL_OPERATION,       "illegal operation");
        E (ZE_HS_IOE_INAPPROPRIATE_TYPE,      "inappropriate type");
        E (ZE_HS_IOE_INTERRUPTED,             "interrupted");
        E (ZE_HS_IOE_INVALID_ARGUMENT,        "invalid argument");
        E (ZE_HS_IOE_PERMISSION,              "permission denied");
        E (ZE_HS_IOE_PROTOCOL_ERROR,          "protocol error");
        E (ZE_HS_IOE_RESOURCE_VANISHED,       "resource vanished");
        E (ZE_HS_IOE_SYSTEM_ERROR,            "system error");
        E (ZE_HS_IOE_TIMEOUT,                 "timeout");
        E (ZE_HS_IOE_UNKNOWN,                 "unspecified error");
        E (ZE_HS_IOE_UNSATISFIED_CONSTRAINTS, "unsatisfied constraints");
        E (ZE_HS_IOE_UNSUPPORTED_OPERATION,   "unsupported operation");
        E (ZE_HS_IOE_USER,                    "user error");
    default:
        return NULL;
    }

#undef E
}
