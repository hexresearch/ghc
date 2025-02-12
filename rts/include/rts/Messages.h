/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Message API for use inside the RTS.  All messages generated by the
 * RTS should go through one of the functions declared here, and we
 * also provide hooks so that messages from the RTS can be redirected
 * as appropriate.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdarg.h>

#if defined(mingw32_HOST_OS) && !defined(__clang__)
/* On Win64, if we say "printf" then gcc thinks we are going to use
   MS format specifiers like %I64d rather than %llu */
#define PRINTF gnu_printf
#else
/* However, on OS X, "gnu_printf" isn't recognised */
#define PRINTF printf
#endif

/* -----------------------------------------------------------------------------
 * Message generation
 * -------------------------------------------------------------------------- */

/*
 * A fatal internal error: this is for errors that probably indicate
 * bugs in the RTS or compiler.  We normally output bug reporting
 * instructions along with the error message.
 *
 * barf() invokes (*fatalInternalErrorFn)().  This function is not
 * expected to return.
 */
void barf(const char *s, ...)
   GNUC3_ATTRIBUTE(__noreturn__)
   GNUC3_ATTRIBUTE(format(PRINTF, 1, 2));

void vbarf(const char *s, va_list ap)
   GNUC3_ATTRIBUTE(__noreturn__);

// declared in Rts.h:
// extern void _assertFail(const char *filename, unsigned int linenum)
//    GNUC3_ATTRIBUTE(__noreturn__);

/*
 * An error condition which is caused by and/or can be corrected by
 * the user.
 *
 * errorBelch() invokes (*errorMsgFn)().
 */
void errorBelch(const char *s, ...)
   GNUC3_ATTRIBUTE(format (PRINTF, 1, 2));

void verrorBelch(const char *s, va_list ap);

/*
 * An error condition which is caused by and/or can be corrected by
 * the user, and which has an associated error condition reported
 * by the system (in errno on Unix, and GetLastError() on Windows).
 * The system error message is appended to the message generated
 * from the supplied format string.
 *
 * sysErrorBelch() invokes (*sysErrorMsgFn)().
 */
void sysErrorBelch(const char *s, ...)
   GNUC3_ATTRIBUTE(format (PRINTF, 1, 2));

void vsysErrorBelch(const char *s, va_list ap);

/*
 * A debugging message.  Debugging messages are generated either as a
 * virtue of having DEBUG turned on, or by being explicitly selected
 * via RTS options (eg. +RTS -Ds).
 *
 * debugBelch() invokes (*debugMsgFn)().
 */
void debugBelch(const char *s, ...)
   GNUC3_ATTRIBUTE(format (PRINTF, 1, 2));

int vdebugBelch(const char *s, va_list ap);


/* Hooks for redirecting message generation: */

typedef void RtsMsgFunction(const char *, va_list);
typedef int RtsMsgFunctionRetLen(const char *, va_list);

extern RtsMsgFunction *fatalInternalErrorFn;
extern RtsMsgFunctionRetLen *debugMsgFn;
extern RtsMsgFunction *errorMsgFn;

/* Default stdio implementation of the message hooks: */

extern RtsMsgFunction rtsFatalInternalErrorFn;
extern RtsMsgFunctionRetLen rtsDebugMsgFn;
extern RtsMsgFunction rtsErrorMsgFn;
extern RtsMsgFunction rtsSysErrorMsgFn;

#undef PRINTF
