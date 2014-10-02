#include "common.h"
#include <stdio.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

extern int R_is_ready;

// TODO: split parse and eval
SEXP RCall_eval(char *cmd) {
    if (! R_is_ready) {
      return NULL;
    }
    R_is_ready = 0;
    SEXP cmdSexp, cmdexpr, ans = R_NilValue;
    ParseStatus status;
    cmdSexp = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(cmdSexp, 0, mkChar(cmd));
    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
    if (status != PARSE_OK) {
        UNPROTECT(2);
        R_is_ready = 1;
        fprintf(stderr, "parse error.\n");
        return NULL;
    }

    int errorOccurred;
    /* Loop is needed here as EXPSEXP will be of length > 1 */
    for (int i = 0; i < length(cmdexpr); i++)
        ans = R_tryEval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv, &errorOccurred);
        if (errorOccurred) {
            R_is_ready = 1;
            return NULL;
        }
    UNPROTECT(2);
    R_is_ready = 1;
    return ans;
}
