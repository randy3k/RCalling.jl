#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>
#include <R_ext/eventloop.h>
#include <julia.h>

extern void R_ProcessEvents();
extern void rcall_register_routines();
extern int R_interrupts_pending;
int R_is_ready = 0;
int R_is_initialized = 0;

int rcall_init()
{
    if (R_is_initialized == 1)
    {
        jl_error("R is running.");
        return -1;
    }
    char *argv[] = {"RCall", "--slave"};
    int argc = sizeof(argv)/sizeof(argv[0]);
    int ret = Rf_initEmbeddedR(argc, argv);
    if (ret < 0)
    {
      jl_error("R initialization failed.");
      return -1;
    }
    rcall_register_routines();
    R_is_ready = 1;
    R_is_initialized = 1;
    return ret;
}

void rcall_process_events()
{
    if (!R_is_ready) return;

    R_is_ready = 0;

    // FIXME: a dirty fix to prevent segfault right after a sigint
    R_interrupts_pending = 0;

#ifdef __APPLE__
    R_ProcessEvents();
#endif

    fd_set* what = R_checkActivity(0,1);
    if (what != NULL) R_runHandlers(R_InputHandlers, what);

    R_is_ready = 1;
}
