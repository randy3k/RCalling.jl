#include "common.h"
#include <stdio.h>
#include <Rembedded.h>
#include <R_ext/eventloop.h>

int R_is_ready = 0;
int R_is_initialized = 0;

int RCall_init(){
    if (R_is_initialized == 1){
        printf("R is running.\n");
        return -1;
    }
    char *argv[] = {"RCall", "--slave"};
    int argc = sizeof(argv)/sizeof(argv[0]);
    int ret = Rf_initEmbeddedR(argc, argv);
    if (ret < 0) {
      printf("R initialization failed.\n");
      return -1;
    }
    R_is_ready = 1;
    R_is_initialized = 1;
    return ret;
}

void RCall_ProcessEvents()
{
    if (!R_is_ready) {
        printf("R is not ready.\n");
        return;
    }
    R_is_ready = 0;
#if defined(HAVE_AQUA) || (defined(Win32) || defined(Win64))
    R_ProcessEvents();
#endif
#if ! (defined(Win32) || defined(Win64))
    R_runHandlers(R_InputHandlers, R_checkActivity(0, 1));
#endif
    R_is_ready = 1;
}
