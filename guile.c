#include <stdio.h>
#include <libguile.h>
#include "scheduler.h"

typedef void (*guile_callback_t) ();

void execute_guile(void *arg)
{
   static int bInit = 1;
   if (bInit) {
      scm_init_guile();
      bInit = 0;
   }

   guile_callback_t func  = (guile_callback_t) arg;
   func();
}

void schedule_guile(scheduler_t *sch, struct timespec tm, guile_callback_t func)
{
   schedule_event(sch, tm, execute_guile, func);
}
