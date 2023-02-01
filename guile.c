#include <stdio.h>
#include <libguile.h>
#include "scheduler.h"

typedef void (*guile_callback_t) (SCM val);

typedef struct {
   guile_callback_t proc;
   SCM arg;
} guile_param_t;

void execute_guile(void *arg)
{
   guile_param_t *par = (guile_param_t*) arg;

   static int bInit = 1;
   if (bInit) {
      scm_init_guile();
      bInit = 0;
   }

   /*guile_callback_t func  = (guile_callback_t) arg;*/
   /*func();*/
   par->proc(par->arg);
   free(arg);
}

void schedule_guile(scheduler_t *sch, struct timespec tm, guile_callback_t func, void *val)
{
   guile_param_t *param = (guile_param_t*) malloc(sizeof(guile_param_t));
   param->proc = func;
   param->arg = scm_from_pointer(val, NULL);
   schedule_event(sch, tm, execute_guile, param);
}
