#include <glib.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <signal.h>

#include "scheduler.h"

/*****************************************************************************************************
* PRIAVTE
*****************************************************************************************************/

typedef struct {
   event_fn_t fn;
   struct timespec tm;
   void *param;
} sch_event_t;

struct scheduler_struct {
   pthread_t thread_id;
   GAsyncQueue *queue;
};

/*** FUNCTIONS ***/

gint event_compare_time(gconstpointer a, gconstpointer b, void *arg)
{
   sch_event_t *ea = (sch_event_t*) a;
   sch_event_t *eb = (sch_event_t*) b;
   return time_compare(ea->tm, eb->tm);
}

void sighandler(int sig)
{
}

void*
thread_func(void *arg)
{
   scheduler_t *sch = (scheduler_t*) arg;
   sch_event_t *current_evt = NULL;
   struct timespec current_time;

   struct sigaction act;
   act.sa_flags = 0;
   act.sa_handler = sighandler;
   sigemptyset(&act.sa_mask);
   sigaction(SIGUSR1, &act, NULL);

   while (TRUE) {
      current_evt = g_async_queue_pop(sch->queue);
      current_time = sch_now();

      if (time_compare(current_evt->tm, current_time) <= 0) {
         current_evt->fn(current_evt->param);
         free(g_steal_pointer(&current_evt));
      } else {
         struct timespec req = time_sub(current_evt->tm, current_time);
         if (nanosleep(&req, NULL) == 0) {
            current_evt->fn(current_evt->param);
            free(g_steal_pointer(&current_evt));
         } else if (errno == EINTR) {
            g_async_queue_push_sorted(sch->queue, current_evt, event_compare_time, NULL);
         } else {
            perror("nanoseleep in thread_func");
            exit(-1);
         }
      }
   }
   return NULL;
}

/*****************************************************************************************************
* PUBLIC
*****************************************************************************************************/

struct timespec
time_add(struct timespec a, struct timespec b)
{
   a.tv_sec += b.tv_sec;
   a.tv_nsec += b.tv_nsec;
   if (a.tv_nsec > 999999999) {
      a.tv_sec ++;
      a.tv_nsec %= 1000000000;
   }
   return a;
}

struct timespec
time_add_nsec(struct timespec a, unsigned long long nsec)
{
   struct timespec b = { .tv_sec = nsec / 1000000000, .tv_nsec = nsec % 1000000000 };
   return time_add(a, b);
}

struct timespec
time_sub(struct timespec a, struct timespec b)
{
   a.tv_sec -= b.tv_sec;
   if (b.tv_nsec > a.tv_nsec) {
      a.tv_sec --;
      a.tv_nsec += 1000000000;
   }
   a.tv_nsec -= b.tv_nsec;
   return a;
}

int
time_compare(struct timespec a, struct timespec b)
{
   if (b.tv_sec == a.tv_sec) {
      if (a.tv_nsec < b.tv_nsec) return -1;
      else if (a.tv_nsec > b.tv_nsec) return 1;
   } else {
      if (a.tv_sec < b.tv_sec) return -1;
      else if (a.tv_sec > b.tv_sec) return 1;
   }
   return 0;
}

void
sch_init(scheduler_t *sch)
{
   sch->queue = g_async_queue_new();

   pthread_attr_t attr;
   assert(pthread_attr_init(&attr) == 0);
   assert(pthread_create(&sch->thread_id, &attr, thread_func, sch) == 0);
}

void
sch_stop(scheduler_t *sch)
{
   pthread_kill(sch->thread_id, SIGKILL);
   sch->thread_id = 0;
}

struct timespec
sch_now()
{
   struct timespec tm;
   assert(clock_gettime(CLOCK_REALTIME, &tm) == 0);
   return tm;
}

void
schedule_event(scheduler_t *sch, struct timespec tm, event_fn_t fnP, void *param)
{
   sch_event_t *evt = (sch_event_t*) malloc(sizeof(sch_event_t));
   assert(evt != NULL);
   evt->fn = fnP;
   evt->tm = tm;
   evt->param = param;

   g_async_queue_push_sorted(sch->queue, evt, event_compare_time, NULL);
   pthread_kill(sch->thread_id, SIGUSR1);
}

//=====================================================================================================
// TEST

void play(void *arg)
{
   printf("play %p\n", arg);
}

void
test()
{
   scheduler_t sch;
   sch_init(&sch);

   sleep(1);
   struct timespec t = sch_now(),
                   d2 = {.tv_sec = 2, .tv_nsec = 500000000};
   schedule_event(&sch, time_add(t, d2), play, (void*)2);
   schedule_event(&sch, time_add_nsec(t, 2000000000), play, (void*)1);
   schedule_event(&sch, t, play, (void*)0);
   sleep(1);
   schedule_event(&sch, sch_now(), play, (void*)-1);

   getchar();
}
