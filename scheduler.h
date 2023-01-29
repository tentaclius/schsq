#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include <pthread.h>

// TODO
// - for internal timing use timer_create(CLOCK_REALTIME...)
// - once per beat/timer-event read the data and schedule events into a GAsyncQueue;
//   notify the sending thread via pthread_kill;
// - the sending thread reads the heap, sends the notes
//   and then calculates the delay till the next event with clock_gettime()
//   and sleeps with nanosleep()

typedef void (*event_fn_t) (void*);
struct scheduler_struct;
typedef struct scheduler_struct scheduler_t;

void sch_init(scheduler_t *sch);
void sch_stop(scheduler_t *sch);
struct timespec sch_now();
void schedule_event(scheduler_t *sch, struct timespec tm, event_fn_t fnP, void *param);
struct timespec time_add(struct timespec a, struct timespec b);
struct timespec time_add_nsec(struct timespec a, unsigned long long nsec);
struct timespec time_sub(struct timespec a, struct timespec b);
int time_compare(struct timespec a, struct timespec b);

void test();

#endif
