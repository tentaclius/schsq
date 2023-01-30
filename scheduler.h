#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include <pthread.h>
#include <stdint.h>

typedef void (*event_fn_t) (void*);
struct scheduler_struct;
typedef struct scheduler_struct scheduler_t;

scheduler_t* sch_run();
void sch_init(scheduler_t *sch);
void sch_stop(scheduler_t *sch);
struct timespec sch_now();
void schedule_event(scheduler_t *sch, struct timespec tm, event_fn_t fnP, void *param);
struct timespec time_add(struct timespec a, struct timespec b);
struct timespec time_add_nsec(struct timespec a, uint64_t nsec);
struct timespec time_sub(struct timespec a, struct timespec b);
int time_compare(struct timespec a, struct timespec b);

void test();

#endif
