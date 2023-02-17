#include <stdio.h>
#include "scheduler.h"
#include "midi.c"

#define SEC 1000000000

int main(int argc, char **argv)
{
   scheduler_t *sch = sch_run();
   midi_handler_t *midi = midi_init("MYCLIENT");

   /*getchar();*/
   /*schedule_midi_note(midi, sch, sch_now(), SND_SEQ_EVENT_NOTEON, 50, 127, 0);*/
   /*schedule_midi_note(midi, sch, time_add_nsec(sch_now(), 2 * SEC), SND_SEQ_EVENT_NOTEOFF, 50, 127, 0);*/
   /*getchar();*/

   printf("%lu\n", sizeof(snd_seq_event_t));

   return 0;
}
