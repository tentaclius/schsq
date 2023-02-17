#include <stdio.h>
#include <sys/poll.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include "scheduler.h"

// TODO: snd_seq_event_input

typedef struct {
   snd_seq_t *seq;
   int port;
} midi_handler_t;


midi_handler_t*
midi_init(char *name)
{
   midi_handler_t *hndl = (midi_handler_t*) malloc(sizeof(midi_handler_t));
   assert(hndl != NULL);

   if (snd_seq_open(&hndl->seq, "default", SND_SEQ_OPEN_DUPLEX, 0) != 0) {
      perror("snd_seq_open");
      exit(-1);
   }
   /*snd_seq_set_client_name(seq, "ASEQ_send");*/
   snd_seq_nonblock(hndl->seq, 1);

   hndl->port = snd_seq_create_simple_port(hndl->seq, name,
         SND_SEQ_PORT_CAP_WRITE |
         SND_SEQ_PORT_CAP_SUBS_WRITE |
         SND_SEQ_PORT_CAP_READ |
         SND_SEQ_PORT_CAP_SUBS_READ,
         SND_SEQ_PORT_TYPE_MIDI_GENERIC |
         SND_SEQ_PORT_TYPE_APPLICATION);

   return hndl;
}

void
midi_send_note(midi_handler_t *h, unsigned type, unsigned note, unsigned velo, unsigned chan)
{
   snd_seq_event_t *event = (snd_seq_event_t*) malloc(sizeof(snd_seq_event_t));
   snd_seq_ev_clear(event);
   event->queue = SND_SEQ_QUEUE_DIRECT;
   event->type = type;
   event->flags = SND_SEQ_EVENT_LENGTH_FIXED;
   event->source.port = h->port;
   event->dest.client = SND_SEQ_ADDRESS_SUBSCRIBERS;
   event->dest.port = SND_SEQ_ADDRESS_UNKNOWN;
   event->data.note.channel = chan;
   event->data.note.note = note;
   event->data.note.velocity = velo;

   snd_seq_event_output(h->seq, event);
   snd_seq_drain_output(h->seq);

   for (unsigned i = 0; i < sizeof(snd_seq_event_t); i ++)
      printf("%d ", ((uint8_t*) event)[i]);
   puts("");
}

snd_seq_event_t*
midi_receive(midi_handler_t *h)
{
   snd_seq_nonblock(h->seq, 0);
   snd_seq_event_t *ev = NULL;
   snd_seq_event_input(h->seq, &ev);
   return ev;
}

typedef struct {
   midi_handler_t *h;
   unsigned type;
   unsigned note;
   unsigned velo;
   unsigned chan;
} midi_arg_t;

void 
execute_midi_note(void *arg)
{
   midi_arg_t *m = (midi_arg_t*) arg;
   midi_send_note(m->h, m->type, m->note, m->velo, m->chan);
   free(arg);
}

void
schedule_midi_note(midi_handler_t *h, scheduler_t *sch, struct timespec tm, unsigned type, unsigned note, unsigned velo, unsigned chan)
{
   midi_arg_t *arg = (midi_arg_t*) malloc(sizeof(midi_arg_t));
   arg->h = h;
   arg->type = type;
   arg->note = note;
   arg->velo = velo;
   arg->chan = chan;
   schedule_event(sch, tm, execute_midi_note, arg);
}

void
midi_send_raw_event(midi_handler_t *h, void *ptr)
{
   snd_seq_event_t *event = (snd_seq_event_t*) ptr;
   snd_seq_event_output(h->seq, event);
   snd_seq_drain_output(h->seq);
}

struct midi_event_param_t {
   midi_handler_t *midi;
   void *event;
};

void
execute_midi_event(void *arg)
{
   struct midi_event_param_t *param = (struct midi_event_param_t*) arg;
   midi_send_raw_event(param->midi, param->event);
}

void
schedule_midi_event(midi_handler_t *h, scheduler_t *sch, struct timespec tm, void *ptr)
{
   struct midi_event_param_t *param = (struct midi_event_param_t*) malloc(sizeof(struct midi_event_param_t));
   param->midi = h;
   param->event = ptr;
   schedule_event(sch, tm, execute_midi_event, param);
}
