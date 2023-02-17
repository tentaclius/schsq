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
   snd_seq_set_client_name(hndl->seq,name);
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

snd_seq_event_t*
midi_receive(midi_handler_t *h)
{
   snd_seq_nonblock(h->seq, 0);
   snd_seq_event_t *ev = NULL;
   snd_seq_event_input(h->seq, &ev);
   return ev;
}

void
midi_send_event(midi_handler_t *h, void *ptr)
{
   snd_seq_event_t *event = (snd_seq_event_t*) ptr;
   snd_seq_event_output(h->seq, event);
   snd_seq_drain_output(h->seq);
}

typedef struct {
   midi_handler_t *midi;
   snd_seq_event_t *event;
   uint8_t bFreeEvent;
} midi_event_param_t;

/* A wrapper function to serve as a callback for the scheduler */
void
execute_midi_event(void *arg)
{
   midi_event_param_t *param = (midi_event_param_t*) arg;
   midi_send_event(param->midi, param->event);

   if (param->bFreeEvent)
      free(param->event);
   free(arg);
}

void
midi_schedule_event(midi_handler_t *midi, scheduler_t *sch, struct timespec tm, void *ptr)
{
   midi_event_param_t *param = (midi_event_param_t*) malloc(sizeof(midi_event_param_t));
   param->midi = midi;
   param->event = ptr;
   param->bFreeEvent = 0;

   param->event->queue = SND_SEQ_QUEUE_DIRECT;
   param->event->source.port = midi->port;
   param->event->dest.client = SND_SEQ_ADDRESS_SUBSCRIBERS;
   param->event->dest.port = SND_SEQ_ADDRESS_UNKNOWN;

   schedule_event(sch, tm, execute_midi_event, param);
}

void
midi_schedule_note(midi_handler_t *h, scheduler_t *sch, struct timespec tm, unsigned type, unsigned note, unsigned velo, unsigned chan)
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

   midi_event_param_t *arg = (midi_event_param_t*) malloc(sizeof(midi_event_param_t));
   arg->midi = h;
   arg->event = event;
   arg->bFreeEvent = 1;

   schedule_event(sch, tm, execute_midi_event, arg);
}

void
midi_schedule_ctrl(midi_handler_t *midi, scheduler_t *sch, struct timespec tm, unsigned ctrl, unsigned value, unsigned chan)
{
   snd_seq_event_t *event = (snd_seq_event_t*) malloc(sizeof(snd_seq_event_t));
   snd_seq_ev_clear(event);
   event->queue = SND_SEQ_QUEUE_DIRECT;
   event->type = SND_SEQ_EVENT_CONTROLLER;
   event->flags = SND_SEQ_EVENT_LENGTH_FIXED;
   event->source.port = midi->port;
   event->dest.client = SND_SEQ_ADDRESS_SUBSCRIBERS;
   event->dest.port = SND_SEQ_ADDRESS_UNKNOWN;
   event->data.control.channel = chan;
   event->data.control.param = ctrl;
   event->data.control.value = value;

   midi_event_param_t *arg = (midi_event_param_t*) malloc(sizeof(midi_event_param_t));
   arg->midi = midi;
   arg->event = event;
   arg->bFreeEvent = 1;

   schedule_event(sch, tm, execute_midi_event, arg);
}
