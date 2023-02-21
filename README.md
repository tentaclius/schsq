# About

A simple toolset targeted for Guile (but the scheduler is written in C and can be used independently)
to be used as an algorithmic composition tool.

It is in an early stage of development at the moment. The plan is to grow it into a Guile-powered MIDI/OSC sequencer for algorithmic composition as an
LV2 plugion or standalone app with basis in C and higher level abstractions in Guile.

It's goals are very similar to what Common Music used to be, but it looks like it is no longer maintained unfortunately.
Apart from that I'd like to see sequencing a bit differently,
it is somewhat of a mix between cl-collider and Tidal Cycles ways of doing things.

Currently allows to send/receive MIDI data via alsaseq in a very simple manner.
There are plans to add OSC and CSound interfaces.
As a far fetched plans there might be SuperCollider interface too.

All it is so far is a set of functions and FFI wrappers within `schsq.scm` file. The functions are described below.

For the moment I don't have resources to port it to any other platform than Linux, so it is Linux only.

# Utilitary functions/macros

- `(cat . args)`  
convert every argument to string and concatenate them.

- `(writeln . args)`  
convert every argument to string and print it out finishing with the newline character.

- `(def . pairs)`  
a macro to define multiple bindings at once.

- `(ht key value ...)`  
a faster way to create hash tables.

# Scheduler

The core of this piece of software is the scheduler implemented in C. It is contained in `schsq.so` file, which is loaded into Guile via FFI.
The wrapper functions are in `schsq.scm`.

- `(sch-init)`  
initialize a scheduler and return a handle. The function is implicitly called by `midi-init`.

- `(sch-stop handle)`  
stop the scheduler.

- `(now)`  
return current time in nanoseconds.

- `(schedule time proc param-list #:optional (sch *scheduler*))`  
schedule a Guile procedure to be executed at `time` (in nanoseconds) with the parameter list `param-list`.

# MIDI

- `(midi-init interface-name)`  
initializes the scheduler and the MIDI interface with one in and one out port. Assigns the newly created
handles to `*scheduler*` and `*midi*` respectively.

- `(midi-note-on #:optional (note C-4) #:key (at (now)) (duration #f) (velo 1) (chan 0) (midi *midi*) (scheduler *scheduler*))`  
schedule note-on event.

- `(midi-note-off #:optional (note C-4) #:key (at (now)) (velo 1) (chan 0) (midi *midi*) (scheduler *scheduler*))`  
schedule note-off event.

- `(midi-note-direct type note velo chan #:optional (handle *midi*))`  
send note event directly, without the scheduler.
Lower level function, shouldn't be used without the scheduler.

- `(midi-receive #:optional (midi *midi*))`  
receive a MIDI event. Returns a byte-vector with the MIDI data.

- `(midi-schedule-event data #:optional (at (now)) (midi *midi*) (scheduler *scheduler*))`  
schedule a raw MIDI event

- `(midi-schedule-note tm type note velo chan #:optional (midi *midi*) (sch *scheduler*))`  
schedule midi note event.

- `(midi-send-ctrl ctrl val #:key (midi *midi*) (scheduler *scheduler*) (at (now)) (chan 0))`  
send control MIDI message.

- `(make-midi-note #:optional (type MIDI_NOTEON) (note C-4) (velo 127) (chan 0))`  
create a raw MIDI message for a note event to be used with `midi-schedule-event`.

There are also midi notes defined as variables from `C-0` to `B-8` (example: `C-4`, `C#4`, `Db4` etc).

# Sequencing

- `*bpm*`  
a variable controlling beats frequency.

- `(time->beat time)` and `(beat->time beats)`  
convert time to beats and vice versa.

- `(beats)`  
current time in beats.

- `(beat-quant n)`  
return the next beat multiple of n.

- `(merge-attrs . hash-tables)`  
merge multiple hashtables togather replacing the attributes from right to left.

- `(S . evens)`  
create a sequential group of events. The time available for the group will be devided between the events.

- `(Sa attrib . events)`  
create a sequential group with provided attributes.

- `(Sl events)`  
create a sequential group from a list of events.

- `(Sal attr events)`  
create a sequential group from a list of events with attributes.

- `(U . events)`  
create group of events triggered simultaneously (in Unison).

- `(Ua attrib . events)`  
simultaneous group of events with attributes.

- `(Ul events)`  
simultaneous group of events from a list.

- `(Ual attrib events)`  
simultaneous group of events from a list with explicit attributes.

- `(A attrib . events)`  
associate attributes with a series of events.

- `(ev-schedule (el <events>) attr)`  
schedule a series of events according to their class.

- `(metro-start)`  
metro is an object executing associated actions every beat. This procedure starts it.
Currently there is only one global metro object.

- `(metro-stop)`  
stop metro beating.

- `(metro-add name event)`  
associate an events with the metro. The name is an identifier you can use to overwrite the existing events.
`event` can be an event series created with `(U...)` and `(S...)` functions or a procedure.
The procedure can optionally return an event series which will also be executed.

- `(metro-trigger beat)`  
trigger a metro event manually (can be useful for synchronizing with an external event received via MIDI).

# Current TODO List

- [ ] MIDI SysEx
- [ ] sequence speed
- [ ] port to CL: metro
- [ ] port `sc`, `euclidian` and `chord` from CL
- [ ] osc-init
- [ ] osc-send
- [ ] osc-receive
- [ ] midi-send-sysex
- [ ] cs-send-score
- [ ] cs-send-instr
