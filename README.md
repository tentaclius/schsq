# About

A simple toolset targeted for Guile (but the scheduler is written in C and can be used independently)
to be used as an algorythmic composition tool.

It is in an early stage of development at the moment. The plan is to grow it into a Guile-powered MIDI/OSC sequencer for algorythmic composition as an
LV2 plugion or standalone app.

Currently allows to send/receive MIDI data in a very simple manner.
There are plans to add OSC and CSound interfaces.
As a far fetched plans there might be SuperCollider interface too.

Currently all it is is a set of functions and FFI wrappers within `seq.scm` file. The functions are described below.

# Utilitary functions/macros

- `(λ args . body)` -- just a shorthand for (lambda ...)
- `(cat . args)` -- convert every argument to string and concatenate them.
- `(writeln . args)` -- convert every argument to string and print it out finishing with the newline character.
- `(def . pairs)` -- a macro to define multiple bindings at once.

# Scheduler

The core of this piece of software is the scheduler implemented in C. It is contained in `schsq.so` file, which is loaded into Guile via FFI.
The wrapper functions are in `seq.scm`.

- `(sch-init)` -- initialize a scheduler and return a handle. The function is implicitly called by `midi-init`.
- `(sch-stop handle)` -- stop the scheduler.
- `(now)` -- return current time in nanoseconds.
- `(schedule time proc param-list #:optional (sch *scheduler*))` -- schedule a Guile procedure to be executed at `time` (in nanoseconds) with the parameter list `param-list`.

# MIDI

- `(midi-init interface-name)` -- initializes the scheduler and the MIDI interface with one in and one out port. Assigns the newly created
handles to *scheduler* and *midi* respectively.
- `(midi-note-on #:optional (note C-4) #:key (at (now)) (duration #f) (velo 1) (chan 0) (midi *midi*) (scheduler *scheduler*))` -- schedule note-on event.
- `(midi-note-off #:optional (note C-4) #:key (at (now)) (velo 1) (chan 0) (midi *midi*) (scheduler *scheduler*))` -- schedule note-off event.
- `(schedule-midi-note tm type note velo chan #:optional (midi *midi*) (sch *scheduler*))` -- schedule midi note event.
- `(midi-note-direct type note velo chan #:optional (handle *midi*))` -- send note event directly, without the scheduler.
- `(midi-receive #:optional)` -- receive a MIDI event. Returns a byte-vector with the MIDI data.

There are also midi notes defined as variables from `C-0` to `B-8` (example: `C-4`, `C#4`, `Db4` etc).