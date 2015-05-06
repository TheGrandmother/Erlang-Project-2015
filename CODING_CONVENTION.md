# Coding Convention
This tiny coding convention will serve as a guide to make code look pretty and be less error prone.

## Structure
Each new module should have its source a forlder with the same name as the module in the `src/` folder and it shoudl contain atleast one file called `<moule name>.erl`.
This file is then to be comipled to `<module_name.beam>` in the `ebin/` folder.
So for example the module `glenn` will have its main source file be `src/glenn/glenn.erl` and it will be compiled to `ebin/glenn.beam`. 

##Testing
Each modules main file (the `.erl` file with the same name as the module) should contain appropriate tests for the all of the modules exported functions. 

## Documentation
All exported functions and types must have a `-spec` thingy and be documented properly.
Documentation for internal  funtions is optional.

## Naming convention

### Variables
Since all variables must start with a capital letter lets use underscores and capital letters for each new word.
So some valid variable names would be:

```
    Bob
    Bobs_Burgers
    Whats_The_Haps_On_The_Craps
```

### Atoms
Since atoms must start with a small letter all atom names should start with a tiny letter and use underscores with each new word
beginning with a tiny letter. Like this:

```
    sad
    sad_cause_bob_is_your_dad
```

### Functions
Functions should use camel case naming.

##Message handling

We must take great care when handling messages indoprder to avoid deadlock and undefined behaviour.

So we must be very careful about just using the `receive` block.
As a standard we should, in all processes which are _state_ _machines_, use the `receiver/2` or `receiver/1` 
wrapper functions to ensure badassery.

In al the scenarios where we use `receive` blocks or handle messages using the `receiver` wrapper we shoudl always
include a catch all clause wich will catch and produce an error if any inapropriate( a message wich in the curent state makes no sense) is caught.


##Process Spawning.
Each process should be spawended using `spawn_link` in order to kill handle errors troughout the simulation.



 