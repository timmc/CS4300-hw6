# Ray tracer for CS 4300

Progressive-render ray tracer accepting .rts scene specification files.

If you want a *good* ray tracer with all the usual features, may I recommend
looking at https://code.google.com/p/cray/ instead.

## Academic integrity

This is an implementation of HW6 (ray tracer) of CS 4300 in Spring 2011 at NEU:

http://www.ccs.neu.edu/course/cs4300/HW6/HW6.html

Fellow students: You have been warned.

## Building and running

Run with Leiningen. Scene data is accepted on standard input, although
this will not be available with `lein run`. Standard input is ignored
if an input file is specified.

A number of optional command line arguments are accepted:

Input and display:
 -w int        Set the width of the canvas in pixels. (Must be positive.)
 -h int        Set the height of the canvas in pixels. (Must be positive.)
 -f file.rts   Add this file's scene data to the render, and ignore stdin. This
               argument is cumulative.

Scene settings overrides:
 -ld bool      Turn diffuse lighting on or off.
 -ls bool      Turn specular lighting on or off.
 -sh bool      Turn shadows on or off.
 -rf int       Set the % reflectivity of surfaces for mirror reflection. (May be
               set > 100 or < 0 for interesting effects!)
 -ml int       Set the maximum number of mirror reflections for a ray. (Must be
               non-negative.)

For example: `lein run -rf 95 -f ./res/xmaskaos.rts`

All options may be repeated, but unless otherwise noted as cumulative, the last
instance of an option will be the one that takes effect. Boolean options may be
specified as true, false, on, off, yes, no, 1, or 0. Integer options must be
decimal integers.

## Known bugs

* File paths specified on command line are relative to script, not to current
  working directory.

## Licensing

Copyright Tim McCormack 2011.

Relies on Incanter Core numerical and statistical library.
