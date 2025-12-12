# Advent of Code 2025

See https://adventofcode.com/

# Notes
Starting at 9 these started getting _hard_.  I have a grip on 9 part 2.  The shape is
a pacman with mouth a small rectangle coming in from the left and the direction of
construction is CCW.  (Thank you R for being able to plot it.)  So you can figure
out the boundary that is just outside the polygon and if any of this boundary is
found in your rectangle it is invalid.  But there's lots of cases involving if the
next segment turned left or right, etc.

10 eluded me.  I had some ideas with the powerSet but it grows too fast.  Doable if
you know the given buttons will find the lights without having to push a button
twice but without that gaurantee I didn't see a way forward.

11 part 2 I thought I had.  You can cache the trees as you traverse them and re-use
the values.  This doesn't work for knowing if FFT and DAC have been encountered but
I broke it down into segments.  Turns out SVR -> FFT -> DAC -> OUT.  I tried
SVR -> FFT (4273) * FFT -> DAC (9214254) * DAC -> OUT (8890) but that wasn't the
right answer.

12 was right out.
