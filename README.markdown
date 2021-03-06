BISC
====

The bit-vector instruction set.
More CISCy than CISC.

BISC is an experimental hypothetical CISC instruction set that operates on bits
as its atomic storage unit instead of bytes.
I wrote this for a CS441 Computer Architecture class in Fall 2009.

Examples
========

See the included 1337.asm and loopio.asm for examples.
The code is assembled from plaintext with lots of comments so it should
be easy to figure out.

Syntax
======

Instructions start with `.`.

* For a list, see the instDefs part of BISC.hs
        
Registers start with $.

The special registers are:

* $ip for the instruction pointer
* $out for the output buffer
* $in for the input buffer

The other registers are:

* $a through $z,
* $α through $ω (alpha through omega)
        
Labels start with * to denote a label,
and & to use a previously-defined label as a location to jump to.
    
Constants start with

* b for binary strings of any length
* B for 8 bit bytes in integer order
* c for 8 bit bytes in character order
* i for 32-bit integers
* " for an ascii string (terminated on whitespace)
        
Features
========

* Instructions are any size for better potential huffman coding!
  Save every bit! Unless it's too hard, then forget it.
* Registers are infinite bit vectors! This would make this instruction
  set hard/impossible to implement in hardware probably, but that is
  somebody else's problem. Also, unicode registers!
* Labels can be set and jumped to! Structured programming!
* Instructions can have dynamic arity! With precision to the bit!
  They can specify how many bits they need for arguments at runtime!
* There is a stack of registers to create lexical closures!
    These are not implemented fully yet but could be made to work easily.
    The programmer need not worry about a function call's side effects on the
    registers. Proper isolation!
        
Caveats
=======

* It's really complex. It'd be much worse in C. Blame the deadline for
  some of the ugliness. As an aside, Hindley-Milner type inference is
  really tremendously useful when putting together big projects in a
  short amount of time.

* The implementation is very fargone and abstracted away from any actual 
  raw bit twiddling. This is arguably a good thing!

* Modern optimizations would be really hard to implement. Some ancient
  optimizations for non-Von Neumann architectures will probably work
  well enough however.
