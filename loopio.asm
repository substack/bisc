; increment a variable until it gets to 8,
; then echo back the keys pressed until the q key is hit
; then return the incremented variable in the exit status

.loadB $i B0
.loadB $j b11110111
.not $j $j ; $j == 8

.loadB $d "q ; stop on the the letter q

*begin
    .addB $i B1
    .jlt &begin $i $j

*input_loop ; echo out what key the user presses until
    .blockB ; wait for a byte of input to be in the buffer
    .movI $c $in i8 ; copy input to $c
    .truncateI $in i0 ; clear input buffer
    
    .movI $out $c i8
    .loadB $out c10
    
    .jne &input_loop $c $d

.exitR $i ; exit with i value from before (8)
