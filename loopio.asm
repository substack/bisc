; Increment a variable until it gets to 8,
; then echo back the keys pressed until the q key is hit
; then return the incremented variable in the exit status.
; Bytecode: 371 bits, 46.375 bytes,     
;     0011000101100000000001101001111111011110100110011110011100110000110100011100    
;     1001001011000000011001000000000000000000000000001001011001011100111111111000    
;     0100010100000100000000000000000000000000001000111101000001000000000000000000    
;     0000000000000000010000100001010000000000000000000000000000100000110000010010    
;     1000010001000000000000000000000000100011110001010001101111100001011    
; 
; Program output:
; aa
; bb
; cc
; qq
; 
; Return value:
;     Binary:         
;         1000        
; 
;     Integer: 8
; 
; 8

.loadB $i B0
.loadB $λ b11110111 ; unicode registers!
.not $λ $λ ; $λ == 8

.loadB $d "q ; stop on the the letter q

*begin
    .addB $i B1
    .jlt &begin $i $λ

*input_loop ; echo out what key the user presses until
    .blockB ; wait for a byte of input to be in the buffer
    .movI $c $in i8 ; copy input to $c
    .truncateVI $in i0 ; clear input buffer
    
    .movI $out $c i8
    .loadB $out c10
    
    .jne &input_loop $c $d

.exitR $i ; exit with i value from before (8)
