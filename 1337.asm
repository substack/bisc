; Do some elite arithmetic. Program output:
; Bytecode: 226 bits, 28.25 bytes,     
;     0010000010000000000000000000000010100101111001010111000000000000000000000000    
;     0000000101101010100001110000010001110000110010011000110110111101001100001001    
;     01001010011000001110101001110001110001001101011011100000000011111100011100    
; 
; Program output:
; 
; Return value:
;     Binary:         
;         10100111001        
; 
;     Integer: 1337
; 
; 1337

.loadI $b i1327 ; load an integer (32 bits)

; load a variable-sized bit vector with an integer-sized size
.loadVI $z i5 b10101 ; 5 bits to store 21 in decimal

.add $z $b $z ; 1327 + 21 = 1348
.loadB $q b00011011 ; load a byte (27)
.divB $q b00001001 ; $q = 27 / 9 = 3
.addB $q b00000111 ; ($q += 7) == 10

.sub $z $z $q ; 1348 - 10 = 1338
.subB $z b00000001 ; $z = 1338 - 1 = 1337
.exitR $z
