; Do some elite arithmetic. Program output:
; Bytecode: 225 bits, 28.125 bytes,     
;    0010000001100000000000000000000010100101111001010110110000000000000000000000    
;    0000000101101010100001101100001101101100110010010000110110111101001000001001    
;    0100101001000000111010100110110110110100100101101101100000001111110011011    
;
; Return value:
;    Binary:         
;        10100111001        
;
;    Integer: 1337

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
