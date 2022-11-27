.section "ClearVram" free
; Write 00 to all vram positions
; Assumes disabled display
ClearVram:
  ; Prepare vram for writes to the data port
  ld a,0
  out (CONTROL_PORT),a
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a

  ld bc,$4000 ; 16K
  -:
    xor a
    out (DATA_PORT),a
    dec bc
    ld a,b
    or c
  jp nz,-
ret


.ends

.section "InitializeRegisters" free
; Initialize the 11 VDP registers
; Entry: HL = Pointer to 11 bytes of initialization data
InitializeRegisters:
  xor b
  -:
    ld a,(hl)
    out (CONTROL_PORT),a
    inc hl
    ld a,b
    or REGISTER_WRITE_COMMAND
    out (CONTROL_PORT),a
    cp REGISTER_WRITE_COMMAND|10
    ret z
    inc b
    jr -
.ends

.section "Set VDP Register" free
; Write to target register.
; A = byte to be loaded into vdp register.
; B = target register 0-10.
SetRegister:
           out (CONTROL_PORT),a
           ld a,REGISTER_WRITE_COMMAND
           or b
           out (CONTROL_PORT),a
           ret
.ends


.SECTION "Output Column" FREE
; Output a column from a buffer to the name table
; Entry: A = Name table column to overwrite
;        HL = Base address of buffer
OutputColumn:
        LD     D,$38            ;POINT DE TO FIRST BYTE IN THE SPECIFIED
        ADD    A,A              ; COLUMN
        LD     E,A
        LD     B,28             ;PREPARE TO OUTPUT 28 WORDS TO VRAM
-:
        CALL   _OUTWD           ;OUTPUT ONE WORD TO VRAM

        LD     A,32*2           ;ALIGNMENT = ONE NAME TABLE ROW
        ADD    A,E              ;APPLY ALIGNMENT VALUE USING 8-BIT
        LD     E,A              ; ADDITION TO AVOID DISTURBING
        LD     A,0              ; ANOTHER REGISTER PAIR
        ADC    A,D
        LD     D,A
        DJNZ   -                ;LOOP THROUGH ALL 28 WORDS
        RET

        ;****************************************************************
        ;ROUTINE: _OUTWD
        ;PURPOSE: OUTPUT WORD FROM BUFFER TO DESTINATION IN VRAM
        ;ENTRY: HL = BUFFER ADDRESS
        ;       DE = DESTINATION ADDRESS
        ;EXIT:  HL = BUFFER ADDRESS + 2
        ;REGISTERS USED: AF, HL
        ;****************************************************************
_OUTWD:
        ;FORMAT AND OUTPUT COMMAND WORD
        LD     A,E              ;GET FIRST COMMAND BYTE (ADDRESS LSB)
        OUT    (CONTROL_PORT),A        ;OUTPUT IT TO VDP CONTROL PORT
        LD     A,D              ;GET SECOND COMMAND BYTE (ADDRESS MSB)
        OR     VRAM_WRITE_COMMAND             ;APPLY WRITE-ENABLE COMMAND BITS
        OUT    (CONTROL_PORT),A        ;OUTPUT BYTE TO VDP CONTROL PORT

        ;OUTPUT ONE DATA WORD FROM BUFFER TO VRAM AND INC BUFFER POINTER
        LD     A,(HL)           ;GET BYTE FROM BUFFER
        OUT    (DATA_PORT),A        ;OUTPUT IT TO DATA PORT (AUTO INCREMENT)
        INC    HL               ;INCREMENT BUFFER POINTER
        LD     A,(HL)           ;GET BYTE FROM BUFFER
        OUT    (DATA_PORT),A        ;OUTPUT IT TO DATA PORT (AUTO INCREMENT)
        INC    HL               ;INCREMENT BUFFER POINTER
        RET
.ENDS


.SECTION "Load vram" FREE
; Load a number of bytes from a source address into vram.
; Entry: BC = Number of bytes to load
;        DE = Destination address in vram
;        HL = Source address
LoadVram:
        ;FORMAT AND OUTPUT COMMAND WORD
        LD     A,E              ;GET FIRST COMMAND BYTE (DEST. LSB)
        OUT    (CONTROL_PORT),A        ;OUTPUT IT TO VDP CONTROL PORT
        LD     A,D              ;GET SECOND COMMAND BYTE (DEST. MSB)
        OR     VRAM_WRITE_COMMAND             ;APPLY WRITE-ENABLE COMMAND BITS
        OUT    (CONTROL_PORT),A        ;OUTPUT BYTE TO VDP CONTROL PORT

        ;LOAD BC NUMBER OF BYTES
-:
        LD     A,(HL)           ;GET SOURCE BYTE
        OUT    (DATA_PORT),A        ;OUTPUT IT TO VDP DATA PORT
        INC    HL               ;INCREMENT SOURCE POINTER
        DEC    BC               ;DECREMENT BYTE COUNTER
        LD     A,C              ;GET BYTE COUNTER
        OR     B                ;RETURN IF NO MORE BYTES TO WRITE
        JP     NZ,-             ; ELSE LOOP AGAIN
        RET
.ENDS


.SECTION "LoadCRAM" FREE
; Load a number of colors into color ram.
; Entry: A = Palette index (0-31)
;        B = Number of colors to load
;        HL = Base address of source data (color words, Game Gear)
LoadCRam:
        CP     32
        RET    NC               ;RETURN IF TARGET PALETTE INDEX > 31

        LD     C,A              ;SAVE PALETTE INDEX PARAMETER
        LD     A,B              ;GET NUMBER OF COLORS TO WRITE
        CP     0
        RET    Z                ;RETURN IF NUMBER OF COLORS = 0

        ADD    A,C
        CP     33
        RET    NC               ;RETURN IF INDEX + NUMBER OF COLORS > 32

        ;PREPARE VDP FOR COLOR RAM UPDATING AT THE DESIRED ADDRESS
        LD     A,C              ;RESTORE PALETTE INDEX PARAMETER
        ADD    A,A              ;DOUBLE PALETTE INDEX TO GET ADDRESS
        OUT    (CONTROL_PORT),A        ;LOAD COLOR RAM ADDRESS (COMMAND BYTE 1)
        LD     A,CRAM_WRITE_COMMAND           ;COLOR RAM UPDATING (COMMAND BYTE 2)
        OUT    (CONTROL_PORT),A

        ;OUTPUT COLOR LOOP
-:
        LD     A,(HL)           ;GET FIRST BYTE OF COLOR
        OUT    (DATA_PORT),A        ;OUTPUT IT TO COLOR RAM
        INC    HL               ;INCREMENT SOURCE POINTER
        LD     A,(HL)           ;GET SECOND BYTE OF COLOR
        OUT    (DATA_PORT),A        ;OUTPUT IT
        INC    HL               ;INCREMENT SOURCE POINTER
        DJNZ   -                ;DECREMENT NUMBER OF COLORS AND LOOP IF
                                ; THERE ARE MORE COLORS LEFT
        RET
.ENDS

.section "Load SAT" free
; Load the vram sat with the SatY and SatXC buffers
; Time: 15 lines, 64 sprites are updated with x,y positions and character codes.
LoadSAT:
  ; Load y-coordinates
  ld hl,SAT_Y_START
  ld a,l
  out (CONTROL_PORT),a
  ld a,h
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a
  ld hl,SpriteBufferY
  ld c,DATA_PORT
  call Outi_64

  ; Load x-coordinates and character codes
  ld hl,SAT_XC_START
  ld a,l
  out (CONTROL_PORT),a
  ld a,h
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a
  ld hl,SpriteBufferXC
  ld c,DATA_PORT
  call Outi_128
ret

.ends

.section "LoadSwabbyPatterns" free
; Load 6 new patterns into the first 6 positions in the generator.
; Then reset AnimFlag
; Time: 14 lines
LoadSwabbyPatterns:
  ld a,$00
  out (CONTROL_PORT),a
  ld a,$20 ; Swabby's patterns are always the first six from $2000
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a

  ld hl,(SwabbyPatternPointer)
  ld c,DATA_PORT
  call Outi_196
  xor a
  ld (AnimFlag),a
ret
.ends

.section "Outiblock" free
Outi_196: ; Fastload 6 patterns
.rept 64
  outi
.endr
Outi_128:                ; Used to fastload SAT XC.
  .rept 64
    outi
  .endr
Outi_64:                 ; Used to fastload SAT Y.
  .rept 64
    outi
  .endr
ret

.ends
