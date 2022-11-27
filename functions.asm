.section "Await frame interrupt" free
; Await frame interrupt.
; This will wait exclusively for the frame interrupt.
; Unlike the 'halt' opcode. Assumes the variable VDPStatus is set by
; the interrupt handler. Clears bit 7 of VDPStatus upon return.
AwaitFrame:
  push af
  -:
    ld a,(VDPStatus)
    bit 7,a
  jp z,-
  res 7,a
  ld (VDPStatus),a
  pop af
ret
.ends

.section "GetKeys" free
; Reads state of d-pad, 1,2 and start button, and store info in variable, so it can
; be assessed with the designated keytest functions below. Should be called once
; every loop.
GetKeys:
  in a,(JOYSTICK_PORT_1)
  and %00111111 ; Keep only the d-pad and two buttons.
  ld b,a ; Save d-pad and buttons in B.
  in a,(MISC_PORT) ; Get start button state (in bit 7).
  and %10000000 ; Mask away the other bits.
  or b ; Combine start button state and the other buttons in one byte.
  ld (Keys),a ; Load this byte into the designated variable.
ret

IsDownPressed:
  ld a,(Keys)
  and %00000010
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

IsUpPressed:
  ld a,(Keys)
  and %00000001
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

IsLeftPressed:
  ld a,(Keys)
  and %00000100
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

IsRightPressed:
  ld a,(Keys)
  and %00001000
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

IsButton1Pressed:
  ld a,(Keys)
  and %00010000
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

IsButton2Pressed:
  ld a,(Keys)
  and %00100000
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

IsStartPressed:
  ld a,(Keys)
  and %10000000
  ret nz ; Return with carry flag reset
  scf
ret ; Return with carry flag set.

.ends


; *****************************************************************************
; INITIALIZATION FUNCTIONS
; *****************************************************************************

.section "Fill memory" free
; Overwrite a chunk of memory with a specified byte value.
; HL = base address, BC = area size, A = fill byte.
; Used by ClearRam.
FillMemory:
           ld (hl),a
           ld d,h
           ld e,l
           inc de
           dec bc
           ld a,b
           or c
           ret z
           ldir
ret
.ends

.section "Clear Ram" free
; Overwrite RAM with zeroes (stay clear of the stack )
ClearRam:
  ld hl,$c000
  ld bc,$1fe0
  ld a,0
  call FillMemory
ret
.ends

.section "Initialize Standard Sega Mapper" free
InitializeMapper:
  ld de,$fffc
  ld hl,MapperInitData
  ld bc,$0004
  ldir
ret

MapperInitData:
  .db $00,$00,$01,$02
.ends


; *****************************************************************************
; FRAME-BY-FRAME BACKGROUND/SCREEN FUNCTIONS
; *****************************************************************************

.section "Score handling" free
; ADD TO SCORE.
; Add points to the score.
; HL = address of the digit we want to increase.
; B = the amount by which to increase the digit.

AddScore:
       ld a,(hl)           ; get the value of the digit.
       add a,b             ; add the amount to this value.
       ld (hl),a           ; put updated digit back in string.
       cp 9                ; test updated digit.
       ret c               ; if 9 or less, relax and return.
       ret z

; Update the next digit to the left.

       sub 10              ; make digit '0'.
       ld (hl),a           ; and load it into position.
-      dec hl              ; move pointer to next digit (left).
       inc (hl)            ; increase that digit.
       ld a,(hl)           ; load value into A.
       cp 9                ; test it like before.
       ret c               ; if 9 or less, then scoring is done.
       ret z               ;
       sub 10              ; else -  make digit '0'.
       ld (hl),a           ; and load it into position.
       jp -                ; update and test next digit.
.ends

.section "Prepare and load Column" free
; Called when it is time to load a new column to the name table at next
; vblank. This program handles related variables, and runs the subroutine to
; load the column buffer.
PrepareNewColumn:
  ld a,(NextNameTableColumn)
  inc a
  cp 32 ; is it time to start over from column 0? (wrap-around)
  jp nz,+
    xor a
    +:
  ld (NextNameTableColumn),a
  ld a,(MapBank)
  ld (SLOT_2_CONTROL),a
  ld hl,NextMapColumn
  inc (hl)
  ld a,(hl)
  ld hl,(MapBase)
  call LoadColumn
  ld a,1
  ld (ColumnFlag),a
ret

; Load a column from a map into ColumnBuffer.
; Map format: A two-dimensional word table, 256x28, ready for the name table.
; Entry: A = Column number to load (0-255)
;        HL = Base address of map
; Exit: ColumnBuffer loaded with words
LoadColumn:
  ; Calculate offset and apply it to map base address
  ex de,hl
  ld h,0
  ld l,a
  add hl,hl
  add hl,de

  ld bc,56
  ld de,ColumnBuffer

  -:
    ; Load 2 bytes and return if byte counter (BC) is depleted
    ldi
    ldi
    ret po

    ; Skip one row, step back, and thus point to the next word in the column
    inc h
    inc h
    dec hl
    dec hl
    jp -
.ends

.section "Handle HScroll" free
HandleHScroll:
  ld a,(HScrollCounter)
  inc a
  cp HSCROLL_COUNTER_MAX
  jp nz,+
    ld a,1
    ld (HScrollStatus),a ; signal to objects that screen scrolled horizontally.
    xor a
    ld hl,ScrollX
    dec (hl)
    +:
    ld (HScrollCounter),a
ret
.ends

.section "ScrollUp" free
; Scroll screen up if not already at top border
; Includes code to handle wrap around and 'scroll jumping'.
ScrollUp:
  ld a,SCROLLED_UP
  ld (VScrollStatus),a
  ld a,(ScrollY)
  dec a
  cp $ff
  jp nz,+
    ld a,223
    jp ++
  +:
  cp 199
  jp nz,++
    xor a
    ld (VScrollStatus),a ; signal no scrolling to the outside..
    ld a,200
  ++:
  ld (ScrollY),a
ret
.ends

.section "ScrollDown" free
; Scroll screen down if not already at bottom border
; Includes code to handle wrap around and 'scroll jumping'.
ScrollDown:
  ld a,SCROLLED_DOWN ; signal to rest of the game that we have scrolled down.
  ld (VScrollStatus),a
  ld a,(ScrollY)
  inc a
  cp $ff
  jp nz,+
    ld a,31
  jp ++
  +:
  cp 57
  jp nz,++
    xor a
    ld (VScrollStatus),a ; no scrolling anyway
    ld a,56
  ++:
  ld (ScrollY),a
ret
.ends

.ramsection "Function variables" slot 3
; 'Behind-the-scenes' variables, used only by the functions above.
  Keys db
.ends

.section "ScoreToHighscore" free

Score2Highscore:
;OMG, time for ugly hacks now....
; Test to see if current score > hiscore.

       ld a,(Record)       ; load pointer to new record flag.
       cp 0                ; is it set already (hiscore beaten)?
       jp z,_CheckScore         ; no - go on to test against score.

; Hiscore is beaten, so let hiscore mirror score.

       ld hl,Score         ; point to score.
       ld de,HiScore        ; point to hiscore.
       ld bc,$0005         ; 4 bytes (digits) to load.
       ldir                ; do it!
       jp _EndScore

_CheckScore:
       ld de,Score         ; point to score.
       ld hl,HiScore        ; point to hiscore.
       ld b,5              ; test 5 digits, from left to right.

-      ld a,(de)           ; load score digit
       cp (hl)             ; is hiscore digit > score digit?
       jp c,_EndScore         ; yes - break out of loop.
       inc hl              ; no - point to next hiscore digit.
       inc de              ; point to next score digit
       djnz -              ; compare up to four digits

       ld a,1              ; fall through = new record is set!
       ld (Record),a       ; set the flag, score > hiscore!

_EndScore:


.ends

.section "Fade" free
EraseData:
  .dw 0000 0000

DirtyFade:
  ld a,0
  ld b,1
  .rept 32
    push af
    push bc
    call AwaitFrame
    call AwaitFrame
    ld hl,EraseData
    call LoadCRam
    pop bc
    pop af
    inc a
  .endr

.ends
