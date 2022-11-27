; Flyerlib
; Provide at set of encapsulated functions to simplify displaying and moving
; a simple 2x2 type of standard flying enemy, here called a 'flyer'.

; Dependencies and assumptions:

.equ FLYER_PATTERN_GENERATOR_ADDRESS $2000+(6*32)

.section "Init" free
; Load flyer patterns into designated space in pattern generator.
; Load 2 x 4 patterns. To be called once at the beginning of a new level.
; Entry: HL = Base address of 2 x 4 patterns
;
InitFlyers:
  ld bc,8*32 ; always load 8 patterns
  ld de,FLYER_PATTERN_GENERATOR_ADDRESS
  call LoadVram
  ret
.ends
