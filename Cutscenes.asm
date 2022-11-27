; Cutsecenes


.section "Setup and run the cutscenes" free
SetupCutScene:
  call PSGStop
  di
  ; Turn screen off, enable vblank interrupts, spritesize 8x8 dots
  ld a,DISABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
  ld b,1
  call SetRegister
  ; Reset v and h scroll.
  xor a
  ld b,8
  call SetRegister
  xor a
  ld b,9
  call SetRegister

  call ClearVram

  ld a,:FontTiles
  ld (SLOT_2_CONTROL),a

  ld hl,FontTiles
  ld bc,FontTilesEnd-FontTiles
  ld de,0
  call LoadVram

  ld hl,FontPalette
  ld a,16
  ld b,16
  call LoadCRam

  ld hl,BreakoutSwabbyTiles
  ld bc,BreakoutSwabbyTilesEnd-BreakoutSwabbyTiles
  ld de,$0c00
  call LoadVram

  ld hl,BreakoutSwabbyPalette
  ld a,0
  ld b,16
  call LoadCRam

  ld hl,BreakoutSwabbyTileMap
  ld bc,BreakoutSwabbyTileMapEnd-BreakoutSwabbyTileMap
  ld de,$3b00
  call LoadVram


    ld hl,ScoreMessage
    ld de,$38cc
    call Print


    ; Print hiscore:
    ld hl,HiScore ;
    ld de,$38cc+(4*2)
    call PrintDigits

    ; Print score:
    ld hl,Score ;
    ld de,$38cc+(4*2)+(10*2)
    call PrintDigits



  ; Write the cut scene message to the name table
  ; Get correct message in HL...
  ld a,(CurrentLevel)
  add a,a
  ld hl,MessageTable
  ld d,0
  ld e,a
  add hl,de
  ld e,(hl)
  inc hl
  ld d,(hl)
  ex de,hl
  ld de,$3a4c ; set the cursor
  call Print

  ; Turn screen on, enable vblank interrupts, spritesize 8x8 dots
  ld a,ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
  ld b,1
  call SetRegister

  ; Reset sleep counter - used for delaying button response
  ld a,STANDARD_SLEEP
  ld (Sleep),a


  ld a,RUN_CUT_SCENE
  ld (GameState),a
  ei
  jp Main

Print:
  ; Print characters on screen. Assume the tile data to be from first pos.
  ; in pattern generator. Prepare vram, then output one char at a time, until
  ; we encounter $ff terminator char.
  ; Entry: HL = Base address of string.
  ;        DE = Start address on name table (where to print the string?)
; Uses AF,BC,DE,HL


  ; First, prepare vram for writes to the name table
  ld a,e
  out (CONTROL_PORT),a
  ld a,d
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a

  -:
    ld a,(hl)
    cp END_STRING
    ret z
    cp MOVE_CURSOR
    call z,MoveCursor
    out (DATA_PORT),a
    ld a,$08
    out (DATA_PORT),a
    inc hl
  jp -

ret

MoveCursor:
; We encountered the control charater MOVE_CURSOR.
; load next two bytes (little endian) from the string. Use these to update these
; to update the vram control port target address.
; Entry: HL = string, at the MOVE_CURSOR control char
; Exit: A = next char in string, ready for output, HL incremented as well.
; USES AF, HL

  inc hl
  ld a,(hl)
  out (CONTROL_PORT),a
  inc hl
  ld a,(hl)
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a
  inc hl
  ld a,(hl)
ret

MessageTable:
  .dw Level0Message Level1Message Level2Message

Level0Message:
  .asc "    FIRST ROUND"
  .db MOVE_CURSOR $cc $3a
  .asc " * THE GRAVEYARD *"
  .db END_STRING

Level1Message:
  .asc "    SECOND ROUND"
  .db MOVE_CURSOR $cc $3a
  .asc "   * GREEN ZONE *"
  .db END_STRING

Level2Message:
  .asc "     LAST ROUND"
  .db MOVE_CURSOR $cc $3a
  .asc "    * THE CITY *"
  .db END_STRING

RunCutScene:
  ; call PSGFrame
  call GetKeys

  ; If Sleep timer is up, then test for button press
  ld a,(Sleep)
  or a
  jp z,+
    dec a
    ld (Sleep),a
    jp _EndSwitch
  +:

  ; Start level on start button or button 1.
  call IsStartPressed
  jp nc,+
      ld a,SETUP_LEVEL
      ld (GameState),a
      jp _EndSwitch
  +:
  call IsButton1Pressed
  jp nc,+
    ld a,SETUP_LEVEL
    ld (GameState),a
    jp _EndSwitch
  +:
_EndSwitch:
  jp Main
.ends
