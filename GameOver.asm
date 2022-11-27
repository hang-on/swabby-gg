
.section "SetupGameOver" free
SetupGameOver:

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

ld a,:GameOverTiles
ld (SLOT_2_CONTROL),a

ld hl,GameOverTiles
ld bc,GameOverTilesEnd-GameOverTiles
ld de,$0c00
call LoadVram

ld hl,GameOverPalette
ld a,0
ld b,16
call LoadCRam

ld hl,GameOverTileMap
ld bc,GameOverTileMapEnd-GameOverTileMap
ld de,$3b00
call LoadVram


ld hl,GameOverMessage
ld de,$3a4c ; set the cursor
call Print ; the Print routine is in the cutscenes.asm

ld hl,GameOverMusic
call PSGPlayNoRepeat


ld hl,ScoreMessage
ld de,$38cc
call Print



  ; Print score:
  ld hl,HiScore ;
  ld de,$38cc+(4*2)
  call PrintDigits

  ; Print hiscore:
  ld hl,Score ;
  ld de,$38cc+(4*2)+(10*2)
  call PrintDigits



; Reset sleep counter - used for delaying button response
ld a,STANDARD_SLEEP
ld (Sleep),a

; Turn screen on, enable vblank interrupts, spritesize 8x8 dots
ld a,ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
ld b,1
call SetRegister
ei

  ld a,RUN_GAME_OVER
  ld (GameState),a
  jp Main

  GameOverMessage:
    .asc "     GAME OVER"
    .db MOVE_CURSOR $cc $3a
    .asc " "
    .db END_STRING

.ends

.section "RunGameOver" free
RunGameOver:
  call PSGFrame
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
      ld a,SETUP_TITLE_SCREEN
      ld (GameState),a
      jp _EndSwitch
  +:
  call IsButton1Pressed
  jp nc,+
    ld a,SETUP_TITLE_SCREEN
    ld (GameState),a
    jp _EndSwitch
  +:
_EndSwitch:
  jp Main

.ends
