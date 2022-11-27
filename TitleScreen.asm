.section "Setup and run the title screen" free
SetupTitleScreen:
  ; This function has two primary purposes: Of course, it loads the vram assets
  ; ect. for the title screen. But it also initializes a new game,
  ; thus initializing level, lives, etc.
  call PSGStop

  ; PART II: Load assets for titlescreen
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

  ld a,:TitleScreenTiles
  ld (SLOT_2_CONTROL),a

  ld hl,FontTiles
  ld bc,FontTilesEnd-FontTiles
  ld de,0
  call LoadVram

  ld hl,FontPalette
  ld a,16
  ld b,16
  call LoadCRam

  ld hl,TitleScreenTiles
  ld bc,176*32
  ld de,$0c00
  call LoadVram

  ld hl,TitleScreenPalette
  ld a,0
  ld b,16
  call LoadCRam

  ld hl,TitleScreenTileMap
  ld bc,32*24*2
  ld de,NAME_TABLE_START
  call LoadVram

  ld hl,TitleMusic
  call PSGPlay


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


  ; Turn screen on, enable vblank interrupts, spritesize 8x8 dots
  ld a,ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
  ld b,1
  call SetRegister


  ld a,RUN_TITLE_SCREEN
  ld (GameState),a

  ei
  jp Main

PrintDigits:
; First, prepare vram for writes to the name table
  ld a,e
  out (CONTROL_PORT),a
  ld a,d
  or VRAM_WRITE_COMMAND
  out (CONTROL_PORT),a
  ld b,5
  -:
    ld a,(hl)
    add a,16
    out (DATA_PORT),a
    ld a,$08
    out (DATA_PORT),a
    inc hl
  djnz -
ret



ScoreMessage:
  .asc " HI-      SCO-"
  .db END_STRING

RunTitleScreen:

  call PSGFrame
  call GetKeys
  call IsStartPressed
  jp nc,+
    call PSGStop
    call DirtyFade
    ld a,SETUP_CUT_SCENE
    ld (GameState),a
    ; PART I: Initialize variables for a new game
    call NewGame ; level number,etc., once per game
    call IsLeftPressed
    jp nc,SkipCheat
      ld a,1
      ld (CheatMode),a
    SkipCheat:

  +:
  jp Main
.ends

.section "New game" free
; An attempt is when the player starts a new game. A game lasts until victory
; or until the lives are depleted (resulting in game over).
NewGame:
  ; Start level
  ld a,FIRST_LEVEL
  ld (CurrentLevel),a

  ; reset score
  ld hl,ScoreMetaSpriteInit+1 ; get the zeroes
  ld de,Score
  ld bc,5
  ldir

  ; reset meta sprite
  ld hl,ScoreMetaSpriteInit
  ld de,ScoreMetaSprite
  ld bc,5+10+1
  ldir

  ; Start with 2 extra lives = 3 attempts in total.
  ld a,2
  ld (Lives),a

  ld hl,LifeMetaSpriteInit
  ld de,LifeMetaSprite
  ld bc,7
  ldir

  xor a
  ld (CheatMode),a
  ld (Record),a

ret

ScoreMetaSpriteInit:
.db 5
.db 0 0 0 0 0
.db 0 48 8 48 16 48 24 48 32 48

LifeMetaSpriteInit:
.db 2
.db 0 0
.db 0 64 10 64


.ends
