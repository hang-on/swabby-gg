
.section "SetupFinal" free
SetupFinal:

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

ld a,:FinaleTiles
ld (SLOT_2_CONTROL),a

ld hl,FinaleTiles
ld bc,FinaleTileEnd-FinaleTiles
ld de,0
call LoadVram

ld hl,FinalePalette
ld a,0
ld b,16
call LoadCRam

ld hl,FinaleTileMap
ld bc,32*24*2
ld de,NAME_TABLE_START
call LoadVram

ld hl,FinaleMusic
call PSGPlay


; Reset sleep counter - used for delaying button response
ld a,STANDARD_SLEEP
ld (Sleep),a

; Turn screen on, enable vblank interrupts, spritesize 8x8 dots
ld a,ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
ld b,1
call SetRegister

ei

  ld a,RUN_FINAL
  ld (GameState),a

  jp Main

.ends

.section "RunFinal" free
RunFinal:
call PSGFrame

; If Sleep timer is up, then test for button press
ld a,(Sleep)
or a
jp z,+
  dec a
  ld (Sleep),a
  jp _EndSwitch
+:

call GetKeys
call IsStartPressed

jp nc,+
  ld a,SETUP_TITLE_SCREEN
  ld (GameState),a
+:
call IsButton1Pressed
jp nc,_EndSwitch
  ld a,SETUP_TITLE_SCREEN
  ld (GameState),a
_EndSwitch:


  jp Main
.ends
