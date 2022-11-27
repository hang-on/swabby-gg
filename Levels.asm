; -----------------------------------------------------------------------------
.section "SETUP LEVEL" free
; SetupLevel runs before the start of a new level
SetupLevel:

  di
  ; Turn screen off, enable vblank interrupts, spritesize 8x8 dots
  ld a,DISABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
  ld b,1
  call SetRegister

  call ClearVram

  ; Initialize byte-sized variables once per level start
  ld hl,InitData
  ld b,(InitDataEnd-InitData)/3
  call InitByteVars

  call ResetEnemies

  call MakeSwabbyCharCodes ; Put Swabby char code references in the SAT

  ld a,(CurrentLevel)
  call LoadLevelAssets

  ; Load the swabby assets
  ld hl,SwabbyTiles
  ld bc,6*32
  ld de,$2000
  call LoadVram
  ld hl,SwabbyShotTiles
  ld bc,32
  ld de,SWABBY_SHOT_PATTERN_ADDRESS
  call LoadVram

  ; Load digits
  ld hl,DigitTiles
  ld bc,10*32
  ld de,DIGITS_PATTERN_GENERATOR_ADDRESS
  call LoadVram

  ; load life tile (little Swabby)
  ld hl,LifeTile
  ld bc,32
  ld de,LIFE_PATTERN_GENERATOR_ADDRESS
  call LoadVram

  ld a,(Lives)
  cp 2
  jp nz,+
    ld a,64
    ld (LifeMetaSprite+4),a
    ld a,64
    ld (LifeMetaSprite+6),a
    jp ++
  +:
  cp 1
  jp nz,++
    ld a,64
    ld (LifeMetaSprite+6),a
  ++:

  ; Turn screen on, enable vblank interrupts, spritesize 8x8 dots
  ld a,ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
  ld b,1
  call SetRegister

  ld a,RUN_LEVEL
  ld (GameState),a

  ei
  jp Main

.ends
; -----------------------------------------------------------------------------

.section "Initialize byte-sized variables" free
InitByteVars:
; Entry: HL = Base address of data block with the following format:
;             variable (word) init value (byte)
;         B = number of one-byte variables to process.
; Uses AF, B, DE, HL
-:
  ld a,(hl)
  inc hl
  ld d,(hl)
  ld e,a
  inc hl
  ld a,(hl)
  ld (de),a
  inc hl
  djnz -

ret
.ends

.section "Reset Enemies" free
ResetEnemies:
  ; Deactivate all flying enemies.
  ld hl,ResetString
  ld de,FlyerStatus
  ld bc,3 ; Number of flyer type enemies.
  ldir
  ld hl,ResetString
  ld de,ShooterStatus
  ld bc,3 ; Number of shooter type enemies.
  ldir

ret
ResetString:
  .db DEACTIVATED DEACTIVATED DEACTIVATED
.ends

.section "Make Swabby Character Codes" free
; Put charcodes 0-5 into the first 6 sprites in the sat (Swabby's sprites).
; NOTE: Via the buffer!
MakeSwabbyCharCodes:
  ld hl,SpriteBufferXC+1
  xor a
  ld b,6
  -:
    ld (hl),a
    inc a
    inc hl
    inc hl
  djnz -
ret
.ends

.section "Load level assets" free
; Entry: A = level number
LoadLevelAssets:
  ld hl,-LEVEL_TABLE_ELEMENT_SIZE
  add hl,sp ; set stack pointer so there is room for level data
  ld sp,hl
  ex de,hl ; destination: stack space

  ; Point HL to correct index in level table
  ld hl,LevelTable
  or a ; if this is level 0, skip applying offset
  jp z,+
    ld b,a ; else apply offset: current level x level table element size
    xor a
    -:
      add a,LEVEL_TABLE_ELEMENT_SIZE
    djnz -
    ld b,0
    ld c,a
    add hl,bc ; apply offset
  +:
  ld bc,LEVEL_TABLE_ELEMENT_SIZE
  ldir ; Load all the level assets on the stack. Now ready for popping...

  pop hl ; level tiles base address
  pop bc ; amount of bytes
  ld de,00
  call LoadVram ; Load level tiles into vram

  pop hl ; Base address of level palette data (expect 16 words)
  ld a,0
  ld b,16
  call LoadCRam ; load palette into color ram

  pop af ; get the tilemap's bank number
  ld (MapBank),a
  pop hl ; get tilemap's base address
  ld (MapBase),hl

  pop hl ; get music
  call PSGPlay

  ; Load flyer enemy patterns into designated pattern generator space.
  pop hl
  ld bc,FLYER_PATTERN_DATA_SIZE
  ld de,FLYER_PATTERN_GENERATOR
  call LoadVram

  ; Load enemy script pointer
  pop hl
  ld (EnemyScriptPointer),hl

  pop hl
  ld (FlyerMetaSpritePointer),hl

  pop hl ; Get shooter dot patterns
  ld bc,SHOOTER_PATTERN_DATA_SIZE+36 ; this is the shot pattern.
  ld de,SHOOTER_PATTERN_GENERATOR
  call LoadVram

  pop hl ; shooter meta sprite pointer.
  ld (ShooterMetaSpritePointer),hl

  pop hl
  ; Load various sprite assets
  ld a,16
  ; ld hl,SpritePalette
  ld b,16
  call LoadCRam

ret
.ends

; -----------------------------------------------------------------------------
.section "RunLevel" free
RunLevel:
; Output a new column to the name table if flag is set
  ld a,(ColumnFlag)
  or a
  jp z,+
    ld a,(NextNameTableColumn)
    ld hl,ColumnBuffer
    call OutputColumn
    xor a
    ld (ColumnFlag),a
  +:

; Load new tiles for Swabby if animation flag is set
  ld a,(AnimFlag)
  or a
  call nz,LoadSwabbyPatterns

  ; Adjust the screen scroll to reflect the scroll variables
  ld a,(ScrollX)
  ld b,HORIZONTAL_SCROLL_REGISTER
  call SetRegister
  ld a,(ScrollY)
  ld b,VERTICAL_SCROLL_REGISTER
  call SetRegister

  call LoadSAT

; NON-VBLANK STUFF...

; Test to see if current score > hiscore.

       ld a,(Record)       ; load pointer to new record flag.
       cp 0                ; is it set already (hiscore beaten)?
       jp z,CheckScore         ; no - go on to test against score.

; Hiscore is beaten, so let hiscore mirror score.

       ld hl,Score         ; point to score.
       ld de,HiScore        ; point to hiscore.
       ld bc,$0005         ; 4 bytes (digits) to load.
       ldir                ; do it!
       jp EndScore

CheckScore:
       ld de,Score         ; point to score.
       ld hl,HiScore        ; point to hiscore.
       ld b,5              ; test 5 digits, from left to right.

-      ld a,(de)           ; load score digit
       cp (hl)             ; is hiscore digit > score digit?
       jp c,EndScore         ; yes - break out of loop.
       inc hl              ; no - point to next hiscore digit.
       inc de              ; point to next score digit
       djnz -              ; compare up to four digits

       ld a,1              ; fall through = new record is set!
       ld (Record),a       ; set the flag, score > hiscore!

EndScore:

; Start of not-so-nice coll.detect.

; If enemy shot is active, then test if it collides with Swabby
ld a,(EnemyShot)
or a
jp z,_NoEnemyShot ; no active shot = no coll. test.
  ld ix,Obj1.x
  ld a,(SwabbyX)
  ld (ix+0),a
  ld a,(SwabbyY)
  ld (ix+1),a
  ld (ix+2),16
  ld (ix+3),10

  ld a,(EnemyShotX)
  ld (ix+4),a
  ld a,(EnemyShotY)
  ld (ix+5),a
  ld (ix+6),4
  ld (ix+7),4
  call DetectCollision
  jp c,_NoEnemyShot ; not optimal here!

    ; This is where Swabby hit code goes  !!
    ld a,1
    ld (SwabbyHit),a


_NoEnemyShot:

; If Swabby shot is active, then test if it collides with one of the active
; flyer type enemies.
ld a,(SwabbyShot)
or a
jp z,_NoSwabbyShot
  ld ix,Obj1.x
  ld a,(SwabbyShotX)
  ld (ix+0),a
  ld a,(SwabbyShotY)
  ld (ix+1),a
  ld (ix+2),4 ; Swabby shot width
  ld (ix+3),4 ; ... and height

  ; all flyers are 16x16, box is a little smaller
  ld (ix+6),12
  ld (ix+7),12
  ld b,3 ;three flyers to test.
  ld iy,FlyerStatus
  FlyerTestLoop:
    ld a,(iy+0)
    or a
    jp nz,+ ; this flyer is not active
      ;inc iy ; next flyer
      jp _EndLoop
    +:
    ; This flyer is active. See if it overlaps.
    ld a,(iy+6) ; get x
    ld (ix+4),a ; put it in parameter
    ld a,(iy+3) ; get y
    ld (ix+5),a ; put in parameter
    call DetectCollision
    jp c,_EndLoop
      ; not carry = collision!
      xor a
      ld (iy+0),a ; deactivate this flyer.
      ld (SwabbyShot),a ; deactivate swabby shot
      ; earn points:
      ld hl,Score+2
      ld b,1
      call AddScore

    _EndLoop:
      inc iy
      djnz FlyerTestLoop

    ; See if Swabby hits one of the shooters...
    ; all shooters are 16x24, box is a little smaller
    ld (ix+6),12
    ld (ix+7),19
    ld b,3 ;three shooters to test.
    ld iy,ShooterStatus
    ShooterTestLoop:
      ld a,(iy+0)
      or a
      jp z,_EndShooterTestLoop ; this shooter is not active

      ; This shooter is active. See if it overlaps.
      ld a,(iy+6) ; get x
      ld (ix+4),a ; put it in parameter
      ld a,(iy+3) ; get y
      ld (ix+5),a ; put in parameter
      call DetectCollision
      jp c,_EndShooterTestLoop
        ; not carry = collision!
        xor a
        ld (iy+0),a ; deactivate this shooter.
        ld (SwabbyShot),a ; deactivate swabby shot
        ; earn points:
        ld hl,Score+2
        ld b,3
        call AddScore
      _EndShooterTestLoop:
        inc iy
        djnz ShooterTestLoop

_NoSwabbyShot:

; Test if is hit by a flyer.
ld ix,Obj1.x
ld a,(SwabbyX)
ld (ix+0),a
ld a,(SwabbyY)
ld (ix+1),a
ld (ix+2),18 ; Swabby width
ld (ix+3),11 ; ... and height

; all flyers are 16x16, box is a little smaller
ld (ix+6),11
ld (ix+7),11
ld b,3 ;three flyers to test.
ld iy,FlyerStatus
FlyerTestLoop2:
  ld a,(iy+0)
  or a
  jp z,_EndLoop2

  ; This flyer is active. See if it overlaps.
  ld a,(iy+6) ; get x
  ld (ix+4),a ; put it in parameter
  ld a,(iy+3) ; get y
  ld (ix+5),a ; put in parameter
  call DetectCollision
  jp c,_EndLoop2
    ; not carry = collision!
    ; Cheat mode code
      ld a,(CheatMode)
      or a
      jp nz,_EndLoop2
    xor a
    ld (iy+0),a ; deactivate this flyer.
    ld (SwabbyShot),a ; deactivate swabby shot
    ld a,1
    ld (SwabbyHit),a

  _EndLoop2:
    inc iy
    djnz FlyerTestLoop2


; Does Swabby collide with one of the shooters?
ld (ix+6),12
ld (ix+7),19
ld b,3 ;three shooters to test.
ld iy,ShooterStatus
ShooterTestLoop2:
  ld a,(iy+0)
  or a
  jp z,_EndShooterTestLoop2 ; this shooter is not active

  ; This shooter is active. See if it overlaps.
  ld a,(iy+6) ; get x
  ld (ix+4),a ; put it in parameter
  ld a,(iy+3) ; get y
  ld (ix+5),a ; put in parameter
  call DetectCollision
  jp c,_EndShooterTestLoop2
    ; not carry = collision!
    xor a
    ld (iy+0),a ; deactivate this shooter.
    ld (SwabbyShot),a ; deactivate swabby shot
    ld a,1
    ld (SwabbyHit),a
  _EndShooterTestLoop2:
    inc iy
    djnz ShooterTestLoop2


; ---- end of coll.detect....


  xor a                 ; Assume no scrolling until otherwise proven.
  ld (VScrollStatus),a  ; This value is changed by ScrollUp/Down routines.
  ld (HScrollStatus),a  ; this is set by HandleHScroll

  call GetKeys

  call AnimateSwabby

  ; Swabby movement - test keys and update Swabby's position accordingly...
  call IsUpPressed
  call c,MoveUp
  call IsDownPressed
  call c,MoveDown
  call IsLeftPressed
  call c,MoveLeft
  call IsRightPressed
  call c,MoveRight

  call UpdateSwabbyMetaSprite

  call IsButton1Pressed
  call c,FireSwabbyShot

  ; Update the TrueVerticalOffset if the player scrolled the screen. So we can
  ; apply offset to ground-based enemies
  ld a,(VScrollStatus)
  cp SCROLLED_UP
  jp nz,+
    ld hl,TrueVerticalOffset
    inc (hl)
  +:
  cp SCROLLED_DOWN
  jp nz,+
    ld hl,TrueVerticalOffset
    dec (hl)
  +:

  ; Handle the HScrollCounter and prepare to scroll horizontally at next vblank
  ; if counter is at max value
  call HandleHScroll

  ld a,(EnemyControl)
  or a
  call  nz,HandleEnemyScript


  call BeginMetaSprites

  call HandleFlyers

  call HandleShooters

  call HandleSwabbyShot

  call HandleEnemyShot

  ; update score :
  ld hl,Score
  ld de,ScoreMetaSprite+7 ; point to first charcode/digit
  ld b,5
  -:
    ld a,(hl)
    add a,48 ; concert score value to digit charcode
    ld (de),a
    inc de
    inc de
    inc hl
  djnz -

  ld a,SCORE_Y
  ld b,SCORE_X
  ld hl,ScoreMetaSprite
  call AddMetaSprite

  ; put lives in the SAT
  ld a,LIFE_Y
  ld b,LIFE_X
  ld hl,LifeMetaSprite
  call AddMetaSprite

  call FinalizeMetaSprites


  ; see if Swabby is hit (loose life, etc, start death routine)
  ld a,(SwabbyHit)
  or a
  jp z,EndSwabbyHitHandler
    ; Play soundfx
    ld hl,DeathTune
    call PSGPlayNoRepeat
    ld b,120
    _DirtyLoop:
      halt
      push bc
      call PSGFrame
      pop bc
    djnz _DirtyLoop

    ld a,(Lives)
    cp 0
    jp nz,+
      ld a,SETUP_GAME_OVER   ; can also be setup game over?!
      ld (GameState),a
      jp EndSwabbyHitHandler
    +:
    dec a
    ld (Lives),a
    cp 1
    jp nz,+
      ld a,65
      ld (LifeMetaSprite+4),a
      ld a, SETUP_LEVEL ; restart level (not so cruel)
      ld (GameState),a
      jp EndSwabbyHitHandler
    +:
      ld a,65
      ld (LifeMetaSprite+6),a
      ld a, SETUP_LEVEL ; restart level (not so cruel)
      ld (GameState),a
      jp EndSwabbyHitHandler
  EndSwabbyHitHandler:

  ; Music
  .ifndef NOMUSIC
    call PSGFrame
  .endif

  ; Prepare to load a new column at the coming vblank, if the MapCounter = 0
  ld a,(MapCounter)
  or a
  call z,PrepareNewColumn

  ; Increment FrameCounter at each frame...
  ld hl,FrameCounter
  inc (hl)

  ; Update the map counter
  ld a,(MapCounter)
  inc a
  cp MAP_COUNTER_MAX
  jp nz,+
    xor a
  +:
  ld (MapCounter),a

  ; For development: Skip to next level on start button:
  ;call IsStartPressed
  ;call c,SkipToNextLevel

  ; For development: Skip to next level on NextMapColumn = 255
  ld a,(NextMapColumn)
  cp 255
  call z,SkipToNextLevel
  ;jp nz,+
  ;  inc a
  ;  ld (NextMapColumn),a
  ;  call SkipToNextLevel
  ;+:
  jp Main
.ends
; -----------------------------------------------------------------------------

.section "Swabby shooting" free
FireSwabbyShot:
  ld a,(SwabbyShot)
  or a  ; does one shot already exist?
  jp nz,+
    ; no, it does not exist, so activate a shot
    cpl
    ld (SwabbyShot),a ; put $ff in Swabbyshot to indicate that he is now firing.
    ld a,(SwabbyX)
    add a,16
    ld (SwabbyShotX),a
    ld a,(SwabbyY)
    add a,4
    ld (SwabbyShotY),a
  +:
ret

HandleSwabbyShot:
; Uses AF, BC, HL
  ld a,(SwabbyShot)
  or a ; is a shot active?
  ret z ; just return if not active shot
  ld a,(SwabbyShotX)
  inc a ; move the shot to the right...
  inc a
  ; test for outside
  cp 210
  jp c,+
    xor a ; deactivate the shot.
    ld (SwabbyShot),a
    ret
  +:
  ld (SwabbyShotX),a
  ld b,a

  ; Apply additional adjustment if player is scrolling the screen right now..
  ld c,0
  ld a,(VScrollStatus)
  cp SCROLLED_UP
  jp nz,+
    inc c
  +:
  cp SCROLLED_DOWN
  jp nz,+
    dec c
  +:

  ld a,(SwabbyShotY)
  add a,c
  ld (SwabbyShotY),a

  ld hl,SwabbyShotMetaSprite
  call AddMetaSprite
ret
.ends

.section "Enemies" free
  ; Decrement the enemy script counter. When the counter reaches zero, then
  ; read and process next instruction from the enemy script.
HandleEnemyScript:
  ld a,(EnemyScriptCounter)
  dec a
  ld (EnemyScriptCounter),a
  or a
  ret nz
    ; Time for a new enemy event
    ld hl,(EnemyScriptPointer)
    ld a,(hl) ; skal laves til jump table struktur!
    cp 0
    jp nz,+
      ; Nop script element, just load counter and forward pointer.
      inc hl
      inc hl
      ld a,(hl)
      ld (EnemyScriptCounter),a
      inc hl
      ld (EnemyScriptPointer),hl
      jp _EndSwitch
    +:
    cp FLYER_ENEMY ; Activate new flyer type enemy.
    jp nz,+
      call GetDeactivatedEnemy ; Now IX points to a deactivated flyer's status.
      jp c,_EndSwitch ; jump out on error - this makes the new flyer pending?
      ld a,ACTIVATED
      ld (ix+0),a
      inc hl
      ld a,(hl) ; load flyer start y from script
      ld (ix+3),a
      ld a,(FLYER_START_X)
      ld (ix+6),a
      inc hl
      ld a,(hl)
      ld (EnemyScriptCounter),a
      inc hl
      ld (EnemyScriptPointer),hl
      xor a ; let's see if it is better to keep this flowing...
      ld (ix+15),a ; Reset FlyerVerticalMovementTableIndex
      jp _EndSwitch
    +:
    cp SHOOTER_ENEMY ; Activate new shooter type enemy.
    jp nz,+
      call GetDeactivatedEnemy ; Now IX points to a deactivated shooter's status.
      jp c,_EndSwitch ; jump out on error - this makes the new shooter pending?
      ld a,ACTIVATED
      ld (ix+0),a
      ; apply TrueVerticalOffset to the y from the script
      ld a,(TrueVerticalOffset)
      ld b,a
      inc hl
      ld a,(hl) ; load flyer start y from script
      add a,b
      ld (ix+3),a
      ld a,FLYER_START_X ; should be FLYER_START_X
      ld (ix+6),a
      inc hl
      ld a,(hl)
      ld (ix+15),a ; ShooterShotCounter init value
      inc hl
      ld a,(hl)
      ld (EnemyScriptCounter),a
      inc hl
      ld (EnemyScriptPointer),hl
      jp _EndSwitch
    +:
    cp $ff ; turn off script events.
    jp nz,+
      ; Turn off enemy script events for the rest of the level
      ld a,DEACTIVATED
      ld (EnemyControl),a
      jp _EndSwitch
    +:
  _EndSwitch:
ret

; The next three routine, GetDeactivatedEnemy, DeactivateEnemy and AnimateEnemy.
; are shared by both flyer and shooter type enemies.

GetDeactivatedEnemy:
; Search for next deactivated enemy. When found, return pointer to this enemy's
; status byte in IX.
; Entry: A = type of enemy (flyer 1 or shooter 2)
; Exit: IX points to deactivated enemy's status. If no deactivated enemies of
; the desired type were found, then carry is set as an error indicator.
; Uses AF, BC, DE, IX  - HL is preserved (it points to EnemyScript)
  push hl
  cp FLYER_ENEMY ; should we get a deactivated flyer?
  jp nz,+
    ld hl,FlyerStatus
    jp ++
  +:            ; else get a deactivated shooter
    ld hl,ShooterStatus
  ++:
  or a ; clear carry
  ld a,DEACTIVATED
  cpi
  jp z,+
  cpi
  jp z,+
  cpi
  jp z,+
  ; fall through to all-full here... (no deactivated flyers at the moment)
  scf ; set carry flag to indicate an error
  +:
  dec hl ; carry is preserved
  ex de,hl ; carry is preserved
  pop hl ; carry is preserved - HL is back to EnemyScriptPointer
  push de ; load IX with flyer status pointer
  pop ix
ret

DeactivateEnemy:
; Purpose: Deactivate this flyer
; Entry: HL = flyer status
; Exit: DEACTIVATED is written to the flyerstatus array element.
; HL (and maybe BC?) is preserved, AF is destroyed
  ld a,DEACTIVATED
  ld (hl),a
ret

AnimateEnemy:
  ; Increment anim counter. If it reaches a certain value, toggle animframe.
  ; Entry: HL Pointer to current enemy status.
  ;        A = Expired anim counter constant
  ; Exit: AnimCounter, and possibly AnimFrame updated.
  ; Uses AF, Saves BC
  push bc
  push hl
  pop ix
  ld b,a
  ld a,(ix+9)
  inc a
  cp b ; compare to expired contant
  jp nz,+
    ld a,(ix+12) ; get flyer anim frame.
    cpl ; toggle (invert all bits)
    ld (ix+12),a
    xor a ; reset anim counter
  +:
  ld (ix+9),a
  pop bc
ret

HandleFlyers:
; Go through the three possible flyers one at a time. If the status is
; ACTIVATED then add a metasprite.
  ld b,0
  ld hl,FlyerStatus
  -:
    ld a,(hl)
    or a
    jp z,+ ; We need to save BC and HL registers in the following...
      ld a,FLYER_ANIM_COUNTER_EXPIRED
      call AnimateEnemy
      call MoveFlyer ; - and set carry if flyer is at kill mark.
      call c,DeactivateEnemy
      call DrawFlyer
    +:
    inc hl
    inc b
    ld a,b
    cp 3
    ret z ; return when we have processed all flyers.
  jp -

MoveFlyer:
  ; Move this flyer according to std. flyer pattern. This pattern is:
  ; 1 pixel pr. frame, from right to left. Vertical movement is regulated
  ; by FlyerVerticalMovementTable.
  ; Enty HL = Pointer to current flyer status
  ; Exit: This flyer's x and y are updated in the flyer array
  ;       If this moves the flyer outside on the left of the screen, set carry
  ;       to indicate that this flyer is ready to be deactivated.
  ; AF and DE are destroyed, BC an HL are preserved
  push hl ; save HL so it can be preserved
  push bc
  push hl ; load the contents of HL into IX
  pop ix

  ; Vertical movement:
  ld a,(ix+15) ; get vertical movement index
  ld hl,FlyerVerticalMovementTable ; base address classic move.
  ld d,0
  ld e,a
  add hl,de
  inc a
  ld (ix+15),a
  ld a,(hl)
  ld b,a
  ld a,(ix+3) ; flyer Y
  add a,b ; apply value found in table.
  ld (ix+3),a ; load it back into the array.

  ; Apply additional adjustment if player is scrolling the screen right now..
  ld a,(VScrollStatus)
  cp SCROLLED_UP
  jp nz,+
    inc (ix+3)
  +:
  cp SCROLLED_DOWN
  jp nz,+
    dec (ix+3)
  +:

  ; Horizontal movement:
  ld a,(ix+6) ; get flyer x.
  or a ; reset carry flag
  dec a ;
  cp 20 ; flyer kill mark
  jp nz,+
    scf ; set carry to indicate that this one is ready for deactivation..
  +:
  ld (ix+6),a ; keep the carry flag preserved at all costs from here on...!
  pop bc
  pop hl
ret

DrawFlyer:
; Entry HL = pointer to current flyer status
; Put the corresponding flyer on the screen this frame...
; AF and DE are destroyed.
  ; Save registers.
  push hl
  push bc

  push hl
  pop ix
  ; Retrieve y and x, and meta sprite pointer.
  ld a,(ix+3) ; Load master y into A
  ld b,(ix+6) ; Load master x into B
  call GetFlyerMetaSpritePointer

  call AddMetaSprite

  pop bc
  pop hl
ret

GetFlyerMetaSpritePointer:
; Given a flyer status pointer, return a pointer to the correct meta sprite
; table. This is determined by the flyer anim frame, which is either
; 00 or $ff
; Entry: HL = pointer to flyer status.
; Exit: HL = pointer to meta sprite table
; AF and BC must stay intact! DE and IX are used
  push af
  push hl
  pop ix
  ld a,(ix+12) ; get FlyerAnimFrame.
  or a
  jp z,+
    ld hl,(FlyerMetaSpritePointer)
    pop af
    ret
    +:
  ld hl,(FlyerMetaSpritePointer) ; needs adjustment!!!
  ld de,13
  add hl,de
  pop af
ret

HandleShooters:
; Go through the three possible shooters one at a time. If the status is
; ACTIVATED then add a metasprite.
  ld b,0
  ld hl,ShooterStatus
  -:
    ld a,(hl)
    or a
    jp z,+ ; We need to save BC and HL registers in the following...
      ld a,(EnemyShot)
      or a
      call z,FireShooterShot ; no shot currently active, so check shooter count.
      ld a,SHOOTER_ANIM_COUNTER_EXPIRED
      call AnimateEnemy
      call MoveShooter ; - and set carry if shooter is at kill mark.
      call c,DeactivateEnemy
      call DrawShooter
    +:
    inc hl
    inc b
    ld a,b
    cp 3
    ret z ; return when we have processed all shooters.
  jp -

FireShooterShot:
; If shooter counter is up, then reset counter to 255 and fire a shot.
; preserve HL and BC! <<<<<
; Entry: HL = Pointer to shooter status
  push hl
  pop ix
  ld a,(ix+15)
  dec a
  ld (ix+15),a
  or a
  ret nz ; if not zero, then just return for no

  ; counter is up! Time to fire!
  ld a,40
  ld (ix+15),a ; rapid fire!
  ld a,ACTIVATED
  ld (EnemyShot),a
  ld a,(ix+6)
  ld (EnemyShotX),a
  ld a,(ix+3)
  ld (EnemyShotY),a
  call CalcEnemyShotVMove ; return value in A... save regs!
  ld (EnemyShotVMove),a
ret

CalcEnemyShotVMove:
; !! take care about registers
; Entry: IX, pointer to shooter status, it reads Swabby y,x
; Exit: A = value to load into EnemyShotVMove
; uSes AF, saves BC - and may´be HL, if needed!
  push bc
  ld a,(SwabbyY)
  ld b,a
  ld a,(ix+3)
  sub b ; compare Swabby and shooter
  jp c,+ ; if shooter is above Swabby, then shoot downwards
    sub 30 ; Swabby is over the shooter, but how much...?
    jp nc,++ ; use R register as random reg, to determine
      ld a,r ; if shooter should shoot straight or with a little angle.
      or a
      jp po,_StraightShot
        ld a,-1 ; not very much, not very steep angled shot
        jp +++
      _StraightShot:
        ld a,0
        jp +++
    ++:
      ld a,-2 ; very much, steep angled shot
      jp +++
    +: ; shoot downwards, shooter is above Swabby
    ld a,1
  +++:
  pop bc
ret

HandleEnemyShot:
; Uses AF, BC, HL
  ld a,(EnemyShot)
  or a ; is a shot active?
  ret z ; just return if not active shot
  ld a,(EnemyShotX)
  dec a ; move the shot to the right...
  dec a
  ; test for outside
  cp 45
  jp nc,+
    xor a ; deactivate the shot.
    ld (EnemyShot),a
    ret
  +:
  ld (EnemyShotX),a
  ld b,a

  ; Apply additional adjustment if player is scrolling the screen right now..
  ld c,0
  ld a,(VScrollStatus)
  cp SCROLLED_UP
  jp nz,+
    inc c
  +:
  cp SCROLLED_DOWN
  jp nz,+
    dec c
  +:

  ld a,(EnemyShotY)
  add a,c

  ld c,a
  ld a,(EnemyShotVMove)
  add a,c

  ld (EnemyShotY),a

  ld hl,EnemyShotMetaSprite
  call AddMetaSprite
ret

DrawShooter:
; Entry HL = pointer to current shooter status
; Put the corresponding shooter on the screen this frame...
; AF and DE are destroyed.
  ; Save registers.
  push hl
  push bc

  push hl
  pop ix
  ; Retrieve y and x, and meta sprite pointer.
  ld a,(ix+3) ; Load master y into A
  ld b,(ix+6) ; Load master x into B
  call GetShooterMetaSpritePointer

  call AddMetaSprite

  pop bc
  pop hl
ret

GetShooterMetaSpritePointer:
; Given a shooter status pointer, return a pointer to the correct meta sprite
; table. This is determined by the shooter anim frame, which is either
; 00 or $ff
; Entry: HL = pointer to shooter status.
; Exit: HL = pointer to meta sprite table
; AF and BC must stay intact! DE and IX are used
  push af
  push hl
  pop ix
  ld a,(ix+12) ; get shooterAnimFrame.
  or a
  jp nz,+
    ld hl,(ShooterMetaSpritePointer)
    pop af
    ret
    +:
  ld hl,(ShooterMetaSpritePointer) ; needs adjustment!!!
  ld de,19 ; next element
  add hl,de
  pop af
ret

MoveShooter:
; Enty HL = Pointer to current shooter status
; Exit: This shooter's x and y are updated in the array
;       If this moves the shooter outside on the left of the screen, set carry
;       to indicate that this shooter is ready to be deactivated.
; AF is destroyed, HL is preserved
; YOU CAN ONLY USE AF and IX NOW!!

  push hl ; load the contents of HL into IX
  pop ix

  ; Apply Y adjustment if player is scrolling the screen right now..
  ld a,(VScrollStatus)
  cp SCROLLED_UP
  jp nz,+
    inc (ix+3)
  +:
  cp SCROLLED_DOWN
  jp nz,+
    dec (ix+3)
  +:

  ; see if screen scrolls this frame
  ld a,(HScrollStatus)
  or a
  ret z ; no h.movement if no scroll.

  ; move w. screen.
  ld a,(ix+6) ; get shooter x.
  or a ; reset carry flag
  dec a ;
  cp 20 ; shooter kill mark
  jp nz,+
    scf ; set carry to indicate that this one is ready for deactivation..
  +:
  ld (ix+6),a ; keep the carry flag preserved at all costs from here on...!

  ret
.ends

.ramsection "AddMetaSprite variables" slot 3
  MasterY db
  MasterX db
.ends

.section "MetaSprites" free
BeginMetaSprites:
; Reset the NextFreeSprite index at the beginning of every frame
  ld a,FIRST_FREE_SPRITE
  ld (NextFreeSprite),a
ret

FinalizeMetaSprites:
  ; Put the terminator in the buffer
  ld de,SpriteBufferY
  ld a,(NextFreeSprite)
  add a,e
  ld e,a
  ld a,0
  adc a,d
  ld d,a
  ld a,SPRITE_TERMINATOR
  ld (de),a
ret
.ends

.section "AddMetaSprite" free
AddMetaSprite:
; Entry: NextFreeSprite is the first buffer position.
; HL = Base address of meta sprite array. First byte is meta sprite size.
; A = Master Y
; B = Master X
; Procedure: First the program processes the Y-offsets. Each offset is
; applied to the master Y, and the result is saved in SpriteBufferY. Then the
; X-offsets are processed, and stored pairwise with char codes in SpriteBufferXC
; Exit: SpriteBufferY and SpriteBufferXC is updated. NextFreeSprite is
; incremented.
; Assume master y,x to be top left corner

  ; Save the parameters in ram
  ld (MasterY),a
  cp 169 ; just outside lcd area
  ret nc ; quick and dirty sprite clipping technique
  ld a,b
  ld (MasterX),a

  ; Point DE to SpriteBufferY[NextFreeSprite]
  ld a,(NextFreeSprite)
  ld de,SpriteBufferY
  add a,e
  ld e,a
  ld a,0
  adc a,d
  ld d,a

  ; Get size of metasprite and put it in B
  ld a,(hl)
  inc hl ; now hl points to first y-offset
  ld b,a ; put size in B, so we can use DJNZ

  ; Apply offsets to MasterY, and save the results in the SpriteBufferY
  push bc ; save the counter
  ld a,(MasterY)
  ld c,a
  -:
    ld a,(hl)
    add a,c
    ld (de),a
    inc hl
    inc de
  djnz -
  pop bc ; restore counter in B

  ; Apply offsets to MasterX, and save results, together with charcodes, in
  ; SpriteBufferXC.
  ; HL is pointing at the first x-offset. B holds the size.
  push bc ; save the counter
  ; First, point DE to SpriteBufferXC[NextFreeSprite]
  ; Point DE to SpriteBufferY[NextFreeSprite]
  ld a,(NextFreeSprite)
  add a,a ; because we are having x,charcode pairs (words) in this buffer.
  ld de,SpriteBufferXC
  add a,e
  ld e,a
  ld a,0
  adc a,d
  ld d,a ; Now, DE is pointing correctly into the SpriteBufferXC

  ld a,(MasterX)
  ld c,a
  -:
    ld a,(hl)
    add a,c
    ld (de),a
    inc hl
    inc de
    ld a,(hl) ; get the char code
    ld (de),a
    inc hl
    inc de
  djnz -

  pop bc ; restore counter
  ld a,(NextFreeSprite) ; add the size of the processed metasprite
  add a,b
  ld (NextFreeSprite),a

ret
.ends

.section "AnimateSwabby" free
; Handle animation by checking timer, setting flags and loading pointer to
; new tile data.
AnimateSwabby:
  ld a,(AnimCounter)
  inc a
  cp 35
  ld (AnimCounter),a
  ret nz

  xor a
  ld (AnimCounter),a

  ld a,(CelCounter)
  inc a
  cp (SwabbyPatternPointerTableEnd-SwabbyPatternPointerTable)/2
  jp nz,+
    xor a
  +:
  ld (CelCounter),a
  add a,a
  ld hl,SwabbyPatternPointerTable
  ld d,0
  ld e,a
  add hl,de
  ld e,(hl)
  inc hl
  ld d,(hl)
  ld (SwabbyPatternPointer),de

  ld a,1
  ld (AnimFlag),a
ret
.ends

.section "MoveSwabby" free
MoveLeft:
  ld a,(SwabbyX)
  dec a
  cp 49
  ret z
  ld (SwabbyX),a
ret

MoveRight:
  ld a,(SwabbyX)
  inc a
  cp 185
  ret z
  ld (SwabbyX),a
ret

MoveUp:
  ld a,(SwabbyY)
  cp 62 ; Should we scroll?
  jp nz,+
    ld b,a ; Save Swabby Y
    ld a,(ScrollY)
    cp $c8
    ld a,b
    jp z,+
    call ScrollUp
    ret
  +:
  cp UPPER_LIMIT
  ret z
  dec a
  ld (SwabbyY),a
ret

MoveDown:
  ld a,(SwabbyY)
  cp 118 ; Should we scroll?
  jp nz,+
    ld b,a ; Save Swabby Y
    ld a,(ScrollY)
    cp $38
    ld a,b
    jp z,+
    call ScrollDown
    ret
  +:
  cp LOWER_LIMIT
  ret z
  inc a
  ld (SwabbyY),a
ret
.ends

.section "UpdateSwabbyMetaSprite" free
; Update (in the sprite buffers) the 6 sprites that make up Swabby
; Assume: SwabbyX and SwabbyY is the top left corner of the Swabby meta sprite.
; The swabby meta sprite is at sprite positions 0-5 in the sprite tables
UpdateSwabbyMetaSprite:
  ; Make y-coordinates
  ld hl,SpriteBufferY
  ld a,(SwabbyY)
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
  add a,8
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
  ld (hl),a

  ; Make x-coordinates (character codes are made with MakeSwabbyCharCodes)
  ld hl,SpriteBufferXC
  .rept 2
    ld a,(SwabbyX)
    ld (hl),a
    inc hl
    inc hl
    add a,8
    ld (hl),a
    inc hl
    inc hl
    add a,8
    ld (hl),a
    inc hl
    inc hl
  .endr

ret
.ends

.section "Skip to next level " free
SkipToNextLevel:
; Skip to next level. If we are at the last level, then back to level 0
; INSERT: Before we do this, reward player for lives left.
  call PSGStop

  ld hl,CutSceneMusic
  call PSGPlayNoRepeat

  ld b,0
  -:
    push bc
    call AwaitFrame
    call PSGFrame
    pop bc
  djnz -


  ld a,(Lives)
  cp 0
  jp z,EndBonus

  ; One or more extra lives left. Reward player...

  cp 2
  jp nz,+
    ld a,65
    ld (LifeMetaSprite+4),a
    ld hl,Score+1
    ld b,1
    call AddScore
    call RewardPlayer
  +:
    ld a,65
    ld (LifeMetaSprite+6),a
    ld hl,Score+1
    ld b,1
    call AddScore
    call RewardPlayer

    ; HUSk at opdatere highscore inden exit! Det må være i final!
EndBonus:

  call Score2Highscore
  call Score2Highscore
  call DirtyFade

  ld a,(CurrentLevel)
  inc a
  cp LAST_LEVEL+1
  jp nz,+
    ;xor a
    ;ld (CurrentLevel),a
    ld a, SETUP_FINAL
    ld (GameState),a
    ret
  +:
  ld (CurrentLevel),a
  ld a, SETUP_CUT_SCENE
  ld (GameState),a
ret

RewardPlayer:
call BeginMetaSprites
; update score :
ld hl,Score
ld de,ScoreMetaSprite+7 ; point to first charcode/digit
ld b,5
-:
  ld a,(hl)
  add a,48 ; concert score value to digit charcode
  ld (de),a
  inc de
  inc de
  inc hl
djnz -

ld a,SCORE_Y
ld b,SCORE_X
ld hl,ScoreMetaSprite
call AddMetaSprite

; put lives in the SAT
ld a,LIFE_Y
ld b,LIFE_X
ld hl,LifeMetaSprite
call AddMetaSprite

call FinalizeMetaSprites

  ld hl,LifeSound
  call PSGPlayNoRepeat

call AwaitFrame
call LoadSAT
ld b,100
MyLoop:
  push bc
  call AwaitFrame
  call PSGFrame
  pop bc
djnz MyLoop

call Score2Highscore
call Score2Highscore


ret

.ends

.ramsection "Collision detection variables" slot 3
  Obj1.x db       ; ix+0
  Obj1.y db       ; ix+1
  Obj1.width db   ; ix+2
  Obj1.height db  ; ix+3

  Obj2.x db       ; ix+4
  Obj2.y db       ; ix+5
  Obj2.width db   ; ix+6
  Obj2.height db  ; ix+7

.ends

.section "Collision detection" free
; Section containing collision detection and helper functions.

DetectCollision:
; Detect collision between two 2D axis aligned boxes. Return with carry set if
; no collision, else return with carry reset.
; Entry: The collision detection variables are assumed to be updated with
; object data.
; Exit: Carry flag set = no coll., reset = collision.

  push bc
  ld ix,Obj1.x

  ; a.x + a.width > = b.x
  ld a,(ix+4)
  ld b,a
  ld a,(ix+0)
  add a,(ix+2) ; Swabby width
  sub b
  jp c,_NoColl

  ; a.x <= b.x+b.widthbuild
  ld a,(ix+0) ; save a.x
  ld b,a
  ld a,(ix+4)
  add a,(ix+6) ; shot width
  sub b
  jp c,_NoColl

; a.y + a.height >= b.y
ld a,(ix+5)
ld b,a
ld a,(ix+1)
add a,(ix+3) ; Swabby height
sub b
jp c,_NoColl

; a.y <= b.y b.height
ld a,(ix+1)
ld b,a
ld a,(ix+5)
add a,(ix+7)
sub b
jp c,_NoColl

  ; If we fall through to thi place, we have a collision. Carry flag is reset
  ; from the last comparison.
  pop bc
ret

_NoColl:
  pop bc
  ret
.ends

; At the momemnt, this data goes into slot 0..?!
.include "Leveldata.asm"
