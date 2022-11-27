.sdsctag 1.11, "Swabby", "Beyond the Maze...", "Anders S. Jensen"

.asciitable
  ; all ascii chars from 32 (space) to 90 (Z) = 59 chars in total.
  map " " to "Z" = 0
.enda

.memorymap
  defaultslot 2
  slotsize $4000
  slot 0 $0000
  slot 1 $4000
  slot 2 $8000
  slotsize $2000
  slot 3 $c000
.endme

.rombankmap
  bankstotal 8
  banksize $4000
  banks 8
.endro

.bank 0 slot 0
.section "Music" free
Level0Music:
  .incbin "sfx\altered4_comp.psg"

Level1Music:
  .incbin "sfx\hedgehog_comp.psg"

Level2Music:
  .incbin "sfx\ninjetti_comp.psg"

TitleMusic:
  .incbin "sfx\beyond_comp.psg"

CutSceneMusic:
  .incbin "sfx\happycut_comp.psg"

DeathTune:
.incbin "sfx\death_comp.psg" ; renamed cutscene


GameOverMusic:
  .incbin "sfx\gameover_comp.psg"

FinaleMusic:
.incbin "sfx\finale_comp.psg"

LifeSound:
.incbin "sfx\life_comp.psg"


.ends


.bank 1 slot 1
.include "const.inc"
.include "vdplib.asm"
.include "functions.asm"
.include "psglib.inc"

; Level assets
.include "levels\Level2Tiles.inc"
.include "levels\Level2Palette.inc"
.include "levels\Level1Tiles.inc"
.include "levels\Level1Palette.inc"
.include "levels\Level0Tiles.inc"
.include "levels\Level0Palette.inc"

; Sprite assets
.include "gfx\SpritePalette.inc"
.include "gfx\swabby_tiles.inc"
.include "gfx\EnemyTiles.inc"

.bank 2 slot 2
.include "levels\Level0TileMap.inc"

.bank 3 slot 2
.include "levels\Level1TileMap.inc"

.bank 4 slot 2
.include "levels\Level2TileMap.inc"

.bank 5 slot 2
.include "titlescreen\titlescreen_tiles.inc"
.include "titlescreen\titlescreen_tilemap.inc"
.include "titlescreen\titlescreen_palette.inc"
.include "fontdata.inc"
.include "cutscenegfx.inc"

.bank 6 slot 2
.include "finalegfx.inc"


.org 0
.bank 0 slot 0
.section "Boot" force
  di
  im 1
  ld sp,STACK_ADDRESS

  ; Initialize standard Sega mapper
  call InitializeMapper

  jp SetupMain
.ends

.org $0038
.section "!VDP interrupt" force
  push af
  exx
  in a,CONTROL_PORT
  ld (VDPStatus),a
  exx
  pop af
  ei
  reti
.ends

.org $0066
.section "!Pause interrupt" force
  retn
.ends

.section "SetupMain" free
; Setup runs once per session...
SetupMain:
  ; Clear ram and vram
  call ClearRam ; except the top (stack, slot/bank registers, etc.)
  call ClearVram

  ; Initialize vdp registers
  ld hl,RegisterInitData
  call InitializeRegisters

  ; Initialize psglib
  call PSGInit

  ; Development!
  ld hl,DevilMetaSprite
  ld a,1
  call AddMetaSprite

  ld a,SETUP_TITLE_SCREEN
  ld (GameState),a
  ei
  jp Main

  RegisterInitData:
      .db $06
      .db DISABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
      .db $ff,$ff,$ff,$ff,$ff,$00,$00,$00,$ff

.ends

.section "Main" free
Main:
  call AwaitFrame
  ld a,(GameState)
  add a,a
  ld d,0
  ld e,a
  ld hl,VectorTable
  add hl,de

  ld a,(hl)
  inc hl
  ld h,(hl)
  ld l,a
  jp (hl)

  jp Main ; we should never get here...!

VectorTable:
  .dw SetupTitleScreen RunTitleScreen SetupCutScene RunCutScene
  .dw SetupLevel RunLevel SetupGameOver RunGameOver SetupFinal
  .dw RunFinal
.ends

; Each module has its own file
.include "TitleScreen.asm"
.include "Cutscenes.asm"
.include "Levels.asm"
.include "GameOver.asm"
.include "Final.asm"

.ramsection "Variables" slot 3
  CheatMode db
  Score dsb 5
  HiScore dsb 5
  Record db

  FrameCounter db
  VDPStatus db
  GameState db

  ; Background and scroll variables
  TrueVerticalOffset db ; From 0 (bottom) to 80 (scrolled to top of map)
  VScrollStatus db ; see constants, updated per frame to reflect vert. scroll.
  HScrollStatus db ; set if screen scrolled horizontally this frame, else 0.
  ScrollX db
  ScrollY db
  HScrollCounter db
  MapCounter db
  NextMapColumn db
  NextNameTableColumn db
  ColumnBuffer dsb 56
  ColumnFlag db
  MapBank db
  MapBase dw

  ; Sprite and game object variables
  SpriteBufferY dsb 64
  SpriteBufferXC dsb 128
  NextFreeSprite db

  ; Swabby's variables:
  SwabbyY db
  SwabbyX db
  AnimFlag db
  AnimCounter db
  CelCounter db
  SwabbyPatternPointer dw
  SwabbyHit db


  ; Flyer type enemy variables, array [0-2]
  ; Flyers are 2 x 2 metasprites, with two anim cels, that are stored in
  ; pattern generator positions 6-9 and 10-13 (right after Swabby)
  ; NOTE! DrawFlyer is depending on this structure
  FlyerStatus dsb 3 ; Activated or deactivated
  FlyerY dsb 3 ; Master Y position for the flyer meta sprite
  FlyerX dsb 3
  FlyerAnimCounter dsb 3
  FlyerAnimFrame dsb 3
  ; --- From here on: Specific to flyer:
  FlyerVerticalMovementTableIndex dsb 3 ; table is part of the level data.

  FlyerMetaSpritePointer dw ; base address of meta sprite data (y,x offsets,cc)

  ; Shooter type enemy.
  ShooterStatus dsb 3
  ShooterY dsb 3
  ShooterX dsb 3
  ShooterAnimCounter dsb 3
  ShooterAnimFrame dsb 3
  ShooterShotCounter dsb 3
  ShotIndex dsb 3
  ShooterLife dsb 3

  ShooterMetaSpritePointer dw

  ; Swabby Shot array:
  SwabbyShot db ; activated or deactivated
  SwabbyShotY db
  SwabbyShotX db

  ; EnemyShot array:
  EnemyShot db
  EnemyShotY db
  EnemyShotX db
  EnemyShotVMove db ; vertical move each frame
  EnemyShotHMove db ; horizontal move each frame.


  EnemyControl db
  EnemyScriptCounter db
  EnemyScriptPointer dw


  ; Other variables
  CurrentLevel db

  ScoreMetaSprite dsb 16
  LifeMetaSprite dsb 7
  Lives db

  Sleep db

  Temp db



  ;DummyScoreMetaSprite:
  ; Placeholder, until real scoring will be implemented
  ;  .db 5
  ;  .db 0 0 0 0 0
  ;  .db 0 48 8 48 16 48 24 48 32 48

.ends
