
.section "Level data" free
FlyerVerticalMovementTable:
  .rept 2
    .rept 32
      .db -1 0 0 0 ; Swoop down
    .endr
    .rept 64
      .db 1 0 0 0 ;
    .endr
    .rept 32
      .db -1 0 0 0
    .endr
  .endr

; Enemy meta sprite layouts
DevilMetaSprite:
  .db 4 ; Header, number of hardware sprites this meta sprite uses
  .db 0 0 8 8 ; Y-offsets
  .db 0 6 8 7 0 8 8 9 ; x-offset, char code pairs
DevilMetaSprite1:
  .db 4 ; Header, number of hardware sprites this meta sprite uses
  .db 0 0 8 8 ; Y-offsets
  .db 0 10 8 11 0 12 8 13 ; x-offset, char code pairs

ZombieMetaSprite: ; or more precisely: ShooterMetaSprite, since it is used by
  .db 6           ; all shooters.
  .db 0 0 8 8 16 16
  .db 0 14 8 15 0 16 8 17 0 18 8 19

ZombieMetaSprite1:
  .db 6
  .db 0 0 8 8 16 16
  .db 0 20 8 21 0 22 8 23 0 24 8 25

EnemyShotMetaSprite:
  .db 1
  .db 0 0 26

DummyScoreMetaSprite:
; Placeholder, until real scoring will be implemented
  .db 5
  .db 0 0 0 0 0
  .db 0 48 8 48 16 48 24 48 32 48

DummyLifeMetaSprite:
.db 2
.db 0 0
.db 0 64 10 64

; These are the dot patterns that the Swabby flying animation consists of.
; Wiggle-wiggle-wiggle-wiggle-wiggle-wink...repeat
SwabbyPatternPointerTable:
    .dw SwabbyFlying3 SwabbyFlying1 SwabbyFlying3
    .dw SwabbyFlying1 SwabbyFlying3 SwabbyFlying2
SwabbyPatternPointerTableEnd:

SwabbyShotMetaSprite: ; ?? meta ?? hmm...
  .db 1
  .db 0 0 32

; Byte-sized variables being initialized at every new level
; Just add label (word) and init value (byte) to this table to have it
; initialized at the start of every level...
InitData:
  ; Init engine and background/scroll system.
  .dw FrameCounter
  .db 00
  .dw ScrollY
  .db 56
  .dw ScrollX
  .db 0
  .dw HScrollCounter
  .db 0
  .dw MapCounter
  .db 0
  .dw NextMapColumn
  .db 0
  .dw NextNameTableColumn
  .db 25
  .dw ColumnFlag
  .db 0
  .dw EnemyScriptCounter
  .db 255
  .dw EnemyControl
  .db ACTIVATED
  .dw TrueVerticalOffset ; screen is scrolled to the bottom
  .db 0
  .dw VScrollStatus
  .db 0
  .dw HScrollStatus
  .db 0
  .dw SwabbyHit ; set to 1 by coll.handler if Swabby gets hit
  .db 0
  .dw EnemyShot
  .db 0
  .dw SwabbyShot
  .db 0

  ; Init Swabby
  .dw SwabbyY
  .db SWABBY_START_Y
  .dw SwabbyX
  .db SWABBY_START_X
InitDataEnd:

; EnemyScript macros
.macro FLYERS_HIGH
  .db FLYER_ENEMY ; activate Flyer
  .db 60 ; y
  .db 70 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 75 ; y
  .db 60 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 95 ; y
  .db \1 ; value to load into EnemyCounter
.endm

.macro FLYERS_MIDDLE
  .db FLYER_ENEMY ; activate Flyer
  .db 70 ; y
  .db 60 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 90 ; y
  .db 80 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 110 ; y
  .db \1 ; value to load into EnemyCounter
.endm

.macro FLYERS_MIXED
  .db FLYER_ENEMY ; activate Flyer
  .db 100 ; y
  .db 80 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 60 ; y
  .db 80 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 140 ; y
  .db \1 ; value to load into EnemyCounter
.endm

.macro FLYERS_LOW
  .db FLYER_ENEMY ; activate Flyer
  .db 140 ; y
  .db 60 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 125 ; y
  .db 60 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 110 ; y
  .db \1 ; value to load into EnemyCounter
.endm

.macro TWO_FLYERS
  .db FLYER_ENEMY ; activate Flyer
  .db 120 ; y
  .db 85 ; value to load into EnemyCounter
  .db FLYER_ENEMY ; activate Flyer
  .db 90 ; y
  .db \1 ; value to load into EnemyCounter
.endm

.macro SHOOTER
  .db SHOOTER_ENEMY ; activate shooter
  .db \1 ; y
  .db \2 ; ShooterShotCounter init value (delay until he shoots)
  .db \3 ; value to load into EnemyCounter
.endm

.macro NO_ENEMY_EVENT
.db 0 ; Nop ; disable enemy script for debugging
.db 0
.db \1 ; value to load into counter
.endm

Level0EnemyScript:
  NO_ENEMY_EVENT 50
  TWO_FLYERS 100
  FLYERS_MIXED 50
  TWO_FLYERS 180
  FLYERS_HIGH 40
  TWO_FLYERS 100
  FLYERS_MIDDLE 50
  SHOOTER 118 200 150 ; on wall
  TWO_FLYERS 180
  FLYERS_HIGH 70
  SHOOTER 118 200 40
  FLYERS_MIXED 170
  NO_ENEMY_EVENT 120
  FLYERS_HIGH 170
  SHOOTER 118 150 10 ; on path
  FLYERS_MIXED 120
  NO_ENEMY_EVENT 120
  SHOOTER 118 200 30 ; on path
  FLYERS_HIGH 170
  .db $ff ; deactivate enemy script handling = no more enemies this level.

Level1EnemyScript:
  NO_ENEMY_EVENT 50
  SHOOTER 129 200 20
  TWO_FLYERS 240
  TWO_FLYERS 100
  SHOOTER 129 10 10
  FLYERS_MIXED 10
  SHOOTER 49 10 10 ; on small hill
  FLYERS_LOW 170
  TWO_FLYERS 50
  SHOOTER 129 30 220 ; on ground
  FLYERS_HIGH 10
  SHOOTER 129 40 40
  FLYERS_MIDDLE 50
  FLYERS_MIXED 70
  SHOOTER 129 60 30
  FLYERS_MIXED 70
  TWO_FLYERS 50
  .db $ff ; deactivate enemy script handling = no more enemies this level.

Level2EnemyScript:
  NO_ENEMY_EVENT 50
  FLYERS_MIDDLE 50
  FLYERS_MIXED 70
  SHOOTER 129 10 40
  FLYERS_HIGH 40
  SHOOTER 129 40 70
  TWO_FLYERS 50
  FLYERS_MIXED 40
  TWO_FLYERS 60
  FLYERS_LOW 90
  SHOOTER 79 10 200
  TWO_FLYERS 40
  SHOOTER 129 21 50
  FLYERS_MIXED 30
  FLYERS_MIDDLE 50
  SHOOTER 129 21 50
  FLYERS_MIXED 40
  FLYERS_MIDDLE 40
  .db $ff ; deactivate enemy script handling = no more enemies this level.

Level0SpritePalette:
  .dw $0A0F $0000 $000A $010F $03AA $07FF $0AAA $0FFF $0025 $009F $00DF $0A05 $0DA0 $0FF6
    ; from spritepal4 and shooter.gif, straigh from BMP2Tile

Level1SpritePalette:
  .dw $0A0F $0000 $000A $010F $03AA $07FF $0AAA $0FFF $04F4 $0080 $00D4 $01F6 $0330 $0AA0
Level2SpritePalette:
  .dw $0A0F $0000 $000A $010F $03AA $07FF $0AAA $0FFF $00A3 $0082 $06AF $013F $001D $0888 $017F $02D0

; N.B: See LEVEL_TABLE_ELEMENT_SIZE equate
; Assumes all assets in slot 0 or 1, except the level tilemap which is assumed
; to occupy a whole bank, which is loaded into slot 2.
LevelTable:
  ; level 0
  .dw Level0Tiles ; tile data
  .dw Level0TilesEnd-Level0Tiles ; amount of tile data in bytes
  .dw Level0Palette ; palette data - maxx colors to 16
  .db $ff :Level0TileMap ; filler byte ($ff), and level tilemap bank number
  .dw Level0TileMap ; level tilemap base address
  .dw Level0Music ; Level background music base address (for PSGStart)
  .dw DevilFlying0 ; Flyer type patterns
  .dw Level0EnemyScript
  .dw DevilMetaSprite ; Flyer enemy meta sprite tables base address
  .dw ZombiePatterns ; shooter type pattern
  .dw ZombieMetaSprite
  .dw Level0SpritePalette

  ; level 1:
  .dw Level1Tiles ; tile data
  .dw Level1TilesEnd-Level1Tiles ; amount of tile data in bytes
  .dw Level1Palette ; palette data - maxx colors to 16
  .db $ff :Level1TileMap ; filler byte ($ff), and level tilemap bank number
  .dw Level1TileMap ; level tilemap base address
  .dw Level1Music ; Level background music base address (for PSGStart)
  .dw BeholderPatterns ; Flyer type patterns
  .dw Level1EnemyScript
  .dw DevilMetaSprite ; Flyer enemy meta sprite tables base address
  .dw RedSkullPatterns ; shooter type pattern
  .dw ZombieMetaSprite
  .dw Level1SpritePalette

  ; level 2:
  .dw Level2Tiles ; tile data
  .dw Level2TilesEnd-Level2Tiles ; amount of tile data in bytes
  .dw Level2Palette ; palette data - maxx colors to 16
  .db $ff :Level2TileMap ; filler byte ($ff), and level tilemap bank number
  .dw Level2TileMap ; level tilemap base address
  .dw Level2Music ; Level background music base address (for PSGStart)
  .dw NinjaPatterns ; Flyer type patterns
  .dw Level2EnemyScript
  .dw DevilMetaSprite ; Flyer enemy meta sprite tables base address
  .dw SoldierPatterns ; shooter type pattern
  .dw ZombieMetaSprite
  .dw Level2SpritePalette

LevelTableEnd:

DigitTiles:
; Digits from 0-9, to load into sprite table. From digits.gif under misc.
; Loaded into tilebank index 48
; Tile index $031
.db $3C $00 $00 $00 $7E $00 $3C $00 $FF $00 $66 $00 $FF $00 $66 $00 $FF $66 $00 $00 $99 $66 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00
; Tile index $032
.db $18 $00 $00 $00 $3C $00 $18 $00 $7C $00 $38 $00 $3C $00 $18 $00 $3C $18 $00 $00 $24 $18 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00
; Tile index $033
.db $3C $00 $00 $00 $7E $00 $3C $00 $FF $00 $66 $00 $FF $00 $66 $00 $7E $0C $00 $00 $4E $30 $00 $00 $81 $7E $00 $00 $7E $00 $00 $00
; Tile index $034
.db $3C $00 $00 $00 $7E $00 $3C $00 $FF $00 $66 $00 $7E $00 $0C $00 $6F $06 $00 $00 $99 $66 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00
; Tile index $035
.db $0C $00 $00 $00 $1E $00 $0C $00 $3E $00 $1C $00 $7E $00 $2C $00 $FE $4C $00 $00 $81 $7E $00 $00 $72 $0C $00 $00 $0C $00 $00 $00
; Tile index $036
.db $7C $00 $00 $00 $FE $00 $7C $00 $FC $00 $60 $00 $FE $00 $7C $00 $7F $06 $00 $00 $99 $66 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00
; Tile index $037
.db $3C $00 $00 $00 $7E $00 $3C $00 $FC $00 $60 $00 $FE $00 $7C $00 $FF $66 $00 $00 $99 $66 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00
; Tile index $038
.db $7E $00 $00 $00 $FF $00 $7E $00 $FF $00 $66 $00 $7E $00 $08 $00 $3C $18 $00 $00 $24 $18 $00 $00 $24 $18 $00 $00 $18 $00 $00 $00
; Tile index $039
.db $3C $00 $00 $00 $7E $00 $3C $00 $FF $00 $66 $00 $7E $00 $3C $00 $FF $66 $00 $00 $99 $66 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00
; Tile index $03A
.db $3C $00 $00 $00 $7E $00 $3C $00 $FF $00 $66 $00 $FF $00 $66 $00 $7F $3E $00 $00 $39 $06 $00 $00 $42 $3C $00 $00 $3C $00 $00 $00


LifeTile: ; little Swabby
.db $00 $00 $00 $00 $78 $00 $00 $00 $BC $78 $00 $00 $9E $78 $00 $00 $85 $78 $02 $00 $7B $00 $06 $00 $FF $00 $7E $00 $FE $00 $00 $00


.ends
