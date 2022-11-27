; development constants
;.equ NOMUSIC ; comment this out to turn on the background music during levels

; Game logic constants
.equ MAP_COUNTER_MAX 16 ; Controls how fast the background scrolls
.equ HSCROLL_COUNTER_MAX MAP_COUNTER_MAX/8
.equ FIRST_LEVEL 0 ; Which level to start the game in... (for debugging)
.equ LAST_LEVEL 2
.equ SWABBY_START_Y 94
.equ SWABBY_START_X 50
.equ DEACTIVATED 0
.equ ACTIVATED 1

; Constants for the vscroll status byte.
.equ NO_VERTICAL_SCROLLING 0
.equ SCROLLED_UP 1
.equ SCROLLED_DOWN 2


; Game state:
.equ SETUP_TITLE_SCREEN 0
.EQU RUN_TITLE_SCREEN 1
.EQU SETUP_CUT_SCENE 2
.EQU RUN_CUT_SCENE 3
.EQU SETUP_LEVEL 4
.EQU RUN_LEVEL 5
.EQU SETUP_GAME_OVER 6
.EQU RUN_GAME_OVER 7
.EQU SETUP_FINAL 8
.EQU RUN_FINAL 9

; Vram constants
.equ HORIZONTAL_SCROLL_REGISTER 8
.equ VERTICAL_SCROLL_REGISTER 9
.equ SAT_Y_START $3f00
.equ SAT_XC_START SAT_Y_START+64+64

; Pattern generator map
; 6 Swabby (flying)
; 8 Flyer type enemy ( 2 x 2 metasprite, 2 cels)
; 12 Shooter type enemy (2x3 metasprite, to cels)

.equ UPPER_LIMIT 36
.equ LOWER_LIMIT 136

.equ SCORE_Y 26
.equ SCORE_X 144
.equ LIFE_Y SCORE_Y
.equ LIFE_X SCORE_X + 3 + (5*8)

.equ END_STRING $ff
.equ MOVE_CURSOR $fe

.EQU    CONTROL_PORT  $BF              ;VDP CONTROL PORT
.EQU    DATA_PORT  $BE              ;VDP DATA PORT
.EQU    VRAM_WRITE_COMMAND   %01000000 		;WRITE-ENABLE VDP DATA PORT COMMAND
.EQU    REGISTER_WRITE_COMMAND   %10000000        ;REGISTER WRITE COMMAND
.EQU    CRAM_WRITE_COMMAND   %11000000        ;COLOR RAM UPDATE COMMAND
.equ NAME_TABLE_START $3800

.equ ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES %11100000 ;$e0
.equ DISABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES %10100000 ;$a0


; Other constants
.equ SLOT_2_CONTROL $ffff
.equ JOYSTICK_PORT_1 $dc
.equ JOYSTICK_PORT_2 $dd
.equ MISC_PORT $00 ; bit 7 is Start button on/off
.EQU STACK_ADDRESS  $DFF0 ;STACK INIT ADDRESS
.equ MUSIC_BANK 6

.equ LEVEL_TABLE_ELEMENT_SIZE 24 ;NewLevelTableEnd-NewLevelTable

.equ LIFE_PATTERN_GENERATOR_ADDRESS $2800
.equ DIGITS_PATTERN_GENERATOR_ADDRESS $2600
.equ SWABBY_SHOT_PATTERN_ADDRESS $2400
.equ ENEMY_SHOT_PATTERN_ADDRESS $2340

.equ FLYER_ENEMY 1
.equ FLYER_PATTERN_GENERATOR $2000+(6*32)
.equ FLYER_PATTERN_DATA_SIZE 4*2*32 ; amount of bytes of pattern data
.equ FLYER_START_X 210 ; for some reason it's important to keep this at 210 ?!
                        ; also used by shooter, should be renamed to enemy...
.equ FLYER_ANIM_COUNTER_EXPIRED 10

.equ SHOOTER_ENEMY 2
.equ SHOOTER_PATTERN_GENERATOR FLYER_PATTERN_GENERATOR+FLYER_PATTERN_DATA_SIZE
.equ SHOOTER_PATTERN_DATA_SIZE 6*2*32
.equ SHOOTER_ANIM_COUNTER_EXPIRED 40
.equ SHOOT_STRAIGHT 0
.equ SHOOT_UP 1
.equ SHOOT_DOWN 2


; From where does the meta sprite engine take over. Below is 6 sprites for
; Swabby, 4 sprites for 4 shots, and 5 sprites for score.
.equ FIRST_FREE_SPRITE 6+4+5
.equ SPRITE_TERMINATOR $d0

.equ STANDARD_SLEEP 80
