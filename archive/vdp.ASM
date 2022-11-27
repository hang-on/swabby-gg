;                                                                       ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;       Title           VDP library                                     ;
;                                                                       ;
;                                                                       ;

        ;GENERAL CONSTANTS
.EQU    BUFFER $C000            ;256 BYTES RAM BUFFER FOR SCRATCHPAD USE
.EQU    CPORT  $BF              ;VDP CONTROL PORT
.EQU    DPORT  $BE              ;VDP DATA PORT
.EQU    WCOM   %01000000 		;WRITE-ENABLE VDP DATA PORT COMMAND


.SECTION "ALNOUT" FREE
;                                                                       ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;       Title           Aligned output                                  ;
;       Name:           ALNOUT                                          ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;       Purpose:        Output a number of words from a buffer to vram, ;
;						given a destination address and an alignment.   ;
;                                                                       ;
;		Procedure:		The program exits immediately if the the number ;
;						of words to output is 0. Otherwise, it outputs  ;
;						words to vram one at a time until the number of ;
;						words to output is 0. The program aligns the    ;
;						words by incrementing the destination address   ;
;						by a given value after each word is output.     ;
;																	    ;
;                                                                       ;
;       Entry:          A = Alignment value 							;
;						B = Number of words to output					;
;						Register pair HL = Base address of buffer       ;
;						Register pair DE = Destination address in vram  :
;                                                                       ;
;       Exit:           None											;
;                                                                       ;
;       Registers used: AF,BC,DE,HL                                     ;
;                                                                       ;
;       Time:           172 cycles for each word                        ;
;                                                                       ;
;       Size:           Program 34 bytes                                ;
;                                                                       ;
;                                                                       ;

ALNOUT:
		LD     C,A				;SAVE ALIGNMENT VALUE
		
OUTLP:
        LD     A,B				;GET WORD COUNTER
		OR	   A				;RETURN IF NO MORE WORDS TO WRITE
		RET    Z
		
		CALL   OUTWRD           ;OUTPUT ONE WORD TO VRAM
		
		DEC    B                ;DECREMENT WORD COUNTER
        
        ;APPLY ALIGNMENT VALUE TO DESTINATION ADDRESS
        LD     A,C              ;GET ALIGNMENT BACK
        ADD    A,E              ;APPLY ALIGNMENT VALUE USING 8-BIT
        LD     E,A              ; ADDITION TO AVOID DISTURBING
        LD     A,0              ; ANOTHER REGISTER PAIR
        ADC    A,D
        LD     D,A 
        JR     OUTLP
		

        ;****************************************************************
        ;ROUTINE: OUTWRD
        ;PURPOSE: OUTPUT WORD FROM BUFFER TO DESTINATION IN VRAM
        ;ENTRY: HL = BUFFER ADDRESS
        ;       DE = DESTINATION ADDRESS
        ;EXIT:  HL = BUFFER ADDRESS + 2
        ;REGISTERS USED: AF, HL
        ;****************************************************************
OUTWRD:
        ;FORMAT AND OUTPUT COMMAND WORD
        LD     A,E              ;GET FIRST COMMAND BYTE (ADDRESS LSB)
        OUT    (CPORT),A        ;OUTPUT IT TO VDP CONTROL PORT
        LD     A,D              ;GET SECOND COMMAND BYTE (ADDRESS MSB)
        OR     WCOM             ;APPLY WRITE-ENABLE COMMAND BITS
        OUT    (CPORT),A        ;OUTPUT BYTE TO VDP CONTROL PORT
        
        ;OUTPUT ONE DATA WORD FROM BUFFER TO VRAM AND INC BUFFER POINTER
        LD     A,(HL)			;GET BYTE FROM BUFFER
		OUT    (DPORT),A		;OUTPUT IT TO DATA PORT (AUTO INCREMENT)
		INC    HL				;INCREMENT BUFFER POINTER
        LD     A,(HL)			;GET BYTE FROM BUFFER
		OUT    (DPORT),A		;OUTPUT IT TO DATA PORT (AUTO INCREMENT)
		INC    HL				;INCREMENT BUFFER POINTER
		RET
.ENDS

.SECTION "LOADVR" FREE
;                                                                       ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;       Title           Load video RAM                                  ;
;       Name:           LOADVR                                          ;
;                                                                       ;
;                                                                       ;
;                                                                       ;
;       Purpose:        Load a number of bytes from a source address    ;
;						to a specified destination address in either    ;
;                       the tile generator or the name table in vram.   ;
;                                                                       ;
;		Procedure:		The program sets up the vdp data port to        ;
;                       receive input starting at a specified address.  ;
;                       Then it loads a number of bytes to this port,   ;
;                       utilizing the vdp auto-increment feature.       ;
;                                                                       ;
;       Entry:          Register pair HL = Base address of source       ;
;                       Register pair DE = Base address of destination  ;
;                       Register pair BC = Number of bytes to load      ;
;                                                                       ;
;       Exit:           None											;
;                                                                       ;
;       Registers used: AF, BC, DE, HL                                  ;
;                                                                       ;
;       Time:           ..........                                      ;
;                                                                       ;
;       Size:           Program .. bytes                                ;
;                                                                       ;
;                                                                       ;


LOADVR:
        ;FORMAT AND OUTPUT COMMAND WORD
        LD     A,E              ;GET FIRST COMMAND BYTE (DEST. LSB)
        OUT    (CPORT),A        ;OUTPUT IT TO VDP CONTROL PORT
        LD     A,D              ;GET SECOND COMMAND BYTE (DEST. MSB)
        OR     WCOM             ;APPLY WRITE-ENABLE COMMAND BITS
        OUT    (CPORT),A        ;OUTPUT BYTE TO VDP CONTROL PORT

        ;LOAD BC NUMBER OF BYTES
-:        
        LD     A,(HL)           ;GET SOURCE BYTE
        OUT    (DPORT),A        ;OUTPUT IT TO VDP DATA PORT
        INC    HL               ;INCREMENT SOURCE POINTER
        DEC    BC               ;DECREMENT BYTE COUNTER
        LD     A,C              ;GET BYTE COUNTER
        OR     B                ;RETURN IF NO MORE BYTES TO WRITE
        JP     NZ,-             ; ELSE LOOP AGAIN
        RET
.ENDS

