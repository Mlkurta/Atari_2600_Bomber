    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with register mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an uninitialized segment at $80 for var declaration.
;; We have memory from $80 to $FF to work with, minus a few at
;; the end if we use the stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         .byte       ; P0XPos
JetYPos         .byte       ; P0YPos
ShipXPos        .byte       ; player 1 x-position
ShipYPos        .byte       ; player 1 y-position
MissileXPos     .byte       ; missile x-position
MissileYPos     .byte       ; missile y-position
Score           .byte       ; player 0 Score (2-Digit) stored as BCD
Timer           .byte       ; 2-digit timer value stored as BCD
Temp            .byte       ; auxillary variable to store variables
OnesDigitOffset .word       ; LUT offset for the score 1's digit
TensDigitOffset .word       ; LUT offset for the score 10's digit
JetSpritePtr    .word       ; pointer to player 0 sprite LUT
JetColorPtr     .word       ; pointer to player 0 color LUT
ShipSpritePtr   .word       ; pointer to player 1 sprite LUT
ShipColorPtr    .word       ; pointer to player 1 color LUT
JetAnimOffset   .byte       ; player 0 sprite frame offset for animation
ShipAnimOffset  .byte       ; player 1 sprite frame offset for animation
Random          .byte       ; random number generated to set enemy position
ScoreSprite     .byte       ; Store the sprite bit pattern for the score
TimerSprite     .byte       ; store the sprite bit pattern for the Timer
TerrainColor    .byte       ; store the color of the terrain
RiverColor      .byte       ; store the color of the river
IntervalByte    .byte       ; store values to update (not every frame)
FrameCounter    .byte       ; For use with frame by frame timing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9              ; player 0 sprite height (num Rows)
SHIP_HEIGHT = 9             ; player 1 sprite height
DIGITS_HEIGHT = 5           ; Digit Height (Playfield) rows in LUT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code segment starting at $F000.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset: 
    CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta JetYPos             ; JetYPos = 10

    lda #50                 ; JetXPos = 60
    sta JetXPos 

    lda #83 
    sta ShipYPos
    lda #54
    sta ShipXPos

    lda #0
    sta IntervalByte        ; Initialize Interval byte at 0
    sta FrameCounter

    lda #%11010100
    sta Random              ; Random = $D4
    lda #0
    sta Score               ; initialize score at 0
    sta Timer               ; Timer start =  0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos     ; compare missile X (current scanline with missile Y pos)
        bne .SkipMissileDraw; if X != missile Y position, skip
.DrawMissile:            
        lda #%00000010      ; Draw missile
        inc MissileYPos
.SkipMissileDraw:
        sta ENAM0           ; store the correct value into Missile draw register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to correct LUT addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr        ; lo-byte for jet sprite LUT
    lda #>JetSprite
    sta JetSpritePtr+1      ; hi-byte pointer for jet sprite LUT

    lda #<JetColor
    sta JetColorPtr         ; lo-byte for jet color LUT
    lda #>JetColor
    sta JetColorPtr+1       ; hi-byte pointer for jet color LUT

    lda #<ShipSprite
    sta ShipSpritePtr       ; lo-byte for ship sprite LUT
    lda #>ShipSprite
    sta ShipSpritePtr+1     ; hi-byte pointer for ship sprite LUT

    lda #<ShipColor
    sta ShipColorPtr        ; lo-byte for ship color LUT
    lda #>ShipColor
    sta ShipColorPtr+1      ; hi-byte pointer for ship color LUT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and rendering 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK              ; turn on VBLANK and VSYNC
    sta VSYNC
    REPEAT 3
        sta WSYNC           ; displays the 3 recommended lines of VSYNC
    REPEND
    lda #0
    sta VSYNC               ; disable VSYNC
    REPEAT 31 
        sta WSYNC
    REPEND   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos       ; set player0 horizontal position

    lda ShipXPos
    ldy #1
    jsr SetObjectXPos       ; set player1 horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos       ; set missile horizontal position   

    jsr CalculateDigitOffset; calculate the scoreboard digit LUT offset

    sta WSYNC
    sta HMOVE               ; apply horizontal offset 
        
    lda #0    
    sta VBLANK              ; turn VBLANK off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard the lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    lda #0
    sta COLUBK 
    sta PF0                 ; clear the TIA registers before each new frame
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF              ; disable playfield reflection

    lda #$1E                ; set playfield / scoreboard to yellow
    sta COLUPF

    ldx #DIGITS_HEIGHT      ; start X counter with 5 (height of digits)

.ScoreDigitLoop:
    ldy TensDigitOffset     ; Get the tens digit offset for the score
    lda Digits,Y            ; load bit pattern from LUT
    and #$F0                ; Mask/remove ones position
    sta ScoreSprite         ; save Tens digit pattern into variable

    ldy OnesDigitOffset     ; get the ones digit offset for the score
    lda Digits,Y            ; load the ones digit pattern
    and #$0F                ; Mask/remove tens digit position
    ora ScoreSprite         ; merge it with the saved tens digit sprite
    sta ScoreSprite         ; saved merged "nibbles" into a byte for Score
    sta WSYNC               ; wait for next scanline
    sta PF1                 ; update the playfield to display the score

    ldy TensDigitOffset+1   ; get the left digit offset for the timer
    lda Digits,Y            ; load digit pattern from the LUT
    and #$F0                ; mask/remove the ones digit
    sta TimerSprite         ; save the timer tens digit pattern in a variable

    ldy OnesDigitOffset+1   ; get the right (ones) digit offset for the timer
    lda Digits,Y            ; load digit pattern from the LUT
    and #$0F                ; mask/remove the tens digit
    ora TimerSprite         ; merge with the saved "nibbles" into a byte
    sta TimerSprite         ; save 

    jsr Sleep12Cycles       ; wastes some cycles

    sta PF1                 ; update PF1 (Timer display)

    ldy ScoreSprite         ; preload for the next scanline
    sta WSYNC               ; wait for next scanline

    sty PF1                 ; update playfield for score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1   ; increment all digits for the next line of data

    jsr Sleep12Cycles

    dex                     ; X--
    sta PF1                 ; update the playfield for the timer display
    bne .ScoreDigitLoop     ; If X != 0, then branch to ScoreDigitLoop

    sta WSYNC               ; when X==0, begin scoreboard buffer, wait for next scanline

    lda #0                  
    sta PF0
    sta PF1
    sta PF2                 ; clear the playfield for the buffer
    sta WSYNC               ; wait / draw three empty scanlines
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 84 visible scanlines (Two Line Kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
GameVisibleLine:
    lda TerrainColor
    sta COLUPF              ; set terrain background color

    lda RiverColor          
    sta COLUBK              ; set the river background color

    lda #%00000001          ; enable playfield reflection
    sta CTRLPF

    lda #$F0
    sta PF0                 ; Setting PF0 bit pattern
    lda #$FC             
    sta PF1  
    lda #0               
    sta PF2

    ldx #89                 ; X counts remaining scanlines
.GameLineLoop:
    DRAW_MISSILE            ; macro to check if we should draw the missile

.AreWeInsideJetSprite:
    txa                     ; transfer X to accumulator
    sec                     ; set carry before subtraction
    sbc JetYPos             ; Subtract JetYPos 
    cmp #JET_HEIGHT          ; are we inside the sprite?
    bcc .DrawSpriteP0       ; Draw SPrite
    lda #0                  ; else, set LUT index to zero
.DrawSpriteP0:
    clc                     ; clear carry flag before addition
    adc JetAnimOffset       ; jmp to the correct sprite frame address in memory
    tay
    lda (JetSpritePtr),Y    ; load Y so we can work with the pointer
    sta WSYNC               ; wait for scanline
    sta GRP0                ; Set graphics for P0
    lda (JetColorPtr),Y     ; load color from LUT
    sta COLUP0              ; set color player 0

.AreWeInsideShipSprite:
    txa                     ; transfer X to accumulator
    sec                     ; set carry before subtraction
    sbc ShipYPos            ; Subtract ShipYPos 
    cmp #SHIP_HEIGHT         ; are we inside the sprite?
    bcc .DrawSpriteP1       ; Draw SPrite
    lda #0                  ; else, set LUT index to zero
.DrawSpriteP1:
    clc
    adc ShipAnimOffset
    tay
    lda (ShipSpritePtr),Y   ; load Y so we can work with the pointer
    sta WSYNC               ; wait for scanline
    sta GRP1                ; Set graphics for P1
    lda (ShipColorPtr),Y    ; load color from LUT
    sta COLUP1              ; set color player 1

    lda #%00000101
    sta NUSIZ1              ; stretch P1 horizontally

    dex
    bne .GameLineLoop       ; X--, repeat until visible area is finished

    lda #0 
    sta JetAnimOffset

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC           ; 30 lines of VBLANK overscan
    REPEND

    lda #0
    sta VBLANK              ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000          ; player 0 joystick up
    bit SWCHA               ; 
    bne CheckP0Down         ; If not up, move to down
    lda JetYPos
    cmp #76
    bpl CheckP0Down
.P0UpPressed:
    inc JetYPos             ;
    lda #0
    sta JetAnimOffset       ; reset offset

CheckP0Down:
    lda #%00100000          ; player 0 joystick down
    bit SWCHA
    bne CheckP0Left         ; if not down, check left
    lda JetYPos
    cmp #5
    bmi CheckP0Left
.P0DownPressed:
    dec JetYPos
    lda #0
    sta JetAnimOffset

CheckP0Left:
    lda #%01000000          ; player 0 joystick left
    bit SWCHA
    bne CheckP0Right        ; if not left, check right
    lda JetXPos
    cmp #35
    bmi CheckP0Right
.P0LeftPressed:
    dec JetXPos
    lda #JET_HEIGHT
    sta JetAnimOffset
 
CheckP0Right:
    lda #%10000000          ; player 0 joystick right
    bit SWCHA
    bne CheckButtonPressed  ; if no input, goto EndInputCheck
    lda JetXPos
    cmp #103
    bpl CheckButtonPressed
.P0RightPressed:
    inc JetXPos             ; else, increment JetXPos
    lda JET_HEIGHT          ; 9
    sta JetAnimOffset       ; set animation offset to second frame

CheckButtonPressed:
    lda #%10000000          ; load MSB
    bit INPT4               ; AND with INPT4 reg: 0 = pressed
    bne SoundCheck
.ButtonPressed:
    lda JetXPos
    clc                     ; clear carry before addition
    adc #4                  ; add 4 to center missile on JetXPos
    sta MissileXPos         ; set the missile x to player 0
    lda JetYPos             
    clc                     ; clear carry before addition
    adc #4                  ; add 4 to center missile on JetYPos
    sta MissileYPos         ; set the missile y to player 0
    jsr BombRelSound

SoundCheck:                 ; Check bits of IntervalByte variable for sound timing
;; Interval Byte bits --   0 0 0 0 0 0 0 0
;;                         ^ ^ ^ ^ ^ ^ ^ ^
;;                         7 6 5 4 3 2 1 0
;; 
;; Bit 0 toggles every frame (meant for MissileYPos, not sound)
;; Bits 3, 2, and 1 are for sound timing of the bomb release
;; Bits 4-8 currently unused
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda IntervalByte         ; load IntervalByte val for comparison
    and #%00000110          ; check if any 1s in [2:1] bits
    beq EndInputCheck       ; if not, skip to EndInputCheck
    lda IntervalByte
    clc                     ; else, clear carry before addition
    adc #2                  ; add 2 to incrment 2s bit
    sta IntervalByte
    and #%00001000          ; check 8's (bit 3 representing 8's)bit
    beq EndInputCheck       ; skip to end input if nothing is there
    lda IntervalByte        ; else, load IntervalByte
    and #%11110001          ; clear bits [3:1] to reset
    sta IntervalByte        ; restore Interval byte to reset sound counter
    lda #0
    sta AUDV0               ; turn off sound volume register for bomb release


EndInputCheck:              ; Fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateShipPosition:
    lda ShipYPos                
    clc
    cmp #0                  ; compare ShipYPos with 0
    bmi .ResetShipPosition  ; if < 0, reset position up top
    lda #%10000000          ;
    bit IntervalByte        ; test for bit 7 to stop animation.
    bne .ResetShipPosition  ; end of animation
    lda #%01110000
    bit IntervalByte        ; check bits 4,5,and 
    beq .DecrementShipPos   ; skip if result == 0
    lda IntervalByte
    clc
    adc #16                 ; Add 16 to increment timer
    sta IntervalByte        ; 
    jmp EndPositionUpdate

.DecrementShipPos
    dec ShipYPos            ; else, decrement enemy y-position for next frame
    jmp EndPositionUpdate
.ResetShipPosition
    jsr GetRandomShipPos    ; call subroutine for next x ship position
.SetScoreValues:
    sed                     ; set decimal mode for score and timer values
    lda Timer
    clc
    adc #1
    sta Timer               ; add 1 to the Timer variable instead of inc

    cld                     ; disable decimal mode

EndPositionUpdate:          ; Fallback for position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
CheckCollisionP0P1:
    lda #%10000000          ; CXPPMM bit 7 detects P0/P1 Collision
    bit CXPPMM              ; AND with CXPPMM
    bne .P0P1Collided       ; branch to collision subroutine if collided
    jsr SetTerrainRiverColor; else, set playfield color to green / blue
    jmp CheckCollisionM0P1   ; else, skip to next check
.P0P1Collided:
    jsr GameOver            ; call GameOver subroutine

CheckCollisionM0P1:
    lda #%10000000          ; Load MSB (M0/P1 collision) in CXM0P register
    bit CXM0P               ; AND with CXM0P
    bne .M0P1Collided       ; Branch if result != 0 (Collided)
    jmp EndCollisionCheck
.M0P1Collided
    sed                     ; set decimal mode
    lda Score               
    clc                     ; clear carry before addition
    adc #1                  ; add w/carry 1
    sta Score               ; store new score
    cld                     ; clear decimal mode
    lda IntervalByte        ;
    ora #%00010000          ; Add mask 4th bit to high to begin destruction timer
    sta IntervalByte        ; store back into IntervalByte to be read elsewhere
    lda #0
    sta MissileYPos         ; cease missile 
    lda #JET_HEIGHT         
    sta ShipAnimOffset      ; store ShipAnimOffset to display explosion
    jsr ExplosionSound      ; jump to explosion sound subroutine

EndCollisionCheck:          ; fallback
    sta CXCLR               ; clear collision flag register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
    jmp StartFrame          ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio the jet engine sound based on the jet y position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BombRelSound subroutine
    lda #3
    sta AUDV0               ; set audio volume register
    lda #29
    sta AUDF0
    lda #8
    sta AUDC0               ; store configuration
    lda IntervalByte        
    ora #%00000010          ; store 2's bit in IntervalByte to begin sound timing
    sta IntervalByte

    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the ship exploding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ExplosionSound subroutine
    lda #3
    sta AUDV1
    lda #30
    sta AUDF1
    lda #2
    sta AUDC1

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the colors for the terrain and river
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor        ; set terrain color to green
    lda #$84
    sta RiverColor          ; Set river to blue
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3: missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos Subroutine
    sta WSYNC               ; start a new fresh scanline
    sec                     ; set carry before subtraction

.Div15Loop
    sbc #15                 ; subtract 15 from accumulator
    bcs .Div15Loop          ; loop until carry-flag is clear
    eor #7                  ; XOR to turn negative remainder to positive
    asl
    asl
    asl
    asl                     ; four left shifts for [7:4] MSB
    sta HMP0,Y              ; store the fine offset
    sta RESP0,Y             ; fix object position in 15-step increments
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta TerrainColor        ; set terrain color to red
    sta RiverColor          ; set river color to red

    lda #0 
    sta Score 

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General LFSR random number generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RandomNumGen    subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl 
    asl
    eor Random
    asl
    rol Random              ; perform a series of shifts and bit operations
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate Linear feedback shift register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number
;; divide the random value by 4 to limit the size to the river width
;; Add 30 to offset number onto river (past gree playfield)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomShipPos  subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl 
    asl
    eor Random
    asl
    rol Random              ; perform a series of shifts and bit operations

    lsr 
    lsr                     ; divide value by 4 with 2 right shifts
    sta ShipXPos            ; save it to the variable ShipXPos
    lda #30
    adc ShipXPos            ; adds 30 to ShipXPos to compensate for left PF
    sta ShipXPos            ; sets new value

    lda #96
    sta ShipYPos            ; Reset the height for the ship
    
    lda IntervalByte
    and #%00001111          ; clear all explosion anim and sound timers by clearing 7:4 bits
    sta IntervalByte

    lda #0
    sta ShipAnimOffset      ; Reset offset to 0 to point to "intact" ship bitmap
    sta AUDV1               ; turn off audio

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of variable Score and Timer
;; inthe the offsets of the digits lookup table so the values can be displayed
;; each digit has a height of 5 bytes in the LUT
;;
;; for the low nibble we need to multiply by 5
;; - we can use left shifts to perform multiplication by 2
;; - for any number N, the value of N*5 = (N*2*2)+N
;; 
;; For the upper nibble, since it's already *16 we need to divide it by
;; and then multiply by 5:
;; - we can use right shifts to perform division by 2
;; - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                  ; X register is loop counter
.PrepareScoreLoop           ; will loop twice, first X=1, and then X=0

    lda Score,X             ; load A with timer when X=1 or score when X=0
    and #$0F                ; keeps 4x LSBs (ones)
    sta Temp                ; save the value of A into the Temp variable
    asl                     ; multiply by 2
    asl                     ; multiply by 4
    adc Temp                ; multiply by 5 (N*4)+N (add the last iteration)
    sta OnesDigitOffset,X   ; Save A in OnesDigitOffset+1 or OnesDIgitOffset+0

    lda Score,X             ; load A with Timer (X=1) or Score (X=0)
    and #$F0                ; keeps 4x MSBs (tens)
    lsr                     ; divide by 2
    lsr                     ; divide by 4
    sta Temp                ; Save value into temp variable
    lsr                     ; divide by 8
    lsr                     ; divide by 16
    adc Temp                ; add N/16 with N/4
    sta TensDigitOffset,X   ; store A in TensDigitOffset+1 or TensDigitOffset+0

    dex                     ; X--
    bpl .PrepareScoreLoop   ; while X > 0, loop to pass a second time
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM Look up tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # # 
    .byte #%01010101        ;  # # # # 
    .byte #%01010101        ;  # # # # 
    .byte #%01110111        ;  ### ###

    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #

    .byte #%01110111        ;  ### ###
    .byte #%00010001        ;    #   #
    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   #  
    .byte #%01110111        ;  ### ###

    .byte #%01110111        ;  ### ###
    .byte #%00010001        ;    #   #
    .byte #%00110011        ;   ##  ##
    .byte #%00010001        ;    #   #
    .byte #%01110111        ;  ### ###

    .byte #%01010101        ;  # # # #
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #

    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   #
    .byte #%01110111        ;  ### ###
    .byte #%00010001        ;    #   #
    .byte #%01110111        ;  ### ###

    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   # 
    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###

    .byte #%01110111        ;  ### ###
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #
    .byte #%00010001        ;    #   #

    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###

    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###
    .byte #%00010001        ;    #   #
    .byte #%01110111        ;  ### ###

    .byte #%00100010        ;   #   # 
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # #
    .byte #%01010101        ;  # # # #

    .byte #%01110111        ;  ### ###
    .byte #%01010101        ;  # # # #
    .byte #%01100110        ;  ##  ## 
    .byte #%01010101        ;  # # # #
    .byte #%01110111        ;  ### ###

    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   #
    .byte #%01000100        ;  #   #
    .byte #%01000100        ;  #   #
    .byte #%01110111        ;  ### ###

    .byte #%01100110        ;  ##  ##
    .byte #%01010101        ;  # # # #
    .byte #%01010101        ;  # # # #
    .byte #%01010101        ;  # # # #
    .byte #%01100110        ;  ##  ##

    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   # 
    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   # 
    .byte #%01110111        ;  ### ###

    .byte #%01110111        ;  ### ###
    .byte #%01000100        ;  #   #
    .byte #%01100110        ;  ##  ##
    .byte #%01000100        ;  #   #
    .byte #%01000100        ;  #   #  

    
JetSprite:
        .byte $%00000000; padding
        .byte #%01000100;$06         #   #
        .byte #%11000110;$04        ##   ##
        .byte #%00111000;$32          ###
        .byte #%00010000;$32           #
        .byte #%00010000;$32           #
        .byte #%00111000;$32          ###
        .byte #%11000110;$04        ##   ##
        .byte #%01000100;$06         #   #
JetSpriteTurnOne:
        .byte $%00000000; padding
        .byte #%00101000;$06          # #
        .byte #%01101100;$04         ## ##
        .byte #%00111000;$32          ###
        .byte #%00010000;$32           #
        .byte #%00010000;$32           #
        .byte #%00111000;$32          ###
        .byte #%01101100;$04         ## ##
        .byte #%00101000;$06          # #
ShipSprite:
        .byte $%00000000; padding
        .byte #%00111000;$40          ###   
        .byte #%00101000;$04          # #
        .byte #%00111000;$06          ###
        .byte #%00101000;$06          # #
        .byte #%00101000;$32          # #
        .byte #%00111000;$32          ###
        .byte #%00101000;$04          # #
        .byte #%00010000;$06           #
ShipDestroy:
        .byte #%00000000; padding
        .byte #%00000000;$0A        
        .byte #%01101100;$40         ## ##
        .byte #%00111100;$32          ####
        .byte #%00011000;$1C           ##
        .byte #%00011000;$1C           ##
        .byte #%00111100;$32          ####
        .byte #%01110110;$40         ### ##
        .byte #%00000000;$0A
;---End Graphics Data---


;---Color Data from PlayerPal 2600---

JetColor:
        .byte #$00; padding
        .byte #$0A;
        .byte #$08;
        .byte #$32;
        .byte #$32;
        .byte #$32;
        .byte #$32;
        .byte #$08;
        .byte #$0A;
JetColorTurn:
        .byte #$00; padding
        .byte #$0A;
        .byte #$08;
        .byte #$32;
        .byte #$32;
        .byte #$32;
        .byte #$32;
        .byte #$08;
        .byte #$0A;
ShipColor:
        .byte #$00; padding
        .byte #$40;
        .byte #$04;
        .byte #$06;
        .byte #$06;
        .byte #$32;
        .byte #$32;
        .byte #$04;
        .byte #$06;
ShipDestroyColor:
        .byte #$00; padding
        .byte #$0A;
        .byte #$40;
        .byte #$32;
        .byte #$1C;
        .byte #$1C;
        .byte #$32;
        .byte #$40;
        .byte #$0A;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
    org $FFFC               ; move to position $FFFC
    word Reset              ; write two bytes with program reset address
    word Reset              ; write 2 bytes with the interruption register