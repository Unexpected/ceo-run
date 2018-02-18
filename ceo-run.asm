  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  
;;;;;;;;;;;;;;;

  .rsset $0000  ;;start variables at ram location 0

; scrolling variables
scroll     .rs 1  ; horizontal scroll count
lastScroll .rs 1  ; horizontal scroll count on previous frame
lastColumn .rs 1  ; last column drawn
nametable  .rs 1  ; which nametable to use, 0 or 1
columnLow  .rs 1  ; low byte of new column address
columnHigh .rs 1  ; high byte of new column address
sourceLow  .rs 1  ; source for column data
sourceHigh .rs 1
columnNumber .rs 1  ; which column of level data to draw



; controllers
; button: A B select start up down left right
; If the bit is 1, that button is pressed.
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button

; gameplay variables
; player state - see PLAYER_STATE_XXX constants
playerState .rs 1

; number of frames with button A pressed for jump
; player cannont jump more than MAX_JUMP_TIME
playerJumpTime .rs 1

playerPosX 	.rs 1		; bot left pixel
playerPosY 	.rs 1		; bot left pixel

playerSpeedX	.rs 1
playerSpeedY	.rs 1

platformPosX	.rs 1
platformPosY	.rs 1
platformLength	.rs 1

; Constants
MAX_SPEED = $03 		; max speed for player
JUMP_SPEED = $06
FALL_SPEED = $FB		; FALL_SPEED == -5
MAX_JUMP_TIME = $10		; max number of frames to jump  

PLAYER_HEIGHT = $10		; player meta-sprite height (used for collision)
PLAYER_WIDTH = $10		; player meta-sprite height (used for collision)

PLAYER_STATE_RUNNING = $00
PLAYER_STATE_FALLING = $01
PLAYER_STATE_JUMPING = $02

  .bank 0
  .org $C000 
  
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA playerSprites, x  ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
              
              
              
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDY #$1E              ; start out at 0
  LDX #$20
  LDA #$24
LoadBackgroundLoop:
  CPY #$08   			; Drawing line 08 ?
  BNE DrawBlueSky
DrawBlock: 
  LDA #$45
DrawBlueSky: 
  STA $2007             ; write to PPU
  LDA #$24
  DEX
  BNE LoadBackgroundLoop
  LDX #$20
  DEY                   
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


						
LoadBackground2:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$24
  STA $2006             ; write the high byte of $2400 address
  LDA #$00
  STA $2006             ; write the low byte of $2400 address
  LDY #$1E              
  LDX #$20
  LDA #$24
LoadBackgroundLoop2:
  CPY #$0C   			; Drawing line 0C ?
  BNE DrawBlueSky2
DrawBlock2: 
  LDA #$53
DrawBlueSky2: 
  STA $2007             ; write to PPU
  LDA #$24
  DEX
  BNE LoadBackgroundLoop2
  LDX #$20
  DEY                   
  BNE LoadBackgroundLoop2  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down

FillAttrib0:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address (nametable 0 attributes)
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$40              ; fill 64 bytes
  LDA #$00
FillAttrib0Loop:
  STA $2007
  DEX
  BNE FillAttrib0Loop


FillAttrib1:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$27
  STA $2006             ; write the high byte of $27C0 address (nametable 1 attributes)
  LDA #$C0
  STA $2006             ; write the low byte of $27C0 address
  LDX #$40              ; fill 64 bytes
  LDA #$FF
FillAttrib1Loop:
  STA $2007
  DEX
  BNE FillAttrib1Loop
              
;****************************************************************************
;****************************                  ******************************
;****************************    GAME INIT     ******************************
;****************************                  ******************************
;****************************************************************************
			  
InitGame: 
  ; initial scroll variables
  LDA #$00
  STA scroll
  STA lastScroll
  STA lastColumn

  ; initial player pos
  LDA #$40		
  STA playerPosX
  LDA #$10
  STA playerPosY	; near top of the screen

  ; initial player state
  LDA #PLAYER_STATE_FALLING
  STA playerState
  LDA $00
  STA playerJumpTime
  
  ; initial player speed
  LDA #MAX_SPEED
  STA playerSpeedX
  LDA #$00
  STA playerSpeedY

  ; Init next platform coordinates
  LDA #$03
  STA platformPosX
  LDA #$08
  STA platformPosY
  LDA #$0C
  STA platformLength

  
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
;****************************************************************************
;****************************                  ******************************
;****************************    SCROLLING     ******************************
;****************************                  ******************************
;****************************************************************************

NMI:
  LDA scroll
  STA lastScroll
  CLC
  ADC playerSpeedX ; add current player speed to scroll each frame
  STA scroll
NTSwapCheck:
  CMP lastScroll		; check if scroll went over 255 (if scroll < lastScroll)
  BCS NTSwapCheckDone
  
NTSwap:
  LDA nametable    ; load current nametable number (0 or 1)
  EOR #$01         ; exclusive OR of bit 0 will flip that bit
  STA nametable    ; so if nametable was 0, now 1
                   ;    if nametable was 1, now 0
NTSwapCheckDone:

NewAttribCheck:
  LDA scroll
  AND #%00011111            ; check for multiple of 32
  BNE NewAttribCheckDone    ; if low 5 bits = 0, time to write new attribute bytes
  JSR DrawNewAttributes
NewAttribCheckDone:


NewColumnCheck:
  LDA lastScroll
  LSR A
  LSR A
  LSR A						; divide by 8
  STA lastColumn			; store value as "lastColumn" 
  LDA scroll
  LSR A
  LSR A
  LSR A						; divide by 8
  CMP lastColumn			; if greater than lastColumn, we need to draw a new one
  BEQ NewColumnCheckDone
  
  JSR DrawNewColumn         ; if lower bits = 0, time for new column
  
  LDA columnNumber
  CLC
  ADC #$01             ; go to next column
  AND #%01111111       ; only 128 columns of data, throw away top bit to wrap
  STA columnNumber
NewColumnCheckDone:

  LDA #$00
  STA $2003       
  LDA #$02
  STA $4014       ; sprite DMA from $0200
  
  ; run other game graphics updating code here

  LDA #$00
  STA $2006        ; clean up PPU address registers
  STA $2006
  
  LDA scroll
  STA $2005        ; write the horizontal scroll count register

  LDA #$00         ; no vertical scrolling
  STA $2005
  
  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ORA nametable    ; select correct nametable for bit 0
  STA $2000
  
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
    
  ; run normal game engine code here
  ; reading from controllers, etc

;****************************************************************************
;****************************                  ******************************
;**************************** MAIN GAME ENGINE ******************************
;****************************                  ******************************
;****************************************************************************
  
  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2

GameEngine:  
;  LDA gamestate
;  CMP #STATETITLE
;  BEQ EngineTitle    ;;game is displaying title screen
    
;  LDA gamestate
;  CMP #STATEGAMEOVER
;  BEQ EngineGameOver  ;;game is displaying ending screen
  
;  LDA gamestate
;  CMP #STATEPLAYING
;  BEQ EnginePlaying   ;;game is playing
GameEngineDone:  
  
  JSR UpdateGameState 
  
  RTI              ; return from interrupt
 
;****************************************************************************
;****************************                  ******************************
;****************************    SUBROUTINES   ******************************
;****************************                  ******************************
;****************************************************************************
  
; Read controller 1 state
ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  
; Read controller 2 state
ReadController2:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController2Loop:
  LDA $4017
  LSR A            ; bit0 -> Carry
  ROL buttons2     ; bit0 <- Carry
  DEX
  BNE ReadController2Loop
  RTS  

; Gameplay instructions
UpdateGameState:
  JSR HandleInput
  JSR ApplyPhysics
  JSR CheckCollision
  JSR UpdatePlayer
  
  RTS

; Update vertical speed based on controller input
HandleInput: 
  LDA buttons1
  AND #%10000000
  BEQ HandleNoJump		; player doesn't press A
    
  LDA playerState
  CMP #PLAYER_STATE_FALLING
  BEQ HandleInputDone		; player currently falling

  LDA #PLAYER_STATE_JUMPING
  STA playerState
  INC playerJumpTime
  JMP HandleInputDone
  
HandleNoJump: 
  LDA playerState
  CMP #PLAYER_STATE_JUMPING
  BNE HandleNoJumpDone
  LDA #PLAYER_STATE_FALLING
  STA playerState  
HandleNoJumpDone: 
HandleInputDone: 
  RTS

  
; Apply "physics" (gravity and acceleration)
ApplyPhysics: 
  LDA playerState
  CMP #PLAYER_STATE_JUMPING		; player is jumping
  BNE JumpDone
  LDX playerJumpTime
  CPX #MAX_JUMP_TIME
  BNE MaxJumpTimeReachedDone	; if player has reached max jump time, stop jumping
MaxJumpTimeReached: 
  LDX #PLAYER_STATE_FALLING
  STX playerState
MaxJumpTimeReachedDone: 
  LDA playerState
  CMP #PLAYER_STATE_JUMPING		; player is jumping
  BNE JumpDone
Jump: 
  LDX #JUMP_SPEED
  STX playerSpeedY
JumpDone: 
  CMP #PLAYER_STATE_FALLING		; player is falling
  BNE FallDone
Fall: 
  LDX playerSpeedY
  CPX #FALL_SPEED				; check if speedY if already falling at max speed
  BEQ FallDone
  DEC playerSpeedY
FallDone: 
  
ApplyPhysicsDone: 
  RTS

; Check collisions with platforms
CheckCollision: 
  LDA playerState
  CMP #PLAYER_STATE_FALLING		; player is jumping
  BNE CheckGroundCollisionDone
CheckGroundCollision: 			; if y >= blockPos && y < blockPos + blockHeight
  LDA playerPosY
  CMP #$B0						; blockPos == B0
  BCS CheckCollisionDone
  CMP #$A0						; blockPos == B0
  BCC CheckCollisionDone
  LDX #$00
  STX playerSpeedY
  STX playerJumpTime
  LDX #PLAYER_STATE_RUNNING
  STX playerState
CheckGroundCollisionDone:
CheckCollisionDone: 
  RTS


; Update player pos based on speed variables
UpdatePlayer: 
  LDX playerSpeedY
  CPX #$00
  BCC GoingUp
GoingDown: 
  INC playerPosY
  INX
  BNE GoingDown
  JMP GoingUpDone
GoindDownDone: 
GoingUp: 
  DEC playerPosY
  DEX
  BNE GoingUp
GoingUpDone: 
  LDA playerPosY
  STA $0200
  LDA playerPosX
  STA $0203
UpdatePlayerDone: 
  RTS

;****************************************************************************
;****************************                  ******************************
;**************************** GRAPHIC ENGINE   ******************************
;****************************                  ******************************
;****************************************************************************
    
DrawNewColumn:
  LDA scroll       ; calculate new column address using scroll register
  LSR A
  LSR A
  LSR A            ; shift right 3 times = divide by 8
  STA columnLow    ; $00 to $1F, screen is 32 tiles wide

  LDA nametable     ; calculate new column address using current nametable
  EOR #$01          ; invert low bit, A = $00 or $01
  ASL A             ; shift up, A = $00 or $02
  ASL A             ; $00 or $04
  CLC
  ADC #$20          ; add high byte of nametable base address ($2000)
  STA columnHigh    ; now address = $20 or $24 for nametable 0 or 1

;  LDA columnNumber  ; column number * 32 = column data offset
;  ASL A
;  ASL A
;  ASL A
;  ASL A
;  ASL A             
;  STA sourceLow
;  LDA columnNumber
;  LSR A
;  LSR A
;  LSR A
;  STA sourceHigh
  
;  LDA sourceLow       ; column data start + offset = address to load column data from
;  CLC 
;  ADC #LOW(columnData)
;  STA sourceLow
;  LDA sourceHigh
;  ADC #HIGH(columnData)
;  STA sourceHigh

DrawColumn:
  LDA #%00000100        ; set to increment +32 mode
  STA $2000
  
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA columnHigh
  STA $2006             ; write the high byte of column address
  LDA columnLow
  STA $2006             ; write the low byte of column address
  LDX #$1E              ; copy 30 bytes
  
  LDY platformPosX		; check if we are drawing a platform
  CPY #$00
  BEQ DrawColumnLoop    ; draw a platform
  
  DEY
  STY platformPosX		; decrement platform distance
DrawEmptyColumnLoop: 
  LDA #$24
  STA $2007	
  DEX
  BNE DrawEmptyColumnLoop
  JMP DrawColumnLoopDone
  
DrawColumnLoop:
  CPX platformPosY
  BEQ LoadBlock
  LDA #$24		
  JMP Draw
LoadBlock: 
  LDA #$45  	; if platform : draw blue except for platform sprite
Draw:   
  STA $2007					; else draw blue
  DEX
  BNE DrawColumnLoop
  
  ; last platform block ?
  LDY platformLength
  CPY #$00
  BEQ PrepareNewPlatform
  DEY
  STY platformLength
  JMP DrawColumnLoopDone

PrepareNewPlatform: 
  ; Init next platform coordinates
  LDA #$08
  STA platformPosX
  ;LDA platformPosY
  ;ASL A
  ;STA platformPosY
  LDA #$0C
  STA platformLength
  
DrawColumnLoopDone:

  RTS
  
  
  
DrawNewAttributes:
  LDA nametable
  EOR #$01          ; invert low bit, A = $00 or $01
  ASL A             ; shift up, A = $00 or $02
  ASL A             ; $00 or $04
  CLC
  ADC #$23          ; add high byte of attribute base address ($23C0)
  STA columnHigh    ; now address = $23 or $27 for nametable 0 or 1
  
  LDA scroll
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  CLC
  ADC #$C0
  STA columnLow     ; attribute base + scroll / 32

  LDY #$00
  LDA $2002             ; read PPU status to reset the high/low latch
DrawNewAttributesLoop
  LDA columnHigh
  STA $2006             ; write the high byte of column address
  LDA columnLow
  STA $2006             ; write the low byte of column address
  LDA #$FF			    ; copy new attribute byte
  STA $2007
  
  INY
  CPY #$08              ; copy 8 attribute bytes
  BEQ DrawNewAttributesLoopDone 
  
  LDA columnLow         ; next attribute byte is at address + 8
  CLC
  ADC #$08
  STA columnLow
  JMP DrawNewAttributesLoop
DrawNewAttributesLoopDone:

  RTS  
  
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;; unused ?  
AccelerateX:
  LDA playerSpeedX
  CMP #MAX_SPEED
  BCS AccelerateXDone
  INC playerSpeedX
AccelerateXDone: 
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;; unused ?  

  
  .bank 1
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$16,$36,$12,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

playerSprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3

attribute:
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1