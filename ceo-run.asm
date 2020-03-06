  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  
;;;;;;;;;;;;;;;

  .rsset $0000  ;;start variables at ram location 0

; Variable $0000
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

; random generator variable
seed 	.rs 1

; function variables
functionParam1	.rs 1
functionParam2	.rs 1
functionOutput	.rs 1

; controllers
; button: A B select start up down left right
; If the bit is 1, that button is pressed.
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button

; gameplay variables
; player state - see PLAYER_STATE_XXX constants
playerState .rs 1

; Variable $0010
; number of frames with button A pressed for jump
; player cannont jump more than MAX_JUMP_TIME
playerJumpTime .rs 1

playerPosX 	.rs 1		; bot left pixel
playerPosY 	.rs 1		; bot left pixel
playerDistance	.rs 1

playerSpeedX	.rs 1
playerSpeedY	.rs 1

platformPosX	.rs 1
platformPosY	.rs 1
platformLength	.rs 1

; Variable 25 -> 57 ($0019 -> $0038)
platformCollisions	.rs	32
platformCollisionsPos .rs 1

; Variable $003A
debugValue	.rs 1
debugValue2	.rs 1

; Constants
MAX_SPEED = $02 		; max speed for player
JUMP_SPEED = $04
FALL_SPEED = $FB		; FALL_SPEED == $FB == -5
MAX_JUMP_TIME = $12		; max number of frames to jump
PLATFORM_MIN_DISTANCE_X = $04
PLATFORM_MAX_DISTANCE_X = $06
PLATFORM_MIN_HEIGHT = $04
PLATFORM_MAX_HEIGHT = $12
PLATFORM_MIN_LENGTH = $04
PLATFORM_MAX_LENGTH = $08

PLAYER_HEIGHT = $10		; player meta-sprite height (used for collision)
PLAYER_WIDTH = $10		; player meta-sprite height (used for collision)

PLAYER_STATE_RUNNING = $00
PLAYER_STATE_FALLING = $01
PLAYER_STATE_JUMPING = $02

BACKGROUND_BLUE_SKY = $24
BACKGROUND_BLK_TOP_LEFT = $53
BACKGROUND_BLK_TOP_RIGHT = $54
BACKGROUND_BLK_BOT_LEFT = $55
BACKGROUND_BLK_BOT_RIGHT = $56

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
  LDA #BACKGROUND_BLUE_SKY
LoadBackgroundLoop:
  CPY #$12   			; Drawing line 04
  BEQ DrawStartMessage
  CPY #$08   			; Drawing line 08
  BNE DrawBlueSky
  BEQ LoadBlock
DrawStartMessage: 		; print message "press B to START !"
  LDX #$00              ; start out at 0
DrawStartMessageLoop:
  LDA message, x        ; load data from address (message + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes
  BNE DrawStartMessageLoop  ; Branch to DrawStartMessageLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down  
  LDA #BACKGROUND_BLUE_SKY
  DEY
  BNE LoadBackgroundLoop
LoadBlock: 
  LDA #$45
DrawBlueSky: 
  STA $2007             ; write to PPU
  LDA #BACKGROUND_BLUE_SKY
  DEX
  BNE LoadBackgroundLoop
  LDX #$20
  DEY                   
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down

; Load second background screen
LoadBackground2:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #BACKGROUND_BLUE_SKY
  STA $2006             ; write the high byte of $2400 address
  LDA #$00
  STA $2006             ; write the low byte of $2400 address
  LDY #$1E              
  LDX #$20
  LDA #BACKGROUND_BLUE_SKY
LoadBackgroundLoop2:
  STA $2007             ; write to PPU
  LDA #BACKGROUND_BLUE_SKY
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
  LDA #$00
  STA playerSpeedX
  LDA #$00
  STA playerSpeedY

  ; Init next platform coordinates
  LDA #$03
  STA platformPosX
  LDA #$08
  STA platformPosY
  LDA #$0B
  STA platformLength

  ; Init collision array
  LDX #$1F
  LDA #$14
InitCollisionsLoop:
  STA platformCollisions, X
  DEX
  BNE InitCollisionsLoop
  STA platformCollisions, X

  LDA #$00
  STA platformCollisionsPos
  
  LDA #$BB
  STA seed
  
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
  
  JSR DebugCollision
  
  RTS

; Update vertical speed based on controller input
HandleInput: 
  LDA playerSpeedX
  CMP #$00
  BNE GameStarted 
  
  INC seed
  
  LDA buttons1
  AND #%01000000
  BEQ HandleInputDone
  
  LDA #MAX_SPEED
  STA playerSpeedX

GameStarted:
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
;CheckGroundCollision: 			; if y >= blockPos && y < blockPos + blockHeight
  LDA platformCollisionsPos		; load current platform pos
  ADC #$07						; add 7 to get "player pos"
  CMP #$20						; module 32
  BCC LoopDone
  SBC #$20
LoopDone: 
  TAX							; store player platform pos in X
  LDA platformCollisions, X     ; read block pos
  ASL A							; block pos is in blocks, multiply by 8 to get pixels 
  ASL A
  ASL A
  TAY							; store in Y in case there's a collision
  CMP #$00						; if no platform, no collision
  BNE CheckNoMoreGroundDone
  LDA playerState
  CMP #PLAYER_STATE_RUNNING
  BNE CheckCollisionDone
  LDA #PLAYER_STATE_FALLING
  STA playerState
  JMP CheckCollisionDone
CheckNoMoreGroundDone:  
  CMP playerPosY 				; if blockPos >= y : carry is set
  BCS CheckCollisionDone
  ADC #$10
  CMP playerPosY				; if blockPos - 16 >= y : carry is set
  BCC CheckCollisionDone
  LDX #$00
  STX playerSpeedY
  STX playerJumpTime
  LDX #PLAYER_STATE_RUNNING
  STX playerState
  STY playerPosY
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
  LDA playerPosY
  STA $0204
  LDA playerPosX
  ADC #$07
  STA $0207
  LDA playerPosY
  ADC #$07
  STA $0208
  LDA playerPosX
  STA $020B
  LDA playerPosY
  ADC #$07
  STA $020C
  LDA playerPosX
  ADC #$07
  STA $020F
UpdatePlayerDone: 
  RTS

DebugCollision: 
  RTS
  LDX #$00
  LDY #$04              ; start at 0
DebugLoop:
  TXA
  ASL A
  ASL A
  ASL A
  STA $0200, Y          ; store into RAM address ($0200 + x)
  INY
  LDA platformCollisions, X
  CMP #$00
  BNE DrawNothingDone
  LDA #$24  
DrawNothingDone: 
  STA $0200, Y          ; store into RAM address ($0200 + x)
  INY
  LDA #$00
  STA $0200, Y          ; store into RAM address ($0200 + x)
  INY
  LDA #$10
  STA $0200, Y          ; store into RAM address ($0200 + x)
  INY
  INX; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE DebugLoop   		; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
              



;  LDA #$10
;  STA $0204
;  STA $0207
;  LDX platformCollisionsPos
;  LDA platformCollisions, X
;  STA $0205

  LDA platformCollisionsPos
  ASL A
  ASL A
  ASL A
  ADC #$40
  STA $0208

  LDA #$1A
  STA $020B
;  LDA platformCollisionsPos
  STA $0209
  
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

StoreCollisionsValue: 
  LDX platformCollisionsPos	; write platformPosX in collision platform array
  LDA #$00
  LDY platformPosX			; if platformPosX == 0: no platform at this pos
  CPY #$00					; we'll save #$00 in platformCollision
  BNE LoadPlatformHeightDone
LoadPlatformHeight: 
  LDA #$1D
  SBC platformPosY			; if platformPosX > 0: load platform height
  LSR A						; round up to an odd number
  ASL A						
LoadPlatformHeightDone:
  STA platformCollisions, X ; store platform height or 0 if collision array
  INX						; increase platformPos
  CPX #$20					; if platformPos == 32 then platformPos = 0
  BNE ResetPlatformPosDone  
  LDX #$00
ResetPlatformPosDone: 
  STX platformCollisionsPos


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
  LDA #BACKGROUND_BLUE_SKY
  STA $2007	
  DEX
  BNE DrawEmptyColumnLoop
  
  JMP DrawColumnLoopDone
  
DrawColumnLoop:
  CPX platformPosY
  BEQ DrawBlock				; if platform, draw block
  JMP DrawEmpty				; else draw blue

DrawBlock: 
  LDA lastColumn
  AND #%00000001
  BEQ DrawRightBlock

DrawLeftBlock: 
  LDA #BACKGROUND_BLK_TOP_LEFT
  STA $2007	
  DEX
  LDA #BACKGROUND_BLK_BOT_LEFT  				
  STA $2007	
  DEX
  BNE DrawColumnLoop
DrawRightBlock: 
  LDA #BACKGROUND_BLK_TOP_RIGHT
  STA $2007	
  DEX
  LDA #BACKGROUND_BLK_BOT_RIGHT  				
  STA $2007	
  DEX
  BNE DrawColumnLoop
  
DrawEmpty:   
  LDA #BACKGROUND_BLUE_SKY
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

PrepareNewPlatform: 		; Init next platform coordinates
  JSR FunctionRandomGenerator 
  LDA functionOutput		; get random number

  STA functionParam1		
  LDA #PLATFORM_MAX_DISTANCE_X
  STA functionParam2		
  JSR FunctionModulo		; apply modulo MAX_DISTANCE_X
  LDA functionOutput
  ADC #PLATFORM_MIN_DISTANCE_X ; add MIN_DISTANCE
  STA platformPosX
  AND #%00000001
  BEQ PrepareNewPlatformPosXDone
  INC platformPosX
PrepareNewPlatformPosXDone:

  JSR FunctionRandomGenerator 
  LDA functionOutput		; get random number

  STA functionParam1		
  LDA #PLATFORM_MAX_HEIGHT
  STA functionParam2		
  JSR FunctionModulo		; apply modulo PLATFORM_MAX_HEIGHT
  LDA functionOutput
  ADC #PLATFORM_MIN_HEIGHT ; add PLATFORM_MIN_HEIGHT
  STA platformPosY
  
  JSR FunctionRandomGenerator 
  LDA functionOutput		; get random number

  STA functionParam1		
  LDA #PLATFORM_MAX_LENGTH
  STA functionParam2		
  JSR FunctionModulo		; apply modulo PLATFORM_MAX_LENGTH
  LDA functionOutput
  ADC #PLATFORM_MIN_LENGTH ; add PLATFORM_MIN_LENGTH
  STA platformLength
  AND #%00000001
  BNE PrepareNewPlatformLengthDone
  INC platformLength
PrepareNewPlatformLengthDone:
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
  
FunctionRandomGenerator: 
  LDA seed
  ASL A
  BCC FunctionRandomGeneratorNoEor
  EOR #$1d
FunctionRandomGeneratorNoEor:  
  STA seed
  STA functionOutput
  RTS

FunctionModulo:
  LDA functionParam1  ; memory addr A
  SEC
Modulus:	
  SBC functionParam2  ; memory addr B
  BCS Modulus
  ADC functionParam2
  STA functionOutput
  
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

message:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$19,$1B,$0E,$1C,$1C,$24,$0B,$24,$1D,$18,$24,$1C,$1D,$0A,$1B,$1D,$2B,$24,$24,$24,$24,$24,$24,$24,$24

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