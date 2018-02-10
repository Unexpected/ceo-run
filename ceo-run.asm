  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  
;;;;;;;;;;;;;;;

  .rsset $0000  ;;start variables at ram location 0

; scrolling variables
scroll     .rs 1  ; horizontal scroll count
nametable  .rs 1  ; current nametable for swapping

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

playerPosX 	.rs 1
playerPosY 	.rs 1
playerSpeedX	.rs 1
playerSpeedY	.rs 1


playerSpriteHi	.rs 1
playerSpriteLo	.rs 1	

; Constants
MAX_SPEED = $03 			; max speed for player
JUMP_SPEED = $04
MAX_JUMP_TIME = $0A		; max number of frames to jump  

PLAYER_STATE_RUNNING = $00
PLAYER_STATE_FALLING = $01
PLAYER_STATE_JUMPING = $02

PLAYER_SPRITES = $04	; number of sprites for player
SPRITE_POS_X = $03		; sprite is Y NUMBER ATTR X 
SPRITE_POS_Y = $00		; sprite is Y NUMBER ATTR X 


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
              

InitGame: 
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
  STA playerSpeedY
  
  LDA #$02
  STA playerSpriteHi
  LDA #$00
  STA playerSpriteLo
  
  
  
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
  LDA scroll
  ADC playerSpeedX
  ;INC scroll       ; add one to our scroll variable each frame
NTSwapCheck:
  ;LDA scroll       ; check if the scroll just wrapped from 255 to 0
  ;BNE NTSwapCheckDone
  CMP scroll
  STA scroll
  BCS NTSwapCheckDone
  
NTSwap:
  LDA nametable    ; load current nametable number (0 or 1)
  EOR #$01         ; exclusive OR of bit 0 will flip that bit
  STA nametable    ; so if nametable was 0, now 1
                   ;    if nametable was 1, now 0
NTSwapCheckDone:

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
  CPX #$FD						; check if speedY if already falling at max speed
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
CheckGroundCollision:
  LDA playerPosY
  CMP #$D0
  BNE CheckCollisionDone
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