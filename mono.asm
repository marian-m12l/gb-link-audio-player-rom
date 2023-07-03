;The lines with
;ld a, $XX
;ldh [rTMA], a
;is where you specify the playback rate.
;the 8-bit value is what the base clock
;is divided by (where the base clock is
;131,072Hz.) How much it is divided by
;can be calculated by subtracting the
;specified amount from 256 and adding 1
;(though you may need to experiment.)
;Play around a bit until you find a rate 
;you like!
;Lowering the rate also makes more audio
;fit onto a single cartridge.
;Using a slightly slower rate than the
;encoded audio makes good lo-fi slowed
;down audio.

;Keep in mind that with interrupts, the
;playback rate of mono audio can't go
;above 32,768Hz while maintaining
;a constant playback rate. Anything
;higher may cause the audio to fluctuate
;a bit.

INCLUDE "hardware.inc"
INCLUDE "ibmpc1.inc"			; nice ascii tileset from devrs.com

; TODO VBlank interrupt --> draw hex characters (using simple shifts) of buffer positions
SECTION "vblank", ROM0 [$40]
	jp VBlank


SECTION "Timer interrupt", ROM0[$50]
TimerInterrupt:
    call PlaySample
	nop
	nop
	nop
	nop
	nop

; TODO Serial Interrupt --> receive audio data at 16KHz (external clock @ 16384*8 = 131072) --> write to audio buffer (need double buffer or ring buffer?)
; TODO Debug --> just increment a received-bytes counter and display every second ???

SECTION "serial", ROM0 [$58]
	jp Serial

SECTION "Header", ROM0[$100]

EntryPoint:
	di
	jp Start

REPT $150 - $104
	; FIXME Header data?
	db 0
ENDR


SECTION "Game code", ROM0[$150]

Start:
	di
	ld a, $01
	; FIXME PICOGBCART not fast enough for CGB double speed?
	;ldh [rKEY1], a	; Switch to CGB Double Speed?
	;stop
	nop
	nop
	xor a
	ldh [rNR52], a	; Disable sound
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	ld a, $80
	ldh [rNR52], a	; Enable sound
	nop
	nop
	nop
	xor a
	ldh [rIE], a	; All interrupts OFF
	ldh [$F8], a	; Reset variable (in High RAM) used in PCM player ???
	ldh [$F7], a	; Reset variable (in High RAM) used in PCM player ???
	ld sp, $FFFF	; Set stack pointer
	;ld hl, BANK1_START	; TODO Play from WRAM buffer instead ($D000 ???)
	;ld bc, $0100	; TODO Value for bank register??? (MBC1) --> starts with bank 1 (b=$01) ==> SHOULD NOT BE OVERWRITTEN BY COPY LOOPS !!! (OR SAVED TO WRAM BEFOREHAND !!!) 
	xor a
	ldh [rNR10], a	; Reset(?) channel 1 (Square)
	ld a, $11
	ldh [rNR51], a	; Enable stereo output
	xor a
	ldh [rNR13], a	; Reset(?) channel 1 (Square)
	ld a, $F0
	ldh [rNR12], a	; Max volume channel 1 (Square)
	ld a, $C0
	ldh [rNR11], a	; Max duty channel 1 (Square)
	ld a, $80
	ldh [rNR14], a	; Enable channel 1 (Square)
	ld a, $44
	ldh [rNR50], a	; L/R medium volume (4/7)
	;ld a, $F9		;Modify this value to change the playback rate!
	;ld a, $F8		;Modify this value to change the playback rate!
	ld a, $FC		;Modify this value to change the playback rate!
	; FIXME is the CGB in single speed fast enough to handle 16KHz playrate AND serial input?
	ld a, $F8		; TODO Try 8HKz playback !!!
	ldh [rTMA], a	; Timer modulo --> When TIMA overflows, it is reset to the value in this register and an interrupt is requested. Example of use: if TMA is set to $FF, an interrupt is requested at the clock frequency selected in TAC (because every increment is an overflow). However, if TMA is set to $FE, an interrupt is only requested every two increments, which effectively divides the selected clock by two. Setting TMA to $FD would divide the clock by three, and so on.
					; $F9 --> freq / 7 ???
					; $F8 --> freq / 8 == 16384Hz ?
					; FIXME if CGB single-speed --> $FC ???
	ld a, %00000110
	ldh [rTAC], a	; Enable timer @ CPU clock / 64 (CGB Double Speed --> 131072 Hz)
	; FIXME Enable VBlank interrupt 0x01
	ld a, $0d		; FIXME 0x08 | 0x04 | 0x01 = 0x0d instead of 0x04
	ldh [rIE], a
	; TODO serial external clock ?
	ld a, $80
	ldh [rSC], a
	; TODO Init audio buffers
	ld de, wPosActiveBuffer		; de holds destination address
	ld a, LOW(wBuffer0)
	ld [de], a
	inc de
	ld a, HIGH(wBuffer0)
	ld [de], a
	ld de, wPosInactiveBuffer		; de holds destination address
	ld a, LOW(wBuffer1)
	ld [de], a
	inc de
	ld a, HIGH(wBuffer1)
	ld [de], a
	; TODO Copy ROM audio to RAM?
	ld bc, wBuffersEnd-wBuffer0		; Buffers can hold 1024 bytes each
	ld de, BANK1_START	; Audio data in ROM --> load from a given offset?
	ld hl, wBuffer0 	; Audio buffers in RAM
.ldAudio:
	ld a, [de]
	ldi [hl], a
	inc de
	dec bc
	; TODO check if bc is 0
	ld a, c
	or b 	; A=(C OR B)
	jr nz, .ldAudio
	; TODO Init and enable LCD display

	; TODO Initialize buffer pointers
	ld de, wPosActiveBuffer		; de holds destination address
	ld a, LOW(wBuffer0)
	ld [de], a
	inc de
	ld a, HIGH(wBuffer0)
	ld [de], a
	ld de, wPosInactiveBuffer		; de holds destination address
	ld a, LOW(wBuffer1)
	ld [de], a
	inc de
	ld a, HIGH(wBuffer1)
	ld [de], a

	; TODO Initialize serial counter
	ld de, wSerialCounter		; de holds destination address
	xor a
	ld [de], a
	inc de
	ld [de], a



.waitvbl:
	ldh a, [rLY]	; Read current line
	cp 144
	jr c, .waitvbl	; Loop if line 144 not reached

	xor a	;A=0
	ldh [rLCDC], a 		; Disable display
	; TODO Copy tiles data to VRAM
	;ld b, 8*2		; B = 16 bytes to copy (1 8x8 tile @ 2bpp)
	ld de, TilesData 	; DE = Tiles address in ROM
	ld hl, vTile1 	; HL=$8010, VRAM destination (Tile 1)
.ldTile:
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, .ldTile		; Jump to the next byte


	; TODO Copy font tiles to VRAM
	ld bc, TilesFontDataEnd-TilesFontData
	ld de, TilesFontData
	ld hl, vFontTiles	; Copy to tiles 0x20-0x3f
.ldtFont:
	ld a, [de]
	ldi [hl], a
	ldi [hl], a		; Font tiles are 1bpp --> copy each byte twice to convert to 2bpp
	inc de
	dec bc
	; TODO check if bc is 0
	ld a, c
	or b 	; A=(C OR B)
	jr nz, .ldtFont		; Jump to the next byte

	; TODO Copy tilemaps to VRAM
	; TODO Configure Sprite in OAM
	ld hl, _OAMRAM 	; HL=$FE00
	;xor a
	; Sprite 0: Tile 1 @ (16, 32) --> custom tile data
	ld a, $20
	ld [hl], a 		; OAM sprite Y = 32
	inc l
	ld [hl], $10	; OAM sprite X = 16
	inc l
	ld [hl], $01 	; OAM sprite tile = 1
	inc l
	ld [hl], $00 	; OAM sprite attributes = 0
	inc l

	; Sprite 1: Tile $19 @ (100, 100) --> Nintendo Logo "(R)" tile
	ld a, 100
	ld [hl], a 		; OAM sprite Y = 100
	inc l
	ld [hl], a		; OAM sprite X = 100
	inc l
	ld [hl], $19 	; OAM sprite tile = $19
	inc l
	ld [hl], $00 	; OAM sprite attributes = 0
	inc l
	; TODO Sprite 2: Tile $33 @ (50, 20) --> character "3"
	ld [hl], 20 	; OAM sprite Y = 20
	inc l
	ld [hl], 50		; OAM sprite X = 50
	inc l
	ld [hl], $33 	; OAM sprite tile = $33
	inc l
	ld [hl], $00 	; OAM sprite attributes = 0
	inc l

	; TODO Sprites 3-7: Text made of digits
	ld b, TextDataEnd-TextData
	ld a, 50
	ld [wTextPosX], a	; X position
	ld de, TextData
.textLoop:
	ld [hl], 40 	; OAM sprite Y = 40
	inc l
	ld a, [wTextPosX]
	ld [hl], a		; OAM sprite X = 50 and onward
	add a, 10
	ld [wTextPosX], a
	inc l
	ld a, [de]
	ld [hl], a 		; OAM sprite tile = digit character
	inc de
	inc l
	ld [hl], $00 	; OAM sprite attributes = 0
	inc l
	dec b
	jr nz, .textLoop

	; Scroll and palettes
	xor a	;A=0
	ldh [rSCY], a	; Reset Scroll Y
	ldh [rSCX], a 	; Reset Scroll X
	ld a, %11100100 ; 11=Black 10=Dark grey 01=Light grey 00=White/Transparent
	ldh [rBGP], a 	; BG palette
	ldh [rOBP0], a 	; Sprite palette 0
	ldh [rOBP0], a 	; Sprute palette 1
	; CGB Palettes
		; BG
	ld a, %10000000
	ldh [rBGPI], a
	ld a, $ff
	ldh [rBGPD], a
	ldh [rBGPD], a
	ld a, $99
	ldh [rBGPD], a
	ldh [rBGPD], a
	ld a, $33
	ldh [rBGPD], a
	ldh [rBGPD], a
	ld a, $00
	ldh [rBGPD], a
	ldh [rBGPD], a
		; OBJ0
	ld a, %10000000
	ldh [rOBPI], a
	ld a, $ff
	ldh [rOBPD], a
	ldh [rOBPD], a
	ld a, $99
	ldh [rOBPD], a
	ldh [rOBPD], a
	ld a, $33
	ldh [rOBPD], a
	ldh [rOBPD], a
	ld a, $00
	ldh [rOBPD], a
	ldh [rOBPD], a
	ld a, %10010011 ; Screen on, Background on, tiles at $8000
	ldh [rLCDC], a

	; Set audio source
	;ld hl, BANK1_START	; Play from ROM data
	;ld hl, wBuffer0	; TODO Play from WRAM buffer
	;ld bc, $0100	; bank
	ei

.waitforInt:
	nop
	nop
	jr .waitforInt

PlaySample:
	; TODO Get read position from WRAM
	; FIXME Uncomment to read from WRAM
	ld de, wPosActiveBuffer
	; de contains wPosActiveBuffer address
	ld a, [de]
	ld l, a		; load low byte of wPosActiveBuffer value into l
	inc de
	ld a, [de]
	ld h, a		; load high byte of wPosActiveBuffer value into h

	ld a, [hl]
	ld e, a   
	and $F0  
	ld d, a    
	jr nz, .regPulse1 
	ld a, $1F
	ldh [rNR12], a
	ld a, $40
	ldh [$F8], a
	jr .pcm1

.regPulse1:
	ld a, $C0
	ldh [$F8], a
	ld a, d
	or $0F
	ldh [rNR12], a

.pcm1:
	ld a, e
	and $07
	ld d, a
	swap a
	or d
	ldh [$F7], a
	ld a, e
	and $08
	jr z, .noVol
	ldh a, [$F7]
	ldh [rNR50], a

	ldh a, [$F8]
	ldh [rNR11], a
	
	ld a, $80
	ldh [rNR14], a

	jr .endPCM

.noVol:	
	ldh a, [$F8]
	ldh [rNR11], a
	
	ld a, $80
	ldh [rNR14], a

	ldh a, [$F7]
	ldh [rNR50], a
.endPCM:
	; TODO Uncomment to read from ROM
	;inc l
	
	; TODO Increment read position in WRAM ($d000)
	; FIXME Uncomment to read from WRAM
	; TODO hl should already contain current sample position
	;ld a, LOW(wPosActiveBuffer)
	;ld l, a
	;ld a, HIGH(wPosActiveBuffer)
	;ld h, a
	inc hl	; TODO inc l or inc hl???
	ld de, wPosActiveBuffer	; de hold the destination address
	ld a, l
	ld [de], a
	inc de
	ld a, h
	ld [de], a
	; Handle end of buffer --> loop
	ld a, h
	cp HIGH(wBuffersEnd)
	jp nz, .keepGoing
	ld de, wPosActiveBuffer + 1		; de holds destination address
	ld a, HIGH(wBuffer0)	; otherwise back to buffer0
	ld [de], a
.keepGoing:

	; TODO No need for bank switching
	; TODO loop between dual audio buffers instead???
	jp .sampleEnd		; TODO Uncomment to read from WRAM

	;; TODO Unused when playing from WRAM

	; FIXME Comment
	jr nz, .sampleEnd	; keep playing if lower byte of "playing sample address" is not $00
	inc h				; increment higher byte of "playing sample address" otherwise
	ld a, h
	cp HIGH(BANK1_END)
	jr nz, .sampleEnd	; keep playing if higher byte of "playing sample address" is not $80 (i.e. we reached $8000, end of bank)
	ld hl, BANK1_START	; otherwise we reached end of bank --> go back to beginning of bank $4000
	inc b				; increment bank
	ld a, b
	ld [rROMB0], a		; switch current bank
	; TODO compare to last bank? (32 overflows)
	cp 5
	jr nz, .sampleEnd	; keep playing if bank did not overflow
	; TODO reset bank
	ld b, $01
	ld a, b
	ld [rROMB0], a		; switch current bank
	ld hl, BANK1_START
	;ld c, $00			; if bank overflowed, increment the 2 upper bits to address more banks ==> not required for 32 banks (128KB) ROM
	;inc c
	;ld a, c
	;ld [rROMB1], a

.sampleEnd:
	ld sp, $FFFF
	ei
	;jp playSample ;Only uncomment this if you know what you're doing!
				   ;No interrupt playback is experimental as I don't
				   ;exactly know the precise playback rate.
				   ;Also, comment the previous 2 lines if you use no
				   ;interrupts.

Lockup:
	nop
	nop
	nop
	nop
	jr Lockup
SECTION "Additional lockup", ROM0[$04FC] ;I included this just to be safe.
	jp Lockup


;VBlank interrupt
VBlank:
	push af
	push bc
	push de
	push hl

	; TODO blink a sprite? sprite2 == digit sprite
	ld hl, _OAMRAM + (4 * 2) + 2 	; tile number for sprite 2
	ld a, [hl]
	inc a
	cp $3a
	jr nz, .counter_end
.counter_reset:
	ld a, $30
.counter_end:
	ld [hl], a


	; TODO Display hex characters
	; FIXME display multipe values !!! ld de, wPosActiveBuffer
	ld de, wSerialCounter
	; de contains wPosActiveBuffer address
	ld a, [de]
	ld l, a		; load low byte of wPosActiveBuffer value into l
	inc de
	ld a, [de]
	ld h, a		; load high byte of wPosActiveBuffer value into h
	; hl contains wPosActiveBuffer VALUE
	; TODO Right-shift 12 bits for 1st nibble
	; TODO Right-shift 8 bits and mask 4 bits for 2nd nibble
	; TODO Right-shift 4 bits and mask 4 bits for 3rd nibble
	; TODO Mask 4 bits for 4th nibble
	; TODO HANDLE ALL FOUR NIBBLES

	; FIXME Make a function call or a macro to convert number to hex char
	
	; 1st nibble
	ld a, h
	srl a
	srl a
	srl a
	srl a
	cp $0a
	jr c, .digit1
	add $07
.digit1
	add $30
	; a contains 1st nibble character
	ld [wText], a
	; 2nd nibble
	ld a, h
	ld b, $0f
	and b
	cp $0a
	jr c, .digit2
	add $07
.digit2
	add $30
	; a contains 2nd nibble character
	ld [wText+1], a
	; 3rd nibble
	ld a, l
	srl a
	srl a
	srl a
	srl a
	cp $0a
	jr c, .digit3
	add $07
.digit3
	add $30
	; a contains 3rd nibble character
	ld [wText+2], a
	; 4th nibble
	ld a, l
	ld b, $0f
	and b
	; a contains 4th nibble value ?
	; TODO add 0x30 for digits, 0x37 for letters
	cp $0a
	jr c, .digit4
	add $07
.digit4
	add $30
	; a contains 4th nibble character
	ld [wText+3], a
	; wText contains hex characters
	; TODO update sprites / background tiles
	ld b, 4; TODO Compute length --> 4 chars ??? TextDataEnd-TextData
	ld a, 50
	ld [wTextPosX], a	; X position
	ld de, wText
	ld hl, _OAMRAM + (4 * 8) 	; HL=$FE00 + 8 sprites
.textLoop:
	ld [hl], 60 	; OAM sprite Y = 40
	inc l
	ld a, [wTextPosX]
	ld [hl], a		; OAM sprite X = 50 and onward
	add a, 10
	ld [wTextPosX], a
	inc l
	ld a, [de]
	ld [hl], a 		; OAM sprite tile = hex character
	inc de
	inc l
	ld [hl], $00 	; OAM sprite attributes = 0
	inc l
	dec b
	jr nz, .textLoop

	pop hl
	pop de
	pop bc
	pop af

	reti



; Serial interrupt
Serial:
	; TODO Move received data to inactive audio buffer
	; TODO If buffer is full, swap buffers ???
	push af
	push bc
	push de
	push hl

	; FIXME increment serial data counter to be displayed on screen?
	ld de, wSerialCounter
	ld a, [de]
	ld l, a		; load low byte of wSerialCounter value into l
	inc de
	ld a, [de]
	ld h, a		; load high byte of wSerialCounter value into h
	inc hl
	ld de, wSerialCounter
	ld a, l
	ld [de], a
	inc de
	ld a, h
	ld [de], a
	
	
	; Load destination address TODO select inactive buffer !!!
	ld de, wPosInactiveBuffer
	; de contains wPosInactiveBuffer address
	ld a, [de]
	ld l, a		; load low byte of wPosInactiveBuffer value into l
	inc de
	ld a, [de]
	ld h, a		; load high byte of wPosInactiveBuffer value into h
	; hl contains destination data address
	; Read received data
	ldh a, [rSB]
	; Copy to destination
	ld [hl], a
	; Increment destination
	inc hl	; TODO inc l or inc hl???
	ld de, wPosInactiveBuffer	; de hold the destination address
	ld a, l
	ld [de], a
	inc de
	ld a, h
	ld [de], a
	; Handle end of buffer --> loop
	cp HIGH(wBuffersEnd)
	jp nz, .noLoopback
	ld a, HIGH(wBuffer0)	; back to buffer 0
	ld [de], a
.noLoopback:

	; TODO Handle end of buffer --> switch active buffer
	; TODO temp = [$d002] & 0xff00
	; TODO [$d002] = [$d000] & 0xff00
	; TODO [$d000] = temp
	; TODO hl = 

	; TODO serial external clock ?
	ld a, $80
	ldh [rSC], a

	pop hl
	pop de
	pop bc
	pop af

	;push af
	;push hl
	;push de
	;push bc
	;call func_006b
	;ld a, $01
	;ldh [$ff00 + $cc], a
	;pop bc
	;pop de
	;pop hl
	;pop af
	reti



SECTION "TILES", ROM0[$1000]

TilesData:
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`32211000
	DW	`32211000

TilesFontData:
	chr_IBMPC1	2,3
TilesFontDataEnd:


TextData:
	db "12345"
TextDataEnd:


SECTION "VRAM Tiles", VRAM[$8000]
vEmptyTile:
	ds 1 * 16
vTile1:
    ds 1 * 16
vUnusedTiles:
	ds 30 * 16
vFontTiles:
	ds 32 * 16


; TODO Data in WRAM

;SECTION	"Variables", RAM[$D000]
;
;posActiveBuffer:	DS	2
;posInactiveBuffer:	DS	2
;
;SECTION	"Buffers", RAM[$D100]
;
;buffer0:		DS	0x100
;buffer1:		DS	0x100

SECTION	"Variables", WRAMX

wTextPosX:			DS 1
wPosActiveBuffer:	DS 2
wPosInactiveBuffer:	DS 2
wSerialCounter:		DS 2
wText:				DS 4
ALIGN 8
wBuffer0:		DS	$400
wBuffer1:		DS	$400
wBuffersEnd:


BANK1_START EQU $4000
BANK1_END	EQU $8000