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

SECTION "Timer interrupt", ROM0[$50]
TimerInterrupt:
    call playSample
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
	ld hl, $4000	; TODO Play from WRAM buffer instead ($D000 ???)
	ld bc, $0100
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
	ldh [rTMA], a	; Timer modulo --> When TIMA overflows, it is reset to the value in this register and an interrupt is requested. Example of use: if TMA is set to $FF, an interrupt is requested at the clock frequency selected in TAC (because every increment is an overflow). However, if TMA is set to $FE, an interrupt is only requested every two increments, which effectively divides the selected clock by two. Setting TMA to $FD would divide the clock by three, and so on.
					; $F9 --> freq / 7 ???
					; $F8 --> freq / 8 == 16384Hz ?
					; FIXME if CGB single-speed --> $FC ???
	ld a, %00000110
	ldh [rTAC], a	; Enable timer @ CPU clock / 64 (CGB Double Speed --> 131072 Hz)
	ld a, $0c		; FIXME 0x08 | 0x04 = 0x0c instead of 0x04
	ldh [rIE], a
	; TODO serial external clock ?
	ld a, $80
	ldh [rSC], a
	; TODO Init audio buffers
	ld a, $00
	ld [$d000], a
	ld a, $d1
	ld [$d001], a
	ld a, $00
	ld [$d002], a
	ld a, $d5 		;FIXME $d2
	ld [$d003], a
	; TODO Copy ROM audio to RAM?
	ld bc, 2048		; Buffers can hold 1024 bytes each
	ld de, $4000	; Audio data in ROM
	ld hl, $d100 	; Audio buffers in RAM
ldaudio:
	ld a, [de]
	ldi [hl], a
	inc de
	dec bc
	; TODO check if bc is 0
	ld a, c
	or b 	; A=(C OR B)
	jr nz, ldaudio
	; TODO Init and enable LCD display


waitvbl:
	ldh a, [rLY]	; Read current line
	cp 144
	jr c, waitvbl	; Loop if line 144 not reached

	xor a	;A=0
	ldh [rLCDC], a 		; Disable display
	; TODO Copy tiles data to VRAM
	;ld b, 8*2		; B = 16 bytes to copy (1 8x8 tile @ 2bpp)
	ld de, tiles_data 	; DE = Tiles address in ROM
	ld hl, $8010 	; HL=$8010, VRAM destination (Tile 1)
ldt:
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, ldt		; Jump to the next byte


	; TODO Copy font tiles to VRAM
	ld bc, tiles_font_data_end-tiles_font_data
	ld de, tiles_font_data
	ld hl, $8200	; Copy to tiles 0x20-0x3f
ldt_font:
	ld a, [de]
	ldi [hl], a
	ldi [hl], a		; Font tiles are 1bpp --> copy each byte twice to convert to 2bpp
	inc de
	dec bc
	; TODO check if bc is 0
	ld a, c
	or b 	; A=(C OR B)
	jr nz, ldt_font		; Jump to the next byte

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
	ld b, text_data_end-text_data
	ld a, 50
	ld [text_pos_x], a	; X position
	ld de, text_data
text_loop:
	ld [hl], 40 	; OAM sprite Y = 40
	inc l
	ld a, [text_pos_x]
	ld [hl], a		; OAM sprite X = 50 and onward
	add a, 10
	ld [text_pos_x], a
	inc l
	ld a, [de]
	ld [hl], a 		; OAM sprite tile = digit character
	inc de
	inc l
	ld [hl], $00 	; OAM sprite attributes = 0
	inc l
	dec b
	jr nz, text_loop

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

	; DEBUG
	ld hl, $4000	; TODO Play from WRAM buffer instead ($D000 ???)
	ei

waitforInt:
	nop
	nop
	jr waitforInt

playSample:
	; TODO Get read position from WRAM
	; FIXME Uncomment
	;ld a, [$d000]
	;ld l, a
	;ld a, [$d001]
	;ld h, a
	ld a, [hl]
	ld e, a   
	and $F0  
	ld d, a    
	jr nz, regPulse1 
	ld a, $1F
	ldh [rNR12], a
	ld a, $40
	ldh [$F8], a
	jr PCM1

regPulse1:
	ld a, $C0
	ldh [$F8], a
	ld a, d
	or $0F
	ldh [rNR12], a

PCM1:
	ld a, e
	and $07
	ld d, a
	swap a
	or d
	ldh [$F7], a
	ld a, e
	and $08
	jr z, noVol
	ldh a, [$F7]
	ldh [rNR50], a

	ldh a, [$F8]
	ldh [rNR11], a
	
	ld a, $80
	ldh [rNR14], a

	jr endPCM

noVol:	
	ldh a, [$F8]
	ldh [rNR11], a
	
	ld a, $80
	ldh [rNR14], a

	ldh a, [$F7]
	ldh [rNR50], a
endPCM:
	inc l
	; TODO Increment read position in WRAM ($d000)
	; FIXME Uncomment
	;ld a, [$d000]
	;ld l, a
	;ld a, [$d001]
	;ld h, a
	;inc hl	; TODO inc l or inc hl???
	;ld a, l
	;ld [$d000], a
	;ld a, h
	;ld [$d001], a
	;ld a, h
	; Handle end of buffer --> loop
	;cp $d9		;FIXME $d3
	;jp nz, keepGoing
	;ld a, $d1	; back to $d100
	;ld [$d001], a
keepGoing:

	; TODO No need for bank switching
	; TODO loop between dual audio buffers instead???

	; FIXME Comment
	jr nz, sampleEnd
	inc h
	ld a, h
	cp $80
	jr nz, sampleEnd
	ld h, $40
	inc b
	ld a, b
	ld [$2000], a
	jr nz, sampleEnd
	ld c, $00
	inc c
	ld a, c
	ld [$3000], a

sampleEnd:
	ld sp, $FFFF
	ei
	;jp playSample ;Only uncomment this if you know what you're doing!
				   ;No interrupt playback is experimental as I don't
				   ;exactly know the precise playback rate.
				   ;Also, comment the previous 2 lines if you use no
				   ;interrupts.

lockup:
	nop
	nop
	nop
	nop
	jr lockup
SECTION "Additional lockup", ROM0[$04FC] ;I included this just to be safe.
	jp lockup



; TODO Data in WRAM
; $d000: position in active buffer (read) --> e.g. $d120
; $d002: position in inactive buffer (write) --> e.g. $d2a5
; $d100: buffer 0 (256 bytes)
; $d200: buffer 1 (256 bytes)


Serial:
	; TODO Move received data to inactive audio buffer
	; TODO If buffer is full, swap buffers ???
	push af
	push bc

	; FIXME increment serial data counter to be displayed on screen?
	
	; Load destination address TODO select inactive buffer !!!
	ld a, [$d002]
	ld l, a
	ld a, [$d003]
	ld h, a
	; Read received data
	ldh a, [rSB]
	; Copy to destination
	ld [hl], a
	; Increment destination
	inc hl
	ld a, l
	ld [$d002], a
	ld a, h
	ld [$d003], a
	; Handle end of buffer --> loop
	cp $d9		;FIXME $d3
	jp nz, noLoopback
	ld a, $d1	; back to $d100
	ld [$d003], a
noLoopback:

	; TODO Handle end of buffer --> switch active buffer
	; TODO temp = [$d002] & 0xff00
	; TODO [$d002] = [$d000] & 0xff00
	; TODO [$d000] = temp
	; TODO hl = 

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

tiles_data:
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`00011223
	DW	`32211000
	DW	`32211000

tiles_font_data:
	chr_IBMPC1	2,2
tiles_font_data_end:


text_data:
	db "12345"
text_data_end:


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

SECTION	"Variables2", WRAM0[$c000]

text_pos_x:		DS 1
