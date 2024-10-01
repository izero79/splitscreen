    INCDIR "include:"
    INCLUDE "include/hardware/intbits.i"
    INCLUDE "include/hardware/cia.i"

KBD_WAIT   EQU     61
TIMERB_LO  EQU     KBD_WAIT&$ff
TIMERB_HI  EQU     KBD_WAIT>>8

IRQ2       EQU     $68

CHIPBASE   EQU     $dffe00
CIAATALO   EQU     $bfe401      ;Timer A low
CIAATAHI   EQU     $bfe501      ;Timer A high
CIAAICR    EQU     $bfed01      ;Interrupt control register
CIAACRA    EQU     $bfee01      ;Timer A control
CIAASDR    EQU     $BFEC01

DMASET2    EQU     %1000001111100000 ; set dma for blitter, sprite, audio, disk, copper and bitplanes
KEY_NONE = $00                  ; No key pressed
KEY_NEW = $01                   ; New key pressed
KEY_REPEAT = $02                ; Key repeated


    SECTION Start,CODE

    INCLUDE "DaWorkBench.s"
    INCLUDE "Startup2.s"

WaitDisk    EQU    30           ; 50-150 to rescue (as appropriate)

START:


    ;Check if were are on NTSC
    MOVE.L  4.w,A6
    CMP.B   #50,$212(A6)
    BEQ.S   .pal                ; equal means PAL
    MOVE.L  #262<<8,WBLANKLINE  ; change for NTSC
.pal:
    MOVE.L  #PIC1,BLITTOTHIS_BUF
    MOVE.L  #PIC2,DISPLAY_BUF
    JMP     Init

SetPic1:
    MOVE.L  #PIC2,BLITTOTHIS_BUF
    MOVE.L  #PIC1,DISPLAY_BUF
    MOVE.L  #PIC1,d0            ; to d0 we put the address of the PIC,
                                ; that is, where the first bitplane begins

    LEA     BPLPOINTERS,A1      ; in a1 we put the address of the
                                ; pointers to COPPERLIST planes
    MOVEQ   #4,D1               ; number of bitplanes -1 (here it's 5-1)
                                ; to loop with the DBRA
    MOVEQ   #0,D2
    MOVE.W  VERT_SCROLL_POS,D2
    CMPI.W  #0,D2
    BGE.S  .check_max
    MOVE.W  #0,VERT_SCROLL_POS 
    MOVEQ   #0,D2
.check_max:
    CMPI.W  #208,D2
    BLE.S  .cont
    MOVE.W  #208,VERT_SCROLL_POS 
    MOVE.W   #208,D2
.cont:
    MULU    #240,D2
    ADD.L   D2,D0
.POINTBP:
    MOVE.W  d0,6(a1)            ; copies the LOW word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (e.g.: 1234 > 3412)
                                ; putting the word HIGH in place of that
                                ; LOW, allowing it to be copied with MOVE.W!!
    MOVE.W  d0,2(a1)            ; copies the HIGH word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (ex: 3412 > 1234)
                                ; resetting the address.
    ADD.L   #48,d0              ; Length of one line of bitplane in bytes, here it is 384/8 = 48

    ADDQ.W  #8,a1               ; add 8 to address, a1 now contains the address of the next
                                ; bplpointers in the copperlist to write.
    DBRA    d1,.POINTBP          ; Redo D1 times POINTBP (D1=num of bitplanes (-1))
    ;MOVE.W  d0,$dff088          ; let's start the copper
    RTS

SetPic2:
    MOVE.L  #PIC1,BLITTOTHIS_BUF
    MOVE.L  #PIC2,DISPLAY_BUF
    MOVE.L  #PIC2,d0            ; to d0 we put the address of the PIC,
                                ; that is, where the first bitplane begins

    LEA     BPLPOINTERS,A1      ; in a1 we put the address of the
                                ; pointers to COPPERLIST planes
    MOVEQ   #4,D1               ; number of bitplanes -1 (here it's 5-1)
                                ; to loop with the DBRA
    MOVEQ   #0,D2
    MOVE.W  VERT_SCROLL_POS,D2
    CMPI.W  #0,D2
    BGE.S  .check_max
    MOVE.W  #0,VERT_SCROLL_POS 
    MOVEQ   #0,D2
.check_max:
    CMPI.W  #208,D2
    BLE.S  .cont
    MOVE.W  #208,VERT_SCROLL_POS 
    MOVE.W   #208,D2
.cont:
    MULU    #240,D2
    ADD.L   D2,D0

.POINTBP:
    MOVE.W  d0,6(a1)            ; copies the LOW word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (e.g.: 1234 > 3412)
                                ; putting the word HIGH in place of that
                                ; LOW, allowing it to be copied with MOVE.W!!
    MOVE.W  d0,2(a1)            ; copies the HIGH word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (ex: 3412 > 1234)
                                ; resetting the address.
    ADD.L   #48,d0              ; Length of one line of bitplane in bytes, here it is 384/8 = 48

    ADDQ.W  #8,a1               ; add 8 to address, a1 now contains the address of the next
                                ; bplpointers in the copperlist to write.
    DBRA    d1,.POINTBP          ; Redo D1 times POINTBP (D1=num of bitplanes (-1))
    ;MOVE.W  d0,$dff088          ; let's start the copper
    RTS

Init:
    BSR.W   SetINT              ; Setup keyboard reading handler


InitBitplanes:
    MOVE.L  #PIC1,d0            ; to d0 we put the address of the PIC,
                                ; that is, where the first bitplane begins

    LEA     BPLPOINTERS,A1      ; in a1 we put the address of the
                                ; pointers to COPPERLIST planes
    MOVEQ   #4,D1               ; number of bitplanes -1 (here it's 5-1)
                                ; to loop with the DBRA
.POINTBP:
    MOVE.W  d0,6(a1)            ; copies the LOW word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (e.g.: 1234 > 3412)
                                ; putting the word HIGH in place of that
                                ; LOW, allowing it to be copied with MOVE.W!!
    MOVE.W  d0,2(a1)            ; copies the HIGH word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (ex: 3412 > 1234)
                                ; resetting the address.
    ADD.L   #48,d0              ; Length of one line of bitplane in bytes, here it is 384/8 = 48

    ADDQ.W  #8,a1               ; add 8 to address, a1 now contains the address of the next
                                ; bplpointers in the copperlist to write.
    DBRA    d1,.POINTBP          ; Redo D1 times POINTBP (D1=num of bitplanes (-1))

    MOVE.W  #DMASET2,$96(a5)    ; DMACON - Enable Bitplane, Copper DMA

.StartCopper:
    MOVE.L  #COPPERLIST,$dff080 ; We set our copperlist
    MOVE.W  d0,$dff088          ; let's start the copper

    MOVE.W  #0,$dff1fc          ; FMODE - Turn off the AGA
    MOVE.W  #$c00,$dff106       ; BPLCON3 - Turn off the AGA
    MOVE.W  #$11,$10c(a5)       ; Turn off the AGA


    BRA.W    Main

InitStatusbar:
    MOVE.L  #STATUSBAR,d0            ; to d0 we put the address of the PIC,
                                ; that is, where the first bitplane begins

    LEA     STATUSBPLPOINTERS,A1      ; in a1 we put the address of the
                                ; pointers to COPPERLIST planes
    MOVEQ   #4,D1               ; number of bitplanes -1 (here it's 5-1)
                                ; to loop with the DBRA
.POINTBP:
    MOVE.W  d0,6(a1)            ; copies the LOW word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (e.g.: 1234 > 3412)
                                ; putting the word HIGH in place of that
                                ; LOW, allowing it to be copied with MOVE.W!!
    MOVE.W  d0,2(a1)            ; copies the HIGH word of the plane address
                                ; in the right word in the copperlist
    SWAP    d0                  ; SWAP the 2 words of d0 (ex: 3412 > 1234)
                                ; resetting the address.
    ADD.L   #48,d0              ; Length of one line of bitplane in bytes, here it is 384/8 = 48

    ADDQ.W  #8,a1               ; add 8 to address, a1 now contains the address of the next
                                ; bplpointers in the copperlist to write.
    DBRA    d1,.POINTBP          ; Redo D1 times POINTBP (D1=num of bitplanes (-1))

    ;MOVE.W  #DMASET2,$96(a5)    ; DMACON - Enable Bitplane, Copper DMA

    RTS

SetINT:

    LEA     CIAA,a2
    MOVE.B  #$7f,CIAICR(a2)         ;disable all CIA-A ints
    MOVE.B  #TIMERB_LO,CIATBLO(a2)
    MOVE.B  #TIMERB_HI,CIATBHI(a2)
    MOVE.B  #$18,CIACRB(a2)         ;one-shot mode and load
    MOVE.L  #0,a0
    MOVE.L  #KeyboardInterrupt,IRQ2(a0)


    MOVE.W  #INTF_SETCLR|INTF_INTEN|INTF_PORTS,INTENA(a5)

SetupKeyboard:
    MOVE.B  #142,(CIAATALO)                 ; init timer-a (~200 µs)
    SF      (CIAATAHI)
    MOVE.B  #$7f,(CIAAICR)                  ; allow interrupts from the keyboard & timer-a
    MOVE.B  #(CIAICRF_SETCLR|CIAICRF_SP|CIAICRF_TA),(CIAAICR)
    TST.B   (CIAAICR)                       ; clear all ciaa-interrupt requests
    AND.B   #~(CIACRAF_SPMODE),(CIAACRA)    ; set input mode
    MOVE    #INTF_PORTS,(CHIPBASE+INTREQ)   ; clear ports interrupt    
    RTS



bufferwritepointer:
    DC.B    0
bufferreadpointer:
    DC.B    0

buffer:
    DCB.B   256,0
bufferend:

KB1_U:
    DC.B    0
KB1_D:
    DC.B    0
KB1_L:
    DC.B    0
KB1_R:
    DC.B    0
KB1_F:
    DC.B    0
KB2_U:
    DC.B    0
KB2_D:
    DC.B    0
KB2_L:
    DC.B    0
KB2_R:
    DC.B    0
KB2_F:
    DC.B    0


Main:
    BSR.W   InitStatusbar
    BSR.W   SwapBuffers    
MainLoop:
    LEA     CIAA,a4             ; ciaa for keyboard
    LEA     CUSTOM,A5           ; vhposr (006)

    MOVE.W  #$cc00,$DFF034      ; set POTGO for second joystick button to work properly for both ports

    ; Do calc stuff

    ; check controls
    BSR.W   GetKeyFromBuffer

    ; if repeat, get the key from memory
    MOVE.B  KeyState,d1
    CMP.B   #KEY_REPEAT,d1
    BNE.S   .1
    MOVE.B  KeyRaw,D0

.1:
    CMP.B   #$01,D0
    BNE.W   .2
    ADD.W   #2,BP0
.2:
    CMP.B   #$02,D0
    BNE.W   .3
    SUB.W   #2,BP0
.3:
    CMP.B   #$03,D0
    BNE.W   .4
    ADD.W   #2,BP1
.4:
    CMP.B   #$04,D0
    BNE.W   .5
    SUB.W   #2,BP1
.5:
    CMP.B   #$05,D0
    BNE.W   .6
    ADD.W   #2,BP2
.6:
    CMP.B   #$06,D0
    BNE.W   .7
    SUB.W   #2,BP2
.7:
    CMP.B   #$07,D0
    BNE.W   .8
    ADD.W   #2,BP3
.8:
    CMP.B   #$08,D0
    BNE.W   .9
    SUB.W   #2,BP3
.9:
    CMP.B   #$09,D0
    BNE.W   .0
    ADD.W   #2,BP4
.0:
    CMP.B   #$0a,D0
    BNE.S   .checkJoystick
    SUB.W   #2,BP4

.checkJoystick:

    BTST    #2,$dff016          ; if the right button is pressed jumps
    BEQ.W   Wait                ; the scroll routine, blocking it

    MOVE.W  $dff00c,D3          ; JOY1DAT = joystick 1 = game

    BTST.L  #1,D3               ; bit 1 tells us if we go right
    BEQ.S   .noRight            ; if it's zero, you don't go right
    BSR.W   MoveRight
    BRA.S   .check_Y            ; go to control of the Y
.noRight:
    BTST.L  #9,D3               ; bit 9 tells us if we go left
    BEQ.S   .check_Y            ; if it's zero, you don't go left
    BSR.W   MoveLeft
.check_Y:
    MOVE.W  D3,D2               ; copy the registry value
    LSR.W   #1,D2               ; shifts bits one place to the right
    EOR.W   D2,D3               ; performs the exclusive or. Now we can test
    BTST.L  #8,D3               ; let's test if it goes high
    BEQ.S   .noUp               ; if not check if it goes down
    BSR.W   MoveWindowUp
    BRA.S   .esc
.noUp
    BTST.L  #0,D3               ; let's test if it goes low
    BEQ.S   .esc      ; if not, finish
    BSR.W   MoveWindowDown

.esc
    CMP.B   #$45,d0             ; ESC
    BEQ.W   .end


.loop:
    MOVE.L  $dff004,d0            ; Wait for Vertical Blank
    AND.L   #$1ff00,d0
    CMP.L   WBLANKLINE,d0
    BNE.S   .loop                ; loop until VB
    BNE.S   .checkl
    NOP
.checkl:

    CMPI.B  #1,CHANGEPOSL
    BNE.S   .checkr
    BSR.W   ChangeStatusPosLeft
.checkr:
    CMPI.B  #1,CHANGEPOSR
    BNE.S   .continue
    BSR.W   ChangeStatusPosRight
.continue:
    BSR.W   SwapBuffers
    BRA.W   MainLoop
.end:
    RTS        ; End the mainloop

SwapBuffers:
    CMPI.L  #PIC1,BLITTOTHIS_BUF
    BNE.S   .setbuffer1
    BSR.W   SetPic1
    BRA.S   .end
.setbuffer1:
    BSR.W   SetPic2
.end:
    RTS
    
GetKeyFromBuffer:
    MOVE.L  A2,-(SP)
    MOVE.L  D3,-(SP)
    MOVEQ   #0,D3
    LEA     buffer,A2
    MOVE.B  bufferreadpointer,D3
    MOVE.B  (A2,D3),D0
    CMP.B   #0,D0
    BEQ.S   .end
    MOVE.B  #0,(A2,D3)
    ADD.B   #1,bufferreadpointer
.end:
    MOVE.L  (SP)+,D3
    MOVE.L  (SP)+,A2
    RTS

KeyRaw:     DC.B    0        ; Raw code of last pressed key
KeyState:   DC.B    0         ; State of last pressed key

mouse:
    CMPI.B  #$ff,$dff006    ; We are at the line 255?
    BNE.S   mouse            ; If not yet, don't move forward

    BTST    #2,$dff016        ; if the right button is pressed jumps
    BEQ.S   Wait            ; the scroll routine, blocking it

    MOVE.W  $dff00c,D3        ; JOY1DAT = joystick 1 = game

    BTST.L  #1,D3            ; bit 1 tells us if we go right
    BEQ.S   .noRight        ; if it's zero, you don't go right
    BSR.W   MoveRight
    BRA.S   .check_Y            ; go to control of the Y
.noRight:
    BTST.L  #9,D3            ; bit 9 tells us if we go left
    BEQ.S   .check_Y        ; if it's zero, you don't go left
    BSR.W   MoveLeft
.check_Y:
    MOVE.W  D3,D2        ; copy the registry value
    LSR.W   #1,D2        ; shifts bits one place to the right
    EOR.W   D2,D3        ; performs the exclusive or. Now we can test
    BTST.L  #8,D3        ; let's test if it goes high
    BEQ.S   .noUp        ; if not check if it goes down
    BSR.W   MoveWindowUp
    BRA.S   .end
.noUp
    BTST.L  #0,D3                ; let's test if it goes low
    BEQ.S   .end                ; if not finish
    BSR.W   MoveWindowDown
.end:


Wait:
    CMPI.B  #$ff,$dff006    ; We are at the line 255?
    BEQ.S   Wait            ; If yes, don't go on, Wait!

    BTST    #6,$bfe001        ; left mouse button pressed?
    BNE.S   mouse            ; if not, go back to mouse:
    BRA.S   mouse

    MOVE.L  OldCop(PC),$dff080    ; Let's point the system cop
    MOVE.W  d0,$dff088            ; let's start the old cop

    MOVE.L  4.w,a6
    JSR     -$7e(a6)                ; Enable - re-enable Multitasking
    MOVE.L  GfxBase(PC),a1        ; Library base to close
    JSR     -$19e(a6)                ; Closelibrary - close the graphics lib
    RTS                            ; EXIT THE PROGRAM

;    Data



OldCop:            ; Here goes the address of the old system COP
    dc.l    0

CHANGEPOSL:
    DC.B    0
CHANGEPOSR
    DC.B    0

ChangeStatusPosLeft:
    ADDI.W  #2,BP0
    ADDI.W  #2,BP1
    ADDI.W  #2,BP2
    ADDI.W  #2,BP3
    ADDI.W  #2,BP4
    CMPI.B  #$28,DDFST
    BNE.S   .check4th
    SUBI.W  #2,BP3
    SUBI.W  #2,BP4
.check4th
    CMPI.B  #$20,DDFST
    BNE.S   .setcon1
    SUBI.W  #2,BP3
    SUBI.W  #2,BP4
.setcon1
    MOVE.B  #0,CHANGEPOSL
    RTS

ChangeStatusPosRight:
    SUBI.W  #2,BP0
    SUBI.W  #2,BP1
    SUBI.W  #2,BP2
    SUBI.W  #2,BP3
    SUBI.W  #2,BP4
    CMPI.B  #$20,DDFST
    BNE.S   .check4th
    ADDI.W  #2,BP3
    ADDI.W  #2,BP4
.check4th
    CMPI.B  #$18,DDFST
    BNE.S   .setcon1
    ADDI.W  #2,BP3
    ADDI.W  #2,BP4
.setcon1
    MOVE.B  #0,CHANGEPOSR
    RTS

MoveLeft:
    CMPI.B  #$EE,MINCON1    ; we have arrived at the normal position, i.e
    BEQ.S   .changemod
    CMPI.B  #$FF,MINCON1    ; we have arrived at the normal position, i.e
    BEQ.S   .changemod
    ADD.B   #$22,MINCON1    ; we add 1 to the bitplanes scroll
    RTS
.changemod:
    CMPI.B  #$30,DDFST    ; we have arrived at the normal position, i.e
    BEQ.S   .end
    ADDI.B  #8,DDFST
    ADDI.W  #2,MOD1
    ADDI.W  #2,MOD2
    MOVE.B  #1,CHANGEPOSL
    MOVE.B  #$00,MINCON1    ; set con1 byte to min
.end:
    RTS

MoveRight:
    CMPI.B  #$00,MINCON1    ; are we at zero...
    BEQ.S   .changemod
    CMPI.B  #$11,MINCON1    ; or one
    BEQ.S   .changemod
    SUB.B   #$22,MINCON1    ; we subtract 2 to the bitplanes scroll
    RTS
.changemod:
    CMPI.B  #$18,DDFST      ; are at the end
    BEQ.S   .end
    SUBI.B  #8,DDFST      ; change data fetch start
    SUBI.W  #2,MOD1       ; and update modulos
    SUBI.W  #2,MOD2
    MOVE.B  #1,CHANGEPOSR
    MOVE.B  #$ff,MINCON1    ; set con1 byte to max
.end
    RTS

MoveWindowUp:
    CMPI.W  #0,VERT_SCROLL_POS
    BLE.S   .end
    SUB.W   #2,VERT_SCROLL_POS
.end:
    RTS

MoveWindowDown:
    CMPI.W  #208,VERT_SCROLL_POS
    BGE.S   .end
    ADD.W   #2,VERT_SCROLL_POS
.end:
    RTS


BLITTOTHIS_BUF:
    DC.L    0
DISPLAY_BUF:
    DC.L    0



WBLANKLINE:
    DC.L    303<<8 ; PAL

vbrBase:    RS.L    0

    INCLUDE "ptplayer.asm"

    EVEN

KeyboardInterrupt:
    MOVEM.L d0-d2/a2,-(a7)
    BTST    #INTB_PORTS,(CHIPBASE+INTREQR+1)                                        ; check if keyboard has caused interrupt
    BEQ.B   .end    
    MOVE.B  CIAAICR,d0                                                              ; timer-a
    BTST    #CIAICRB_TA,d0
    BEQ     .cont        
    SF      CIAACRA                                                                 ; set input mode (handshake end)
    BRA.B   .end
.cont:        
    BTST    #CIAICRB_SP,d0
    BEQ.B   .end
    MOVE.B  CIAASDR,d0                                                              ; read keycode
    MOVE.B  #CIACRAF_SPMODE|CIACRAF_LOAD|CIACRAF_RUNMODE|CIACRAF_START,(CIAACRA)    ; set output mode (handshake start)    
    NOT.B   d0
    ROR.B   #1,d0                                                                   ; calculate rawkeycode
; check the key and save it into buffer
    bpl.b   .Down                    ; IF Key was reLEAsed
    CMP.B   KeyRaw,d0                ; IF old key == new key 
    BNE.B   .New
    MOVE.B  #KEY_REPEAT,KeyState    ;   KeyState = KEY_NONE
    MOVE.B  d0,KeyRaw                ;   KeyRaw = new key with bit 7 set!
    BRA.B   .end                    ; ENDIF
.Down:
    CMP.B   KeyRaw,d0                ; IF old key == new key 
    BNE.B   .New
    MOVE.B  #KEY_REPEAT,KeyState    ;   KeyState = KEY_REPEAT
    BRA.B   .end                    ; ENDIF
.New:
    MOVE.B  D0,KeyRaw                ; KeyRaw = new key
    MOVEQ   #0,D2
    MOVE.B  bufferwritepointer,D2
    LEA     buffer,A2
    MOVE.B  D0,(A2,D2)
    ADD.B   #1,bufferwritepointer
    MOVE.B  #KEY_NEW,KeyState        ; KeyState = KEY_NEW


.end:
    MOVE.W  #INTF_PORTS,(INTREQ+CHIPBASE)
    TST.W   (INTREQR+CHIPBASE)                                                      ; to avoid timing problems on very fast machines we do another custom register access
    MOVEM.L (a7)+,d0-d2/a2
    RTE

VERT_SCROLL_POS:
    DC.W    0
HORI_SCROLL_POS:
    DC.W    0

    EVEN

    SECTION GRAPHIC,DATA_C

COPPERLIST:

; We make the sprites point to ZERO, to eliminate them

    DC.W    $120,$0000,$122,$0000,$124,$0000,$126,$0000,$128,$0000
    DC.W    $12a,$0000,$12c,$0000,$12e,$0000,$130,$0000,$132,$0000
    DC.W    $134,$0000,$136,$0000,$138,$0000,$13a,$0000,$13c,$0000
    DC.W    $13e,$0000

    DC.W    $8e,$2c81   ; DiwStrt    (logs with normal values)
    DC.W    $90,$F4C1   ; DiwStop
    DC.W    $92         ; DdfStart
    DC.B    $00
DDFST:
    DC.B    $30
    DC.W    $94,$00d0   ; DdfStop
    DC.W    $102        ; BplCon1
    DC.B    $00
MINCON1:
    DC.B    $ff
    DC.W    $104,0      ; BplCon2
    DC.W    $108        ; Bpl1Mod
MOD1:
    DC.W    198         ; 384/4=48, 48*(5-1) = 192
    DC.W    $10a
MOD2:
    DC.W    198         ; Bpl2Mod  ; 384/4=48, 48*(5-1) = 192

; the BPLCON0 ($dff100) For a 5 bitplanes screen: (32 colors)

                ; 5432109876543210
    DC.W    $100,%0101001000000000    ; bits 14 and 12 on!! (5 = %101)

; We make bitplanes stake directly by putting them in the copperlist
; the registers $dff0e0 and following below with the addresses
; of the bitplanes which will be set by the POINTBP routine

BPLPOINTERS:
    DC.W    $e0,$0000,$e2,$0000    ;first   bitplane - BPL0PT
    DC.W    $e4,$0000,$e6,$0000    ;second  bitplane - BPL1PT
    DC.W    $e8,$0000,$ea,$0000    ;third   bitplane - BPL2PT
    DC.W    $ec,$0000,$ee,$0000    ;fourth  bitplane - BPL3PT
    DC.W    $f0,$0000,$f2,$0000    ;fifth   bitplane - BPL4PT

; Palette

    INCBIN  "4bitcop.pal"

; Enter any WAIT effects here

    DC.W    $D501
    DC.W    $ff00;        ; Wait for vpos >= 188 and hpos >= 222
STATUSBPLPOINTERS:
    DC.W    $e0,$0000
    DC.W    $e2
BP0:
    DC.W    $0000    ;first   bitplane - BPL0PT
    DC.W    $e4,$0000
    DC.W    $e6
BP1:
    DC.W    $0000    ;second   bitplane - BPL0PT
    DC.W    $e8,$0000
    DC.W    $ea
BP2:
    DC.W    $0000    ;third   bitplane - BPL0PT
    DC.W    $ec,$0000
    DC.W    $ee
BP3:
    DC.W    $0000    ;fourth   bitplane - BPL0PT
    DC.W    $f0,$0000
    DC.W    $f2
BP4:
    DC.W    $0000    ;fifth   bitplane - BPL0PT


	DC.W	$92
	DC.B	$00
MIDDdfStart:
    DC.B    $35		    ; DdfStart
	DC.W	$102		; BplCon1
	DC.B	$00			; BplCon1
MIDBPLCON1:
	DC.B	$88			; BplCon1
    DC.W    $108        ; Bpl1Mod
MIDBPL1MOD:
	DC.W	198;200-192
	DC.W	$10a		; Bpl2Mod
MIDBPL2MOD:
	DC.W	198;200-192 for 384

    DC.W    $FFFF,$FFFE    ; END OF THE COPPERLIST

    EVEN

STATUSBAR:
    INCBIN "test_statusbar_384x32_32c.iblit"

PIC1:
    INCBIN "test_playfield_384x384_32c.iblit"    ; here we load the figure in RAW,
;    DS.B    142080
PIC2:
    INCBIN "test_playfield_384x384_32c.iblit"    ; here we load the figure in RAW,
;    DS.B    142080


    END
