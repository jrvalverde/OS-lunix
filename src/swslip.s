; Packetdriver for LUnix-TCP/IP
; using swiftlink and SLIP encapsulation

; packetdriver MUST keep the order of the packets
; in both, send and receive direction !

#include "kernel.inc"

#define swift_base  $de00      ; baseaddress of swiftlink cardridge
#define maxbufs     12         ; max number of handled buffers

          sw_ioreg   equ swift_base+$00
          sw_status  equ swift_base+$01
          sw_command equ swift_base+$02
          sw_control equ swift_base+$03

#ifdef debug
#  begindef db(text)
    php
    pha
    txa
    pha
    tya
    pha
    print_string(text)
    pla
    tay
    pla
    tax
    pla
    plp
#  enddef
#else
#  define db(text)
#endif

.head "swslip", .ptr

tcp_clock equ 161     ; counter that is incremented every 4 seconds

.data

;magic

          .asc "ipack"
          .byte 1,0

.code

;API

          jmp  user_init
          jmp  user_getpack
          jmp  user_putpack
          jmp  err_notsup
          jmp  err_notsup

err_notsup:
          db("unsupported call to swslip\n")
          sec
          rts

;-------------------------------------------------------------------
; NMI routine
;-------------------------------------------------------------------

; NMI routine for receiving and sending packets

nmi_start:
          pha
          lda  #$03
          sta  sw_command
          txa
          pha
          lda  1
          pha
          ora  #7
          sta  1
          cld

          lda  sw_status
          and  #$18
          beq  nmi_ret
          and  #$08
          beq  _sndnmi         ; send...

; receiving...

        - lda  sw_ioreg
          bit  recstat
          bmi  esced
          cmp  #192
          beq  packend
          cmp  #219
          beq  isesc

dest_addr equ  *+1
mem_write:
          sta  $0400
          inc  dest_addr
          beq  ++              ; crossed page boundary

nmi_end:  ldx  sw_status
          txa
          and  #$08
          bne  -

          txa
          and  #$10
          beq  nmi_ret
          lda  sndstat
          bne  _sndnmi         ; send...

nmi_ret:
          bit  $dd0d
          bpl  +
          inc  tcp_clock
        + pla
          sta  1
          pla
          tax
swiftcom  equ  *+1
          lda  #0
          sta  sw_command
          pla
          rti

_sndnmi:  jsr  sendevent
          jmp  nmi_end
          
isesc:    lda  #128
          sta  recstat
          jmp  nmi_end
          
        + clc                  ; ... next page
          lda  dest_addr+1
          adc  #1
          sta  dest_addr+1
          sec
          ldx  reclst.c
          sbc  buf.mid,x       ; i assume buf.lenl is $00 !!!!!
          cmp  buf.lenh,x
          bcc  isnotfull

ignoreRest:                    ; packet is too big ! (disard)
          lda  #192
          sta  recstat
isnotfull:
recend:   jmp  nmi_end

esced:    bvs  ignore2
          cmp  #221
          beq  convtoesc
          cmp  #220
          bne  ignoreRest
          lda  #192
          .byte $2c

convtoesc:
          lda  #219
          ldx  #0
          stx  recstat
          jmp  mem_write

ignore2:  cmp  #192
          bne  recend
          
packend:  bit  recstat
          bvc  isvalid1

skip:     inc  errcnt
          bne  +
          inc  errcnt+1
        + inc  lost_count
          jmp  setpage

isvalid1: inc  reccnt
          bne  +
          inc  reccnt+1
          bne  +
          inc  reccnt+2
        +
#ifdef debug
          inc  $401
#endif
          ldx  reclst.c
          lda  dest_addr+1
          sec
          sbc  buf.mid,x
          bne  +
          lda  dest_addr
          cmp  #20
          bcc  skip            ; discard packets smaller than 20 bytes
          lda  #0
        + sta  buf.lenh,x
          lda  dest_addr
          sta  buf.lenl,x
          lda  #$40
          sta  buf.stat,x      ; mark buffer ("done")
          lda  buf.l2nx,x      ; switch no next buffer
          bmi  endof_reclst
          sta  reclst.c

setpage:  ldx  reclst.c        ; prepare for writing into current buffer
          bmi  nomorebuffer
          lda  buf.mid,x
          sta  dest_addr+1
          lda  #0
          sta  dest_addr
          sta  recstat
          jmp  nmi_end

endof_reclst:
          sta  reclst.c

nomorebuffer:
          lda  #192
          sta  recstat
          jmp  nmi_end

; sending...

sendevent:
          lda  #$20
          bit  sndstat
          bvc  _00xx
          beq  _010x
          ;  _011x is snd next

source_addr equ *+1
mem_read:
          ldx  $0400
          cpx  #192
          beq  escend
          cpx  #219
          beq  escesc
aftlda:   stx  sw_ioreg
          inc  source_addr
          bne  +
          inc  source_addr+1
        + lda  #$60
          sta  sndstat
          inc  sndlenl
          bne  +
          inc  sndlenh
          bne  +
          
          ;   end of packet
          lda  #$10
          sta  sndstat
        + rts
          
escend:   lda  #$40
          .byte $2c
escesc:   lda  #$20
          sta  sndstat
          lda  #219
          sta  sw_ioreg
          rts
          
_010x:    ;   is esced end
          ldx  #220
          .byte $2c
_001x:    ;   is esced esc
          ldx  #221
          jmp  aftlda
          
_00xx:    bne  _001x
          ;  _000x
          lda  sndstat
          beq  +
          and  #$10
          beq  snp
          
          lda  #$08
          sta  sndstat
          lda  #$c0
          sta  sw_ioreg
        + rts
          
snp:      bit  sndlock         ; get next buffer
          bmi  eloo
          ldx  sndlst.c
          lda  #$40
          sta  buf.stat,x      ; mark buffer ("done")
          lda  buf.l2nx,x
          sta  sndlst.c
#ifdef debug
          inc  $402
#endif

news:     ldx  sndlst.c
          bmi  nompage
          lda  buf.mid,x
          sta  source_addr+1
          lda  #0
          sta  source_addr
          clc
          lda  buf.lenl,x
          eor  #255
          adc  #1
          sta  sndlenl
          lda  buf.lenh,x
          eor  #255
          adc  #0
          sta  sndlenh
          lda  #$60
          sta  sndstat
eloo:     lda  #$c0
          sta  sw_ioreg
          rts
          
nompage:  lda  #0
          sta  sndstat
          lda  #$09
          sta  swiftcom
          rts
                    
ioinit:   sei

          ; swiftlink part

          ldx  #0
          stx  sndstat
          lda  #$c0
          sta  recstat
          ldx  [+]+1
          ldy  [+]+2
          stx  $318
          sty  $319
          lda  #0
          sta  sndlock
_baudrate equ *+1
          lda  #$1c
          sta  sw_control
          lda  sw_status
          lda  #$09
          sta  swiftcom
          sta  sw_command

          ; RT timer part

          lda  #<9849   ; timer A set to 1/100 s
          sta  $dd04
          lda  #>9849
          sta  $dd05
          lda  #<400    ; timer B set to 400 -> 4s
          sta  $dd06
          lda  #>400
          sta  $dd07
          lda  $dd0e
          and  #$80
          ora  #$11
          sta  $dd0e
          lda  #$51
          sta  $dd0f
          lda  #$7f
          sta  $dd0d
          lda  $dd0d
          lda  $dd0d
          lda  #$82
          sta  $dd0d

          cli
          rts

        + bit  nmi_start

io_suspend:
          lda  #$02
          sta  $dd0d
          lda  #3
          sta  sw_command
          rts

io_wakeup:
          lda  swiftcom
          sta  sw_command
          lda  #$82
          sta  $dd0d
          rts
          
clean_sndlst:
          sei
          ldx  sndlst.t
          bmi  ++
          lda  buf.stat,x
          cmp  #$40
          bne  ++
          lda  buf.l2nx,x
          sta  sndlst.t
          bpl  +
          sta  sndlst.b
        + txa
          pha
          lda  buf.mid,x
          jsr  raw_mfree
          pla
          tax
          sei
          ldy  freelst
          stx  freelst
          tya
          sta  buf.l2nx,x
          cli
          jmp  clean_sndlst

        + cli
          rts
          
;-------------------------------------------------------------------
; API
;-------------------------------------------------------------------

        - lda  ipid
          cmp  user_ipid
          bne  +               ; no permission
          lda  #$ff
          sta  user_ipid
          clc
          cli
          rts

user_init:
          sei
          bcc  -
          lda  user_ipid
          bpl  +               ; can't can 2 users
          lda  ipid
          sta  user_ipid
          clc
          cli
          rts

        - db("putpack overrun\n")
          jsr  raw_mfree
      + - sec
          cli
          rts

user_getpack:

          sei
          ldx  reclst.t
          bmi  -
          lda  buf.stat,x
          cmp  #$40
          bne  -
          lda  buf.l2nx,x
          sta  reclst.t
          bpl  +
          sta  reclst.b
        + ldy  freelst
          stx  freelst
          tya
          sta  buf.l2nx,x
          lda  buf.mid,x
          pha
          ldy  buf.lenh,x
          lda  buf.lenl,x
          tax
          pla
          clc
          cli
          db("got pack\n")
          rts

user_putpack:
  
          sei
          bit  freelst
          bmi  --
          pha
          txa
          pha
          ldx  freelst
          tya
          sta  buf.lenh,x
          pla
          sta  buf.lenl,x
          pla
          sta  buf.mid,x
          lda  buf.l2nx,x
          sta  freelst
          lda  #$80
          sta  buf.stat,x
          sta  buf.l2nx,x
          bcc  _putinreclst
          ldy  sndlst.b
          bmi  +
          txa
          sta  buf.l2nx,y
          bpl  ++
        + stx  sndlst.t
        + stx  sndlst.b
          dec  sndlock
          lda  sndstat
          bne  +
          stx  sndlst.c
          lda  #$05            ; initialize sender, neccessary
          sta  swiftcom
          sta  sw_command
          jsr  news
        + inc  sndlock
          clc
          cli
          db("put snd pack\n")
          rts

_putinreclst:
          ldy  reclst.b
          bmi  +
          txa
          sta  buf.l2nx,y
          bpl  ++
        + stx  reclst.t
        + stx  reclst.b
          bit  reclst.c
          bpl  +
          stx  reclst.c
        + clc
          cli
          db("put rec pack\n")
          rts

;-------------------------------------------------------------------
; main
;-------------------------------------------------------------------

_init:
          lda  _base+8
          beq  _default_speed
          sta  ptr+1
          ldy  #0
          sty  ptr
          iny

        - lda  (ptr),y
          beq  _default_speed
          cmp  #" "
          bne  +
          iny
          bne  -

        + sty  ptr
          ldx  #0

        - ldy  #0
          stx  current_rate

        - lda  baud_rates,x
          beq  +
          cmp  (ptr),y
          bne  _nx
          iny
          inx
          bne  -

_nx:      inx
          lda  baud_rates,x
          bne  _nx
          inx
          inx
          lda  baud_rates,x
          bpl  --

          ldx  stderr_ch
          jsr  strout
          bit  txt_howto
          lda  #$ff
          jmp  suicide

        + lda  baud_rates+1,x
          sta  _baudrate

_default_speed:
          lda  _base+8
          sei
          beq  +
          ldx  #0
          stx  _base+8
          jsr  raw_mfree
        + cli

          lda  #$ff
          ldx  #maxbufs-1

        - sta  buf.l2nx,x
          txa
          dex
          bpl  -

          ldx  #15

        - lda  $c2a0,x
          beq  +
          dex
          bpl  -
          jmp  suicide

        + lda  #"P"
          sta  $c2a0,x
          lda  _base
          sta  $c2b0,x

          ldx  stdout_ch
          jsr  strout
          bit  txt_running

          ldy  current_rate

        - lda  baud_rates,y
          beq  +
          jsr  putc
          iny
          bne  -
          
        + lda  #"\n"
          jsr  putc

          ldx  #0
          jsr  lock
          jsr  ioinit

; main loop

main_loop:
          jsr  clean_sndlst

          lda  reclst.c
          bpl  +
          lda  reclst.t
          bmi  +
          sta  reclst.c
          db("swslip:reclst reactivated\n")

        + lda  sndlst.c
          bpl  +
          lda  sndlst.t
          bmi  +
          sta  sndlst.c
          db("swslip:sndlst reactivated\n")

        + lda  lost_count
          beq  +
          db("swslip:lost incoming packet\n")
          dec  lost_count
          lda  reclst.c
          bmi  +
          db("swslip:internal error!!\n")

        + lda  freelst
          bpl  +
          db("swslip:freelist is empty\n")

        + ; check for blocked processes

          sei
          ldx  #31

        - lda  $c000,x
          and  #64
          beq  +
          lda  $c0c0,x
          cmp  #8
          bne  +
          lda  $c0e0,x
          cmp  #0
          beq  ++

        + dex
          bpl  -
          cli
          jsr  break
          jmp  main_loop

        + jsr  io_suspend
          ldx  #0
          jsr  unlock
          jsr  break
          ldx  #0
          jsr  lock
          jsr  io_wakeup
          jmp  main_loop

_drv_cleanup:
          rts

.endofcode 

;-------------------------------------------------------------------
; variables
;-------------------------------------------------------------------

recstat:   .byte 0
reccnt:    .byte 0,0,0
lost_count: .byte 0
errcnt:    .byte 0,0
sndstat:   .byte 0
sndlenl:   .byte 0
sndlenh:   .byte 0
sndlock:   .byte 0

user_ipid: .byte $ff

freelst:   .byte 0   ; list of free slots

sndlst.t:  .byte $ff ; top of send-list
sndlst.c:  .byte $ff ; pointer into send-list (to current buffer)
sndlst.b:  .byte $ff ; bottom of send-list

reclst.t:  .byte $ff ; top of receive-list
reclst.c:  .byte $ff ; pointer into receive-list (to current buffer)
reclst.b:  .byte $ff ; bottom of receive-list

buf.stat:  .buf maxbufs
buf.mid:   .buf maxbufs
buf.lenl:  .buf maxbufs
buf.lenh:  .buf maxbufs
buf.l2nx:  .buf maxbufs


current_rate: .byte baud_rdefault-baud_rates

baud_rates:
          .asc "16x\0"
                       .byte $00
          .asc "300\0"
                       .byte $15
          .asc "600\0"
                       .byte $16
          .asc "1200\0"
                       .byte $17
          .asc "2400\0"
                       .byte $18
          .asc "4800\0"
                       .byte $1a
baud_rdefault:
          .asc "9600\0"
                       .byte $1c
          .asc "19200\0"
                       .byte $1e
          .asc "38400\0"
                       .byte $1f
          .byte $ff

txt_howto:
          .asc "usage:  swslip [<baudrate>]\n"
          .asc "  baudrates: 16x 300 600 1200 2400\n"
          .asc "   4800 9600 19200 38400\n\0"

txt_running:
          .asc "swiftlink ($de00) internet packetdriver\n"
          .asc "installed. baudrate = \0"



