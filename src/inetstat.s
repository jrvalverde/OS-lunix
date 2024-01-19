#include "kernel.inc"

.head "inetstat", .zp1, zp2, zp3

.data
magic:

ip_struct:
          .buf 8

.code

local_port  equ ip_struct+0
remote_ip   equ ip_struct+2
remote_port equ ip_struct+6

ip_api:

ip_connect:  jmp  suicide
ip_listen:   jmp  suicide
ip_accept:   jmp  suicide
ip_sockinfo: jmp  suicide
ip_putbyte:  jmp  suicide
ip_getbyte:  jmp  suicide
ip_close:    jmp  suicide

; print decimal to stdout

print_decimal:
          ldx  #3
          ldy  #0

        - sec
          lda  zp1
          sbc  dec_tab,x
          lda  zp1+1
          sbc  dec_tabh,x
          bcc  +
          sta  zp1+1
          lda  zp1
          sbc  dec_tab,x
          sta  zp1
          iny
          bne  -

        + tya
          beq  +
          ora  #"0"
          jsr  putc
          inc  zp2
          ldy  #"0"
        + dex
          bpl  -

          lda  zp1
          ora  #"0"
          inc  zp2
          jmp  putc

print_ip:
          lda  #0
          sta  zp1+1
          lda  remote_ip
          sta  zp1
          jsr  print_decimal
          ldx  #1

        - stx  tmp
          lda  #"."
          jsr  putc
          inc  zp2
          lda  remote_ip,x
          sta  zp1
          jsr  print_decimal
          ldx  tmp
          inx
          cpx  #4
          bne  -

          rts

fillup:
          sta  zp1
        - lda  #" "
          jsr  putc
          ldx  zp2
          inx
          stx  zp2
          cpx  zp1
          bne  -
          rts

print_status:
          and  #$0f
          ldy  #0
          tax
          beq  +

        - iny
          lda  txt_stati-1,y
          bne  -
          dex
          bne  -

      - + lda  txt_stati,y
          beq  +
          jsr  putc
          iny
          bne  -
        + rts

_init:
          lda  _base+8
          sei
          beq  +
          ldx  #0
          stx  _base+8
          jsr  raw_mfree
        + cli

          ldx  stdin_ch
          jsr  pclose

          ; search for packet interface

          ldx  #15

        - lda  $c2a0,x
          cmp  #"I"            ; "I" for TCP/IP (driver)
          beq  +
        - dex
          bpl  --

          ldx  stderr_ch
          jsr  strout
          bit  txt_tcpip
          jmp  suicide

        + lda  $c2b0,x
          sta  zp1+1
          ldy  #(magic-_base)
          sty  zp1
          ldy  #9

        - lda  (zp1),y
          cmp  txt_tcpip_magic,y
          bne  --
          dey
          bpl  -

          ; plug in TCP/IP driver

          lda  #(magic-_base)+10
          sta  zp1
          ldy  #20

        - lda  (zp1),y
          sta  ip_api,y
          dey
          bpl  -

          ; start
 
          ldx  stdout_ch
          jsr  strout
          bit  txt_tabhead

          lda  #0
          sta  zp3

        - ldx  zp3
          jsr  ip_sockinfo
          bit  ip_struct
          bcs  +

          ; print socket info

          pha
          lda  #0
          sta  zp2
          sta  zp1+1
          lda  zp3
          sta  zp1
          jsr  print_decimal
          lda  #3
          jsr  fillup
          lda  local_port
          sta  zp1
          lda  local_port+1
          sta  zp1+1
          jsr  print_decimal
          lda  #9
          jsr  fillup
          jsr  print_ip
          lda  #25
          jsr  fillup
          lda  remote_port
          sta  zp1
          lda  remote_port+1
          sta  zp1+1
          jsr  print_decimal
          lda  #31
          jsr  fillup
          pla
          and  #$0f
          jsr  print_status
          lda  #"\n"
          jsr  putc
#ifdef debug
          jsr  deep_debug
#endif

        + inc  zp3
          bne  -

          jmp  suicide

#ifdef debug
deep_debug:
          
          
#endif

.endofcode

tmp:      .buf 1

dec_tab:
          .byte <10,<100,<1000,<10000
dec_tabh:
          .byte >10,>100,>1000,>10000

txt_tcpip_magic:
          .asc "ipv4/tcp" ; type
          .byte 1,0       ; version
txt_tcpip:
          .asc "no TCP/IP driver found\n\0"
txt_tabhead:
          .asc "so lport remote-ip       rport status\n\0"

txt_stati:
          .asc "closed\0"
          .asc "listen\0"
          .asc "syn-sent\0"
          .asc "syn-rcvd\0"
          .asc "estab\0"
          .asc "finwait1\0"
          .asc "finwait2\0"
          .asc "close-w\0"
          .asc "closing\0"
          .asc "last-ack\0"
          .asc "timewait\0"
          .asc "$b\0"
          .asc "$c\0"
          .asc "$d\0"
          .asc "$e\0"
          .asc "$f\0"