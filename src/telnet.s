#include "kernel.inc"

.head "telnet", .zp1, .zp2

#define IPV4_TCP    $01
#define IPV4_UDP    $02
#define IPV4_ICMP   $03

#define E_CONTIMEOUT $80
#define E_CONREFUSED $81
#define E_NOPERM     $82
#define E_NOPORT     $83
#define E_NOROUTE    $84
#define E_NOSOCK     $85
#define E_NOTIMP     $86
#define E_PROT       $87
#define E_PORTINUSE  $88

.data
magic:

tcp_stream: 
          .byte $ff
ip_struct:
          .buf 8

.code

remote_ip   equ ip_struct+0
remote_port equ ip_struct+4

ip_api:

ip_connect:  jmp  suicide
ip_listen:   jmp  suicide
ip_accept:   jmp  suicide
ip_sockinfo: jmp  suicide
ip_putbyte:  jmp  suicide
ip_getbyte:  jmp  suicide
ip_close:    jmp  suicide

; read decimal

read_decimal:
          lda  #0
          sta  zp2

        - lda  (zp1),y
          sec
          sbc  #"0"
          bcc  ++
          cmp  #10
          bcs  ++
          sta  zp2+1
          lda  zp2
          cmp  #26
          bcs  +
          asl  a
          asl  a
          adc  zp2
          asl  a
          sta  zp2
          adc  zp2+1
          bcs  +
          sta  zp2
          iny
          bne  -
        + sec
          rts

        + lda  zp2
          clc
          rts

; eat up white spaces

eat_spaces:
        - lda  (zp1),y
          cmp  #" "
          bne  +
          iny
          bne  -
          rts

        + cmp  #0
          rts

; read IP address from commandline

        - ldx  stderr_ch
          jsr  strout
          bit  txt_howto
          jmp  suicide

read_IPnPort:
          lda  _base+8
          beq  -
          sta  zp1+1
          ldy  #0
          sty  zp1
          iny
          jsr  eat_spaces
          beq  -
          jsr  read_decimal
          bcs  err_syntax
          sta  remote_ip
          ldx  #1

        - lda  (zp1),y
          cmp  #"."
          bne  err_syntax
          iny
          beq  err_syntax
          jsr  read_decimal
          bcs  err_syntax
          sta  remote_ip,x
          inx
          cpx  #4
          bne  -

          jsr  eat_spaces
          beq  +

          jsr  read_decimal
          bcs  err_syntax
          .byte $2c

        + lda  #23
          sta  remote_port
          lda  #0
          sta  remote_port+1

          rts

err_syntax:
          ldx  stderr_ch
          jsr  strout
          bit  txt_syntax
          jmp  suicide

; print decimal to stdout

print_decimal:
          ldx  #1
          ldy  #0

        - cmp  dec_tab,x
          bcc  +
          sbc  dec_tab,x
          iny
          bne  -

        + sta  zp2
          tya
          beq  +
          ora  #"0"
          jsr  putc
          ldy  #"0"
        + lda  zp2
          dex
          bpl  -

          ora  #"0"
          jmp  putc

print_ip:
          lda  remote_ip
          jsr  print_decimal
          ldx  #1

        - stx  zp1
          lda  #"."
          jsr  putc
          ldx  zp1
          lda  remote_ip,x
          jsr  print_decimal
          ldx  zp1
          inx
          cpx  #4
          bne  -

          rts

_init:
          jsr  read_IPnPort

          lda  _base+8
          sei
          beq  +
          ldx  #0
          stx  _base+8
          jsr  raw_mfree
        + cli

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

          ldx  stdout_ch
          jsr  strout
          bit  txt_trying
          jsr  print_ip
          lda  #"."
          jsr  putc
          jsr  putc
          jsr  putc
          lda  #"\n"
          jsr  putc

          ldx  #IPV4_TCP
          jsr  ip_connect
          bit  ip_struct
          bcc  +
          jmp  telnet_err
        + stx  tcp_stream

          ldx  stdout_ch
          jsr  strout
          bit  txt_conn
          jsr  print_ip
          lda  #"."
          jsr  putc
          lda  #"\n"
          jsr  putc

main_loop:
          jsr  break
          ldx  tcp_stream
          jsr  ip_sockinfo
          bit  ip_struct
          bcs  rem_closed
          and  #$0f
          cmp  #4
          bcc  rem_closed
          cmp  #8
          bcs  rem_closed
          cmp  #7
          bne  _getchar
          lda  #$ff
          sta  end_flag

_getchar: ldx  tcp_stream
          jsr  ip_getbyte
          bcs  ++
          bit  iacflag
          bmi  is_iac
          cmp  #$ff
          bne  +
          lda  #$80
          sta  iacflag
          bne  _getchar

        + jsr  putc
          bcs  u_break
          jmp  _getchar

        + bit  end_flag
          bmi  rem_closed
          ldx  stdin_ch
          jsr  pipe_chk
          bcs  u_break
          beq  main_loop
          jsr  getc
          bcs  u_break
          cmp  #171
          beq  u_break
          ldx  tcp_stream
          jsr  ip_putbyte
          bcc  main_loop
          beq  main_loop
          jmp  telnet_err

rem_closed:
          ldx  stdout_ch
          jsr  strout
          bit  txt_closed
u_break:
          ldx  tcp_stream
          jsr  ip_close
          jmp  suicide

is_iac:
          bvs  +
          sta  chbuf
          lda  #$c0
          sta  iacflag
          jmp  _getchar

        + tax
          lda  #0
          sta  iacflag
          lda  chbuf
          cmp  #$fd
          bne  _getchar

          ; respond $ff $fc $xx to $ff $fd $xx

          txa
          pha
          lda  #$ff
          ldx  tcp_stream
          jsr  ip_putbyte
          lda  #$fc
          ldx  tcp_stream
          jsr  ip_putbyte
          pla
          ldx  tcp_stream
          jsr  ip_putbyte
          jmp  _getchar

telnet_err:
          pha
          ldx  stderr_ch
          jsr  strout
          bit  txt_unable
          pla

          cmp  #E_CONTIMEOUT
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_CONTIMEOUT
          jmp  suicide
          +
          cmp  #E_CONREFUSED
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_CONREFUSED
          jmp  suicide
          +
          cmp  #E_NOPERM
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_NOPERM
          jmp  suicide
          +
          cmp  #E_NOPORT
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_NOPORT
          jmp  suicide
          +
          cmp  #E_NOROUTE
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_NOROUTE
          jmp  suicide
          +
          cmp  #E_NOSOCK
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_NOSOCK
          jmp  suicide
          +
          cmp  #E_NOTIMP
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_NOTIMP
          jmp  suicide
          +
          cmp  #E_PROT
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_PROT
          jmp  suicide
          +
          cmp  #E_PORTINUSE
          bne  +
          ldx  stderr_ch
          jsr  strout
          bit  txt_E_PORTINUSE
          +
          jmp  suicide

.endofcode

dec_tab:
          .byte 10,100
iacflag:  .byte 0
chbuf:    .byte 0
end_flag: .byte 0
txt_tcpip_magic:
          .asc "ipv4/tcp" ; type
          .byte 1,0       ; version
txt_trying:
          .asc "Trying \0"
txt_conn:
          .asc "Connected to \0"
txt_E_CONTIMEOUT:
          .asc "timeout error\n\0"
txt_E_CONREFUSED:
          .asc "connection refused\n\0"
txt_E_NOPERM:
          .asc "no permisson\n\0"
txt_E_NOPORT:
          .asc "no port\n\0"
txt_E_NOROUTE:
          .asc "no route to host\n\0"
txt_E_NOSOCK:
          .asc "no socket available\n\0"
txt_E_NOTIMP:
          .asc "not implemented\n\0"
txt_E_PROT:
          .asc "protocoll error\n\0"
txt_E_PORTINUSE:
          .asc "port in use\n\0"
txt_unable:
          .asc "Unable to connect to remote host\n::\0"
txt_tcpip:
          .asc "no TCP/IP driver found\n\0"
txt_howto:
          .asc "usage:  telnet <IP> [<port>]\n"
          .asc "  IP internet address of remote\n"
          .asc "    host in digits.\n"
          .asc "  port port to connect to\n\0"
txt_syntax:
          .asc "syntax of\n"
          .asc "IP: <num>.<num>.<num>.<num>\n"
          .asc "port: <num>\n"
          .asc "each number in range 0..255\n\0"
txt_closed:
          .asc "\nConnection closed by foreign host.\n\0"
