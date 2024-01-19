; TCP/IP stack for LUnix (v0.1)
;
; needs packet driver installed (eg. swslip)
; the interface of the packet driver
;   putpack  < a=address, x/y=length, c: 0=empty/1=filled
;           putpack may return with error (pack not sent) but
;           has to "mfree" the memory used by the packet.
;   getpack  > a=address, x/y=length, c=error

#include "kernel.inc"

;#define debug

; Constants you have to use, when calling TCP/IP routines

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

tcp_clock equ 161     ; counter that is incremented every 4 seconds

#define bufnum 16     ; number of buffers (each up to 256*bufmax bytes!)
#define socknum 8     ; number of sockets
#define bufpremal 3   ; number of pre-malloced buffers
#define bufmax 4      ; max size of buffers in pages (256 byte)
#define sockbufs 1    ; max number of used buffers per socket
#define contimeout 20 ; connect-timeout minutes*15

#define tcp_listen       $01
#define tcp_syn_sent     $42
#define tcp_syn_received $43
#define tcp_established  $c4
#define tcp_fin_wait1    $45
#define tcp_fin_wait2    $46
#define tcp_close_wait   $87
#define tcp_closing      $08
#define tcp_last_ack     $09
#define tcp_timewait     $0a

hl  equ  4
hh  equ  5

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
  #define db(text)
#endif

.head "tcpip", .zp1, .zp2, .zp3, .zp4

;------------------------------------------------------------------------
; magic header, API to other processes
;------------------------------------------------------------------------

.data

.global magic
magic:
  .asc "ipv4/tcp" ; type
  .byte 1,0       ; version

.code

  jmp  user_connect
  jmp  user_listen
  jmp  user_accept
  jmp  user_sockinfo
  jmp  user_putbyte
  jmp  user_getbyte
  jmp  user_close
  jmp  err_notimp

;------------------------------------------------------------------------
; packet delivery subsystem
;------------------------------------------------------------------------

allocpack_drv:
          jmp  suicide         ; will be replaced on init
getpack:
          jmp  suicide         ; will be replaced on init
putpack:
          jmp  suicide         ; will be replaced on init

#ifdef debug
        - db("*** sent illegal MID\n")
          pla
          pla
          rts

        - db("*** sent illegal buffer\n")
          rts
#endif

; send packet X
;         < X=buf

sendpacket:
#ifdef debug
          cpx  #bufnum
          bcs  -
#endif
          txa
          pha
          inc  packid+1
          bne  +
          inc  packid
        + lda  buf.mid,x
          pha
#ifdef debug
          tay
          lda  $c300,y
          cmp  ipid
          bne  --
#endif
          ldy  buf.lenh,x
          lda  buf.lenl,x
          tax
          pla
          db("Putpack\n")
          sec
          jsr  putpack
          pla
          tax
          jmp  addtofreelst

; look for received packets and allocate
; memory for new packets

        - jmp  mema

        - cli
          pla
          pla
          jsr  raw_mfree           ; throw away
          cli
          rts

pack_poll:
          lda  freelst
          bpl  +
          ; can't receive packet
          db("free list is empty!\n")
          rts
        + jsr  getpack
          bcs  --              ; no new packet received
          dec  availbuf
          pha
          txa
          pha

          sei                  ; get and remove item from
          ldx  freelst         ; free-list
          bmi  -               ; (error)
          lda  buf.l2nx,x
          sta  freelst
          cli

          db("Getpack\n")
          tya
          sta  buf.lenh,x
          pla
          sta  buf.lenl,x
          pla
          sta  buf.mid,x
          lda  #$ff
          sta  buf.l2nx,x
          stx  zp3             ; remember buf

          ldy  buf.lenh,x
          lda  buf.lenl,x
          beq  +
          iny
        + cpy  #bufmax
          bcs  +               ; check if there are unused pages
          tya
          adc  buf.mid,x
          sta  zp4             ; (zp4=new MID)
          tay
          lda  $c400,y
          sta  zp4+1           ; (zp4+1=old MID)

        - lda  zp4
          sta  $c400,y         ; adapt MIDs of unused pages
          iny
          lda  $c400,y
          cmp  zp4+1
          beq  -

          lda  zp4
          jsr  raw_mfree       ; free unused pages

        + ldx  zp3

          sei                  ; queue new packet in TCP-list
          ldy  iplst+1         ; (new last item)

          bpl  +
          stx  iplst
          bmi  ++
        + txa
          sta  buf.l2nx,y
        + stx  iplst+1
          cli
          db(" -> ip\n")
          
mema:     ; allocate new buffer for receiving packets

          lda  availbuf
#ifdef debug
          sta  2023
          bpl  +
          print_string("Availbuf went negative\n")
          ldx  #0
          stx  availbuf
        +
#endif
          cmp  #bufpremal
          bcs  +               ; enough buffers available, then skip

          lda  #bufmax
          ldx  #$34
          jsr  raw_malloc
          cli
          bcs  ++
          inc  availbuf
          ldx  #0
          ldy  #bufmax
          db("Premalloc\n")
          clc
          jsr  putpack
          bcc  +
          db("got error from putpack\n")
          dec  availbuf
          ;lda  #bufpremal
          ;sta  availbuf        ; adapt availbuf counter, if packetdriver
        + rts                   ; can't handle that many receive buffers

        + db("can't alloc buffer\n")
          rts

; free buffer (X=buf) and add item to free-list

-         db("tried to free illegal buffer\n")
          rts

killbuffer:
          cpx  #bufnum
          bcs  -
          txa
          pha
          lda  buf.mid,x
          jsr  raw_mfree
          pla
          tax

addtofreelst:
          sei
          lda  freelst
          sta  buf.l2nx,x
          stx  freelst
          db("Buffer freed\n")
        - cli
          rts

; free all memory used by a socket.
; (zp2=socket)

fresockmem:
          db("Fresockmem\n")
        - ldx  zp2
          ldy  zp2
          sei
          ldx  reclstt,y
          bmi  --
          inc  recposh,x
          lda  buf.l2nx,x
          sta  reclstt,y
          bpl  +
          sta  reclstb,y
        + cli
          jsr  killbuffer
          jmp  -

; allocate buffer and remove item from
; free-list
;  < A=length of buffer
;  > zp1=pointer to buffer, Y=0, zp2+1=buf, c=error
 
getbuffer:
          bit  freelst
          bmi  ++
          ldx  #$33
          jsr  raw_malloc
          bcs  ++
          ldx  freelst
          bmi  +
          sta  buf.mid,x
          sta  zp1+1
          lda  buf.l2nx,x
          sta  freelst
          cli
          stx  zp2+1
          ldy  #0
          sty  zp1
          rts

        + jsr  raw_mfree
        + sec
          cli
          rts

; calculate sum of IP header
;  < X=buf
;  > .zp4=sum

sumheader:
          clc
          lda  buf.mid,x
          sta  zp3+1
          ldy  buf.offs,x      ; points to end of IP header
          ldx  #0
          stx  zp4+1
          stx  zp3

        - dey
          txa
          adc  (zp3),y
          tax
          dey
          lda  zp4+1
          adc  (zp3),y
          sta  zp4+1
          tya
          bne  -

          stx  zp4

sumend:   bcc  +               ; add carry to sum
        - inc  zp4
          bne  +
          inc  zp4+1
          beq  -
        + rts

; calculate sum of packet data (not including IP-header)
;  < X=buf

sumdata:  clc
          lda  buf.mid,x
          sta  zp3+1
          lda  #0
          sta  zp3
          sta  zp4
          sta  zp4+1
          ldy  buf.offs,x
          txa
          pha                  ; remember buf
          lda  buf.lenh,x
          beq  +

          ; check pages 1..n-1

          pha                  ; remember n
          ldx  zp4

        - iny
          txa
          adc  (zp3),y
          tax
          dey
          lda  (zp3),y
          adc  zp4+1
          sta  zp4+1
          iny
          iny
          bne  -

        - stx  zp4
          inc  zp3+1
          pla                  ; check n
          tay
          dey
          beq  +

          tya
          pha
          ldy  #0

        - dey
          txa
          adc  (zp3),y
          tax
          dey
          lda  (zp3),y
          adc  zp4+1
          sta  zp4+1
          tya
          bne  -

          beq  --

        + pla                  ; X=buf
          tax

          php                  ; last page to check
          sec
          tya
          eor  #255
          adc  buf.lenl,x      ; A=buf.lenl - Y
          bcc  +               ; skip if A<=0
          beq  +
          tax
          plp

        - dex
          beq  ++
          iny
          lda  (zp3),y
          adc  zp4
          sta  zp4
          dey
          lda  (zp3),y
          adc  zp4+1
          sta  zp4+1
          iny
          iny
          dex
          bne  -

          .byte $24
        + plp
          jmp  sumend

        + lda  zp4             ; odd length of packet
          adc  #0
          sta  zp4
          lda  (zp3),y
          adc  zp4+1
          sta  zp4+1
          jmp  sumend

;------------------------------------------------------------------------
; IP MODUL
;------------------------------------------------------------------------

        - cli
          rts

ip_modul:
          sei                  ; remove top item from TCP-list
          ldx  iplst
          bmi  -               ; skip if nothing queued
          lda  buf.l2nx,x
          sta  iplst
          bpl  +
          sta  iplst+1
        + cli

          stx  zp1             ; (zp1=buf)
          lda  buf.mid,x
          sta  zp2+1
          ldy  #0
          sty  zp2             ; (.zp2=buffer)
          sty  zp3             ; (.zp3=$0000)
          sty  zp3+1
          lda  (zp2),y         ; look at protocol version
          and  #$f0
          cmp  #$40            ; = ipv4 ?
          beq  ++

throwaway:
          db("IP-throwaway\n")
          inc  errcnt+1
          bne  +
          inc  errcnt
        + ldx  zp1
          jmp  killbuffer

        + lda  (zp2),y         ; get size of IP header
          and  #$0f
          asl  a
          asl  a
          sta  buf.offs,x      ; is pointer to sub header

          ldy  #3              ; check IP-length field
          lda  (zp2),y
          cmp  buf.lenl,x
          bne  throwaway
          dey
          lda  (zp2),y
          cmp  buf.lenh,x
          bne  throwaway

          jsr  sumheader       ; check IP sum
          ldx  zp1             ; (X=buf for throwaway)
          lda  zp4
          and  zp4+1
          cmp  #$ff
          bne  throwaway

          ldy  #16             ; check dest.IP field
          lda  (zp2),y         ; (must match my own IP)
          cmp  ownip
          bne  throwaway
          iny
          lda  (zp2),y
          cmp  ownip+1
          bne  throwaway
          iny
          lda  (zp2),y
          cmp  ownip+2
        - bne  throwaway
          iny
          lda  (zp2),y
          cmp  ownip+3
          bne  throwaway

          ldy  #6              ; check fragmented-flag
          lda  (zp2),y
          and  #$20
          bne  -

          lda  #$ff
          sta  buf.l2nx,x

          ldy  #9              ; check protocol field
          lda  (zp2),y
          cmp  #1
          beq  isicmp          ; pass to ICMP-modul
          cmp  #6
          beq  istcp           ; pass to TCP-modul
          cmp  #17
          bne  -               ; unknown, then throwaway

          ; pass to  UDP-modul

          sei
          ldy  udplst+1
          bpl  +
          stx  udplst
          bmi  ++
        + txa
          sta  buf.l2nx,y
        + stx  udplst+1
          cli
          db("ip -> udp\n")
          rts

isicmp:   sei
          ldy  icmplst+1
          bpl  +
          stx  icmplst
          bmi  ++
        + txa
          sta  buf.l2nx,y
        + stx  icmplst+1
          cli
          db("ip -> icmp\n")
          rts

istcp:    sei
          ldy  tcplst+1
          bpl  +
          stx  tcplst
          bmi  ++
        + txa
          sta  buf.l2nx,y
        + stx  tcplst+1
          cli
          db("ip -> tcp\n")
          rts

;------------------------------------------------------------------------
; TCP-modul
;------------------------------------------------------------------------

; read one byte from a TCP stream
;  < X=socket
;  > A=byte, c=error

getbyte:
          sei
          ldy  reclstt,x
          bmi  e_tryagain
          lda  buf.mid,y
          clc
          adc  buf.offsh,y
          sta  [+]+2
          lda  buf.offs,y
          sta  [+]+1
          adc  #1
          sta  buf.offs,y
          lda  buf.offsh,y
          adc  #0
          sta  buf.offsh,y
          cmp  buf.lenh,y
          bne  +
          lda  buf.offs,y
          cmp  buf.lenl,y
          beq  ++
      - + lda  $ffff
          cli
          clc
          rts

        + inc  recposh,x       ; increase available counter
          tya                  ; and switch to next
          pha                  ; rec-buffer
          lda  buf.l2nx,y
          sta  reclstt,x
          bpl  +
          sta  reclstb,x
        + pla
          tax
          jsr  -
          pha
          jsr  killbuffer
          pla
          db("switched to next recbuf\n")
          clc
          rts

e_tryagain: 
          lda  #0
          sec
          cli
          rts

; write one byte to a TCP stream
;  < A=byte, X=socket
;  > c=error

putbyte:
          ldy  sockstat,x      ; write enabled ?
          bpl  e_tryagain
          ldy  sndbufstat,x
          bmi  e_tryagain      ; sendbuffer full ?
          sei
          ldy  sndbufpg,x
          sty  [+]+2
          ldy  sndwrnx,x
        + sta  $ff00,y
          iny
          tya
          sta  sndwrnx,x
          cmp  sndrdnx,x
          clc
          beq  +
          cli
          rts

        + lda  sndbufstat,x
          ora  #$c0            ; bit7+6 = full+push
          sta  sndbufstat,x
          cli
          rts

; generate standard header of TCP packets
;  < X=socket, zp1=buffer


setnormipdat:
          ldy  #0
          lda  #$45            ; protocol version+header length
          sta  (zp1),y
          tya
          iny
          sta  (zp1),y         ; type of service
          ;   total length not set here !!!!
          ldy  #4
          lda  packid          ; packet id
          sta  (zp1),y
          lda  packid+1
          iny
          sta  (zp1),y
          lda  #$40            ; df+mf+fragment offset
          iny
          sta  (zp1),y
          lda  #0
          iny
          sta  (zp1),y
          lda  #$ff            ; time to live
          iny
          sta  (zp1),y
          lda  #6              ; protocol (tcp)
          iny
          sta  (zp1),y
          lda  #0              ; header checksum
          iny
          sta  (zp1),y
          iny
          sta  (zp1),y
          lda  ownip           ; source address (=own ip)
          iny
          sta  (zp1),y
          lda  ownip+1
          iny
          sta  (zp1),y
          lda  ownip+2
          iny
          sta  (zp1),y
          lda  ownip+3
          iny
          sta  (zp1),y
          lda  remipa,x        ; remote ip....
          iny
          sta  (zp1),y
          lda  remipb,x
          iny
          sta  (zp1),y
          lda  remipc,x
          iny
          sta  (zp1),y
          lda  remipd,x
          iny
          sta  (zp1),y
          rts

; set timeout value (4*4=16 seconds)
;  < x=socket

settimeout:
          sta  sockstat,x
          lda  tcp_clock
          clc
          adc  #4
          adc  #0
          sta  timeout,x
          rts

; generate standard TCP header
;  < x=socket, zp1=buffer, y=offset

setnormtcpdat:
          lda  localporth,x    ; local port
          sta  (zp1),y
          lda  localportl,x
          iny
          sta  (zp1),y
          lda  remporth,x      ; remote port
          iny
          sta  (zp1),y
          lda  remportl,x
          iny
          sta  (zp1),y
          lda  sndunaa,x       ; sequence number
          iny
          sta  (zp1),y
          lda  sndunab,x
          iny
          sta  (zp1),y
          lda  sndunac,x
          iny
          sta  (zp1),y
          lda  sndunad,x
          iny
          sta  (zp1),y
          lda  rcvnxta,x       ; acknowledgment number
          iny
          sta  (zp1),y
          lda  rcvnxtb,x
          iny
          sta  (zp1),y
          lda  rcvnxtc,x
          iny
          sta  (zp1),y
          lda  rcvnxtd,x
          iny
          sta  (zp1),y
          lda  #$50            ; data-offset
          iny
          sta  (zp1),y
          lda  sndbufstat,x    ; flags
          and  #$48
          beq  +
          eor  sndbufstat,x
          ora  #$08
          sta  sndbufstat,x
          lda  #$08            ; psh
        + ora  #$10            ; ack
          iny
          sta  (zp1),y
          iny                  ; window

          lda  freelst
          ora  recposh,x       ; running out of buffs ?
          bmi  ++              ; then send win=0 !

          lda  sndbufstat,x    ; clear "snd win0"
          and  #$df
          sta  sndbufstat,x

          txa
          pha
          lda  recposh,x
          sec
          sbc  #1
          ldx  freelst
          ora  buf.l2nx,x
          asl  a
          pla
          tax
          bcs  +               ; win=1*limit

          lda  #>1880          ; win=1880 (2*limit)
          sta  (zp1),y
          lda  #<1880
          bne  +++

        + lda  #>940           ; win=940 (1*limit)
          sta  (zp1),y
          lda  #<940
          bne  ++

        + lda  sndbufstat,x    ; win=0
          ora  #$20
          sta  sndbufstat,x
          lda  #0
          sta  (zp1),y

        + iny
          sta  (zp1),y
          lda  #0              ; chksum
          iny
          sta  (zp1),y
          iny
          sta  (zp1),y
          iny                  ; urgptr
          sta  (zp1),y
          iny
          sta  (zp1),y
        - rts

; send packet filled with data collected with
; putbyte calls.
;  < zp2=socket

senddatapacket:
          db("send datapacket\n")
          ldx  zp2
          lda  sndbufstat,x
          and  #128
          asl  a
          bcs  +               ; skip on full buffer
          sec
          lda  sndwrnx,x
          sbc  sndrdnx,x
          cmp  #216            ; 1 or 2 pages needed for packet ?
        + sta  zp4             ; store lo byte of length
          lda  #1
          adc  #0
          sta  zp4+1           ; and hi byte
          jsr  getbuffer       ; allocate buffer of that size
          ;  > zp1=buffer, Y=0, zp2+1=buf, c=error
          bcs  -
          lda  zp4+1
          sbc  #0
          sta  buf.lenh,x      ; calculate size of packet (hi)
          ldy  #2
          sta  (zp1),y
          lda  zp4
          adc  #39
          sta  buf.lenl,x      ; ...(lo)
          iny
          sta  (zp1),y
          lda  #20
          sta  buf.offs,x
          ldx  zp2
          lda  sndbufstat,x
          and  #$ef
          sta  sndbufstat,x
          jsr  setnormipdat    ; create standard headers
          ldy  #20
          jsr  setnormtcpdat

          lda  sndwrnx,x
          sta  zp3+1
          lda  sndrdnx,x
          sta  zp3
          cmp  zp3+1
          bne  +
          lda  sndbufstat,x
          bpl  tcp_eod
        + ;   there will be new unacked data
          ;   so activate timeout
          jsr  settimeout+3
          lda  sndbufpg,x
          sta  [+]+2
          lda  #40
          sta  zp1
          ldy  #0
          ldx  zp3

      - + lda  $ff00,x         ; copy data from rec-buf into
          sta  (zp1),y         ; TCP-packet
          iny
          inx
          cpx  zp3+1
          bne  -

          ldy  #0
          sty  zp1
tcp_eod:  jsr  tcpsumsetup     ; calculate checksum
          jsr  sendpacket      ; and send packet
          clc
          rts

; calculate TCP checksum
;  < .zp1=buffer, zp2+1=buf

sumtcp:
          ldx  zp2+1
          jsr  sumdata
          ;   add pseudo header to chksum
          sec
          ldx  zp2+1
          lda  buf.lenl,x
          sbc  #14
          sta  zp3
          lda  buf.lenh,x
          sbc  #0
          sta  zp3+1           ; packet length is also part of checksum
          clc
          lda  zp4
          adc  zp3
          sta  zp4
          lda  zp4+1
          adc  zp3+1
          sta  zp4+1
          ldy  #19

        - lda  (zp1),y         ; add pseudo header
          adc  zp4
          sta  zp4
          dey
          lda  (zp1),y
          adc  zp4+1
          sta  zp4+1
          dey
          tya
          eor  #11
          bne  -

          jmp  sumend

; calculate TCP checksum and update checksum field in packet
;  < zp1=buffer, zp2+1=buf

tcpsumsetup:
          ldx  zp2+1
          lda  #0
          sta  zp1
          jsr  sumheader       ; calculate IP checksum
          ldy  #10             ; write sum into IP-header
          lda  zp4+1
          eor  #$ff
          sta  (zp1),y
          lda  zp4
          eor  #$ff
          iny
          sta  (zp1),y
          jsr  sumtcp          ; calculate TCP checksum
          ldx  zp2+1           ; write sum into TCP header
          lda  buf.offs,x
          sta  zp1
          ldy  #16
          lda  zp4+1
          eor  #$ff
          sta  (zp1),y
          lda  zp4
          eor  #$ff
          iny
          sta  (zp1),y
          rts

; extract data from incomming TCP packets
;  < zp2=socket, zp2+1=buf, zp1=buffer
;  > c = 0 : A > $00 new information in this packet
;    c = 0 : A = $00 : packet is okay, but no new information in it
;    c = 1 : packet's sequencenumber is invalid
;    if packet is passed to another packet queue zp2+1 inreased by $80 !!

extractdata:
          sec
          ldx  zp2+1
          lda  buf.offs,x
          sta  zp1
          ldy  #13
          lda  (zp1),y
          sta  tcpflags        ; remember flags for later
          ldx  zp2
          ldy  #7
          sec                  ; incomming seq.num - outgoing ack.num
          lda  rcvnxtd,x
          sbc  (zp1),y
          sta  zp3
          lda  rcvnxtc,x
          dey
          sbc  (zp1),y
          sta  zp3+1
          lda  rcvnxtb,x
          dey
          sbc  (zp1),y
          sta  zp4
          lda  rcvnxta,x
          dey
          sbc  (zp1),y
          beq  _extract2       ; result is >= 0 !
          ; result is <0 for future use ??
          cmp  #255
          bne  _noextr
          cmp  zp4
          bne  _noextr ; skip if the difference is too big
          cmp  zp3+1
          bne  _noextr

          ;   this packet may be stored for
          ;   future use (maybe 2 packets arrived
          ;   out of order) (not implemented!)
          lda  sndbufstat,x
          ora  #$00
          sta  sndbufstat,x
          db("packet from future\n")

          ;  jmp$<dontpanic
          ;   disabled for now
          ;  ldyx<queueddatlst
          ;  bmi$<storeit
          ;  lday<buf.mid
          ;  sta$<zp4+1
          ;  lday<buf.offs
          ;  sta$<zp4
          ;  ldy##7
          ;  ldii<zp1
          ;  - ii<zp4
          ;  dey
          ;  ldii<zp1
          ;  - ii<zp4
          ;  dey
          ;  ldii<zp1
          ;  - ii<zp4
          ;  dey
          ;  ldii<zp1
          ;  - ii<zp4
          ;  bpl$<_noextr
          ;  ldax<queueddatlst
          ;  sta$!!undefined!!
          ;  <storeit
          ;  lda$<zp2+1
          ;  stax<queueddatlst
          ;  sec
          ;  rts
          jmp  _discard

_noextr:  db("Packet with illegal seq.num\n")
          sec
          rts

_discard:
          db("Packet without data\n")
          lda  #0
          clc
          rts

_extract2:
          lda  zp4
          bne  _noextr         ; discard, if difference too big
          lda  tcpflags
          and  #$03            ; mask flags that count
          lsr  a
          adc  #0
          sta  zp4
          eor  #$ff            ; and subtract from new-byte-offset
          sec
          adc  zp3
          sta  zp3
          lda  zp3+1
          adc  #$ff
          sta  zp3+1
          cmp  #4              ; more than 4*256 bytes behind, then skip
          bcs  _discard
          ldy  #12
          lda  (zp1),y         ; calculate offset to new data
          and  #$f0
          lsr  a
          lsr  a
          ldy  zp2+1
          adc  zp1
          adc  zp3
          sta  buf.offs,y
          lda  zp3+1
          adc  #0
          sta  buf.offsh,y
          cmp  buf.lenh,y      ; new informations in packet ?
          bcc  +               ; (yes)
          bne  _discard        ; (no)
          lda  buf.offs,y
          cmp  buf.lenl,y
          beq  nulldata        ; (no, seems to be a resent packet)
          bcs  _discard        ; (no)
        + lda  sockipid,x
          bmi  _noextr         ; received data for a terminated process
                               ; treat like a stray packet (send rst back)

          ; now check if there is new data in this packet

          clc
          lda  buf.offs,y
          adc  zp4
          sta  buf.offs,y
          bcc  +
          lda  buf.offsh,y
          adc  #1
          sta  buf.offsh,y
          lda  buf.offs,y
        + cmp  buf.lenl,y
          bne  isokgoon
          lda  buf.offsh,y
          cmp  buf.lenh,y
          bne  isokgoon

          lda  #$ff
          clc
          rts                  ; only flags delivered with this packet

nulldata: lda  zp3
          ora  zp3+1
          bne  +
          lda  tcpflags
          and  #$03
          bpl  ++
        + jsr  setflag
        + clc
          lda  #0
          rts

isokgoon: ;   decrement available counter and add packet to rcv-list
          db("TCP -> reclst\n")
          dec  recposh,x
          ;   put in receive-list
          lda  zp2+1
          sei
          ldy  reclstb,x
          bmi  +
          sta  buf.l2nx,y
        + sta  reclstb,x
          ldy  reclstt,x
          bpl  +
          sta  reclstt,x
          ;   refresh rcv-nxt
        + tay
          lda  #$ff
          sta  buf.l2nx,y      ; keep IRQ disabled until "computeack"

          tya
          ora  #$80
          sta  zp2+1           ; mark buf ("passed")

          sec                  ; calculate number of extracted bytes
          lda  buf.lenl,y
          sbc  buf.offs,y
          sta  zp3
          lda  buf.lenh,y
          sbc  buf.offsh,y
          sta  zp3+1
          clc                  ; increase ack.num
          lda  rcvnxtd,x
          adc  zp3
          sta  rcvnxtd,x
          lda  rcvnxtc,x
          adc  zp3+1
          sta  rcvnxtc,x
          bcc  setflag
          inc  rcvnxtb,x
          bne  +
          inc  rcvnxta,x
        + clc

setflag:  lda  sndbufstat,x    ; set flag 'ACK needed'
          ora  #$10
          sta  sndbufstat,x
          rts

; check ACK field of incomming packets
; and remove acked data from buffer
;  < zp2=socket, zp2+1=buf, zp1=buffer (+TCP)

        - cli
          rts

computeack:
          ldy  #13
          lda  (zp1),y         ; check ACK flag
          and  #$10
          beq  -               ; nothing acked, then exit
          ldy  #11
          ldx  zp2
          sec
          lda  (zp1),y
          sbc  sndunad,x
          sta  zp3
          dey
          lda  (zp1),y
          sbc  sndunac,x
          sta  zp3+1
          dey
          lda  (zp1),y
          sbc  sndunab,x
          sta  zp4
          dey
          lda  (zp1),y
          sbc  sndunaa,x
          cli
          beq  +
          cmp  #255
          bne  unsyned
          cmp  zp4
          bne  unsyned
          clc                  ; not acked new data
          rts

        + lda  zp4
          bne  unsyned
          lda  zp3+1
          beq  +               ; acked less than 256 bytes
          cmp  #2
          bcs  unsyned
          lda  zp3
          bne  unsyned
          lda  sndbufstat,x    ; acked 256 bytes
          bpl  unsyned
          and  #$77
          sta  sndbufstat,x
          ;   all acked so skip timeout
          ;     and clr rem.pushflag
          lda  #0
          sta  timeout,x       ; all acked, no timeout
          jmp  _ack256

unsyned:  db("Unsynced packet\n")
          sec
          rts

        + lda  sndbufstat,x
          bmi  +
          sec
          lda  sndwrnx,x
          sbc  sndrdnx,x
          cmp  zp3
          bcc  unsyned
        + clc
          lda  sndrdnx,x
          adc  zp3
          sta  sndrdnx,x
          cmp  sndwrnx,x
          bne  +
          lda  sndbufstat,x
          bmi  +
          ;   all acked so skip timeout
          lda  #0
          sta  timeout,x
          ;     ..and clr rem.pushflag
          lda  sndbufstat,x
          and  #$f7
          sta  sndbufstat,x
        + lda  zp3
          beq  +
          lda  sndbufstat,x
          and  #$7f
          sta  sndbufstat,x
        + clc
          lda  sndunad,x
          adc  zp3
          sta  sndunad,x
          bcc  +
_ack256:  inc  sndunac,x
          bne  +
          inc  sndunab,x
          bne  +
          inc  sndunaa,x
        + lda  #255
          clc
        - rts

; JOB: active open (TCP connection)
;  zp2=socket

activeopen:
          db("JOB activeopen\n")
          lda  #1              ; get buffer for initial packet
          jsr  getbuffer
          bcs  -
          lda  #44             ; 40+4 bytes
          ldy  #3
          sta  buf.lenl,x
          sta  (zp1),y
          dey
          lda  #0
          sta  buf.lenh,x
          sta  (zp1),y
          lda  #20
          sta  buf.offs,x
          ldx  zp2
          jsr  setnormipdat
          ldy  #20
          jsr  setnormtcpdat
          ;   switch to syn-sent
          lda  #tcp_syn_sent
          ;    write stat and set timeout
          jsr  settimeout
          ;    and clear job
          lda  sockjob,x
          and  #$fe
          sta  sockjob,x
          lda  #$02            ; syn
 _open_wflags:
          ldy  #33
          sta  (zp1),y
          ;   reset bufferptr
          ;    increase sndwrnx (syn)
          lda  #1
          sta  sndwrnx,x       ; (syn sent!)
          lda  #0
          sta  sndrdnx,x
          lda  #$00
          sta  sndbufstat,x
          dey
          lda  #$60
          sta  (zp1),y
          ldy  #40             ; add option
          lda  #2
          sta  (zp1),y
          iny
          lda  #4
          sta  (zp1),y
          iny
          lda  #>896           ; MTU=896
          sta  (zp1),y
          iny
          lda  #<896
          sta  (zp1),y
          jsr  tcpsumsetup
          jmp  sendpacket

; calculate total number of items that need to be acked
;  < .zp1=buffer (TCP)
;  > .zp4=#

calcplen: ldy  #12
          ldx  zp1
          lda  (zp1),y
          and  #$f0
          lsr  a
          lsr  a
          adc  zp1
          eor  #$ff
          sec
          ldy  #0
          sty  zp1
          ldy  #3
          adc  (zp1),y
          sta  zp4
          dey
          lda  (zp1),y
          sbc  #0
          sta  zp4+1
          stx  zp1
          ldy  #13
          lda  (zp1),y
          and  #3
          lsr  a
          adc  #0
          adc  zp4
          sta  zp4
          bcc  +
          inc  zp4+1
        + rts

; calculate 'random' bytes
;  > A=#

rnd:      lda  lstrnd
          adc  #1
          sta  lstrnd
          rts

ignore:   ldx  zp2+1
          jmp  killbuffer

        - jmp  listen_success

; TCP-state: listen for TCP connection
;  < zp1=buffer (TCP)

do_listen:
          db("JOB: listen\n")
          ldy  #13
          lda  (zp1),y
          ;   chkflags
          and  #$17
          cmp  #$02            ; syn
          beq  -
          and  #$04            ; rst
          bne  ignore
          ; [syn] ack

sendrst:  ;   send rst in response to a
          ;   unsynced incomming packet
          ;   (overwrite old packet)
          ;   zp1=ptr to tcp-header
          db("sendrst\n")
          jsr  calcplen
          lda  (zp1),y
          and  #$10
          beq  +               ; no ack field in incoming packet
          ;   set rcvnxt pointer (ack field)
          ;   of rst-packet
          ldy  #11
        - lda  (zp1),y
          sta  seq-8,y
          dey
          cpy  #7
          bne  -
          jmp  ++

        + ldy  #3              ; no ack, then seq=$00000000
        - sta  seq,y
          dey
          bpl  -

          ;  set ack field of rst-packet, ack.out=seq.in + ack
        + clc
          ldy  #7
          lda  (zp1),y
          adc  zp4
          sta  ack+3
          dey
          lda  (zp1),y
          adc  zp4+1
          sta  ack+2
          dey
          lda  (zp1),y
          adc  #0
          sta  ack+1
          dey
          lda  (zp1),y
          adc  #0
          sta  ack
          ;   init rest of packet
          lda  #0
          sta  zp1
          ldy  #2
          sta  (zp1),y         ; total length (hi)
          ldx  zp2+1
          sta  buf.lenh,x
          lda  #40
          iny
          sta  buf.lenl,x
          sta  (zp1),y         ; total length (lo)
          lda  #20
          sta  buf.offs,x
          lda  #$45
          ldy  #0
          sta  (zp1),y         ; protocol + length of IP header (20 bytes)
          lda  #0
          ldy  #7
          sta  (zp1),y         ; fragment offset
          ldy  #10
          sta  (zp1),y         ; IP checksum
          iny
          sta  (zp1),y
          ldy  #34
        - sta  (zp1),y         ; window, TCP checksum, Urgent pointer
          iny
          cpy  #40
          bne  -
          lda  #$40
          ldy  #6
          sta  (zp1),y         ; don't fragment
          lda  #$ff
          ldy  #8
          sta  (zp1),y         ; ttl
          lda  #$50
          ldy  #32
          sta  (zp1),y         ; data offset (40)
          lda  #$14
          iny
          sta  (zp1),y         ; flags (ack rst)
          ;   set all 4byte fields
          ldx  #3
          stx  zp1

        - lda  ack,x
          ldy  #28
          sta  (zp1),y         ; acknowledge number
          lda  seq,x
          ldy  #24
          sta  (zp1),y         ; sequence number
          ldy  #12
          lda  (zp1),y
          ldy  #16
          sta  (zp1),y         ; destIP = sourceIP
          lda  ownip,x
          ldy  #12
          sta  (zp1),y         ; sourceIP = myID
          dec  zp1
          dex
          bpl  -

          ;   swap ports
          lda  #0
          sta  zp1
          ldy  #20
          lda  (zp1),y
          pha
          iny
          lda  (zp1),y
          pha
          iny
          lda  (zp1),y
          ldy  #20
          sta  (zp1),y         ; source port = dest. port (hi)
          ldy  #23
          lda  (zp1),y
          ldy  #21
          sta  (zp1),y         ; source port = dest. port (lo)
          ldy  #23
          pla
          sta  (zp1),y         ; dest. port = source port (lo)
          dey
          pla
          sta  (zp1),y         ; dest. port = source port (hi)
          ;   set id
          lda  packid
          ldy  #4
          sta  (zp1),y         ; packIP (hi)
          lda  packid+1
          iny
          sta  (zp1),y         ; packID (lo)

          lda  #0
          ldy  #1
          sta  (zp1),y         ; type of service
          lda  #6
          ldy  #9
          sta  (zp1),y         ; protocol (TCP)

          jsr  tcpsumsetup
          jmp  sendpacket

listen_success:
          ldx  zp2
          ldy  #7
          ;   store initial seqnum+1
          lda  (zp1),y
          clc
          adc  #1
          sta  rcvnxtd,x
          dey
          lda  (zp1),y
          adc  #0
          sta  rcvnxtc,x
          dey
          lda  (zp1),y
          adc  #0
          sta  rcvnxtb,x
          dey
          lda  (zp1),y
          adc  #0
          sta  rcvnxta,x
          ;   generate own initial seqencenumber
          sec
          jsr  rnd
          sta  sndunad,x
          jsr  rnd
          sta  sndunac,x
          jsr  rnd
          sta  sndunab,x
          jsr  rnd
          sta  sndunaa,x
          ;   store remoteport and ip
          ldy  #1
          lda  (zp1),y
          sta  remportl,x
          dey
          lda  (zp1),y
          sta  remporth,x
          ;   ip...
          ldy  #0
          sty  zp1
          ldy  #15
          lda  (zp1),y
          sta  remipd,x
          dey
          lda  (zp1),y
          sta  remipc,x
          dey
          lda  (zp1),y
          sta  remipb,x
          dey
          lda  (zp1),y
          sta  remipa,x
          ;  switch to synreceived
          lda  #tcp_syn_received
          jsr  settimeout

; send standard packet with SYN and ACK flag present

sendsynack:
          lda  #0
          sta  zp1
          ldx  zp2+1
          sta  buf.lenh,x
          ldy  #2
          sta  (zp1),y
          iny
          lda  #44
          sta  buf.lenl,x
          sta  (zp1),y
          lda  #20
          sta  buf.offs,x
          ldx  zp2
          jsr  setnormipdat
          ldy  #20
          jsr  setnormtcpdat
          lda  #$12            ; ack, syn
          jmp  _open_wflags

        - jmp abort            ; got rst so abort connection

; TCP-state SYN-SENT
;  < zp1=buffer (TCP), zp2=socket, zp2+1=buf

syn_sent: 
          db("TCP: syn_sent\n")
          ldy  #13
          ;   chk falgs
          lda  (zp1),y
          and  #$17            ; (mask for ack,rst,syn,fin)
          cmp  #$12
          beq  +               ; ACK + SYN
          cmp  #$02
          beq  ++              ; SYN
          and  #4
          bne  -               ; don't respond to rst packets
        - jmp  sendrst

        + jsr  computeack
          ldx  zp2
          lda  sndrdnx,x
          beq  -
          lda  #tcp_established            ; switch to established
          sta  sockstat,x
          bne  ++
        + lda  #tcp_syn_received ; switch to syn_received
          ldx  zp2
          jsr  settimeout
          ;   extract initial seqnum
        + ldy  #7
          lda  (zp1),y
          clc
          adc  #1
          sta  rcvnxtd,x
          dey
          lda  (zp1),y
          adc  #0
          sta  rcvnxtc,x
          dey
          lda  (zp1),y
          adc  #0
          sta  rcvnxtb,x
          dey
          lda  (zp1),y
          adc  #0
          sta  rcvnxta,x

; send stadard packet with ACK flag set
;  < .zp1=buffer, zp2=socket, zp2+1=buf

sendack:  db("send ack\n")
          lda  #0
          sta  zp1
          ldx  zp2+1
          sta  buf.lenh,x
          ldy  #2
          sta  (zp1),y
          lda  #40
          sta  buf.lenl,x
          iny
          sta  (zp1),y
          lda  #20
          sta  buf.offs,x
          ldx  zp2
          jsr  setnormipdat
          ldy  #20
          jsr  setnormtcpdat
          jsr  tcpsumsetup
          jmp  sendpacket

refbyrst: jmp  sendrst

rmpack:   ldx  zp2+1
          jsr  killbuffer
          lda  zp2+1
          ora  #$80
          sta  zp2+1
        - rts

; TCP-State SYN-RECEIVED
;

syn_received:
          db("TCP:syn-recvd\n")
          jsr  extractdata
          bcs  refbyrst        ; send rst back if not in window

          lda  tcpflags
          and  #4              ; rst ?
          bne  abort
          lda  tcpflags
          ;  chk flags
          and  #$17
          cmp  #$10            ; ack only ?
          bne  rmpack          ; if not then discard
          jsr  computeack
          jsr  rmpack
          ldx  zp2
          lda  sndrdnx,x
          beq  -
          ;   wrong !! should send syn
          ;   but to difficult to implement
          ;   (..yet)
          lda  #tcp_established
          ldx  zp2
          ;   switched to established
          jmp  settimeout

abort:    lda  #$ff

errclose: ;   switch to timewait and set err
          ldx  zp2
          sta  sndrdnx,x
          lda  #tcp_timewait
          sta  sockstat,x
          ;   set long timeout
          lda  tcp_clock
          adc  #40             ; 40*4=160 seconds
          adc  #0
          sta  timeout,x
          jsr  rmpack
        - rts

; TCP-State ESTABLISHED

estab:    db("TCP: estab\n")
          jsr  extractdata
          bcs  refbyrst
          ;   in window
          jsr  computeack
          lda  tcpflags
          and  #$04
          bne  abort
          ;   react to a fin
          jsr  rmpack
          lda  tcpflags
          and  #$01
          beq  -
          ;   switch to closew
          ldx  zp2
          lda  #tcp_close_wait
          sta  sockstat,x
          lda  sndbufstat,x
          ora  #$10
          sta  sndbufstat,x
          inc  rcvnxtd,x
          bne  +
          inc  rcvnxtc,x
          bne  +
          inc  rcvnxtb,x
          bne  +
          inc  rcvnxta,x
        + lda  #1
          jsr  getbuffer
          bcs  -
          jmp  sendack

; JOB: FIN-WAIT0

fin_wait0: ;  is a *JOB* not a state of TCP
          ;    !! must first wait for ack of
          ;       all sent data !!
          ;   send a fin packet and switch to
          ;   fin_wait1
          ;   zp2=socket
          ;   retry if there was not enough
          ;   memory !
          db("JOB: finwait\n")
          ldx  zp2
          lda  sndwrnx,x
          cmp  sndrdnx,x
          bne  +
          lda  #1
          jsr  getbuffer
          bcs  ++
          lda  #0
          sta  buf.lenh,x
          ldy  #2
          sta  (zp1),y
          lda  #40
          sta  buf.lenl,x
          iny
          sta  (zp1),y
          lda  #20
          sta  buf.offs,x
          ldx  zp2
          ;   add one pseudo byte in buffer
          inc  sndwrnx,x
          ;   clear job
          lda  sockjob,x
          and  #$ff-$02
          sta  sockjob,x
          jsr  setnormipdat
          ldy  #20
          jsr  setnormtcpdat
          ldy  #33
          lda  #$11            ; ack fin
          sta  (zp1),y
          jsr  tcpsumsetup
          ldx  zp2+1
          jsr  sendpacket
          ldx  zp2
          sei
          lda  sockstat,x
          and  #$0f
          cmp  #($0f & tcp_close_wait)
          ; was it in state 'close wait' ?
          beq  _lastack
          lda  #tcp_fin_wait1
          .byte $2c
_lastack:
          lda  #tcp_last_ack
          jsr  settimeout
          cli
          clc
          .byte $24
        + sec
        + rts

;TCP-state FIN-WAIT1

fin_wait1: ;  wait for a ack of fin
          ;   or a fin
          db("TCP: fin-wait1\n")
          jsr  extractdata
          bcs  _fw1_err
          jsr  computeack
          jsr  rmpack
          lda  tcpflags
          and  #4
          bne  _fw1_reset
          ldx  zp2
          lda  sndwrnx,x
          cmp  sndrdnx,x
          bne  _fw1_noack
          lda  tcpflags
          and  #$01
          beq  _to_finwait2
_to_timewait:
          lda  #0
          lda  sndrdnx,x
          jsr  ack_one
          lda  #tcp_timewait
          ;   dest=time wait
          .byte $2c
_to_closing:
          lda  #tcp_closing
          ;   send ack of fin
          ldx  zp2
          jsr  settimeout
          lda  #1
          jsr  getbuffer
          bcs  +
          jsr  sendack
          ldx  zp2
          lda  sockstat,x
          cmp  #tcp_timewait
          beq  longtimeout
          jmp  settimeout+3

longtimeout:
          lda  tcp_clock
          adc  #40
          adc  #0
          sta  timeout,x
        + rts

_fw1_noack:
          lda  tcpflags
          and  #1
          beq  +
          jsr  ack_one
          jmp  _to_closing

_fw1_err: jmp  sendrst

          ;   if no ack and no fin...ignore
_to_finwait2:
          lda  #tcp_fin_wait2
          sta  sockstat,x
      - + rts

;TCP-state FIN-WAIT2

fin_wait2:;   wait for fin and send ack
          ;   (continue receiving data)
          db("TCP: fin-wait2\n")
          jsr  extractdata
          bcs  _fw1_err
          jsr  computeack
          jsr  rmpack
          lda  tcpflags
          and  #$05
          beq  -
          cmp  #$01
          beq  _to_timewait
_fw1_reset: 
          ;   was a rst-packet so goto
          ;    time_wait no ack to send
          jmp  abort

; TCP-state TIMEWAIT

time_wait:;   wait for a special time
          ;   donno yet.. wait for ever
          db("TCP: timewait\n")
          ldy  #13
          lda  (zp1),y
          and  #$04
          bne  +
          jmp  sendrst

          ;    every packet exept rst will
          ;   cause a rst packet
        + jmp  rmpack

ack_one:  inc  rcvnxtd,x
          bne  +
          inc  rcvnxtc,x
          bne  +
          inc  rcvnxtb,x
          bne  +
          inc  rcvnxta,x
        + rts

;TCP-state CLOSE-WAIT

close_wait:
          ;   nothing to do but waiting for
          ;   a userfin
          ;   but sending is still possible so
          ;   the ack-field has to be computed
          ;   and therefor the seq-num must be
          ;   checked
          db("TCP:close wait\n")
          jsr  extractdata
          bcs  _cw_err
          jsr  computeack
          jsr  rmpack
          lda  tcpflags
          and  #4
          beq  +
          jmp  abort

;TCP-state LAST-ACK

last_ack: ;   wait for ack of fin then closed
          ;   (mostly identical to closing)
          ;   all data is sent + a fin is sent
          db("TCP: last ack\n")
          jsr  chkfinack
          bne  +
          lda  #$00
          ldx  zp2
          sta  sockstat,x
          sta  timeout,x
          sta  sndrdnx,x
        + rts

;TCP-state CLOSING

closing:  ;   wait for ack of fin then time-w
          db("TCP: closing\n")
          jsr  chkfinack
          bne  +
          lda  #tcp_timewait
          ldx  zp2
          sta  sockstat,x
          lda  tcp_clock
          clc
          adc  #60            ; 60*4=240s = 4min timeout
          adc  #0
          sta  timeout,x
          lda  #0
          sta  sndrdnx,x
      + - rts

_cw_err:  beq  -
          jmp  sendrst

; check incoming packet for FIN+ACK

chkfinack:
          jsr  extractdata
          bcs  +               ; skip on invalid seq.num
          jsr  computeack
          jsr  rmpack
          lda  tcpflags
          and  #4
          bne  ++              ; RST set !
          ldx  zp2
          lda  sndwrnx,x
          cmp  sndrdnx,x
          rts

        + jmp  sendrst

        + pla
          pla
          jmp  abort

; USER CALL:
;  close TCP-stream
;  < X=socket

close:
          ;   del process-conection and
          ;   set job-shutdown also free memory
          cpx  #socknum        ; check for valid socket num.
          bcs  +
          lda  sockipid,x      ; check process ID
          cmp  ipid
          bne  +
raw_close:
          lda  #$80
          sta  sockipid,x
          sei
          lda  #4              ; add JOB: ??
          ora  sockjob,x
          and  #$ff-$03
          sta  sockjob,x
          lda  sndbufpg,x      ; free memory used by sendbuffer
          jsr  raw_mfree
          cli
          rts

; USER CALL:
;  fin, send EOF to TCP-stream
;  < X=socket

        - clc
          rts

user_fin: ;   x=socket
          cpx  #socknum        ; check for valid socket num.
          bcs  +
          lda  sockipid,x      ; check process ID
          eor  ipid
          bne  +
          lda  sockstat,x
          and  #$7f
          sta  sockstat,x
          and  #$0f
          cmp  #($0f & tcp_syn_received)
          beq  _dofin
          cmp  #($0f & tcp_established)
          beq  _dofin
          cmp  #($0f & tcp_close_wait)
          bne  -
_dofin:
          lda  #2              ; add JOB:??
          ora  sockjob,x
          sta  sockjob,x
          rts

        + sec
        - rts

; JOB: shutdown (cut opened TCP-stream, no FIN use RST)
; < zp2=socket 

shutdown: ;   is a *JOB* not a state of TCP
          ldx  zp2
          lda  sockstat,x
          and  #$0f
          beq  +               ; arrived in state closed ?
          cmp  #($0f & tcp_timewait)
          beq  -               ; arrived in state time_wait ?
          lda  #tcp_timewait   ; switch to time_wait
          sta  sockstat,x
          lda  #1
          jsr  getbuffer
          bcs  -
          lda  #0
          sta  buf.lenh,x
          ldy  #2
          sta  (zp1),y
          iny
          lda  #40
          sta  buf.lenl,x
          sta  (zp1),y
          lda  #20
          sta  buf.offs,x
          ldx  zp2
          jsr  longtimeout
          ;   clear job
          lda  #0
          sta  sockjob,x
          jsr  setnormipdat
          ldy  #20
          jsr  setnormtcpdat
          ldy  #33
          lda  (zp1),y
          ora  #$04            ; set RST flag
          sta  (zp1),y
          jsr  tcpsumsetup
          jmp  sendpacket

        + jsr  fresockmem      ; done, then free other used memory
          ldx  zp2
          lda  #$ff            ; release slot
          sta  sockipid,x
          lda  #0
          sta  sockstat,x
          rts

; USER CALL:
;  get socket, allocate socket no other action
;  > X=socket, c=error

user_getsock:
          sei
          ldx  #socknum-1
        - lda  sockipid,x      ; search for unused slot
          cmp  #$ff
          beq  +
          dex
          bpl  -
          cli
        - sec
          rts

        + lda  #$00            ; reset socket data
          sta  sockstat,x
          sta  sockjob,x
          sta  sndwrnx,x       ; reset bufptr
          sta  sndrdnx,x
          sta  sndbufstat,x
          sta  socktype,x      ; (active open per default)
          lda  #$ff
          sta  reclstt,x
          sta  reclstb,x
          lda  ipid            ; set process ID
          sta  sockipid,x
          jsr  rnd             ; set initial local port (for passive open)
          sta  localportl,x
          jsr  rnd
          sta  localporth,x
          cli
          ;   allocate bufferpage
          txa
          pha
          lda  #1              ; allocate sendbuffer (256 bytes)
          jsr  malloc
          tay
          pla
          tax
          tya
          sta  sndbufpg,x
          clc
          rts

; USER CALL:
;  open TCP-stream
;  < X=socket, Y: $00=passive, $ff=active 

user_open:
          cpx  #socknum        ; check for valid stream num
          bcs  [-]+1 ; (rts)
          lda  sockipid,x      ; check process ID
          cmp  ipid
          bne  -

          ;   init available counter
          lda  #sockbufs
          sta  recposh,x
          lda  sockstat,x
          bne  -
          lda  #0
          sta  timeout,x       ; no timeout
          tya
          bne  +

          ;   passive open-
          ;   just switch to listen

          lda  #tcp_listen
          sta  sockstat,x
          clc
          rts

          ;   active open-
          ;    start job
        + lda  sockjob,x
          bne  -
          lda  #1              ; add JOB ??
          sta  sockjob,x
          jsr  rnd             ; set initial seq-num
          sta  sndunaa,x
          jsr  rnd
          sta  sndunab,x
          jsr  rnd
          sta  sndunac,x
          jsr  rnd
          sta  sndunad,x
          clc
          rts

; examine an incoming packet and find the socket
; to use
;  < .zp1=buffer, zp3=IP header offset, zp3+1=TCP header offset
;  > X=socket, c=error

findsock: 
          ;   find a socket with perfect match
          ;   buf in state listen
          ;   there can't be a'perfect' match
          ;   of ports and ip
          ldx  #socknum-1

        - lda  sockstat,x
          and  #15
          cmp  #2
          bcc  +               ; skip if socket closed or listen
          lda  zp3             ; check source (remote) ip
          sta  zp1
          ldy  #12
          lda  (zp1),y
          cmp  remipa,x
          bne  +
          iny
          lda  (zp1),y
          cmp  remipb,x
          bne  +
          iny
          lda  (zp1),y
          cmp  remipc,x
          bne  +
          iny
          lda  (zp1),y
          cmp  remipd,x
          bne  +
          lda  zp3+1           ; check source (remote) port
          sta  zp1
          ldy  #0
          lda  (zp1),y
          cmp  remporth,x
          bne  +
          iny
          lda  (zp1),y
          cmp  remportl,x
          bne  +
          iny                  ; check destination (local) port
          lda  (zp1),y
          cmp  localporth,x
          bne  +
          iny
          lda  (zp1),y
          cmp  localportl,x
          bne  +
          lda  sockipid,x      ; check for process
          bmi  +

        - clc                  ; found socket!
          rts

        + dex                  ; doesn't match
          bpl  --              ; try next socket

          ;   now try to find a non syned
          ;   socket
          lda  zp3+1
          sta  zp1
          ldx  #socknum-1

        - lda  sockstat,x
          and  #15
          cmp  #1              ; skip if socket closed
          bne  +
          ldy  #2              ; check local port
          lda  (zp1),y
          cmp  localporth,x
          bne  +
          iny
          lda  (zp1),y
          cmp  localportl,x
          bne  +
          lda  sockipid,x      ; check ipid
          bmi  +
          jmp  --              ; found!

        + dex
          bpl  -

          ; can't find anything
          sec
          rts

;tcp jumptable

tcp_jmptab:
          jmp  do_listen
          nop
          jmp  syn_sent
          nop
          jmp  syn_received
          nop
          jmp  estab
          nop
          jmp  fin_wait1
          nop
          jmp  fin_wait2
          nop
          jmp  close_wait
          nop
          jmp  closing
          nop
          jmp  last_ack
          nop
          jmp  time_wait


        - cli
          rts

; TCP-modul

tcp_modul:
          ;   gets packets from ip-modul
          ;   and trys to find a suitable
          ;   socket

          sei
          ldx  tcplst          ; skip if there is no packet
          bmi  -
          stx  zp2+1           ; remove from TCP-list
          lda  buf.l2nx,x
          sta  tcplst
          bpl  +
          sta  tcplst+1
        + cli
          lda  buf.mid,x
          sta  zp1+1
          stx  zp2+1
          ;   check chksum
          lda  #0
          sta  zp1
          jsr  sumtcp
          lda  zp4
          and  zp4+1
          cmp  #$ff
          beq  +
          db("wrong tcp checksum\n")
        - jmp  rmpack

        + lda  #0
          sta  zp3
          ldx  zp2+1
          lda  buf.offs,x
          sta  zp3+1
          jsr  findsock
          bcs  ++              ; no socket found!

          ;  passthrough
          ;  call tcp-subroutine
          ;  check ipid
          stx  zp2
          lda  sockstat,x
          and  #15
          beq  -               ; discard packet for closed stream ??
          cmp  #11
          bcs  -               ; dicard if illegal state
          asl  a
          asl  a
          tax
          lda  tcp_jmptab-3,x
          sta  [+]+1
          lda  tcp_jmptab-2,x
          sta  [+]+2
        + jmp  $ffff

        + jmp  sendrst

; timeout related stuff....

timeout.closed:
timeout.listen:
timeout.finwait2:
          db("illegal timeout\n")
          lda  #0
          sta  sockstat,x
          rts

timeout.estab:
timeout.closewait:
          jsr  settimeout+3
          jmp  senddatapacket

timeout.timewait:
          lda  #0
          sta  sockstat,x
          sta  timeout,x
          lda  sockipid,x
          cmp  #$ff
          beq  +
          jsr  fresockmem
          ldx  zp2
        + lda  #$ff
          sta  sockipid,x
        - rts

timeout.synrecved:
          ;   send syn+ack
          txa
          pha
          jsr  settimeout+3
          lda  #1
          jsr  getbuffer
          pla
          bcs  -
          tax
          jmp  sendsynack

timeout.synsent:
          jmp  activeopen

snd_finack:
          sta  sndwrnx,x
          sta  sndrdnx,x
          jmp  fin_wait0

timeout.finwait1:
          jsr  snd_finack
          lda  #tcp_fin_wait1
          ldx  zp2
          jmp  settimeout

timeout.closing:
          jsr  snd_finack
          lda  #tcp_closing
          ldx  zp2
          jmp  settimeout

timeout.lastack:
          jsr  snd_finack
          lda  #tcp_last_ack
          ldx  zp2
          jmp  settimeout

timeout_jmptab: 
          jmp  timeout.closed
          nop
          jmp  timeout.listen
          nop
          jmp  timeout.synsent
          nop
          jmp  timeout.synrecved
          nop
          jmp  timeout.estab
          nop
          jmp  timeout.finwait1
          nop
          jmp  timeout.finwait2
          nop
          jmp  timeout.closewait
          nop
          jmp  timeout.closing
          nop
          jmp  timeout.lastack
          nop
          jmp  timeout.timewait

; check for timeouts on all opened TCP sockets/streams

tout_check:
          ldx  #socknum-1
          ;  ldaxsockipid
          ;  bmi$<no
        - lda  sockstat,x
          beq  +
          lda  timeout,x
          bne  ++
      - + dex
          bpl  --

          rts

        + sec
          sbc  tcp_clock
          cmp  #100
          bcc  -
          ;   time is out !
          lda  sockstat,x
          and  #15
          cmp  #$0b            ; avoid illegal jumps
          bcs  -
          stx  zp2
          asl  a
          asl  a
          tax
          lda  timeout_jmptab+1,x
          sta  [+]+1
          lda  timeout_jmptab+2,x
          sta  [+]+2
          ldx  zp2
        + jmp  $ffff

; check socket and do JOBs if neccessary
;  < X=socket

sockserv: ;   zp2=socket
          ldx  zp2
          lda  sockstat,x
          and  #$0f
          cmp  #($0f & tcp_syn_received)
          bcc  +++
          cmp  #($0f & tcp_last_ack)
          bcs  +++

          ; states established, fin_wait1, fin_wait2, close_wait, closing

          lda  sndbufstat,x
          ;   first look if we have to send
          ;   data or an acknowledge
          and  #$50
          bne  +

          ;   check if we have sent win=0
          ;   and window rised
          lda  sndbufstat,x
          and  #$20
          beq  +++
          lda  freelst
          ora  recposh,x
          bmi  +++
        + lda  sockstat,x
          and  #$0f
          cmp  #($0f & tcp_syn_received)
          beq  +
          jmp  senddatapacket

        + jmp  timeout.synrecved

          ;   are there some jobs to do ?
        + lda  sockjob,x
          and  #1
          bne  +               ; job: active open
          lda  sockjob,x
          and  #2
          bne  ++              ; job: fin-wait0
          lda  sockjob,x
          and  #4
          bne  +++             ; job: shutdown
          rts

        + jmp  activeopen
        + jmp  fin_wait0
        + jmp  shutdown

;------------------------------------------------------------------------
; ICMP modul
;------------------------------------------------------------------------

; calculate icmp sum and send packet

icmpsumnsnd:
          jsr  sumheader
          lda  zp4
          eor  #255
          ldy  #11
          sta  (zp1),y
          lda  zp4+1
          eor  #255
          dey
          sta  (zp1),y
          ldx  zp2+1
          lda  buf.offs,x
          sta  zp1
          jsr  sumdata
          lda  zp4
          eor  #255
          ldy  #3
          sta  (zp1),y
          lda  zp4+1
          eor  #255
          dey
          sta  (zp1),y
          ldx  zp2+1
          jmp  sendpacket

icmp_type3:
          ;   no contact because of
          ;      code
          ;       0       net unreachable
          ;       1       host unreachable
          ;       2       protocol unreachable
          ;       3       port unreachable
          ;       4       fragmentation needed
          ;       5       source route failed

          ldy  #1
          lda  (zp1),y
          sta  zp4
          lda  zp1
          clc
          adc  #8
          sta  zp3
          sta  zp1
          ldy  #0
          lda  (zp1),y
          and  #$0f
          asl  a
          asl  a
          adc  zp1
          sta  zp3+1
          ldx  #4

        - ldy  #16
          lda  (zp1),y
          ldy  #12
          sta  (zp1),y
          inc  zp1
          dex
          bne  -

          ldx  #2
          lda  zp3+1
          sta  zp1

        - ldy  #2
          lda  (zp1),y
          pha
          ldy  #0
          lda  (zp1),y
          ldy  #2
          sta  (zp1),y
          pla
          ldy  #0
          sta  (zp1),y
          inc  zp1
          dex
          bne  -

          jsr  findsock
          bcs  +
          stx  zp2
          ldx  zp2+1
          jsr  killbuffer
          lda  zp4
          ora  #$80
          jmp  errclose

        + ldx  zp2+1
          jmp  killbuffer

icmp_type11:
          ;   lost packet because of
          ;      code
          ;       0    time to live exceeded
          ;       1 fr.reassembly tm exceeded
          ;   put a message out would be much
          ;   better. something like
          ;   icmp; sock #x <message>
          ;  ldx$<zp2+1
          ;  jmp$<killbuffer

icmp_type12:
          ;   parameter problem message
          ;   should put something out
          ;   .. later !!
          ;  ldx$<zp2+1
          ;  jmp$<killbuffer

icmp_type4:
          ;   source quench message
          ;   seems our little commi was to
          ;   fast !!? sorry slowing down is
          ;   not implemented
          ;  ldx$<zp2+1
          ;  jmp$<killbuffer

icmp_type5:
          ;   redirect message
          ;    just ignore it
          ;  ldx$<zp2+1
          ;  jmp$<killbuffer

icmp_type13:
icmp_type14:
          ;   ignore both timestamp
          ;    massage and reply massage
          ;  ldx$<zp2+1
          ;  jmp$<killbuffer

icmp_type15:
icmp_type16:
          ;   also ignore information
          ;   request and reply message

icmp_typeunknown:
          db("unknown icmp type\n")
          ldx  zp2+1
          jmp  killbuffer

icmp_type8:
          ;   echo message
          ;   send a echo reply message
          lda  #3
          sta  zp1

        - ldy  #12
          lda  (zp1),y
          tax
          ldy  #16
          lda  (zp1),y
          ldy  #12
          sta  (zp1),y
          txa
          ldy  #16
          sta  (zp1),y
          dec  zp1
          bpl  -

          ldy  #10
          lda  #0
          sta  zp1
          sta  (zp1),y
          iny
          sta  (zp1),y
          ldx  zp2+1
          lda  buf.offs,x
          sta  zp1
          ldy  #0
          lda  #0
          sta  (zp1),y
          ldy  #2
          sta  (zp1),y
          iny
          sta  (zp1),y
          sta  zp1
          jmp  icmpsumnsnd

icmp_type0:
          ;   echo reply message
          ;   should be notifyed to the
          ;   process that sent the
          ;   echo message...LATER !!
          db("got echo reply\n")
          ldx  zp2+1
          jmp  killbuffer

icmp_jmptab:
          jmp  icmp_type0
          nop
          jmp  icmp_typeunknown
          nop
          jmp  icmp_typeunknown
          nop
          jmp  icmp_type3
          nop
          jmp  icmp_type4
          nop
          jmp  icmp_type5
          nop
          jmp  icmp_typeunknown
          nop
          jmp  icmp_typeunknown
          nop
          jmp  icmp_type8
          nop
          jmp  icmp_typeunknown
          nop
          jmp  icmp_typeunknown
          nop
          jmp  icmp_type11
          nop
          jmp  icmp_type12
          nop
          jmp  icmp_type13
          nop
          jmp  icmp_type14
          nop
          jmp  icmp_type15
          nop
          jmp  icmp_type16

        - cli
          rts

; ICMP-modul

icmp_modul:
          sei
          ldx  icmplst         ; remove top element from icmp-list
          bmi  -
          stx  zp2+1
          lda  buf.l2nx,x
          sta  icmplst
          bpl  +
          sta  icmplst+1
        + cli
          lda  buf.mid,x
          sta  zp1+1
          lda  buf.offs,x
          sta  zp1
          jsr  sumdata
          lda  zp4
          and  zp4+1
          cmp  #$ff
          bne  ++              ; wrong checksum
          ldy  #0
          lda  (zp1),y
          cmp  #17
          bcs  +++             ; unknown type
          asl  a
          asl  a
          tax
          lda  icmp_jmptab+1,x
          sta  [+]+1
          lda  icmp_jmptab+2,x
          sta  [+]+2
        + jmp  $ffff

        + db("wrong ICMP checksum\n")
          ldx  zp2+1
          jmp  killbuffer

        + jmp  icmp_typeunknown

;------------------------------------------------------------------------
; USER CALLs
;------------------------------------------------------------------------

getbitadr: 
          sei                  ; get parameter passed by a BIT-instruction
          tsx                  ; after the JSR-instruction.
          inx                  ;   jsr <adr>, bit $<par>
          inx
          inx
          pha
          lda  $100,x
          sta  hl
          inx
          lda  $100,x
          sta  hh
          ldy  #1
          lda  (hl),y
          cmp  #$2c            ; check for BIT-opcode
          bne  +               ; if there is no BIT exit with errormessage
          iny
          lda  (hl),y
          tax
          iny
          lda  (hl),y
          tay
          pla
          clc
          rts                  ; parameter is returned in X/Y (A stays
                               ; unchanged)

        + db("missing bit$ parameter")
          sec
          cli
          rts

; connect
;  <- bit$ address of 4 byte inet addr (+2 byte port for TCP/UDP)
;     x = protocol (IPV4_TCP, IPV4_UDP, ...)
;  -> c = 0: x = stream
;     c = 1: a = error code	  
;                           E_NOTIMP, E_PROT, E_NOROUTE, E_NOPERM

con_cleanup0:
          jsr  close
          lda  #0
          .byte $2c
err_notimp:
          lda  #E_NOTIMP
          .byte $2c
err_nosock:
          lda  #E_NOSOCK
          sec
          cli
          rts

user_connect:
          cpx  #IPV4_TCP
          bne  err_notimp
          jsr  user_getsock
          bcs  err_nosock
          txa
          jsr  getbitadr
          stx  hl
          sty  hh
          tax
          bcs  con_cleanup0    ; missing bit$-parameter
          ldy  #0
          lda  (hl),y
          sta  remipa,x
          iny
          lda  (hl),y
          sta  remipb,x
          iny
          lda  (hl),y
          sta  remipc,x
          iny
          lda  (hl),y
          sta  remipd,x
          iny
          lda  (hl),y
          sta  remportl,x
          iny
          lda  (hl),y
          sta  remporth,x
          cli
          ldy  #$ff
          jsr  user_open
          bcs  con_cleanup0
          ldy  tcp_clock

        - jsr  break
          lda  sockstat,x
          and  #$0f
          beq  con_broken
          cmp  #9
          bcs  con_broken
          cmp  #($0f & tcp_established)
          beq  +
          sec
          tya
          eor  #$ff
          adc  tcp_clock
          cmp  #contimeout
          bcc  -
          jsr  close
          lda  #E_CONTIMEOUT
          sec
          rts

        + clc
          rts

con_broken:
          jsr  close
          lda  #E_CONREFUSED
          sec
          rts

        - lda  #E_NOPERM
          rts

; close
;  <- x = file-nr

user_close:
          jsr  user_fin
          bcs  -
          ldy  tcp_clock

        - jsr  break
          lda  sockstat,x
          and  #$0f
          beq  +
          cmp  #($0f & tcp_timewait)
          bcs  +
          sec
          tya
          eor  #$ff
          adc  tcp_clock
          cmp  #contimeout
          bcc  -

        + lda  socktype,x
          bpl  +
          lda  localportl,x
          pha
          lda  localporth,x
          pha
          lda  #$ff
        + pha
          jsr  close
          pla
          bmi  +               ; listen, then reopen socket
          clc
          rts

        - pla
          pla
          jmp  err_nosock
        - jmp  err_notimp

        + pla
          tay
          pla
          ldx  #IPV4_TCP
          clc

; listen
;  (open)
;  <- c = 0: a/y = 2 byte port number (TCP/UDP)
;     x = protocol
;  -> c = 0: ok, x = listenport
;     c = 1: a = error code
;                           E_NOTIMP, E_PROT, E_PORTINUSE
;  (close)
;  <- c = 1: x = listenport 
;  -> c = 0: ok
;     c = 1: a = error code
;                           E_NOTIMP, E_NOPORT

user_listen:
          bcs  end_listen
          cpx  #IPV4_TCP
          bne  -
          pha
          tya
          pha
          jsr  user_getsock
          bcs  --
          pla
          sei
          sta  hl
          pla
          sta  hh
          ldy  #socknum-1

        - lda  sockipid,y      ; check, if port is already used
          cmp  #$ff
          beq  +
          lda  hl
          cmp  localporth,y
          bne  +
          lda  hh
          cmp  localportl,y
          bne  +
          lda  #E_NOPORT
          cli
          rts

        + dey
          bpl  -
          lda  hl
          sta  localporth,x
          lda  hh
          sta  localportl,x
          cli
          lda  #$80
          sta  socktype,x
          ldy  #0
          jsr  user_open
          clc
          rts

        - pla
        - lda  #E_NOPERM
          sec
          rts

end_listen:
          cpx  #socknum
          bcs  -
          lda  sockipid,x
          cmp  ipid
          bne  -
          lda  #0
          sta  socktype,x
          jmp  user_close

; accept
;  <- bit$ = address of buffer for 4 byte IP address + 2 byte port
;     x = listenport
;     c = 0: don't block
;     c = 1: block
;  -> c = 0: x = file-nr for write
;     y = file-nr for read

user_accept:
          php
          cpx  #socknum
          bcs  --
          lda  sockipid,x
          cmp  ipid
          bne  --
          lda  socktype,x
          beq  --

        - jsr  break
          lda  sockstat,x
          and  #$0f
          beq  ++
          cmp  #9
          bcs  ++
          cmp  #($0f & tcp_established)
          bcs  +
        - plp
          php
          bcs  --
          sec
          .byte $24

        + clc
          pla
          lda  #0
          rts

        + jsr  user_close
          bcc  -
          plp
          sec
        - rts

; sockinfo
;  <- X=socket, bit$ pointer to struct (localport,rem.IP,rem.port)
;  -> A=sockstat 0..10 (bit7=listen), X/Y=PID

user_sockinfo:
          cpx  #socknum
          bcs  -
          lda  sockipid,x
          cmp  #$ff
          beq  -
          txa
          jsr  getbitadr
          stx  hl
          sty  hh
          ldy  #0
          tax
          lda  localportl,x
          sta  (hl),y
          lda  localporth,x
          iny
          sta  (hl),y
          lda  remipa,x
          iny
          sta  (hl),y
          lda  remipb,x
          iny
          sta  (hl),y
          lda  remipc,x
          iny
          sta  (hl),y
          lda  remipd,x
          iny
          sta  (hl),y
          lda  remportl,x
          iny
          sta  (hl),y
          lda  remporth,x
          iny
          sta  (hl),y
          lda  sockstat,x
          and  #$0f
          ldy  socktype,x
          bpl  +
          ora  #$80
        + pha
          lda  sockipid,x
          tax
          ldy  $c1c0,x ; PID hi
          lda  $c1a0,x ; PID lo
          tax
          pla
          clc
          cli
          rts

        - lda  #E_NOPERM
          sec
          rts

user_getbyte:
          cpx  #socknum
          bcs  -
          lda  sockipid,x
          cmp  ipid
          bne  -
          jmp  getbyte

user_putbyte:
          cpx  #socknum
          bcs  -
          ldy  sockipid,x
          cpy  ipid
          bne  -
          jsr  putbyte
          bcs  +
          lda  sndbufstat,x
          ora  #$40
          sta  sndbufstat,x
        + rts

;------------------------------------------------------------------------
; other stuff
;------------------------------------------------------------------------

        - jsr  break

main_loop:
          cli
          jsr  pack_poll
          jsr  tout_check
          lda  #socknum-1
          sta  zp2

        - ldx  zp2
          lda  sockipid,x
          cmp  #$ff
          beq  +
          jsr  sockserv
        + dec  zp2
          bpl  -

          lda  iplst
          and  tcplst
          and  icmplst
          ;and  udplst
          bmi  --

#ifdef debug
          inc  53280
#endif
          jsr  ip_modul
          jsr  icmp_modul
          jsr  tcp_modul
          ;jsr  udp_modul
          jmp  main_loop

        - rts

_drv_cleanup:
          ; this is called eyervtime a process is going to be killed
          ; A=ipid of that process

          bit  globflags
          bmi  -               ; skip

          ldx  #socknum-1

        - cmp  sockipid,x
          bne  +
          txa
          pha
          jsr  raw_close
          pla
          tax
        + dex
          bpl  -
          rts

_cleanup:
          ldx  mydrvnum
          bmi  +
          lda  #0
          sta  $c2a0,x
        + bit  globflags
          bvs  --
          clc
          jmp  allocpack_drv

.data

;------------------------------------------------------------------------
; global variables
;------------------------------------------------------------------------

          packid:     .word 0
          globflags:  .byte $ff
          freelst:    .byte 0
          iplst:      .word $ffff
          icmplst:    .word $ffff
          tcplst:     .word $ffff
          udplst:     .word $ffff
          errcnt:     .word 0
          availbuf:   .byte 0
          mydrvnum:   .byte $ff

          ownip:      .byte 0,0,0,0
          lstrnd:     .buf 1

          ack:        .buf 4
          seq:        .buf 4

          tcpflags:   .buf 1

; per buffer data (bufnum = 16)

          buf.offsh:  .buf bufnum
          buf.l2nx:   .buf bufnum
          buf.mid:    .buf bufnum
          buf.lenl:   .buf bufnum
          buf.lenh:   .buf bufnum
          buf.offs:   .buf bufnum

; per socket data (socknum = 8)

          sockstat:   .buf socknum
          localportl: .buf socknum
          localporth: .buf socknum
          remportl:   .buf socknum
          remporth:   .buf socknum
          remipa:     .buf socknum
          remipb:     .buf socknum
          remipc:     .buf socknum
          remipd:     .buf socknum
          reclstb:    .buf socknum
;          recposl:    .buf socknum
          recposh:    .buf socknum
          sndbufpg:   .buf socknum
          sndwrnx:    .buf socknum
          sndrdnx:    .buf socknum
          sndbufstat: .buf socknum
          sndunaa:    .buf socknum ; = sequence number in sent packets
          sndunab:    .buf socknum
          sndunac:    .buf socknum
          sndunad:    .buf socknum
          rcvnxta:    .buf socknum ; = acknowledge number
          rcvnxtb:    .buf socknum
          rcvnxtc:    .buf socknum
          rcvnxtd:    .buf socknum
          timeout:    .buf socknum
          sockipid:   .buf socknum
          reclstt:    .buf socknum
;          queueddatlst: .buf socknum
          sockjob:    .buf socknum
          socktype:   .buf socknum ; bit7:listen, bit6:accepted

.code


;------------------------------------------------------------------------
end_of_permanent_code:
;------------------------------------------------------------------------

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

read_IP:
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
          sta  ownip
          ldx  #1

        - lda  (zp1),y
          cmp  #"."
          bne  err_syntax
          iny
          beq  err_syntax
          jsr  read_decimal
          bcs  err_syntax
          sta  ownip,x
          inx
          cpx  #4
          bne  -

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

_init:
          ; initialize used variables

          ldx  stdin_ch
          jsr  pclose

          jsr  read_IP
          lda  _base+8
          sei
          beq  +
          ldx  #0
          stx  _base+8
          jsr  raw_mfree
        + cli

          ldx  stdout_ch
          jsr  strout
          bit  txt_startup

          ldx  #socknum-1
        - lda  #$ff
          sta  sockipid,x
          lda  #0
          sta  sockstat,x
          dex
          bpl  -
          ldx  #bufnum-1
          lda  #$ff
        - sta  buf.l2nx,x
          txa
          dex
          bpl  -
          lda  $dc04
          eor  $dc05
          eor  $d011
          eor  $d012
          sta  lstrnd

          ; search for packet interface

          ldx  #15

        - lda  $c2a0,x
          cmp  #"P"            ; "P" for Packet (driver)
          beq  +
        - dex
          bpl  --

          ldx  stderr_ch
          jsr  strout
          bit  txt_nopack
          jmp  suicide

        + lda  $c2b0,x
          sta  zp1+1
          ldy  #(magic-_base)
          sty  zp1
          ldy  #6

        - lda  (zp1),y
          cmp  txt_pack_magic,y
          bne  --
          dey
          bpl  -

          ; plug in packet driver

          ldy  #8
          lda  (zp1),y
          sta  allocpack_drv+1
          iny
          lda  (zp1),y
          sta  allocpack_drv+2
          ldy  #11
          lda  (zp1),y
          sta  getpack+1
          iny
          lda  (zp1),y
          sta  getpack+2
          ldy  #14
          lda  (zp1),y
          sta  putpack+1
          iny
          lda  (zp1),y
          sta  putpack+2

          ; try to connect to packetdriver

          sec
          sei
          jsr  allocpack_drv
          bcc  +

          ldx  stderr_ch
          jsr  strout
          bit  txt_packnotavail
          jmp  suicide

          ; add driver to system

        + sei
          lda  #$80
          sta  globflags

          ldx  #15

        - ldy  $c2a0,x
          beq  +
          dex
          bpl  -

          ldx  stderr_ch
          jsr  strout
          bit  txt_generror
          jmp  suicide

        + lda  _base
          sta  $c2b0,x
          lda  #"I"      ; I for IP driver
          sta  $c2a0,x
          stx  mydrvnum
          lda  #0
          sta  globflags
          cli

          ldx  stdout_ch
          jsr  strout
          bit  txt_ok
          lda  ownip
          jsr  print_decimal
          ldx  #1

        - stx  zp3
          lda  #"."
          jsr  putc
          ldx  zp3
          lda  ownip,x
          jsr  print_decimal
          ldx  zp3
          inx
          cpx  #4
          bne  -

          lda  #"\n"
          jsr  putc
          sei
          ; remove init code
          clc
          lda  #(>(end_of_permanent_code-_base-1))+1
          adc  _base
          tax
          tay

          lda  $c400,x
        - cmp  $c400,y
          bne  +
          pha
          txa
          sta  $c400,y
          pla
          iny
          bne  -
        + txa
          jsr  raw_mfree

          jmp  main_loop-3

.endofcode

dec_tab:
          .byte 10,100

txt_startup:
          .asc "TCP/IP for LUnix Version2.0\n"
          .asc "  by Poldi 1995 - May 9 1997\n\0"
txt_nopack:
          .asc "no packetdriver found\n\0"
txt_pack_magic:
          .asc "ipack"
          .byte 1,0
txt_packnotavail:
          .asc "packetdriver refused to connect\n\0"
txt_generror:
          .asc "error, tcp/ip not installed\n\0"
txt_howto:
          .asc "usage:  tcpip <IP>\n"
          .asc "  IP is current internet\n"
          .asc "  address in decimals\n\0"
txt_syntax:
          .asc "format of IP must match\n"
          .asc "<num>.<num>.<num>.<num> with each\n"
          .asc "number in the range of 0 to 255 !\n\0"
txt_ok:
#ifdef debug
          .asc " (debug version)\n"
#endif
          .asc "TCP/IP started with IP=\0"
