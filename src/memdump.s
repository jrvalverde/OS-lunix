begin 644 memdump
M__\``0,`Q-D$`!Q`````````````````````````````````````````````
M`````````````````$U%341535``J1`@49!,2!&QV<D@T`3(T/=@R0!@&&"I
M`(7;A=RQV3CI,)#QR0J0"FDO*1_)![#E:0FJI=PI\-`<I=P&VRH&VRH&VRH&
MVRJ%W(IEVX7;D`+FW,C0R#A@JJDD(&F0BDA*2DI*JKW8$2!ID&@I#ZJ]V!%,
M:9"@`+D'$O`(,`<@:9#(T/-@IMK)@-`)O0#$()X03,`0R8'0";T`PR">$$S`
M$,F"T"*]`,/)(+`32*J]P,$@GA!HJKV@P2">$$S`$*DM(&F03,`0O0##R2"P
M\:J]`,)XA0682*DXA02@`+$$F2(2\`K(P`C0]*D`F2(26*X"$"!=D"PB$FBH
M3,`0K@,0(%V0+.@13!B0K@@0\.^&VJ``A-G(($@0\.,@5Q"PWJ7;2*7<2"!(
M$/#3(%<0L,YHA=IHA=DLUQ$0`R"T$*D`C=<1C=81I=H@EQ"EV2">$*D@(&F0
M(&F0H`"QV2">$.;9T`?FVJG_C=<1I=G%V]`0I=K%W-`*J0T@:9"I`$P8D.[6
M$:W6$<D(\`6I($R3$:D-(&F03'01`@#_,#$R,S0U-C<X.6%B8V1E9E5304=%
M.B`@345-1%5-4"`\4U1!4E0^(#Q%3D0^#0!M:60]@"!O5TY%4CV!('!I9#V"
((&-O;3V##0``
`
end

--------------------------------------------------------------

#include "kernel.inc"

.head "memdump", .ptr, .tmp

; eat up white spaces

hl equ $04
hh equ $05

eat_spaces:
        - lda  (ptr),y
          cmp  #" "
          bne  +
          iny
          bne  -
          rts

        + cmp  #0
          rts

        - clc
          rts

get_hex:
          lda  #0
          sta  tmp
          sta  tmp+1

        - lda  (ptr),y
          sec
          sbc  #"0"
          bcc  --
          cmp  #10
          bcc  +
          adc  #"0"-1
          and  #31
          cmp  #7
          bcs  --
          adc  #9
        + tax
          lda  tmp+1
          and  #$f0
          bne  ++
          lda  tmp+1
          asl  tmp
          rol  a
          asl  tmp
          rol  a
          asl  tmp
          rol  a
          asl  tmp
          rol  a
          sta  tmp+1
          txa 
          adc  tmp
          sta  tmp
          bcc  +
          inc  tmp+1
        + iny
          bne  -
        + sec
          rts

print_hexp:
          tax
          lda  #"$"
          jsr  putc
          txa
print_hex:
          pha
          lsr  a
          lsr  a
          lsr  a
          lsr  a
          tax
          lda  hex_tab,x
          jsr  putc
          pla
          and  #$0f
          tax
          lda  hex_tab,x
          jmp  putc

print_pageheader:
          ldy  #0

        - lda  txt_header,y
          beq  +
          bmi  ++
          jsr  putc
_pmore:   iny
          bne  -
        + rts

        + ldx  ptr+1
          cmp  #$80
          bne  +

          lda  $c400,x
          jsr  print_hex
          jmp  _pmore

        + cmp  #$81
          bne  +

          lda  $c300,x
          jsr  print_hex
          jmp  _pmore

        + cmp  #$82
          bne  +

          lda  $c300,x
          cmp  #32
          bcs  __null
          pha
          tax
          lda  $c1c0,x
          jsr  print_hex
          pla
          tax
          lda  $c1a0,x
          jsr  print_hex
          jmp  _pmore

__null:   lda  #"-"
          jsr  putc
          jmp  _pmore

        + lda  $c300,x
          cmp  #32
          bcs  __null
          tax
          lda  $c200,x
          sei
          sta  hh
          tya
          pha
          lda  #$38
          sta  hl
          ldy  #0
        - lda  (hl),y
          sta  com_buffer,y
          beq  +
          iny
          cpy  #8
          bne  -
          lda  #0
          sta  com_buffer,y
        + cli
          ldx  stdout_ch
          jsr  strout
          bit  com_buffer
          pla
          tay
          jmp  _pmore

howto:
          ldx  stderr_ch
          jsr  strout
          bit  txt_howto
          jmp  suicide

_init:
          ldx  _base+8
          beq  howto
          stx  ptr+1
          ldy  #0
          sty  ptr
          iny
          jsr  eat_spaces
          beq  howto
          jsr  get_hex
          bcs  howto
          lda  tmp
          pha
          lda  tmp+1
          pha
          jsr  eat_spaces
          beq  howto
          jsr  get_hex
          bcs  howto
          pla
          sta  ptr+1
          pla
          sta  ptr

new_line:
          bit  flag
          bpl  +
          jsr  print_pageheader
        + lda  #0
          sta  flag
          sta  cnt
          lda  ptr+1
          jsr  print_hexp
          lda  ptr
          jsr  print_hex
          lda  #" "
          jsr  putc

        - jsr  putc
          ldy  #0
          lda  (ptr),y
          jsr  print_hex
          inc  ptr
          bne  +
          inc  ptr+1
          lda  #$ff
          sta  flag
        + lda  ptr
          cmp  tmp
          bne  +
          lda  ptr+1
          cmp  tmp+1
          bne  +
          lda  #"\n"
          jsr  putc
          lda  #0
          jmp  suicide          

        + inc  cnt
          lda  cnt
          cmp  #8
          beq  +
          lda  #" "
          jmp  -

        + lda  #"\n"
          jsr  putc
          jmp  new_line

.endofcode

cnt: .buf 1
flag: .byte $ff

hex_tab:  .asc "0123456789ABCDEF"

txt_howto:
          .asc "usage:  memdump <start> <end>\n\0"

txt_header:
          .asc "MID="
          .byte $80
          .asc " Owner="
          .byte $81
          .asc " PID="
          .byte $82
          .asc " COM="
          .byte $83
          .asc "\n"
          .byte 0

com_buffer: .buf 9
