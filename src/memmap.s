begin 644 memmap
M__\``0(`QMD"`!Q``````````````````````$Q($````````$Q-$```````
M`````````````````$953$Q30U(`J1`@49!,5Q"I`$P8D*(!($*0H@=,0I"N
M`A`@79`L&1&B`2`_D*('(#^0J0Z-(-"I!HTAT*``A-FI!(7:H@2I()'9R-#[
MYMK*T/:B!*G8A=JI!Y'9R-#[YMK*T/8@WA`@$I"FQO#V>""TY<F,\`?)4?`=
M3)P0H@$@0I"B!R!"D"`2D*(!(#^0H@<@/Y!,:A"B`2!"D*('($*0J0!,&)"B
M`(;9J02%VJ``O2#"(`81Z+T@PB`&$>@8I=EI*(79D`+FVN`@T.%@."KP^TBI
MH)`"J2Z1V<AH&)#O`G1(25,@25,@02!&54Q,(%-#4D5%3B!!4%!,24-!5$E/
M3@UP4D534R!F."!43R!35$50(%1(4D]51T@@5$A%32X-`%!215-3($8X(%1/
,(%-7251#2"!"04-+
`
end

--------------------------------------------------------------

#include "kernel.inc"

#define  char_map    $0400
#define  color_map   $d800
#define  bg_color    $d020
#define  fg_color    $d021

.head "fullscr"  , .ptr

_sig.userbreak:
          ; reset screen (don't needed here because we haven't changed
          ; that much.
          lda  #$00
          jmp  suicide

_cleanup:
          ldx  #1
          jsr  unlock
          ldx  #7
          jmp  unlock          ; included because of kernel bug.

_init:
          ; before we can take over control of the screen, we have to
          ; lock the related semaphores (no.1 = keyboard, no.7 = screen)

          ldx  stdout_ch       ; print a little message
          jsr  strout
          bit  txt_go

          ldx  #1
          jsr  lock
          ldx  #7
          jsr  lock            ; lock keyboard and screen

refresh:
          lda  #14             ; change border color
          sta  bg_color
          lda  #6
          sta  fg_color

          ldy  #0
          sty  ptr
          lda  #>char_map
          sta  ptr+1
          ldx  #4
          lda  #" "            ; clear screen

        - sta  (ptr),y
          iny
          bne  -
          inc  ptr+1
          dex
          bne  -

          ldx  #4
          lda  #>color_map
          sta  ptr+1
          lda  #7              ; yellow color
          
        - sta  (ptr),y
          iny
          bne  -
          inc  ptr+1
          dex
          bne  -

_redraw:
          jsr  draw_table

          ; now check for F8 - Key

          jsr  break           ; force rescheduling (task switch)
          ldx  198
          beq  _redraw
          sei
          jsr  $e5b4           ; get character code from buffer
          cmp  #140            ; compare with F8
          beq  +
          cmp  #"q"
          beq  leave
          jmp  _redraw

        + ; F8 has been pressed, so temporary free screen and keyboard
          ; (init does also backups the whole screen here)

          ldx  #1
          jsr  unlock
          ldx  #7
          jsr  unlock
          jsr  break           ; give other processes a chance to get
                               ; the screen
          ldx  #1
          jsr  lock
          ldx  #7
          jsr  lock            ; ok, screen is ours again :-)
          jmp  refresh

leave:
          ldx  #1
          jsr  unlock
          ldx  #7
          jsr  unlock
          lda  #0
          jmp  suicide

draw_table:
          ldx  #0
          stx  ptr
          lda  #>char_map
          sta  ptr+1

        - ldy  #0
          lda  $c220,x
          jsr  put_bytemap
          inx
          lda  $c220,x
          jsr  put_bytemap
          inx
          clc
          lda  ptr
          adc  #40
          sta  ptr
          bcc  +
          inc  ptr+1
        + cpx  #32
          bne  -
        - rts

put_bytemap:
          sec
        - rol  a
          beq  --
          pha
          lda  #160
          bcc  +
          lda  #"."
        + sta  (ptr),y
          iny
          pla
          clc
          bcc  -

.endofcode

txt_go:   .asc "This is a full screen application\n"
          .asc "Press F8 to step through them.\n\0"

txt_words: .asc "press f8 to switch back"
words_length equ *-txt_words




