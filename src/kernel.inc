; *********************************************************
;  include-file for luna/lupo and LUnix_v0.1pX Version 0.2
; *********************************************************

#define mk_runtab    $9000
#define decl_process $9003
#define raw_send_signal   $9006
#define unblock      $900f
#define break        $9012
#define kill         $9015
#define suicide      $9018
#define panic        $901b
; panic is suicide with error
#define munblock     $901e
; to unblock all processes with a special waitstate
#define popen        $9021
#define pjoin        $9024
#define pclose       $9027
#define pgetc        $902a
#define pputc        $902d
#define raw_malloc   $9030
#define raw_mfree    $9033
#define raw_palloc   $9036
#define lmemblk      $9039
#define getexitcode  $903c
#define lock         $903f
#define unlock       $9042
#define init_process $9045
#define setsup       $9048
#define send_signal  $904b
#define getdrvpage   $904e
#define mover        $9051
#define relocator    $9051
#define strout       $905d
#define getipid      $9060
#define pload        $9063
#define getc         $9066
#define putc         $9069
#define putc_err     $906c
#define pipe_chk     $906f
#define malloc       $9072
#define mfree        $9075

#define stdin_ch     [_base+1]
#define stdout_ch    [_base+2]
#define stderr_ch    [_base+3]

#define _hl          $04    ; system zeropage (2 bytes)
#define ipid         $02    ; IPID of current process
#define _zpbase      217    ; hardcoded into luna !!

;special things

#begindef exit(code)
  lda  #code
  jmp  suicide
#enddef

#begindef print_string(string)
   ldx  _base+2  ; stdout channel
   jsr  strout
   bit  __pr%%next,pcur,push%%
   jmp  __pr%%next,pcur%%
   .data
 __pr%%ptop%%: .asc "string"
   .byte 0
   .code
 __pr%%pcur,pop%%:
#enddef

#ifndef _date
#msg note: no date "(DD.MM.YY) " defined
#define _date ""
#endif
#begindef ident(string,number)
   .asc "$ver: "
   .asc "string"
   .byte 32
   .asc "number"
   .byte 32
   .asc "_date"
   .asc "$\0"
#enddef
