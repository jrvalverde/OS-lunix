;// macros for if, ifnot, else and endif

#begindef if(cond)
  cond  +   ; if
  jmp   _if%%next,push,ptop%%
  +
#enddef

#define ifnot(cond) cond  _if%%next,push,ptop%%  ; ifnot

#begindef else
  jmp  %%next,push,ptop%% 
_if%%swap,ptop,pop%%:  ; else
#enddef

#define endif _if%%ptop,pop%%:  ; endif

;// macros for while, whilenot, wend

#begindef while(cond)
_wh%%next,push,ptop%%:  ; while
  cond +
  jmp  _wh%%next,push,ptop%%
  +
#enddef

#begindef whilenot(cond)
_wh%%next,push,ptop%%:  ; whilenot
  cond  _wh%%next,push,ptop%%
#enddef

#begindef wend
  jmp _wh%%plast 1%%  ; wend
_wh%%ptop,pop,pop%%: 
#enddef
  
;// macros for repeat, until, untilnot=aslongas

#begindef repeat
_ru%%next,push,ptop%%:  ; repeat
#enddef

#begindef until(cond)
  cond  +  ; until
  jmp  _ru%%ptop,pop%%
  +
#enddef

#define untilnot(cond) cond  _ru%%ptop,pop%%  ; untilnot
#define aslongas(cond) cond  _ru%%ptop,pop%%  ; aslongas

