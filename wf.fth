header : ] ] [
macro header : ] header ;

\ literals
inst lit64  0A  48 B8 00 00 00 00 00 00 00 00 \ mov rax, imm64
inst lit32s 07  48 C7 C0 00 00 00 00          \ mov rax, imm32 (sign extend)
inst lit32  05  B8 00 00 00 00                \ mov eax, imm32
inst lea    07  48 8D 05 00 00 00 00          \ lea rax, [rip+0]

\ control flow
inst call   05  E8 00 00 00 00                \ call rel32
inst jz     06  0F 84 00 00 00 00             \ jz rel32
inst jnz    06  0F 85 00 00 00 00             \ jnz rel32
inst jbe    06  0F 86 00 00 00 00             \ jbe rel32

\ stack
inst -sp    04  48 8D 5B F8                   \ lea rbx, [rbx-8]
inst +sp    04  48 8D 5B 08                   \ lea rbx, [rbx+8]
inst s!     03  48 89 03                      \ mov [rbx], rax
inst s0     03  48 8B 03                      \ mov rax, [rbx]
inst s      04  48 8B 43 00                   \ mov rax, [rbx+0]
inst s>n    03  48 8B 13                      \ mov rdx, [rbx]

\ memory
inst !n     03  48 89 10                      \ mov [rax], rdx
inst d!n    02  89 10                         \ mov [rax], edx
inst w!n    03  66 89 10                      \ mov [rax], dx
inst b!n    02  88 10                         \ mov [rax], dl
inst @      03  48 8B 00                      \ mov rax, [rax]

\ logic
inst ?      03  48 85 C0                      \ test rax, rax
inst #cmp   06  48 3D 00 00 00 00             \ cmp rax, imm32
inst cmp    03  48 3B 03                      \ cmp rax, [rbx]

\ arithmetic
inst b#-    04  48 83 E8 00                   \ sub rax, imm8
inst b#+    04  48 83 C0 00                   \ add rax, imm8
inst #-     06  48 2D 00 00 00 00             \ sub rax, imm32
inst #+     06  48 05 00 00 00 00             \ add rax, imm32
inst neg    03  48 F7 D8                      \ neg rax
inst s+     03  48 03 03                      \ add rax, [rbx]
inst n+     03  48 01 D0                      \ add rax, rdx

\ pair ops
inst 2drop  08  48 8B 43 08 48 8D 5B 10

\ postpone
: ` m' call [ m' call xrel -d,x ] xrel -d,x ;

85 0 sv!

: dup ` -sp ` s0 ;

forth
: jcc@ 0 sv@  ;
: jcc! 0 sv! ;
: jcc, 0F b,x jcc@ b,x 0 d,x ;
macro
: if jcc, xhere ;

: cmp
  lc-lit? if pop-lit ` #cmp -d,x ;; then
  ` cmp -d,x ` +sp ;

: u<= ` cmp 87 jcc! ;
: u>= ` cmp 82 jcc! ;

\ TODO: needs to update lc_stack
: lit
  ` dup
  7FFFFFFF u<= if ` lit32 -d,x ;; then
  -80000000 u>= if ` lit32s -d,x ;; then
  ` lit64 -,x ;

: +
  lc-lit? if pop-lit ` #+ -d,x ;; then
  ` s+ ` +sp ;

: -
  lc-lit? if pop-lit ` #- -d,x ;; then
  ` neg ` + ;

: #do
  pop-lit pop-lit \ #lo #hi
  55415441 d,x    \ push r12, r13
  BD41 w,x d,x    \ mov r13d, #hi
  BC41 w,x d,x    \ mov r12d, #lo
  xhere ;

forth
: !  s>n !n  2drop ;
: d! s>n d!n 2drop ;
: w! s>n w!n 2drop ;
: b! s>n b!n 2drop ;

: + + ; : - - ;

: 1+ 1 + ;
: 2+ 2 + ;
: 4+ 4 + ;
: 8+ 8 + ;

: 1- 1 - ;
: 2- 2 - ;
: 4- 4 - ;
: 8- 8 - ;
