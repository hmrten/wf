header : ] ] [
macro header : ] header ;

\ literals
inst lit    0A  48 B8 00 00 00 00 00 00 00 00 \ mov rax, imm64
inst lit32s 07  48 C7 C0 00 00 00 00          \ mov rax, imm32 (sign extend)
inst lit32  05  B8 00 00 00 00                \ mov eax, imm32
inst lea    07  48 8D 05 00 00 00 00          \ lea rax, [rip+0]

\ control flow
inst call   05  E8 00 00 00 00                \ call rel32
inst jz     06  0F 84 00 00 00 00             \ jz rel32
inst jnz    06  0F 85 00 00 00 00             \ jnz rel32

\ stack
inst -sp    04  48 8D 5B F8                   \ lea rbx, [rbx-8]
inst +sp    04  48 8D 5B 08                   \ lea rbx, [rbx+8]
inst s!     03  48 89 03                      \ mov [rbx], rax
inst s0@    03  48 8B 03                      \ mov rax, [rbx]
inst s@     04  48 8B 43 00                   \ mov rax, [rbx+0]
inst s>n    03  48 8B 13                      \ mov rdx, [rbx]

\ memory
inst !n     03  48 89 10                      \ mov [rax], rdx
inst d!n    02  89 10                         \ mov [rax], edx
inst w!n    03  66 89 10                      \ mov [rax], dx
inst b!n    02  88 10                         \ mov [rax], dl
inst @      03  48 8B 00                      \ mov rax, [rax]

\ logic
inst ?      03  48 85 C0                      \ test rax, rax

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

: if ` jnz xhere ;

: +
  lc-lit? if ` pop-lit ` #+ -d,x ;; then
  ` s+ ` +sp ;

forth
: !  s>n !n  2drop ;
: d! s>n d!n 2drop ;
: w! s>n w!n 2drop ;
: b! s>n b!n 2drop ;

forth
: lit-: header ` b#- -b,x ` ; ;

1 lit-: 1-
2 lit-: 2-
4 lit-: 4-
8 lit-: 8-
