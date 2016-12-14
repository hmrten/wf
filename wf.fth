header : ] ] [
macro header : ] header ;

macro

\ literals
inst `lea   07  48 8D 05 00 00 00 00 \ lea rax, [rip+0]

\ control flow
inst `call  05  E8 00 00 00 00       \ call rel32

\ stack
inst `-sp   04  48 8D 5B F8          \ lea rbx, [rbx-8]
inst `+sp   04  48 8D 5B 08          \ lea rbx, [rbx+8]
inst `>s    03  48 89 03             \ mov [rbx], rax
inst `s>    03  48 8B 03             \ mov rax, [rbx]
inst `>x    03  48 8B 13             \ mov rdx, [rbx]

\ memory
inst `!     03  48 89 10             \ mov [rax], rdx
inst `d!    02  89 10                \ mov [rax], edx
inst `w!    03  66 89 10             \ mov [rax], dx
inst `b!    02  88 10                \ mov [rax], dl
inst `@     03  48 8B 00             \ mov rax, [rax]
inst `a!    03  48 89 07             \ mov [rdi], rax
inst `>a    03  48 8B 3B             \ mov rdi, [rbx]

\ arithmetic
inst t-#    04  48 83 E8 00          \ sub rax, imm8
inst t+#    04  48 83 C0 00          \ add rax, imm8
inst t-##   06  48 2D 00 00 00 00    \ sub rax, imm32
inst t+##   06  48 05 00 00 00 00    \ add rax, imm32
inst tneg   03  48 F7 D8             \ neg rax
inst t+     03  48 03 03             \ add rax, [rbx]

\ pair ops
inst 2drop  08  48 8B 43 08 48 8D 5B 10

inst b#- 03 48 83 E8
inst d#- 02 38 2D

\ : b|d,x 128 < if b,x else d,x then ;
\ : #,x word number b|d,x ;

forth
: 1- t-# [ 01 -b,x ] ;
: 2- t-# [ 02 -b,x ] ;
: 3- t-# [ 03 -b,x ] ;
: 4- t-# [ 04 -b,x ] ;
: !  `>x  `! 2drop ;
: d! `>x `d! 2drop ;
: w! `>x `w! 2drop ;
: b! `>x `b! 2drop ;
: @  `@ ;
: - tneg t+ `+sp ;
: call, `call [ m' `call xhere - -d,x ] xhere - -d,x ;
macro
: postpone m' call, ;

: 1- postpone b#- 01 b,x ;
: 2- postpone b#- 02 b,x ;
: 3- postpone b#- 03 b,x ;
: 4- postpone b#- 04 b,x ;
: 8- postpone b#- 08 b,x ;

forth
