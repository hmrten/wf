; Copyright (c) 2016 Mårten Hansson <hmrten@gmail.com>
; 
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
; 
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
; 
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.

; register allocation
; rax - top of stack           xmm0  - scratch
; rbx - data stack             xmm1  - scratch
; rsp - return stack           xmm2  - scratch
; rbp - locals   (saved rsp)   xmm4  - scratch
; rdi - addr reg (A)           xmm3  - scratch
; rsi - addr reg (B)           xmm5  - scratch
; rcx - scratch (win arg1)     xmm6  - scratch
; rdx - scratch (win arg2)     xmm7  - scratch
; r8  - scratch (win arg3)     xmm8  - reserved
; r9  - scratch (win arg4)     xmm9  - reserved
; r10 - scratch                xmm10 - reserved
; r11 - scratch                xmm11 - reserved
; r12 - loop counter           xmm12 - reserved
; r13 - loop limit             xmm13 - reserved
; r14 - reserved               xmm14 - reserved
; r15 - reserved               xmm15 - reserved

format pe64 console 6.0
entry _start

macro import [lib,api] {
  common
  local part0,part1,first
  macro part0 lib0,[api0] \{
    \common  lib0\#_STR db \`lib0
    \forward rb 2 - RVA $ AND 1
             label api0\#_STR at $-2
             db \`api0
    \common  db 0
  \}
  first = 7
  macro part1 lib1,[api1] \{
    \common  rb (8 - RVA $ AND 7) AND first
             first = 15
             label lib1\#_TAB
    \forward api1 dq RVA api1\#_STR
  \}
  forward part0 lib,api
  forward part1 lib,api
  common  data import
  forward dd 0,0,0,RVA lib#_STR,RVA lib#_TAB
  common  rd 5
  end data
}

macro win_enter n {
  mov rbp, rsp
  and rsp, -16
  sub rsp, n
}

macro win_leave {
  mov rsp, rbp
}

macro _dup {
  lea rbx, [rbx-8]
  mov [rbx], rax
}

macro _drop n=1 {
  assert n > 0
  mov rax, [rbx+8*(n-1)]
  lea rbx, [rbx+8*n]
}

macro _nip {
  lea rbx, [rbx+8]
}

macro dict_name name {
  local i
  i = $
  db name
  times 16-$+i db 0
}

macro dict_symb symb, xtra=0 {
  dq symb, xtra
}

macro checkstk {
  lea rcx, [stack_space]
  cmp rbx, rcx
  je abort.underflow
}

BUFSIZE equ 256

section '.data' data readable writeable

; == INITIALIZED DATA

import \
kernel32, <\
  ExitProcess,\
  GetStdHandle,\
  ReadFile,\
  WriteFile,\
  GetConsoleScreenBufferInfo,\
  SetConsoleCursorPosition,\
  GetFileType,\
  CreateFileA,\
  SetFilePointer>

align 8
forth_here dq forth_space + forth_name.size
macro_here dq macro_space + macro_name.size
dict_here  dq forth_here
code_here  dq code_space
data_here  dq data_space
action     dq interpret
var_base   dd 16

align 16
basedigits db '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
digitmap   dq $03FF000000000000, $07FFFFFE07FFFFFE

conin_str  db 'CONIN$', 0

align 16
forth_name:
  dict_name ''
  dict_name '\'
  dict_name 'h.'
  dict_name '.'
  dict_name '.s'
  dict_name 'cr'
  dict_name 'space'
  dict_name 'emit'
  dict_name 'type'
  dict_name 'accept'
  dict_name 'refill'
  dict_name 'parse'
  dict_name 'word'
  dict_name 'number'
  dict_name 'header'
  dict_name 'create'
  dict_name 'does>'
  dict_name ','
  dict_name 'b,'
  dict_name 'w,'
  dict_name 'd,'
  dict_name ',x'
  dict_name 'b,x'
  dict_name 'w,x'
  dict_name 'd,x'
  dict_name '-b,x'
  dict_name '-d,x'
  dict_name ']'
  dict_name 'forth'
  dict_name 'macro'
  dict_name 'here'
  dict_name 'xhere'
  dict_name 'inst'
  dict_name "f'"
  dict_name "m'"
.size = $ - forth_name

forth_symb:
  dict_symb 0
  dict_symb backslash
  dict_symb hexdot
  dict_symb dot
  dict_symb dot_s
  dict_symb cr
  dict_symb space
  dict_symb emit
  dict_symb type
  dict_symb accept
  dict_symb refill
  dict_symb parse
  dict_symb word_
  dict_symb number
  dict_symb header
  dict_symb create
  dict_symb does
  dict_symb comma
  dict_symb bcomma
  dict_symb wcomma
  dict_symb dcomma
  dict_symb comma_x
  dict_symb bcomma_x
  dict_symb wcomma_x
  dict_symb dcomma_x
  dict_symb bpatch_x
  dict_symb dpatch_x
  dict_symb rbracket
  dict_symb forth
  dict_symb macro_
  dict_symb here
  dict_symb xhere
  dict_symb inst
  dict_symb ftick
  dict_symb mtick
.size = $ - forth_symb

assert forth_name.size = forth_symb.size

macro_name:
  dict_name ''
  dict_name '\'
  dict_name '['
  dict_name ';'
  dict_name 'for'
  dict_name 'next'
  dict_name 'i'
.size = $ - macro_name

macro_symb:
  dict_symb 0
  dict_symb backslash
  dict_symb lbracket
  dict_symb semicolon
  dict_symb for
  dict_symb next
  dict_symb i
.size = $ - macro_symb

assert macro_name.size = macro_symb.size

; == UNINITIALIZED DATA

nob       rb BUFSIZE
tob       rb BUFSIZE
tib       rb BUFSIZE
tib_n     dd ?
tib_i     dd ?
cib       rb BUFSIZE
cib_cur   dq ?
cib_end   dq ?
align 16
lname     rb 32
stdin     dq ?
stdout    dq ?
conin     dq ?
reset_rsp dq ?

align 4096
forth_space rb 32*1024
macro_space rb 32*1024
align 4096
stack_end   rb 4096
stack_space = $
align 4096
data_space  rb 4096

section '.text' code readable writeable executable

; \ ( ignore rest of line )
backslash:
  _dup
  mov al, $0A
  call parse
  _drop 2
  ret

; h. ( x -- )
hexdot:
  checkstk
  bswap rax
  mov rcx, $F0F0F0F0F0F0F0F0
  and rcx, rax
  xor rax, rcx
  shr rcx, 4
  movq xmm0, rax
  movq xmm1, rcx
  punpcklbw xmm1, xmm0
  movdqa xmm0, xword [basedigits]
  pshufb xmm0, xmm1
  lea rax, [tob]
  movdqa [rax], xmm0
  mov byte [rax+16], $20
  _dup
  mov eax, 17
  jmp type

; . ( x -- )
dot:
  checkstk
  lea rsi, [basedigits]
  lea rdi, [nob+BUFSIZE-2]
  mov ecx, [var_base]
  mov r8, rax
  mov byte [rdi+1], ' '
  test rax, rax
  jns .loop
  neg rax
.loop:
  xor edx, edx
  div rcx
  mov rdx, [rsi+rdx]
  mov byte [rdi], dl
  dec rdi
  test rax, rax
  jnz .loop
  test r8, r8
  jns .nosign
  mov byte [rdi], '-'
  dec rdi
.nosign:
  inc rdi
  mov rax, rdi
  _dup
  lea rax, [nob+BUFSIZE]
  sub rax, rdi
  jmp type

; .S ( -- )
dot_s:
  _dup
  mov rsi, rbx
  lea rdi, [stack_space-8]
  call cr
  jmp .check
.loop:
  _dup
  mov rax, [rsi]
  call hexdot
  call cr
  lea rsi, [rsi+8]
.check:
  cmp rsi, rdi
  jne .loop
  _drop
  ret

; CR ( -- )
cr:
  _dup
  mov eax, $0A
  jmp emit

; SPACE ( -- )
space:
  _dup
  mov eax, $20

; EMIT ( char -- )
emit:
  mov cl, al
  lea rax, [tob]
  mov [rax], cl
  _dup
  mov eax, 1

; TYPE ( c-addr u -- )
type:
  win_enter $30
  mov rcx, [stdout]
  mov rdx, [rbx]
  mov r8d, eax
  xor r9, r9
  mov [rsp+$20], r9
  call [WriteFile]
  win_leave
  _drop 2
  ret

; ACCEPT ( c-addr u1 -- u2 )
accept:
  win_enter $50
  virtual at rsp+$28
    .lpNumberOfBytesRead      dd ?
                              dd ?
    .csbi.dwSize              dd ?
    .csbi.dwCursorPosition    dd ?
    .csbi.wAttributes         dw ?
    .csbi.srWindow            rw 4
    .csbi.dwMaximumWindowSize dd ?
  end virtual

  mov rcx, [stdin]
  mov rdx, [rbx]
  mov r8d, eax
  lea r9, [.lpNumberOfBytesRead]
  mov qword [rsp+$20], 0
  call [ReadFile]

  mov rcx, [stdout]
  lea rdx, [.csbi.dwSize]
  call [GetConsoleScreenBufferInfo]

  mov eax, [.lpNumberOfBytesRead]
  test eax, eax
  je .empty

  mov rdi, [rbx]
  mov rsi, rdi
  mov ecx, eax
  mov al, $0A
  repne scasb
  cmp byte [rdi-1], $0D
  jne @f
  dec rdi
@@:
  neg ecx
  mov edx, ecx
  mov rcx, [stdin]
  xor r8, r8
  mov r9d, 1            ; FILE_CURRENT
  call [SetFilePointer]

  dec rdi
  mov rax, rdi
  sub rax, rsi

  cmp qword [conin], 0
  jne .echo
  mov rsi, rax
  mov edx, [.csbi.dwCursorPosition]
  add edx, $FFFF0000
  dec eax
  or edx, eax
  mov rcx, [stdout]
  call [SetConsoleCursorPosition]
  mov rax, rsi
.leave:
  win_leave
  _nip
  ret
.empty:
  mov rdi, [conin]
  test rdi, rdi
  je .leave
  mov [stdin], rdi
  xor eax, eax
  mov [conin], rax
  jmp .leave
.echo:
  win_leave
  push rax
  call type
  pop rax
  _dup
  ret

; REFILL ( -- )
refill:
  _dup
  lea rax, [tib]
  _dup
  mov eax, BUFSIZE
  call accept
  mov dword [tib_n], eax
  mov dword [tib_i], 0
  test eax, eax
  _drop
  ret

; PARSE ( c -- c-addr u )
parse:
  lea rsi, [tib]
  mov edi, [tib_n]
  mov ecx, [tib_i]
  lea rbx, [rbx-8]
  lea r8, [rsi+rcx]
  mov edx, eax
  mov [rbx], r8
  xor eax, eax
  cmp ecx, edi
  jae .empty
.scan:
  cmp byte [rsi+rcx], dl
  je .done
  inc ecx
  cmp ecx, edi
  jb .scan
.done:
  lea rax, [rsi+rcx]
  sub rax, r8
  inc ecx
  mov [tib_i], ecx
.empty:
  ret

; WORD ( -- c-addr u )
word_:
  lea rsi, [tib]
  mov edx, [tib_n]
  mov ecx, [tib_i]
  _dup
  xor eax, eax
  _dup
  cmp ecx, edx
  jae .empty
.skip:
  cmp byte [rsi+rcx], $20
  ja .scan0
  inc ecx
  cmp ecx, edx
  jb .skip
.scan0:
  lea r8, [rsi+rcx]
  lea r9, [lname]
  lea r10, [lname+32]
  pxor xmm0, xmm0
  movdqa [r9], xmm0
.scan:
  mov al, byte [rsi+rcx]
  cmp al, $20
  jbe .done
  cmp r9, r10
  je .next
  mov byte [r9], al
  inc r9
.next:
  inc ecx
  cmp ecx, edx
  jb .scan
.done:
  lea rax, [rsi+rcx]
  sub rax, r8
  inc ecx
  mov [tib_i], ecx
  mov [rbx], r8
.empty:
  ret

; FIND ( c-addr u -- c-addr u | -- )
; ZF=1 found, ZF=0 not found, dict symb in rsi
; if found, consumes string, otherwise leaves it
mfind:
  mov rsi, [macro_here]
  jmp find.shared
find:
  mov rsi, [forth_here]
.shared:
  movdqa xmm0, xword [lname]
  jmp .next
.loop:
  pcmpeqb xmm1, xmm0
  pmovmskb ecx, xmm1
  cmp ecx, $FFFF
  je .match
.next:
  lea rsi, [rsi-16]
  movdqa xmm1, [rsi]
  ptest xmm1, xmm1
  jne .loop
  or ecx, 1
  ret
.match:
  _drop 2
  mov rsi, [rsi+16*1024]
  ret

; NUMBER ( c-addr u -- x )
; ZF=1 ok, entire string converted, ZF=0 not a number
number:
  test rax, rax
  jz .empty
  mov rsi, [rbx]
  mov edi, [var_base]
  xor r8, r8
  xor r9, r9
  cmp byte [rsi], '-'
  sete r9b
  add rsi, r9
  sub rax, r9
.loop:
  movzx ecx, byte [rsi]
  inc rsi
  and cl, $7F
  bt dword [digitmap], ecx
  jnc .done
  sub cl, $30
  mov edx, ecx
  cmp cl, 9
  jbe .digit
  add cl, $30
  and cl, $DF
  lea rdx, [rcx-('0'+7)]
.digit:
  cmp edx, edi
  jge .error
  imul r8, rdi
  add r8, rdx
  dec rax
  jnz .loop
.error:
  test r9, r9
  je .done
  neg r8
.done:
  test eax, eax
  mov rax, r8
.empty:
  _nip
  ret

; HEADER ( -- )
header:
  call word_
  movdqa xmm0, xword [lname]
  mov rdi, [dict_here]
  mov rdx, [rdi]
  movdqa [rdx], xmm0
  movq xmm1, [code_here]
  movdqa [rdx+16*1024], xmm1
  add rdx, 16
  mov [rdi], rdx
  _drop 2
  ret

; CREATE ( -- )
create:
  call header

; default DOES> semantics
; 0000: 48 8D 5B F8        lea         rbx,[rbx-8]
; 0004: 48 89 03           mov         qword ptr [rbx],rax
; 0007: 48 8D 05 00 00 00  lea         rax,[.data]
;       00
; 000E: C3  
  mov rdi, [code_here]
  mov rdx, [data_here]
  mov dword [rdi+$00], $F85B8D48
  mov dword [rdi+$04], $038948
  mov dword [rdi+$07], $058D48
  mov byte  [rdi+$0E], $C3
  add rdi, $0E
  sub rdx, rdi
  mov dword [rdi-$04], edx
  inc rdi
  mov [code_here], rdi
  ret

; DOES>
does:
  pop rdx                ; addr of instructions after DOES>
  mov rdi, [code_here]
  add rdi, 4             ; -4 for size of jump (rel32)
  sub rdx, rdi
  mov byte  [rdi-5], $E9 ; backpatch jmp, overwrite old ret
  mov dword [rdi-4], edx
  mov [code_here], rdi
  ret

; , ( x -- )
comma:
  mov ecx, 8
.shared:
  mov rdi, [data_here]
  mov [rdi], al
  lea rdi, [rdi+rcx]
  mov [data_here], rdi
  _drop
  ret
bcomma:
  mov ecx, 1
  jmp comma.shared
wcomma:
  mov ecx, 2
  jmp comma.shared
dcomma:
  mov ecx, 4
  jmp comma.shared

; ,x ( x -- )
comma_x:
  mov ecx, 8
.shared:
  mov rdi, [code_here]
  mov [rdi], rax
  lea rdi, [rdi+rcx]
  mov [code_here], rdi
@@:
  _drop
  ret
bcomma_x:
  mov ecx, 1
  jmp comma_x.shared
wcomma_x:
  mov ecx, 2
  jmp comma_x.shared
dcomma_x:
  mov ecx, 4
  jmp comma_x.shared
bpatch_x:
  mov rdi, [code_here]
  mov [rdi-1], al
  jmp @b
dpatch_x:
  mov rdi, [code_here]
  mov [rdi-4], eax
  jmp @b

; ] ( switch to compiler )
rbracket:
  lea rdx, [compile]
  mov [action], rdx
  ret

; FORTH ( start compiling into forth dict )
forth:
  lea rdi, [forth_here]
@@:
  mov [dict_here], rdi
  ret
; MACRO ( start compiling into macro dict )
macro_:
  lea rdi, [macro_here]
  jmp @b

; HERE ( -- addr )
here:
  _dup
  mov rax, [data_here]
  ret

; XHERE ( -- addr )
xhere:
  _dup
  mov rax, [code_here]
  ret

; INST ( -- )
inst:
  call header
  call word_
  call number
  jne abort.notfound
; 0000: B9 04 00 00 00     mov         ecx,4
; 0005: E8 00 00 00 00     call        00
  mov rdi, [code_here]
  lea rdx, [inst.docopy-$0A]
  sub rdx, rdi
  mov byte  [rdi+$00], $B9
  mov dword [rdi+$01], eax
  mov byte  [rdi+$05], $E8
  mov dword [rdi+$06], edx
  add rdi, $0A
  mov [code_here], rdi
  push r12
  mov r12, rax
  _drop
.loop:
  call word_
  call number
  jne abort.notfound
  call bcomma_x
  dec r12
  jnz .loop
  pop r12
  ret
.docopy:
  pop rsi
  mov rdi, [code_here]
  rep movsb
  mov [code_here], rdi
  ret

; F' ( -- xt )
ftick:
  call word_
  call find
  jne @f
.shared:
  _dup
  mov rax, rsi
  ret

; M' ( -- xt )
mtick:
  call word_
  call mfind
  je ftick.shared
@@:
  _drop 2
  jmp abort.notfound

; [ ( switch to interpreter )
lbracket:
  lea rdx, [interpret]
  mov [action], rdx
  ret

; ; ( end current definition )
semicolon:
  call lbracket
  mov rdi, [code_here]
  mov byte [rdi], $C3
  inc rdi
  mov [code_here], rdi
  ret

; FOR (  -- orig )
for:
; 0000: 41 54              push        r12
; 0002: 49 89 C4           mov         r12,rax
; 0005: 48 8B 03           mov         rax,qword ptr [rbx]
; 0008: 48 8D 5B 08        lea         rbx,[rbx+8]
  mov rdi, [code_here]
  mov word  [rdi+$00], $5441
  mov dword [rdi+$02], $C48949
  mov dword [rdi+$05], $038B48
  mov dword [rdi+$08], $085B8D48
  add rdi, 12
  mov [code_here], rdi
  _dup
  mov rax, rdi
  ret

; NEXT ( orig -- )
next:
; 0000: 49 FF CC           dec         r12
; 0003: 75 00              jne         00
; 0003: 0F 85 00 00 00 00  jne         00
; 0005: 41 5C              pop         r12
  mov rdi, [code_here]
  mov dword [rdi+$00], $CCFF49
  sub rax, 5
  sub rax, rdi
  add rdi, 3
  cmp eax, -128
  jl .rel32
  mov byte  [rdi+$00], $75
  mov byte  [rdi+$01], al
  add rdi, 2
  jmp .rest
.rel32:
  sub rax, 4
  mov  word [rdi+$00], $850F
  mov dword [rdi+$02], eax
  add rdi, 6
.rest:
  mov word  [rdi+$00], $5C41
  add rdi, 2
  mov [code_here], rdi
  _drop
  ret

; I ( -- x )
i:
  mov rdi, [code_here]
  mov dword [rdi+$00], $F85B8D48
  mov dword [rdi+$04], $038948
  mov dword [rdi+$07], $E0894C
  add rdi, 10
  mov [code_here], rdi
  ret

interpret:
  call find
  jne .number
  jmp rsi
.number:
  call number
  jne abort.notfound
  ret

compile:
  call mfind
  jne .call
  jmp rsi
.call:
  call find
  jne .number
  mov rdi, [code_here]
  mov byte [rdi], $E8
  add rdi, 5
  sub rsi, rdi
  mov dword [rdi-4], esi
  mov [code_here], rdi
  ret
.number:
  call number
  jne abort.notfound
  mov rdi, [code_here]
  mov dword [rdi+$00], $F85B8D48 ; lea rbx, [rbx-8]
  mov dword [rdi+$04], $038948   ; mov [rbx], rax
  mov word  [rdi+$07], $B848     ; mov rax, 0
  mov qword [rdi+$09], rax
  add rdi, 17
  mov [code_here], rdi
  _drop
  ret

quit:
  call refill
  je quit
  call space
.loop:
  call word_
  test eax, eax
  jz .refill
  call qword [action]
  jmp .loop
.refill:
  lea rdx, [tob]
  mov dword [rdx+0], $0D6B6F20 ; ok
  mov byte  [rdx+4], $0A
  mov [rbx], rdx
  mov eax, 5
  call type
  jmp quit

abort:
.underflow:
  _dup
  lea rax, [tob]
  mov dword [rax], '#UF'
  _dup
  mov eax, 3
  jmp .print
.notfound:
  _dup
  lea rax, [tob]
  movdqa xmm0, xword [lname]
  mov word [rax+0], '? '
  movdqu [rax+2], xmm0
  _dup
  mov eax, 18
.print:
  call type
  call cr
.reset:
  mov rsp, [reset_rsp]
  lea rbx, [stack_space]
  cmp [conin], 0
  je quit
  win_enter $20
  mov rcx, [stdin]
  mov edx, 0
  xor r8, r8
  mov r9d, 2 ; FILE_END
  call [SetFilePointer]
  win_leave
  jmp quit

_movedicts:
  lea rdi, [forth_space]
  lea rsi, [forth_name]
  mov ecx, forth_name.size
  push rcx
  rep movsb
  lea rdi, [forth_space+16*1024]
  lea rsi, [forth_symb]
  pop rcx
  rep movsb
  lea rdi, [macro_space]
  lea rsi, [macro_name]
  mov ecx, macro_name.size
  push rcx
  rep movsb
  lea rdi, [macro_space+16*1024]
  lea rsi, [macro_symb]
  pop rcx
  rep movsb
  ret

_wininit:
  win_enter $40
  mov ecx, -10        ; stdin
  call [GetStdHandle]
  mov [stdin], rax
  mov rcx, rax
  call [GetFileType]
  cmp al, 2
  je .noredir
  lea rcx, [conin_str]
  mov edx, $80000000     ; GENERIC_READ
  mov r8d, 1             ; FILE_SHARE_READ
  xor r9, r9
  mov dword [rsp+$20], 3 ; OPEN_EXISTING
  mov dword [rsp+$28], r9d
  mov qword [rsp+$30], r9
  call [CreateFileA]
  mov [conin], rax
.noredir:
  mov ecx, -11        ; stdout
  call [GetStdHandle]
  mov [stdout], rax
  win_leave
  ret

_start:
  call _movedicts
  call _wininit
  mov [reset_rsp], rsp
  lea rbx, [stack_space]
  jmp quit

align 4096
code_space rb 4096
