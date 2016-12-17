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
; r15 - user variables         xmm15 - reserved

format pe64 console 6.0
entry start

macro IMPORT [lib,api] {
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

macro SNAME name {
  local i, n
  i = $
  db name
  times 16-($-i) db 0
}

macro FORTHNAMES { SNAME '--END-OF-NAMES--' }
macro FORTHSYMBS { dq -1, -1 }
macro MACRONAMES { SNAME '--END-OF-NAMES--' }
macro MACROSYMBS { dq -1, -1 }

macro FORTHENTRY name, xt, ct=0 {
  macro FORTHNAMES \{
    FORTHNAMES
    SNAME name
  \}
  macro FORTHSYMBS \{
    FORTHSYMBS
    dq xt, ct
  \}
}

macro MACROENTRY name, xt, ct=0 {
  macro MACRONAMES \{
    MACRONAMES
    SNAME name
  \}
  macro MACROSYMBS \{
    MACROSYMBS
    dq xt, ct
  \}
}

macro FORTHCODE name, xt {
  FORTHENTRY name, xt
  label xt
}

macro MACROCODE name, xt {
  MACROENTRY name, xt
  label xt
}

macro USERVAR name, addr, val {
  FORTHENTRY name, addr
  addr: dq val
}

macro RELOCDICT names, symbs, space {
  lea rsi, [names]
  lea rdi, [space]
  mov ecx, names#.size
  push rcx
  rep movsb
  pop rcx
  lea rsi, [symbs]
  lea rdi, [space+16*1024]
  rep movsb
}

macro WINENTER n {
  mov rbp, rsp
  and rsp, -16
  sub rsp, n
}

macro WINLEAVE {
  mov rsp, rbp
}

macro DUP {
  lea rbx, [rbx-8]
  mov [rbx], rax
}

macro DROP n=1 {
  assert n > 0
  mov rax, [rbx+8*(n-1)]
  lea rbx, [rbx+8*n]
}

macro NIP {
  lea rbx, [rbx+8]
}

section '.data' data readable writeable

align 16
basedigits db '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
digitmap   dq $03FF000000000000, $07FFFFFE07FFFFFE

align 8
user_vars:
USERVAR 'fd'  , fd, forth_space+forth_names.size
USERVAR 'md'  , md, macro_space+macro_names.size
USERVAR 'hd'  , hd, fd
USERVAR '#tib', tib_n, 0
USERVAR '>in' , tib_i, 0
USERVAR 'base', base, 16

; == UNINITIALIZED DATA

align 8
stdin       dq ?
stdout      dq ?

align 4096
forth_space rb 32*1024
macro_space rb 32*1024

BUFSIZE equ 256
tib         rb BUFSIZE
tob         rb BUFSIZE

align 16
lname       rb 32

align 4096
data_space  rb 32*1024*1024

align 4096
stack_end   rb 4096
stack_space = $

section '.text' code executable readable writeable

; NUMBER ( a u -- x )
; ZF=1 ok, entire string converted, ZF=0 not a number
FORTHCODE 'number', number
  test rax, rax
  jz .empty
  mov rsi, [rbx]
  mov edi, [base]
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
  jmp nip

; WORD ( -- a u )
FORTHCODE 'word', word_
  lea rsi, [tib]
  mov edx, [tib_n]
  mov ecx, [tib_i]
  lea rbx, [rbx-16]
  mov [rbx+8], rax
  xor eax, eax
  mov [rbx], rax
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
  lea r10, [lname+31]
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

; REFILL ( -- ZF )
FORTHCODE 'refill', refill
  lea rbx, [rbx-16]
  mov [rbx+8], rax
  lea rdx, [tib]
  mov [rbx], rdx
  mov eax, BUFSIZE
  call accept
  mov dword [tib_n], eax
  mov dword [tib_i], 0
  test eax, eax

; DROP ( x -- )
FORTHCODE 'drop', drop
  DROP
  ret

; ACCEPT ( a u1 -- u2 )
FORTHCODE 'accept', accept
  mov rdi, [rbx]
  WINENTER $50
  virtual at rsp+$28
    .nread                  dd ?
                            dd ? ; align .bi struct
    .bi_dwSize              dd ?
    .bi_dwCursorPosition    dd ?
    .bi_wAttributes         dw ?
    .bi_srWindow            rw 4
    .bi_dwMaximumWindowSize dd ?
  end virtual
  mov rcx, [stdin]
  mov rdx, rdi
  mov r8d, eax
  lea r9, [.nread]
  mov qword [rsp+$20], 0
  call [ReadFile]

  mov eax, [.nread]
  test eax, eax
  je .empty

  ; trim trailing [\r]\n
  mov rsi, rdi
  mov ecx, eax
  mov al, $0A
  repne scasb
  cmp byte [rdi-2], $0D
  jne @f
  dec rdi ; drop \r
@@:
  dec rdi ; drop \n

  ; adjust file pointer if we read past first [\r]\n
  ; (can happen when stdin is redirected to a file)
  neg ecx
  mov edx, ecx
  mov rcx, [stdin]
  xor r8, r8
  mov r9d, 1            ; FILE_CURRENT
  call [SetFilePointer]

  mov rax, rdi
  sub rax, rsi

  mov esi, eax
  mov rcx, [stdout]
  lea rdx, [.bi_dwSize]
  call [GetConsoleScreenBufferInfo]
  mov edx, [.bi_dwCursorPosition]
  add edx, $FFFF0000
  or edx, esi
  mov rcx, [stdout]
  call [SetConsoleCursorPosition]
  mov eax, esi

.empty:
  WINLEAVE

; NIP ( x y -- x )
FORTHCODE 'nip', nip
  NIP
  ret

; TYPE ( a u -- )
FORTHCODE 'type', type
  WINENTER $30
  mov rcx, [stdout]
  mov rdx, [rbx]
  mov r8d, eax
  xor r9, r9
  mov [rsp+$20], r9
  call [WriteFile]
  WINLEAVE
  jmp _2drop

; [M]FIND ( a u -- a u | -- )
; ZF=1 found, ZF=0 not found, symb offset in rsi
; if found, consumes string, otherwise leaves it
mfind:
  mov rsi, [md]
  lea rdi, [macro_space]
  jmp @f
find:
  mov rsi, [fd]
  lea rdi, [forth_space]
@@:
  movdqa xmm0, xword [lname]
  jmp .next
.loop:
  movdqa xmm1, [rsi]
  pcmpeqb xmm1, xmm0
  pmovmskb ecx, xmm1
  cmp ecx, $FFFF
  je .match
.next:
  sub rsi, 16
  cmp rsi, rdi
  ja .loop
  or ecx, 1
  ret
.match:
  lea rsi, [rsi+16*1024]

; 2DROP ( x y -- )
FORTHCODE '2drop', _2drop
  DROP 2
  ret

start:
  RELOCDICT forth_names, forth_symbs, forth_space
  RELOCDICT macro_names, macro_symbs, macro_space

  WINENTER $20
  mov ecx, -10
  call [GetStdHandle]
  mov [stdin], rax
  mov ecx, -11
  call [GetStdHandle]
  mov [stdout], rax
  WINLEAVE

  lea rbx, [stack_space]
  lea r15, [user_vars]

  call refill

  DUP
  lea rax, [tob]
  mov dword [rax+0], $0D6B6F20 ; ok
  mov byte  [rax+4], $0A
  DUP
  mov eax, 5
  call type

@@:
  call word_
  test eax, eax
  jz bye
  call find
  je bye0

  DROP 2
  DUP
  lea rdi, [lname]
  xor eax, eax
  mov ecx, -1
  repne scasb
  not ecx
  dec ecx
  lea rax, [lname]
  DUP
  mov eax, ecx
  call type

cr:
  lea rbx, [rbx-16]
  mov [rbx+8], rax
  lea rdi, [tob]
  mov word [rdi], $0A0D ; \r\n
  mov [rbx], rdi
  mov eax, 2
  call type
  jmp @b

  ; mov edx, eax
  ; lea rax, [tib]
  ; DUP
  ; mov eax, edx
  ; call type
bye0:
  lea rdi, [stack_space]
  cmp rbx, rdi
  jne badstk
bye:
  WINENTER $20
@@:
  xor ecx, ecx
  jmp [ExitProcess]
badstk:
  WINENTER $20
  xor ecx, ecx
  lea rdx, [.str1]
  lea r8d, [.str2]
  xor r9, r9
  call [MessageBoxA]
  jmp @b
.str1: db '#ERR: Unbalanced data stack', 0
.str2: db 'Error', 0

section '.idata' data readable

IMPORT \
kernel32, <\
  GetConsoleScreenBufferInfo,\
  SetConsoleCursorPosition,\
  SetFilePointer,\
  ReadFile,\
  WriteFile,\
  GetStdHandle,\
  ExitProcess>,\
user32, <\
  MessageBoxA>

align 16
forth_names:
  FORTHNAMES
.size = $ - forth_names
forth_symbs:
  FORTHSYMBS
.size = $ - forth_symbs

macro_names:
  MACRONAMES
.size = $ - macro_names
macro_symbs:
  MACROSYMBS
.size = $ - macro_symbs

assert forth_names.size = forth_symbs.size
assert macro_names.size = macro_symbs.size

macro PRINT n {
  local d
  repeat 4
    d = '0' + n shr (16-%*4) and $F
    if d > '9'
      d = d + 'A'-'9'-1
    end if
    display d
  end repeat
}

display 'forths: '
PRINT (forth_names.size / 16 - 1)
display $0D, $0A, 'macros: '
PRINT (macro_names.size / 16 - 1)
