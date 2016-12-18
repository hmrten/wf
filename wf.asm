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

macro CHECKSTK {
  lea rcx, [stack_space]
  cmp rbx, rcx
  je abort.uf
}

section '.data' data readable writeable

align 16
basedigits db '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
digitmap   dq $03FF000000000000, $07FFFFFE07FFFFFE

align 8
action     dq interpret

user_vars:
USERVAR 'fd'   , fd, forth_space+forth_names.size
USERVAR 'md'   , md, macro_space+macro_names.size
USERVAR 'hd'   , hd, fd
USERVAR '#tib' , tib_n, 0
USERVAR '>in'  , tib_i, 0
USERVAR 'base' , base, 16
USERVAR 'xhere', xhere, code_space

; == UNINITIALIZED DATA

align 8
stdin       dq ?
stdout      dq ?
rsp0        dq ?

align 4096
forth_space rb 32*1024
macro_space rb 32*1024

BUFSIZE equ 256
tib         rb BUFSIZE
tob         rb BUFSIZE
nob         rb BUFSIZE

align 16
lname       rb 32
lname_n     db ?

align 4096
data_space  rb 32*1024*1024
data_end = $

align 4096
stack_end   rb 4096
stack_space = $

section '.text' code executable readable

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

; CR ( -- )
FORTHCODE 'cr', cr
  DUP
  mov dl, $0A
  jmp emit.0

; SPACE ( -- )
FORTHCODE 'space', space
  DUP
  mov al, ' '

; EMIT ( ch -- )
FORTHCODE 'emit', emit
  mov dl, al
.0:
  lea rax, [tob]
  mov byte [rax], dl
  DUP
  mov eax, 1

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

; H. ( x -- )
FORTHCODE 'h.', hexdot
  CHECKSTK
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
  DUP
  mov eax, 17
  jmp type

; . ( x -- )
FORTHCODE '.', dot
  CHECKSTK
  lea rsi, [basedigits]
  lea rdi, [nob+BUFSIZE-2]
  mov ecx, [base]
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
  DUP
  lea rax, [nob+BUFSIZE]
  sub rax, rdi
  jmp type

; .S ( -- )
FORTHCODE '.s', dot_s
  DUP
  mov rsi, rbx
  lea rdi, [stack_space-8]
  call cr
  jmp .check
.loop:
  DUP
  mov rax, [rsi]
  call hexdot
  call cr
  lea rsi, [rsi+8]
.check:
  cmp rsi, rdi
  jne .loop
  DROP
  ret

; .DEPTH ( -- )
FORTHCODE '.depth', dot_depth
  DUP
  lea rax, [stack_space]
  sub rax, rbx
  shr rax, 3
  dec rax
  jmp dot

; COLOR ( x -- )
FORTHCODE 'color', color
  mov edx, eax
  DROP
.set:
  push rax
  WINENTER $20
  mov rcx, [stdout]
  call [SetConsoleTextAttribute]
  WINLEAVE
  pop rax
  ret

FORTHCODE 'red', red
  mov edx, $0C ; FOREGROUND_RED | FOREGROUND_INTENSITY
  jmp color.set
FORTHCODE 'silver', silver
  mov edx, $07
  jmp color.set

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
  lea r10, [lname]
  sub r9, r10
  mov byte [lname_n], r9b
  lea rax, [rsi+rcx]
  sub rax, r8
  inc ecx
  mov [tib_i], ecx
  mov [rbx], r8
.empty:
  ret

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

; HEADER ( -- )
FORTHCODE 'header', header
  call word_
  movdqa xmm0, xword [lname]
  mov rdx, [hd]
  mov rdi, [rdx]
  movdqa [rdi], xmm0
  mov rcx, [xhere]
  mov [rdi+16*1024], rcx
  add rdi, 16
  mov [rdx], rdi
  jmp _2drop

; ,x ( x -- )
FORTHCODE ',x', comma_x
  mov rdi, [xhere]
  mov byte [rdi], al
  inc qword [xhere]
  DROP
  ret

; FORTH ( switch to compiling into forth dict )
FORTHCODE 'forth', forth
  lea rdi, [fd]
@@:
  mov [hd], rdi
  ret

; MACRO ( switch to compiling into macro dict )
FORTHCODE 'macro', macro_
  lea rdi, [md]
  jmp @b

; ] ( switch to compiler )
FORTHCODE ']', rbrack
  mov qword [action], compile
  ret

; [ ( switch to interpreter )
MACROCODE '[', lbrack
  mov qword [action], interpret
  ret

; ; ( end current definition )
MACROCODE ';', semi
  call lbrack
  mov rdi, [xhere]
  mov byte [rdi], $C3
  inc qword [xhere]
  ret

; ASM ( parse: "n" n*i opcodes and assemble inline -- )
MACROCODE 'asm', asm
  call word_
  call number
  jne abort.notfnd
  push r12
  mov r12, rax
  DROP
.loop:
  call word_
  call number
  jne abort.notfnd
  call comma_x
  dec r12
  jnz .loop
  pop r12
.docopy:
  ret

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

interpret:
  call find
  jne .number
  mov rdi, [rsi]
  lea rdx, [data_end]
  cmp rdi, rdx
  jb .uservar
  jmp rdi
.uservar:
  DUP
  mov rax, [rdi]
  ret
.number:
  call number
  jne abort.notfnd
  ret

compile:
  call mfind
  jne .ccall
  jmp qword [rsi]
.ccall:
  call find
  jne .cnum
  mov rdx, [rsi]
  mov rdi, [xhere]
  mov byte [rdi], $E8 ; call rel32
  add rdi, 5
  sub rdx, rdi
  mov dword [rdi-4], edx
  mov [xhere], rdi
  ret
.cnum:
  call number
  jne abort.notfnd
  mov rdi, [xhere]
  mov dword [rdi+$00], $F85B8D48 ; lea rbx, [rbx-8]
  mov dword [rdi+$04], $038948   ; mov [rbx], rax
  mov word  [rdi+$07], $B848     ; mov rax, 0
  mov qword [rdi+$09], rax
  add rdi, 17
  mov [xhere], rdi
  DROP
  ret

quit:
  call refill
  je .prompt0
  call space
.loop:
  call word_
  test eax, eax
  je .prompt
  call qword [action]
  jmp .loop
.prompt0:
  call space
  lea rbx, [rbx-16]
.prompt:
  lea rdi, [tob]
  mov dword [rdi+0], $3A6B6F20 ; ok:
  mov [rbx], rdi
  mov eax, 4
  call type
  call dot_depth
  call cr
  jmp quit

abort:
.uf:
  lea rbx, [rbx-16]
  mov [rbx+8], rax
  lea rdi, [tob]
  mov dword [rdi], '#UF'
  mov [rbx], rdi
  mov eax, 3
  jmp .print
.notfnd:
  lea rbx, [rbx-16]
  mov [rbx+8], rax
  lea rdi, [tob]
  push rdi
  mov word [rdi], '? '
  add rdi, 2
  mov eax, 2
  lea rsi, [lname]
  movzx ecx, byte [lname_n]
  add eax, ecx
  rep movsb
  pop qword [rbx]
.print:
  call red
  call type
  call cr
  call silver
.reset:
  mov rsp, [rsp0]
  lea rbx, [stack_space]
  jmp quit

start:
  RELOCDICT forth_names, forth_symbs, forth_space
  RELOCDICT macro_names, macro_symbs, macro_space

  WINENTER $30
  mov ecx, -10
  call [GetStdHandle]
  mov [stdin], rax
  mov ecx, -11
  call [GetStdHandle]
  mov [stdout], rax

  lea rcx, [code_space]
  mov edx, 32*1024
  mov r8d, $40          ; PAGE_EXECUTE_READWRITE
  lea r9, [rsp+$28]
  call [VirtualProtect]
  WINLEAVE

  lea rbx, [stack_space]
  lea r15, [user_vars]
  mov [rsp0], rsp
  jmp quit

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

align 4096
code_space rb 32*1024

section '.idata' data readable

IMPORT \
kernel32, <\
  VirtualProtect,\
  SetConsoleTextAttribute,\
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
