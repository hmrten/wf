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
sbuf_i     dd 0

user_vars:
USERVAR 'fd'   , fd, forth_space+forth_names.size
USERVAR 'md'   , md, macro_space+macro_names.size
USERVAR 'hd'   , hd, fd
USERVAR '#tib' , tib_n, 0
USERVAR '>in'  , tib_i, 0
USERVAR 'base' , base, 16
USERVAR 'xhere', xhere, code_space
USERVAR 'here' , here, data_space

; == UNINITIALIZED DATA

align 8
stdin       dq ?
stdout      dq ?
conin       dq ?
rsp0        dq ?

align 4096
forth_space rb 32*1024
macro_space rb 32*1024

BUFSIZE equ 256
tib         rb BUFSIZE
tob         rb BUFSIZE
nob         rb BUFSIZE
sbuf        rb 8*BUFSIZE

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

; ACCEPT ( adr u1 -- u2 )
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

  cmp qword [conin], 0
  jne .echo

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

.leave:
  WINLEAVE
  NIP
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
  WINLEAVE
  push rax
  call type
  pop rax
  DUP
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

; EMIT ( c -- )
FORTHCODE 'emit', emit
  mov dl, al
.0:
  lea rax, [tob]
  mov byte [rax], dl
  DUP
  mov eax, 1

; TYPE ( adr u -- )
FORTHCODE 'type', type
  WINENTER $30
  mov rcx, [stdout]
  mov rdx, [rbx]
  mov r8d, eax
  xor r9, r9
  mov [rsp+$20], r9
  call [WriteFile]
  WINLEAVE
  DROP 2
  ret

; H. ( n -- )
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

; . ( n -- )
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

; COLOR ( u -- )
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
FORTHCODE 'white', white
  mov edx, $0F
  jmp color.set

; CHAR ( parse: next character -- c )
FORTHCODE 'char', char
  call word_
  cmp eax, 1
  jne abort.notfnd
  mov rsi, [rbx]
  movzx eax, byte [rsi]
  NIP
  ret
MACROCODE '[char]', mchar
  call char
  jmp lit

; PARSE ( c -- adr u )
FORTHCODE 'parse', parse
  lea rsi, [tib]
  mov edi, [tib_n]
  mov ecx, [tib_i]
  lea rbx, [rbx-8]
  lea r8, [rsi+rcx]
  movzx edx, al
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

; WORD ( -- adr u )
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

; NUMBER ( adr u -- n )
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
  NIP
  ret

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
  DROP 2
  ret

; allot ( n -- )
; allocate n bytes of data space if n > 0, otherwise deallocate n bytes
FORTHCODE 'allot', allot
  add qword [here], rax
  DROP
  ret

; , ( n -- )
; store and allocate space for a number in data space
FORTHCODE ',', comma
  mov ecx, 8
@@:
  mov rdi, [xhere]
  mov [rdi], rax
  add rdi, rcx
  mov [xhere], rdi
  DROP
  ret
FORTHCODE 'l,', lcomma
  mov ecx, 4
  jmp @b
FORTHCODE 'w,', wcomma
  mov ecx, 2
  jmp @b
FORTHCODE 'b,', bcomma
  mov ecx, 1
  jmp @b

; ,x ( n -- )
; store and allocate space for a number in code space
FORTHCODE ',x', comma_x
  mov ecx, 8
@@:
  mov rdi, [xhere]
  mov [rdi], rax
  add rdi, rcx
  mov [xhere], rdi
.ret:
  DROP
  ret
FORTHCODE 'l,x', lcomma_x
  mov ecx, 4
  jmp @b
FORTHCODE 'w,x', wcomma_x
  mov ecx, 2
  jmp @b
FORTHCODE 'b,x', bcomma_x
  mov ecx, 1
  jmp @b

; -q,x ( n -- )
; patch last qword/dword/byte of code
FORTHCODE '-q,x', qpatch
  mov rdi, [xhere]
  mov [rdi-8], rax
  jmp comma_x.ret
FORTHCODE '-l,x', lpatch
  mov rdi, [xhere]
  mov [rdi-4], eax
  jmp comma_x.ret
FORTHCODE '-b,x', bpatch
  mov rdi, [xhere]
  mov [rdi-1], al
  jmp comma_x.ret

; #,x ( n -- )
; try to compile a signed imm8 or imm32 value
FORTHCODE '#,x', numcomma_x
  mov rdi, [xhere]
  movsx rdx, al
  cmp rdx, rax
  jne .l
  mov byte [rdi], al
  inc rdi
.imm32:
.ret:
  mov [xhere], rdi
  DROP
  ret
.l:
  movsxd rdx, eax
  cmp rdx, rax
  jne abort.imm32
  mov dword [rdi], eax
  add rdi, 4
  jmp .ret

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
  lea rdx, [compile]
  mov qword [action], rdx
  ret

FORTHCODE 'bye', bye
  WINENTER $20
  xor ecx, ecx
  jmp [ExitProcess]

FORTHCODE '\', backslash
MACROCODE '\', mbackslash
  DUP
  mov al, $0A
  call parse
  DROP 2
  ret

; ' ( -- xt )
; parse word and lookup its xt in forth dict
FORTHCODE "'", tick
  call word_
  call find
  jne mtick.err
.ret:
  DUP
  mov rax, [rsi]
  ret
; '' ( -- xt )
; parse word and lookup its xt in macro dict
FORTHCODE "''", mtick
  call word_
  call mfind
  je tick.ret
.err:
  DROP 2
  jmp abort.notfnd

; z" ( -- adr )
; parse a string delimited by " and push its address on the stack
FORTHCODE 'z"', zquote
  DUP
  mov al, '"'
  call parse
  mov edx, [sbuf_i]
  inc dword [sbuf_i]
  and edx, 7
  shl edx, 8
  lea rdi, [sbuf+rdx]
  push rdi
  mov rsi, [rbx]
  mov ecx, eax
  rep movsb
  pop rax
  NIP
  ret

; DLL-LOAD ( zstr -- )
FORTHCODE 'dll-load', dll_load
  WINENTER $20
  mov rcx, rax
  call [LoadLibraryA]
  WINLEAVE
  push rax
  call header
  pop rax
  call lit
  jmp exit

; DLL-PROC ( mod cnt zstr -- )
FORTHCODE 'dll-proc', dll_proc
  call header
  mov rdi, [xhere]
  ret

; DLL-CALL ( args.. dll "proc" -- )
FORTHCODE 'dll-call', dll_call
  WINENTER $20
  mov rcx, [rbx]
  mov rdx, rax
  call [GetProcAddress]
  mov r9,  [rbx+$08]
  mov r8,  [rbx+$10]
  mov rdx, [rbx+$18]
  mov rcx, [rbx+$20]
  call rax
  WINLEAVE
  DROP 6
  ret

; [ ( switch to interpreter )
MACROCODE '[', lbrack
  lea rdx, [interpret]
  mov qword [action], rdx
  ret

; ; ( end current definition )
MACROCODE ';', semi
  call lbrack
MACROCODE ';;', exit
  mov rdi, [xhere]
  mov byte [rdi], $C3
  inc qword [xhere]
  ret

MACROCODE 'then', then
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
  call bcomma_x
  dec r12
  jnz .loop
  pop r12
.docopy:
  ret

; z" ( -- adr )
; parse a string delimited by " and compile code to push its address
; on the data stack
MACROCODE 'z"', mzquote
  DUP
  mov al, '"'
  call parse
  mov rdi, [xhere]
  lea rdx, [.pushstr]
  add rdi, 5
  sub rdx, rdi
  mov byte  [rdi-5], $E8
  mov dword [rdi-4], edx
  mov byte [rdi], al
  inc rdi
  mov ecx, eax
  mov rsi, [rbx]
  rep movsb
  mov [xhere], rdi
  DROP 2
  ret
.pushstr:
  pop rdi
  movzx edx, byte [rdi]
  lea rdx, [rdi+rdx+1]
  push rdx
  DUP
  lea rax, [rdi+1]
  ret

MACROCODE 'for', for
; 0000: 41 54              push        r12
; 0002: 49 89 C4           mov         r12,rax
; 0005: 48 8B 03           mov         rax,qword ptr [rbx]
; 0008: 48 8D 5B 08        lea         rbx,[rbx+8]
  DUP
  mov rax, [xhere]
  mov rdx, $038B48C489495441
  mov ecx, $085B8D48
  mov [rax+0], rdx
  mov [rax+8], ecx
  add rax, 12
  mov [xhere], rax
  ret

MACROCODE 'next', next
; 0000: 49 FF CC           dec         r12
; 0003: 75 00              jne         00
; 0003: 0F 85 00 00 00 00  jne         00
; 0005: 41 5C              pop         r12
  mov rdi, [xhere]
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
  mov [xhere], rdi
  DROP
  ret

MACROCODE 'i', i
  call mdup
  mov dword [rdi], $E0894C
  add rdi, 3
  mov [xhere], rdi
  ret

MACROCODE 'dup', mdup
  mov rdi, [xhere]
  mov dword [rdi+$00], $F85B8D48 ; lea rbx, [rbx-8]
  mov dword [rdi+$04], $038948   ; mov [rbx], rax
  add rdi, 7
  mov [xhere], rdi
  ret

; [m]find ( adr u -- adr u | -- )
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
  DROP 2
  ret

interpret:
  call find
  jne .num
  mov rdi, [rsi]
  lea rdx, [data_end]
  cmp rdi, rdx
  jb .var
  jmp rdi
.var:
  DUP
  mov rax, [rdi]
  ret
.num:
  call number
  jne abort.notfnd
  ret

compile:
  call mfind
  jne @f
  jmp qword [rsi]
@@:
  call find
  jne lit0
  DUP
  mov rax, [rsi]

; call, ( xt -- )
FORTHCODE 'call,', call_comma
  mov rdi, [xhere]
  mov byte [rdi], $E8 ; call rel32
  inc rdi
  mov [xhere], rdi
  jmp @f
; rel32, ( adr -- )
FORTHCODE 'rel32,', rel32_comma
  mov rdi, [xhere]
@@:
  add rdi, 4
  sub rax, rdi
  mov dword [rdi-4], eax
  mov [xhere], rdi
  DROP
  ret

lit0:
  call number
  jne abort.notfnd
; lit ( n -- )
MACROCODE 'lit', lit
  call mdup
  mov word  [rdi+$00], $B848     ; mov rax, 0
  mov qword [rdi+$02], rax
  add rdi, 10
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

; TODO: clean this up
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
  xor eax, eax
  lea rdi, [tob]
  push rdi
  lea rsi, [strings.abort.error]
  mov ecx, strings.abort.error.size + strings.abort.notfnd.size
  add eax, ecx
  rep movsb
  lea rsi, [lname]
  movzx ecx, byte [lname_n]
  add eax, ecx
  rep movsb
  pop qword [rbx]
  jmp .print
; ( n -- )
.imm32:
  lea rbx, [rbx-40] ; allocate 5 args
  mov [rbx+32], rax
  lea rdx, [strings.abort.error]
  mov [rbx], rdx
  mov eax, strings.abort.error.size
  call white
  call type
  mov rax, [rbx+16]
  call dot
  lea rdx, [strings.abort.imm32]
  mov [rbx], rdx
  mov eax, strings.abort.imm32.size
  jmp @f
.print:
  call white
@@:
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

  WINENTER $40
  mov ecx, -10
  call [GetStdHandle]
  mov [stdin], rax

  mov rcx, rax
  call [GetFileType]
  cmp al, 2
  je @f
  lea rcx, [strings.conin]
  mov edx, $80000000     ; GENERIC_READ
  mov r8d, 1             ; FILE_SHARE_READ
  xor r9, r9
  mov dword [rsp+$20], 3 ; OPEN_EXISTING
  mov dword [rsp+$28], r9d
  mov qword [rsp+$30], r9
  call [CreateFileA]
  mov [conin], rax
@@:
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

align 4096
code_space rb 32*1024

section '.rdata' data readable

IMPORT \
kernel32, <\
  GetProcAddress,\
  LoadLibraryA,\
  VirtualProtect,\
  SetConsoleTextAttribute,\
  GetConsoleScreenBufferInfo,\
  SetConsoleCursorPosition,\
  SetFilePointer,\
  CreateFileA,\
  ReadFile,\
  WriteFile,\
  GetStdHandle,\
  GetFileType,\
  ExitProcess>

macro STRING name, s {
  .#name: db s
  .#name#.size = $ - .#name
}


strings:
  STRING conin, <'CONIN$', 0>
  STRING abort.error, 'ERROR: '
  STRING abort.notfnd, 'undefined word: '
  STRING abort.imm32, 'cannot be encoded as an imm32 operand'

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
