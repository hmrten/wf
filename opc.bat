@echo off
fasm opc.asm && dumpbin /nologo /disasm opc.obj
