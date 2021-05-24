; minimal x86-64 Linux ELF executable
bits 64
            org 0x08048000

ehdr:                                           ; Elf64_Ehdr
            db  0x7F, "ELF", 2, 1, 1, 0         ;   e_ident
    times 8 db  0
            dw  2                               ;   e_type
            dw  62                              ;   e_machine
            dd  1                               ;   e_version
            dq  _start                          ;   e_entry
            dq  phdr - $$                       ;   e_phoff
            dq  0                               ;   e_shoff
            dd  0                               ;   e_flags
            dw  ehdrsize                        ;   e_ehsize
            dw  phdrsize                        ;   e_phentsize
            dw  1                               ;   e_phnum
            dw  0                               ;   e_shentsize
            dw  0                               ;   e_shnum
            dw  0                               ;   e_shstrndx

ehdrsize    equ $ - ehdr

phdr:                                           ; Elf64_Phdr
            dd  1                               ;   p_type
            dd  5                               ;   p_flags
            dq  0                               ;   p_offset
            dq  $$                              ;   p_vaddr
            dq  $$                              ;   p_paddr
            dq  filesize                        ;   p_filesz
            dq  filesize                        ;   p_memsz
            dq  0x1000                          ;   p_align

phdrsize    equ     $ - phdr

_start:
   call main
   mov edi, eax
   xor eax, eax
   mov al, 60
   syscall
foo:
    push rbp
    mov rbp, rsp
    mov eax, 0x5
    pop rbp
    ret
main:
    push rbp
    mov rbp, rsp
    sub rsp, 0x10
    call foo
    add eax, 0x1
    mov [rbp - 0x4], eax
    mov eax, [rbp - 0x4]
    mov [rbp + 0x8], eax
    call foo
    mov ecx, eax
    mov eax, [rbp + 0x8]
    add eax, ecx
    add rsp, 0x10
    pop rbp
    ret

filesize      equ     $ - $$
