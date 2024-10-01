default rel
bits 64

NEWLINE db 10,0

section .text

strlen:
    ; rdi : string
    mov rax, rdi
.strlen_loop:
    inc rax
    cmp byte [rax-1], 0
    jne .strlen_loop
    sub rax, rdi
    dec rax
    ret

print:
    ; rdi : string
    push rsi  ; save registers
    push rdx

    call strlen  ; (rdi) -> rax

    ; write (fd, buf, count)
    mov rdx, rax  ; count
    mov rsi, rdi  ; buf
    mov rdi, 1    ; stdout
    mov rax, 1    ; write
    syscall

    pop rdx  ; restore registers
    pop rsi
    ret

println:
    ; rdi : string
    call print
    mov rdi, NEWLINE
    call print
    ret
    
iprint:
    ; rdi : n
    push rsp      ; save registers

    mov rcx, 10   ; constant for division
    mov r8, rsp   ; save the bottom of the stack
    mov rsi, rsp  ; use rsi to move on the stack
    sub rsp, 3*8  ; allocate space on rsp

    mov rax, rdi  ; prepare for division

.iprint_loop:
    xor rdx, rdx  ; zero the upper 64 bits
    dec rsi
    div rcx       ; get remainder into rdx
    add rdx, '0'  ; convert to char
    mov [rsi], dl
    cmp rax, 0    ; exit condition
    jne .iprint_loop


    ; write (fd, buf, count)
    mov rdx, r8  ; count
    sub rdx, rsi
    mov rdi, 1   ; stdout
    mov rax, 1   ; write
    syscall

    add rsp, 3*8  ; restore space on rsp
    pop rsp       ; restore register
    ret
    
iprintln:
    ; rdi : string
    call iprint
    mov rdi, NEWLINE
    call print
    ret

exit:
    mov rax, 60
    xor rdi, rdi
    syscall
    ret

