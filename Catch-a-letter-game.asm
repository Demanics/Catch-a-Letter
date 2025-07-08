[org 0x0100]

jmp start

Title1:     db 'Alphabet Catcher', 16
Line1:      db 'Developed by: ',13
Line2:      db 'Saad & Ali', 10
Line3:      db 'Submitted to: ' ,13
Line4:      db 'Salman Mubarak', 14
Line5:      db 'COAL ',4
Line6:      db 'Semester Project', 16
Prompt:     db 'Press any key to start...', 0
line7:      db 'Game Over', 0
line8:      db 'Congratulations', 0
line9:      db 'Thanks for playing', 0
line10:     db 'Your score is', 0
str1:       db 'SCORE BOARD'
str2:       db 'SCORE: ', 0
str3: db 'Alphabet Catcher:', 0
str4: db 'Catch Them All', 0
str5: db 'Press e for easy, m for medium, h for hard', 0
str6: db 'LIVES: ',6
score:      dw 0
over:       db 'Game Over'
lives:      dw 10
oldisr:     dd 0
rand:       dw 0
randnum:    dw 0
chars:      dw 0, 0, 0, 0, 0
pos:        dw 0,0,0,0,0
speed:      dw 0,0,0,0,0
timer:      dw 0,0,0,0,0
old_int8:   dd 0
clock:      dw 0
difficulty: dw 0
board_location:     dw 3728, 3766

delay:
    push cx
    mov cx, 0xffff
sleep:
    loop sleep
    pop cx
    ret

printstr:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di

    push ds
    pop es
    mov di, [bp+4]
    mov cx, 0xffff
    xor al, al
    repne scasb
    mov ax, 0xffff
    sub ax, cx
    dec ax
    jz exit

    mov cx, ax
    mov ax, 0xb800
    mov es, ax
    mov al, 80
    mul byte [bp+8]
    add ax, [bp+10]
    shl ax, 1
    mov di,ax
    mov si, [bp+4]
    mov ah, [bp+6]
    cld

nextchar:
    call delay
    call delay
    call delay
    lodsb
    stosw
    loop nextchar
exit:
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 8

effects:
    push di
    push es
    push bx
    push cx
    push dx
    push ax

    mov cx, 15
    push 0xb800
    pop es

char_effect:
    xor di, di
    xor dx, dx

    sub sp, 2
    push 7    
    call randG
    pop ax
    inc ax

    sub sp, 2
    push 25      
    call randG
    pop dx
    add dx, 0x41
    mov dh, al

    sub sp, 2
    push 1460
    call randG
    pop di
    test di, 1
    jz no_sub
    sub di, 1

no_sub:
    mov [es:di], dx
    loop char_effect

    pop ax
    pop dx
    pop cx
    pop bx
    pop es
    pop di
    ret

GenerateHomeScreen:
	call clrscr
	call effects
    push ax

    mov ax, 27
    push ax
    mov ax, 10
    push ax
    mov ax, 12
    push ax
    mov ax, str3
    push ax
    call printstr

    mov ax, 34
    push ax
    mov ax, 12
    push ax
    mov ax, 12
    push ax
    mov ax, str4
    push ax
    call printstr

    mov ax, 22
    push ax
    mov ax, 16
    push ax
    mov ax, 141
    push ax
    mov ax, str5
    push ax
    call printstr

    pop ax
    ret

start:
    mov ax, 0x4c00
    int 0x21