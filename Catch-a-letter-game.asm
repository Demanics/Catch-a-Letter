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


move_left:
    push ax
    push cx
    push es
    push di
    push 0xb800
    pop es
    mov ax, [board_location+2]
    mov di, [board_location]
    cmp di, 3682
    jle no_change_left
    sub di, 2
box_left_space:
    add di, 2
    mov word[es:di],0x0720
    cmp di, ax
    jne box_left_space
    mov di, [board_location]
    sub di, 2
    sub ax, 2
    mov [board_location], di
    mov [board_location+2], ax
    mov di, [board_location]
    sub di, 2
box_left_move:
    add di, 2
    mov word[es:di], 0x0DDC
    cmp di, ax
    jne box_left_move
no_change_left:
    pop di
    pop es
    pop cx
    pop ax
    ret

move_right:
    push ax
    push cx
    push es
    push di
    push 0xb800
    pop es
    mov ax, [board_location+2]
    mov di, [board_location]
    cmp ax, 3796
    jge no_change_right
    sub di, 2
box_right_space:
    add di, 2
    mov word[es:di],0x0720
    cmp di, ax
    jne box_right_space
    mov di, [board_location]
    add di, 2
    add ax, 2
    mov [board_location], di
    mov [board_location+2], ax
    mov di, [board_location]
    sub di, 2
box_right_move:
    add di, 2
    mov word[es:di], 0x0DDC
    cmp di, ax
    jne box_right_move
no_change_right:
    pop di
    pop es
    pop cx
    pop ax
    ret

clrscr:
    push es
    push ax
    push cx
    push di
    mov ax, 0xb800
    mov es, ax
    xor di, di

    mov ax, 0x0720
    mov cx, 2000
    cld
    rep stosw
    pop di
    pop cx
    pop ax
    pop es
    ret

print_char:
    push bp
    mov bp, sp
    push di
    push es
    push dx

    push 0xb800
    pop es
    mov di, [bp+4]
    mov dx, [bp+6]
    mov [es:di], dx

    pop dx
    pop es
    pop di
    pop bp
    ret 4

printnum:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xb800
    mov es, ax
    mov ax, [bp+4]
    mov bx, 10
    mov cx, 0
nextdigit:
    mov dx, 0
    div bx
    add dl, 0x30
    push dx
    inc cx
    cmp ax, 0
    jnz nextdigit
    mov di, [bp+6]
nextpos:
    pop dx
    mov ax, [bp+8]
    mov dh, ah 
    mov [es:di], dx
    add di, 2
    loop nextpos

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 6

re_intialize:
    push bp
    mov bp, sp
    push ax
    push bx
    push di

    mov bx, [bp+4]
    sub sp, 2
    push 25      
    call randG
    pop ax
    add ax, 0x41
    mov ah, 0100b
    mov [chars+bx], ax

    xor ax, ax
    sub sp, 2
    push 8
    call randG
	call more_difficult
    pop ax
    add ax, 1
    mov [timer+bx], ax
    ;call remove_redundancy

    sub sp, 2
    push 114
    call randG
    pop di
    test di, 1
    jz no_addition
    sub di, 1
no_addition:
	add di,162
    mov [pos+bx], di
    push word[chars+bx]
    push word[pos+bx]
    call print_char

    pop di
    pop bx
    pop ax
    pop bp
    ret 2

endscreen:
	call clrscr
	call effects
    mov ax, 30
    push ax
    mov ax, 10
    push ax
    mov ax, 12
    push ax
    mov ax, line7
    push ax
    call printstr

    mov ax, 27
    push ax
    mov ax, 12
    push ax
    mov ax, 12
    push ax
    mov ax, line8
    push ax
    call printstr

    mov ax, 28
    push ax
    mov ax, 14
    push ax
    mov ax, 12
    push ax
    mov ax, line10
    push ax
    call printstr

    push 0x0C00
    push 2324
    push word[score]
    call printnum

    mov ax, 26
    push ax
    mov ax, 18
    push ax
    mov ax, 3
    push ax
    mov ax, line9
    push ax
    call printstr

    ret

start:
    mov ax, 0x4c00
    int 0x21