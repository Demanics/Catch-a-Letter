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

score_board:
    push es
    push ax
    push bp
    push bx
    push cx
    push dx

    push es
    mov ax, 0xb800
    mov es, ax
    mov di, 120
    mov cx, 20
    mov dx, 25
board:
    mov word[es:di], 0x2020
    add di, 2
    loop board

    add di, 120
    mov cx, 20
    dec dx
    cmp dx, 0
    jnz board

    mov di, [board_location]
    sub di, 2
box:
    add di, 2
    mov word[es:di], 0x0DDC
    cmp di, [board_location+2]
    jne box

    pop es

	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 141
    mov cx, 16
    push cs
    pop es
    mov bp, Title1
    int 0x10

    mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 477
    mov cx, 13
    push cs
    pop es
    mov bp, Line1
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 733
    mov cx, 10
    push cs
    pop es
    mov bp, Line2
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 1245
    mov cx, 13
    push cs
    pop es
    mov bp, Line3
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 1501
    mov cx, 14
    push cs
    pop es
    mov bp, Line4
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 2013
    mov cx, 4
    push cs
    pop es
    mov bp, Line5
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 2269
    mov cx, 16
    push cs
    pop es
    mov bp, Line6
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 3037
    mov cx, 11
    push cs
    pop es
    mov bp, str1
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 3293
    mov cx, 6
    push cs
    pop es
    mov bp, str2
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 3549
    mov cx, 6
    push cs
    pop es
    mov bp, str6
    int 0x10
	
    push 0x3E00
	push 2374
    mov ax, [score]
    push ax
    call printnum
	
	push 0x3E00
	push 2534
    mov ax, [lives]
    push ax
    call printnum
	

    pop dx
    pop cx
    pop bx
    pop bp
    pop ax
    pop es
    ret
	
print_border:
	call clrscr
	pusha
	push 0xb800
	pop es
	mov ax, 0x2020  
	
	mov cx,60
	mov di,0
	top:
		stosw
		loop top
	
	mov cx,25
	sub di,2
	right:
		stosw
		add di,158
		loop right
	
	sub di,160
	mov cx,60
	bottom:
		stosw
		sub di,4
		loop bottom
	
	mov cx,25
	add di,2
	left:
		stosw
		sub di,162
		loop left;
	
	popa
	ret

kbisr:
    push ax
    push es

    mov ax, 0xb800
    mov es, ax

    in al, 0x60
    cmp al, 0x4B
    jne nextcmp

    call move_left
    jmp nomatch

nextcmp:
    cmp al, 0x4D
    jne nomatch
    call move_right

nomatch:
    pop es
    pop ax
    jmp far [cs:oldisr]

move_box:
    push cx
    push ax
    push es

    xor ax, ax
    mov es, ax
    mov ax, [es:9*4]
    mov [oldisr], ax
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax
    cli
    mov word [es:9*4], kbisr
    mov [es:9*4+2], cs
    sti

    call game_play
    mov ax, [oldisr]
    mov bx, [oldisr+2]
    cli
    mov [es:9*4],ax
    mov [es:9*4+2],bx
    sti

    mov ax, [old_int8]
    mov bx, [old_int8+2]
    cli
    mov [es:8*4],ax
    mov [es:8*4+2],bx
    sti

    pop es
    pop ax
    pop cx
    ret

; taking n as parameter, generate random number from 0 to n nad return in the stack
randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

  MOV     AH, 00h   ; interrupt to get system timer in CX:DX
  INT     1AH
  inc word [rand]
  mov     [randnum], dx
  jmp next1

  next:
  mov     ax, 25173          ; LCG Multiplier
  mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
  add     ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov     [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2

game_play:
    push ax
    push bx
    push cx
    push dx
    push di
    push es
    mov cx, 5
    push 0xb800
    pop es
    mov bx, 0

store_char:
    xor dx, dx
    xor di, di
    sub sp, 2
    push 25      
    call randG
    pop dx
    add dx, 0x41
    mov dh, 0x07
    mov [chars+bx], dx

    xor dx, dx
    sub sp, 2
    push 8
    call randG
	call more_difficult
    pop ax
    add ax, 1
    mov [timer+bx], ax
    call remove_redundancy

    sub sp, 2
    push 114
    call randG
    pop di
    test di, 1
    jz no_add
    sub di, 1

no_add:
	add di,162
    mov [pos+bx], di
    add bx, 2
    loop store_char

    mov bx, 0
    mov cx, 5
init_print:
    push word[chars+bx]
    push word[pos+bx]
    call print_char
    add bx, 2
    loop init_print
    xor ax, ax
    mov es, ax
    mov ax, [es:8*4]
    mov [old_int8], ax
    mov ax, [es:8*4+2]
    mov [old_int8+2], ax
    cli
    mov word [es:8*4], timeisr
    mov [es:8*4+2], cs
    sti
continue:
    cmp word[lives], 0
    jge continue
    
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

timeisr:
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    push 0xb800
    pop es

    mov cx, 5
    xor bx, bx
inc_timer:
    add word[speed+bx], 1
    add bx, 2
    loop inc_timer
    
    xor bx, bx
time_speed_cmp:
    mov cx, [speed+bx]
    cmp cx, [timer+bx]
    jne next_loop
fall_char:
    mov word[speed+bx], 0
    mov dx,[chars+bx]
    mov di,[pos+bx]
    mov word[es:di],0x0720
    add di, 160
    mov [pos+bx], di
    mov [es:di], dx
    cmp di, [board_location]
    jl not_catched
    cmp di, [board_location+2]
    jg not_catched
    mov word[es:di], 0x0DDC
    mov ax, [score]
    inc ax
    mov [score], ax
    push bx
    call re_intialize
    jmp next_loop
not_catched:
    cmp di, 3680
    jle next_loop
    mov word[es:di], 0x0720
    push bx
    call re_intialize
dec_lives:
    mov ax, [lives]
    dec ax
    mov [lives], ax
	call score_board
next_loop:
    add bx, 2
    cmp bx, 10
    jnz time_speed_cmp
endofloop:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    jmp far [cs:old_int8]

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

<<<<<<< HEAD
remove_redundancy:
    push dx
    push cx
    push bx
    push di
    push ax
    push si

    xor cx, cx               ; CX = outer loop counter
outer_loop:
    mov si, timer            ; Load base address of timer array
    add si, cx               ; SI = address of current outer element
    mov bx, si               ; BX = pointer to compare element
    add bx, 2                ; Start comparison from the next element
    mov dx, 5                ; Inner loop limit (hardcoded size)

inner_loop:
    cmp dx, 0                ; If no more elements to check, exit
    jz next_iter

    mov ax, [si]             ; AX = current outer element
    cmp ax, [bx]             ; Compare with inner element
    jne continue_inner       ; If not equal, continue inner loop

    ; Duplicate found, increment the duplicate element
    inc word [bx]

continue_inner:
    add bx, 2                ; Move to the next inner element
    dec dx                   ; Decrement inner loop counter
    jmp inner_loop

next_iter:
    add cx, 2                ; Move to the next outer element
    cmp cx, 10               ; Check if outer loop completed (5 elements × 2 bytes each)
    jl outer_loop

end_remove:
    pop si
    pop ax
    pop di
    pop bx
    pop cx
    pop dx
    ret

set_difficulty:
	push ax
	
	mov ah,0
	int 0x16
	mov word[difficulty],ax
	cmp al,'e'
	je easy_difficulty
	cmp al,'E'
	je easy_difficulty
	cmp al,'m'
	je mid_difficulty
	cmp al,'M'
	je mid_difficulty
	cmp al,'h'
	je hard_difficulty
	cmp al,'H'
	je hard_difficulty
	jmp easy_difficulty
	
hard_difficulty:	
	add word[board_location],10
	sub word[board_location+2],0
	sub word[lives],5

mid_difficulty:
	add word[board_location],0
	sub word[board_location+2],10
	
easy_difficulty:

	pop ax
	ret

more_difficult:
	push bp
	mov bp,sp
	push ax
	push dx
	mov dx,[bp+4]
	mov ax,difficulty
	cmp al,'m'
	je add_difficulty
	cmp al,'M'
	je add_difficulty
	cmp al,'h'
	je add_difficulty
	cmp al,'H'
	je add_difficulty
	jmp end_more_difficulty
add_difficulty:
	mov word[bp+4],1
end_more_difficulty:
	pop dx
	pop ax
	pop bp
	ret

=======
score_board:
    push es
    push ax
    push bp
    push bx
    push cx
    push dx

    push es
    mov ax, 0xb800
    mov es, ax
    mov di, 120
    mov cx, 20
    mov dx, 25
board:
    mov word[es:di], 0x2020
    add di, 2
    loop board

    add di, 120
    mov cx, 20
    dec dx
    cmp dx, 0
    jnz board

    mov di, [board_location]
    sub di, 2
box:
    add di, 2
    mov word[es:di], 0x0DDC
    cmp di, [board_location+2]
    jne box

    pop es

	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 141
    mov cx, 16
    push cs
    pop es
    mov bp, Title1
    int 0x10

    mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 477
    mov cx, 13
    push cs
    pop es
    mov bp, Line1
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 733
    mov cx, 10
    push cs
    pop es
    mov bp, Line2
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 1245
    mov cx, 13
    push cs
    pop es
    mov bp, Line3
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 1501
    mov cx, 14
    push cs
    pop es
    mov bp, Line4
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 2013
    mov cx, 4
    push cs
    pop es
    mov bp, Line5
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 2269
    mov cx, 16
    push cs
    pop es
    mov bp, Line6
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 3037
    mov cx, 11
    push cs
    pop es
    mov bp, str1
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 3293
    mov cx, 6
    push cs
    pop es
    mov bp, str2
    int 0x10
	
	mov ah, 0x13
    mov al, 1
    mov bh, 0
    mov bl, 0x3E
    mov dx, 3549
    mov cx, 6
    push cs
    pop es
    mov bp, str6
    int 0x10
	
    push 0x3E00
	push 2374
    mov ax, [score]
    push ax
    call printnum
	
	push 0x3E00
	push 2534
    mov ax, [lives]
    push ax
    call printnum
	

    pop dx
    pop cx
    pop bx
    pop bp
    pop ax
    pop es
    ret
	
print_border:
	call clrscr
	pusha
	push 0xb800
	pop es
	mov ax, 0x2020  
	
	mov cx,60
	mov di,0
	top:
		stosw
		loop top
	
	mov cx,25
	sub di,2
	right:
		stosw
		add di,158
		loop right
	
	sub di,160
	mov cx,60
	bottom:
		stosw
		sub di,4
		loop bottom
	
	mov cx,25
	add di,2
	left:
		stosw
		sub di,162
		loop left;
	
	popa
	ret


kbisr:
    push ax
    push es

    mov ax, 0xb800
    mov es, ax

    in al, 0x60
    cmp al, 0x4B
    jne nextcmp

    call move_left
    jmp nomatch

nextcmp:
    cmp al, 0x4D
    jne nomatch
    call move_right

nomatch:
    pop es
    pop ax
    jmp far [cs:oldisr]

move_box:
    push cx
    push ax
    push es

    xor ax, ax
    mov es, ax
    mov ax, [es:9*4]
    mov [oldisr], ax
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax
    cli
    mov word [es:9*4], kbisr
    mov [es:9*4+2], cs
    sti

    call game_play
    mov ax, [oldisr]
    mov bx, [oldisr+2]
    cli
    mov [es:9*4],ax
    mov [es:9*4+2],bx
    sti

    mov ax, [old_int8]
    mov bx, [old_int8+2]
    cli
    mov [es:8*4],ax
    mov [es:8*4+2],bx
    sti

    pop es
    pop ax
    pop cx
    ret

randG:
    push bp
    mov bp, sp
    pusha
    cmp word [rand], 0
    jne next

    MOV AH, 00h   ; interrupt to get system timer in CX:DX
    INT  1AH
    inc word [rand]
    mov     [randnum], dx
    jmp next1  

    next:
    mov     ax, 25173          ; LCG Multiplier
    mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
    add     ax, 13849          ; Add LCG increment value
    ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
    mov     [randnum], ax          ; Update seed = return value

    next1:
    xor dx, dx
    mov ax, [randnum]
    mov cx, [bp+4]
    inc cx
    div cx

    mov [bp+6], dx
    popa
    pop bp
    ret 2
>>>>>>> 6d72144ff84ed9ba039fc0f75ff9155ef507e3bd

start:
	call GenerateHomeScreen
	call set_difficulty
	call print_border
    call score_board
    call move_box
    call endscreen

    mov ax, 0x4c00
    int 0x21