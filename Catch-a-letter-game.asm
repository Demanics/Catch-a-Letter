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



start:
    mov ax, 0x4c00
    int 0x21