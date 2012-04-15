$MOD186
NAME TIMER
; Main program for uPD70208 microcomputer system
;
; Author: 	Dr Tay Teng Tiow
; Address:     	Department of Electrical Engineering
;         	National University of Singapore
;		10, Kent Ridge Crescent
;		Singapore 0511.
; Date:   	6th September 1991
;
; This file contains proprietory information and cannot be copied
; or distributed without prior permission from the author.
; =========================================================================

public SERIAL_REC_ACTION, TIMER2_ACTION

INT_VEC_SEG	SEGMENT AT 0H
; Define the interrupt vector locations
; System reserved interrupts
ORG 0000H
	DIV_ZERO	DD	? ;not defined yet
	SINGLE_STEP	DD	? ;not defined yet
	NMI_INPUT	DD	START ;start of downloaded program
	BRK_3_VEC	DD	? ;not defined yet
	OVERFLOW	DD	? ;not defined yet
	ARRAY_BND	DD	? ;Array Bounds
ORG 020H
    TIMER0_VEC	DD	? ;route for timer 0
; Interrupt control unit
ORG 030H
	INTP0		DD	SERIAL_INTR
	INTP1		DD	? ;external, not used yet
	INTP2		DD	? ;external, not used yet
	INTP3		DD	? ;external, not used yet
    NUMERICS    DD	?
    RSVED       DD	? ;system reserved
    TIMER1_VEC  DD	? ;route for timer 1
    TIMER2_VEC  DD	TIMER2_INTR ;Timer2 Route
    ;Reserved from 050H to 080H
ORG 080H
;Interrupt Vector addrerss from 080h (type 32) to 3fCH (type 255)
;are avaiable for user software interrupt
; Software interrupts
	SOFT0		DD	? ;TYPE 32
	SOFT1		DD	? ;TYPE 33
	SOFT2		DD	?
	SOFT3		DD	?
	SOFT4		DD	?
	SOFT5		DD	?
	SOFT6		DD	?
	SOFT7		DD	?
INT_VEC_SEG	ENDS



INT_RAM_AREA SEGMENT
	QUEUE_LEN	EQU	128

	QUEUE_TRANS	DB	QUEUE_LEN DUP(?)
	QUEUE_HEAD	DW	0H
	QUEUE_TAIL	DW	0H
INT_RAM_AREA ENDS


$include(80188.inc)


MISC_ROUTINE SEGMENT
ASSUME CS:MISC_ROUTINE

UPDATE_DISPLAY PROC FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH DS

	MOV BX, DATA_SEG
	MOV DS, BX

	MOV BX, 06h
	MOV CX, 10
UPDATE_DISPLAY_LOOP:

	MOV AX, DS:DISPLAY_PASS

	XOR DX, DX
	DIV CX

	MOV DS:DISPLAY_PASS, AX

	MOV DS:LED_VALS[BX][-1], DL

	CMP DS:DISPLAY_PASS, 0
	JE LEADING_ZEROS_START

	DEC BX
	JNZ UPDATE_DISPLAY_LOOP

LEADING_ZEROS_START:

	CMP BX, 0
	JE UPDATE_DISPLAY_END
	DEC BX

LEADING_ZEROS:

	MOV DS:LED_VALS[BX][-1], LED_BLANK
	DEC BX
	JNZ LEADING_ZEROS

UPDATE_DISPLAY_END:

	POP DS
	POP DX
	POP CX
	POP BX
	POP AX
	RET
UPDATE_DISPLAY ENDP

UPDATE_CASH_DISPLAY PROC FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH DS

	MOV BX, DATA_SEG
	MOV DS, BX

	; Cents
	MOV BX, 6
	MOV CX, 10
UPDATE_CASH_DISPLAY_LOOP:

	MOV AX, DS:DISPLAY_PASS
	XOR DX, DX
	DIV CX

	MOV DS:DISPLAY_PASS, AX
	MOV DS:LED_VALS[BX][-1], DL

	DEC BX
	CMP BX, 5
	JAE UPDATE_CASH_DISPLAY_LOOP

	; Last dollar digit (BX=4)

	MOV AX, DS:DISPLAY_PASS
	XOR DX, DX
	DIV CX

	MOV DS:DISPLAY_PASS, AX
	OR DL, 10h
	MOV DS:LED_VALS[BX][-1], DL

	DEC BX

	CMP DS:DISPLAY_PASS, 0
	JE UPDATE_CASH_DISPLAY_END

UPDATE_CASH_DISPLAY_LOOP2:

	MOV AX, DS:DISPLAY_PASS
	XOR DX, DX
	DIV CX

	MOV DS:DISPLAY_PASS, AX
	MOV DS:LED_VALS[BX][-1], DL

	CMP DS:DISPLAY_PASS, 0
	JE LEADING_ZEROS_CASH_START

	DEC BX
	JNZ UPDATE_CASH_DISPLAY_LOOP2

LEADING_ZEROS_CASH_START:

	CMP BX, 0
	JE UPDATE_CASH_DISPLAY_END
	DEC BX

LEADING_ZEROS_CASH:

	MOV DS:LED_VALS[BX][-1], LED_BLANK
	DEC BX
	JNZ LEADING_ZEROS_CASH

UPDATE_CASH_DISPLAY_END:

	POP DS
	POP DX
	POP CX
	POP BX
	POP AX
	RET
UPDATE_CASH_DISPLAY ENDP

; ---This procdeure initialize the system I/O area and on-chip devices-----
IODEFINE PROC FAR
	PUSH AX
    PUSH DX

; Initialize SCU for operation
	MOV AL, SMD_DATA
	OUT SMD, AL
; Enable serial interrupts
	MOV AL, S_INT_ENA
	OUT SIER, AL
; =============== INITIALIZATION OF INTERRUPT CONTROL UNIT =============
; Initialize ICU for operation

; Mask all interrupts except SCU
    ;disable TX interrupt,ENABLE RX.
	MOV AL, 1
	OUT SIER, AL
; SCU use INT0, enable INT0
	MOV DX, INT0_CTRL
	XOR AX, AX
	OUT DX, AL
; Mask other Int
    CLI
    MOV DX, IMKW
	MOV AX, 0EEH
	OUT DX, AL
	POP DX
	POP AX
	RET
IODEFINE ENDP

; ----------------Start of procedure PRINT_2HEX ------------------------
PRINT_2HEX PROC FAR
; The byte to be printed as 2 HEX number is put into AL.
; This procedure is then called.
	CALL FAR PTR CHAR2HEX
; Result is return in AX
	PUSH AX
	MOV AL, AH
	CALL FAR PTR PRINT_CHAR
	POP AX
	CALL FAR PTR PRINT_CHAR
	RET
PRINT_2HEX ENDP

; ---------------- Start of procedure PRINT_CHAR ------------------------
PRINT_CHAR PROC FAR
; This procedure is called to put a character into queue for transmission
; through the serial port.
; The data to be transmitted is put in AL before the procedure is called.
; Data is put at the tail. Queue_tail is then inc to point to next loc.
; Data is taken from the head. Queue_head is then inc to point to next data.

	PUSH BX ;Save BX
	PUSH ES

	PUSH AX

	MOV BX, SEG QUEUE_TAIL ;Init segment register.
	MOV ES, BX ;ES now contains seg of INT_RAM_AREA

	IN AL, SIER ;disable TX interrupt.
	AND AL, 11111101B
	OUT SIER,AL

	POP AX
	MOV BX, ES:QUEUE_TAIL
	MOV ES:QUEUE_TRANS[BX], AL ;Put data to queue_tail.
	INC ES:QUEUE_TAIL ;Increment queue_tail
	CMP ES:QUEUE_TAIL, QUEUE_LEN ;and wrap around
	JL L_PRINT1 ;to zero if needed.
	MOV ES:QUEUE_TAIL,0
L_PRINT1:
	IN AL, SIER ;enable TX interrupt
	OR AL, 00000010B
	OUT SIER, AL

	POP ES
	POP BX
	RET
PRINT_CHAR ENDP

;------------------Start of Procedure CHAR2HEX ----------------------------
CHAR2HEX PROC FAR
; Char to be converted to HEX is put in AL before calling this procedure.
; HEX version is return in AX.
	MOV AH, AL
	AND AL, 00001111B
	CMP AL, 9
	JG GT9_1
	OR AL, 00110000B
	JMP DIGIT2
GT9_1:
	SUB AL, 9
	OR AL, 01000000B
DIGIT2:
	SHR AH, 4
	CMP AH, 9
	JG GT9_2
	OR AH, 00110000B
	JMP DONE
GT9_2:
	SUB AH,9
	OR AH, 01000000B
DONE:
	RET
CHAR2HEX ENDP

SET_TIMER2 PROC FAR
	PUSH AX
	PUSH DX

	;initialize timer2
	MOV AX, 0;
	MOV DX, T2_CNT;
	OUT DX, AL

	MOV AX, 180;
	MOV DX, T2_CA;
	OUT DX, AL

	MOV AX, 0E001H
	MOV DX, T2_CON
	OUT DX, AL

	MOV DX, TIMER_CTRL
	MOV AL, 01H
	OUT DX, AL
	POP DX
	POP AX

	RET
SET_TIMER2 ENDP
; ************************************************************************
;			INTERRUPT ROUTINES				 *
; ************************************************************************

; **************** Start of SERIAL_INTR service routine ******************

;*****************CAUTION*****************
;At the end of interrutp routines, you must write EOI (end of Int) +
;with the INT type (INT0-type 12) (timer-type 8) 		   +
;comment added by Zhu Shunyu	March,2000			   +
;Interrupt Routines Modified accordly to fit 80C188XL
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SERIAL_INTR:
	CLI

	PUSH AX
	PUSH BX
    PUSH DX

	IN AL, IIR ;read in serial INT ID
    AND AL, 00000111B
	CMP AL, 00000100B ;check if rx interrupt
	JE RECEIVE_INTR

	CMP AL, 00000010B ;check if tx interrupt
	JE TRANSMIT_INTR

;RESET_INT_CTL
    MOV DX, EOI
    MOV AX, 12
    OUT DX, AL

    POP DX
	POP BX			;false serial interrupt
	POP AX

	STI
	IRET				;return

RECEIVE_INTR:

	IN AL, SRB
; Information received will be used by user routine
; Action to be taken will be contained in SERIAL_REC_ACTION
	;CALL FAR PTR SERIAL_REC_ACTION

	MOV DX, EOI
    MOV AX, 12
    OUT DX, AL

	POP DX
	POP BX ;false serial interrupt
	POP AX
	STI
	IRET

TRANSMIT_INTR:

	PUSH ES
	MOV BX, SEG QUEUE_TAIL ;set ES to SERIAL_Q_SEG
	MOV ES, BX
	MOV BX, ES:QUEUE_TAIL
	CMP BX, ES:QUEUE_HEAD ;more data to be transmitted?
	JE L_TX2
	MOV BX, ES:QUEUE_HEAD ;get data from queue
	MOV AL, ES:QUEUE_TRANS[BX]
	OUT STB, AL ;tx data
	INC ES:QUEUE_HEAD ;point to next data
	CMP ES:QUEUE_HEAD, QUEUE_LEN ;wrap around if necessary
	JL L_TX1
	MOV ES:QUEUE_HEAD,0
L_TX1:
	MOV BX, ES:QUEUE_TAIL
	CMP BX, ES:QUEUE_HEAD ;more data to be transmitted?
	JNE L_TX3
L_TX2:
	IN AL, SIER ;no more, disable TX interrupt.
	AND AL, 11111101B
	OUT SIER,AL
L_TX3:

;RESET_INT_CTL
    MOV DX, EOI
    MOV AX, 12
    OUT DX, AL
	POP ES

    POP DX
    POP BX ;return serial interrupt
	POP AX

	STI
	IRET
; **************** End of SERIAL_INTR service routine ************************



; **************** Start of TIMER2_INTR service routine ******************
TIMER2_INTR:
	CLI
	PUSH AX
	PUSH DX

	; Action to be taken on timer0 interrupt to be written by user
	CALL FAR PTR TIMER2_ACTION

	;RESET_INT_CTL
	MOV DX, EOI
	MOV AX, 8
	OUT DX, AL

	POP DX
	POP AX
	STI
	IRET
; **************** End of TIMER2_INTR service routine ************************

MISC_ROUTINE ENDS


STACK_SEG SEGMENT
						DB	256 DUP(?)
	TOS					LABEL WORD
STACK_SEG ENDS

; --------------- Cash Register Begins --------------------

DATA_SEG SEGMENT

	HEXCHAR				DB	'0', '1', '2', '3', '4', '5', '6', '7'
						DB	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'

	LED_COUNTER_MAX		EQU	10h
	LED_COUNTER			DB	10h
	KEYPAD_COUNTER_MAX	EQU	40h
	KEYPAD_COUNTER		DW	3E8h

	NAME_APPLES			DB	'Apples'
	NAME_BATTERIES		DB	'Batteries'
	NAME_CANDIES		DB	'Candies'
	NAME_CARROTS		DB	'Carrots'
	NAME_COKE			DB	'Coke'
	NAME_EGGS			DB	'Eggs'
	NAME_MILK			DB	'Milk'
	NAME_MELONS			DB	'Melons'
	NAME_ORANGES		DB	'Oranges'
	NAME_PENS			DB	'Pens'

	; Setup: LEA each string to PRODUCT_NAMES[i]
	PRODUCT_NAMES		DW	OFFSET NAME_APPLES, OFFSET NAME_BATTERIES, OFFSET NAME_CANDIES
						DW	OFFSET NAME_CARROTS, OFFSET NAME_COKE, OFFSET NAME_EGGS
						DW	OFFSET NAME_MILK, OFFSET NAME_MELONS, OFFSET NAME_ORANGES
						DW	OFFSET NAME_PENS
	PRODUCT_NAME_LEN	DB	6, 9, 7, 7, 4, 4, 4, 6, 7, 4

	EVEN
	ITEM_PRICE			DW	50, 200, 100, 150, 40, 500, 350, 200, 100, 60000
	ITEM_INVENTORY		DB	255, 250, 200, 100, 50, 15, 22, 3, 150, 5 ;Update from serial
	SESSION_TALLY		DW	0

	TXN_QTYS			DB	10 DUP(0)
	TXN_TALLY			DW	0

	TXN_ITEM			DB	0
	TXN_QTY				DB	0
	TXN_STATUS			DB	0

	SOUND_ADDR			DD	0
	SOUND_REM			DW	0
	SOUND_BASE_ADDR		DD	0, 4865, 8896, 12895, 17440, 20714, 24677, 28691, 32870
						DD	36365, 40601, 44350, 48551, 53457, 59047, 64721, 70175, 76184
						DD 	82691, 87911, 93795, 98767, 103063, 108137, 112818, 117968, 123643
						DD	127749, 132966, 137436, 142619, 146419, 151159, 155061, 159908, 162770
						DD	166044, 170357, 173763, 175735, 180441, 184180, 189124, 194944, 200462
						DD	205222, 208373, 211085, 214730, 219582, 225264, 229360, 232980, 236735
						DD	240454, 244877, 248998, 253314
	SOUND_SIZE			DW	4864, 4031, 3999, 4545, 3274, 3963, 4014, 4179, 3495
						DW	4236, 3749, 4201, 4906, 5590, 5674, 5454, 6009, 6507
						DW	5220, 5884, 4972, 4296, 5074, 4681, 5150, 5675, 4106
						DW	5217, 4470, 5183, 3800, 4740, 3902, 4847, 2862, 3274
						DW	4313, 3406, 1972, 4706, 3739, 4944, 5820, 5518, 4760
						DW	3151, 2712, 3645, 4852, 5682, 4096, 3620, 3755, 3719
						DW	4423, 4121, 4316, 3283

	SOUND_TWENTY		EQU	20
	SOUND_THIRTY		EQU	21
	SOUND_FORTY			EQU	22
	SOUND_FIFTY			EQU	23
	SOUND_SIXTY			EQU	24
	SOUND_SEVENTY		EQU	25
	SOUND_EIGHTY		EQU	26
	SOUND_NINETY		EQU	27
	SOUND_HUNDRED		EQU	28
	SOUND_THOUSAND		EQU 29

	SOUND_DOLLAR		EQU	30
	SOUND_DOLLARS		EQU	31
	SOUND_CENT			EQU	32
	SOUND_CENTS			EQU	33

	SOUND_AND			EQU	34
	SOUND_FOR			EQU 35

	SOUND_PLEASE		EQU 36
	SOUND_PAY			EQU 37
	SOUND_IN			EQU 38
	SOUND_CHANGE		EQU 39
	SOUND_LEFT			EQU 40

	SOUND_APPLES		EQU 41
	SOUND_BATTERIES		EQU 42
	SOUND_CANDIES		EQU 43
	SOUND_CARROTS		EQU 44
	SOUND_COKE			EQU 45
	SOUND_EGGS			EQU 46
	SOUND_MILK			EQU 47
	SOUND_MELONS		EQU 48
	SOUND_ORANGES		EQU 49
	SOUND_PENS			EQU 50

	SOUND_QUEUE			DB	64 DUP(?)
	SOUND_QUEUE_HEAD	DB	0
	SOUND_QUEUE_TAIL	DB	0

	MODE_IDL			EQU	00h
	MODE_TXN			EQU	01h
	MODE_QRY			EQU	02h
	MODE_EOD			EQU 04h
	MODE_232			EQU 08h

	; TXN State Machine
	TXN_STATE			DB	0

	TXN_STATE_IDLE		EQU	00h
	TXN_STATE_HASH		EQU	01h
	TXN_STATE_STAR		EQU	02h

	PORTA_VAL			DB	0
	LED_CURRENT			DB	0
	LED_VALS			DB	6 DUP(?)

	; .GFEDCBA
	LED_LUT				DB	00111111B, 00000110B, 01011011B, 01001111B, 01100110B
						DB	01101101B, 01111101B, 00100111B, 01111111B, 01101111B
						DB	11110111B, 11111111B, 10111001B, 10111111B, 11111001B
						DB	11111001B
						DB	10111111B, 10000110B, 11011011B, 11001111B, 11100110B
						DB	11101101B, 11111101B, 10100111B, 11111111B, 11101111B
						DB	00000000B

	LED_BLANK			EQU	1Ah

	KEYR_PRI_SUSPECT	DB	0
	KEYR_PRI_READ		DB	0
	KEYR_SEC_SUSPECT	DB	0
	KEYR_SEC_READ		DB	0
	KEYPAD_QUEUE		DB	16 DUP(?)
	KEYPAD_QUEUE_HEAD	DB	0
	KEYPAD_QUEUE_TAIL	DB	0

	KEY_HASH			EQU	0Ch
	KEY_STAR			EQU	0Ah

	DISPLAY_PASS		DW	0

DATA_SEG ENDS

CODE_SEG SEGMENT

	PUBLIC START

ASSUME CS:CODE_SEG, SS:STACK_SEG

START:
;initialize stack area
	MOV AX, STACK_SEG
	MOV SS, AX
	LEA SP, TOS

; Initialize the on-chip pheripherals
	CALL FAR PTR IODEFINE

	IOCWR_VAL		EQU	0089h
	MPCS_VAL		EQU	2083h
	MMCSBA			EQU	0FFA6h
	MMCS_VAL		EQU	4004h

	AUDIO_EEP		EQU	4000h

	PORTA			EQU 0080h
	PORTB			EQU 0081h
	PORTC			EQU 0082h
	IOCWR			EQU 0083h

	DISPLAY_SELECT	EQU	100h
	DISPLAY_VAL		EQU	180h
	DAC_SELECT		EQU	200h

	MOV DX, IOCWR
	MOV AX, IOCWR_VAL
	OUT DX, AX

	MOV DX, MPCS
	MOV AX, MPCS_VAL
	OUT DX, AX

	MOV DX, MMCSBA
	MOV AX, MMCS_VAL
	OUT DX, AX

	MOV AX, AUDIO_EEP
	MOV ES, AX


; ^^^^^^^^^^^^^^^^^  Start of User Main Routine  ^^^^^^^^^^^^^^^^^^
	CALL SET_TIMER2
	STI

	MOV AX, DATA_SEG
	MOV DS, AX

	MOV DS:LED_VALS[0], 18h
	MOV DS:LED_VALS[1], 18h
	MOV DS:LED_VALS[2], 18h
	MOV DS:LED_VALS[3], 18h
	MOV DS:LED_VALS[4], 18h
	MOV DS:LED_VALS[5], 18h
	MOV DS:PORTA_VAL, 0FFh

	MOV DS:SOUND_QUEUE_HEAD, 0
	MOV DS:SOUND_QUEUE_TAIL, 0

	MOV DS:KEYPAD_QUEUE_HEAD, 0
	MOV DS:KEYPAD_QUEUE_TAIL, 0

	MOV DS:LED_VALS[0], LED_BLANK
	MOV DS:LED_VALS[1], LED_BLANK
	MOV DS:LED_VALS[2], LED_BLANK
	MOV DS:LED_VALS[3], LED_BLANK
	MOV DS:LED_VALS[4], LED_BLANK
	MOV DS:LED_VALS[5], LED_BLANK
	MOV DS:PORTA_VAL, 00h

	MOV AL, 3
	CALL NEAR PTR ADD_SOUND_QUEUE

	MOV AL, 2
	CALL NEAR PTR ADD_SOUND_QUEUE

	MOV AL, 0
	CALL NEAR PTR ADD_SOUND_QUEUE

	MOV AL, 8
	CALL NEAR PTR ADD_SOUND_QUEUE

	MOV AX, 2012
	CALL NEAR PTR ADD_SQ_NUM

NEXT:
	MOV AL, DS:PORTA_VAL
	AND AL, 0F0h
	OR AL, MODE_IDL
	MOV DS:PORTA_VAL, AL

QUEUE_RESET:
	MOV DS:KEYPAD_QUEUE_HEAD, 0
	MOV DS:KEYPAD_QUEUE_TAIL, 0
REPOLL:
	MOV AL, DS:KEYPAD_QUEUE_TAIL
	CMP AL, DS:KEYPAD_QUEUE_HEAD
	JAE QUEUE_RESET

	; Read keypad input
	XOR BH, BH
	MOV BL, DS:KEYPAD_QUEUE_TAIL
	MOV AL, DS:KEYPAD_QUEUE[BX]
	INC DS:KEYPAD_QUEUE_TAIL

	; BL = keypad ID, AL = value
	MOV BL, AL
	MOV CL, 4
	SHR BL, CL
	AND AL, 0Fh

	; Primary keypad
	CMP BL, 01h
	JNE SECONDARY_KEYPAD

	CMP AL, KEY_STAR
	JE DAILY_STATS

	JMP REPOLL

SECONDARY_KEYPAD:
	CMP BL, 02h
	JNE REPOLL ; keypad input error

	CMP AL, KEY_STAR
	JE INV_QUERY

	CMP AL, KEY_HASH
	JE TRANSACTION

	JMP REPOLL

TRANSACTION:
	MOV DS:PORTA_VAL, MODE_TXN
	CALL NEAR PTR MODE_TRANSACTION
	JMP NEXT

NEXT2FAR:
	JMP NEXT

INV_QUERY:
	MOV DS:PORTA_VAL, MODE_QRY
	CALL NEAR PTR MODE_QUERY
	JMP QUEUE_RESET

DAILY_STATS:
	MOV DS:PORTA_VAL, MODE_EOD
	CALL NEAR PTR MODE_STATISTICS
	JMP QUEUE_RESET

END_OF_DAY:
	MOV DS:PORTA_VAL, MODE_232
	JMP QUEUE_RESET

JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^

MODE_TRANSACTION PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX

	MOV AL, 'T'
	CALL FAR PTR PRINT_CHAR

	MOV DS:TXN_STATE, TXN_STATE_IDLE
	MOV DS:TXN_STATUS, 00h

	MOV DS:TXN_TALLY, 0
	MOV DS:TXN_ITEM, 0
	MOV DS:TXN_QTY, 0
	CALL NEAR PTR SHOW_TXNSTATE

M_TXN_POLLRST:
	; Poll keypad
	MOV DS:KEYPAD_QUEUE_HEAD, 0
	MOV DS:KEYPAD_QUEUE_TAIL, 0
M_TXN_POLL:
	MOV AL, DS:KEYPAD_QUEUE_TAIL
	CMP AL, DS:KEYPAD_QUEUE_HEAD
	JAE M_TXN_POLLRST

	; Read keypad input
	XOR BH, BH
	MOV BL, DS:KEYPAD_QUEUE_TAIL
	MOV AL, DS:KEYPAD_QUEUE[BX]
	INC DS:KEYPAD_QUEUE_TAIL

	; BL = keypad ID, AL = value
	MOV BL, AL
	MOV CL, 4
	SHR BL, CL
	AND AL, 0Fh

	MOV CL, DS:KEYPAD_QUEUE_HEAD
	CMP CL, DS:KEYPAD_QUEUE_TAIL
	JA M_TXN_NORST
	; Reset queue pointers
	MOV DS:KEYPAD_QUEUE_HEAD, 0
	MOV DS:KEYPAD_QUEUE_TAIL, 0
M_TXN_NORST:

	MOV CL, DS:TXN_STATE

	CMP CL, TXN_STATE_IDLE
	JE M_TXN_IDLE

	CMP CL, TXN_STATE_HASH
	JE M_TXN_HASH2FAR

	CMP CL, TXN_STATE_STAR
	JE M_TXN_STAR2FAR

	MOV DS:TXN_STATE, TXN_STATE_IDLE
	JMP M_TXN_POLL

M_TXN_POLL2FAR:
	JMP M_TXN_POLL

	; Idle state
M_TXN_IDLE:

	; Primary Keypad
	CMP BL, 01h
	JNE M_TXN_IDLE_NOTPRI

	CMP AL, KEY_HASH
	JE M_TXN_HAVEBOTH

	CMP AL, KEY_STAR
	JNE M_TXN_NODEL

	XOR AH, AH
	MOV AL, DS:TXN_QTY
	MOV BL, 10
	DIV BL

	MOV DS:TXN_QTY, AL
	CALL NEAR PTR SHOW_TXNSTATE

	JMP M_TXN_QTYBOOM

M_TXN_NODEL:
	; Multiply TXN_QTY by 10 and add new digit
	MOV BL, AL
	MOV AL, DS:TXN_QTY
	MOV AH, 10
	MUL AH

	; Hard cap at 255
	TEST AH, AH
	JNZ M_TXN_QTYBOOM

	ADD AL, BL

	; Cap at inventory limit
	XOR BH, BH
	MOV BL, DS:TXN_ITEM
	CMP AL, DS:ITEM_INVENTORY[BX]
	JA M_TXN_QTYBOOM

	MOV DS:TXN_QTY, AL
	CALL NEAR PTR SHOW_TXNSTATE

M_TXN_QTYBOOM:
	OR DS:TXN_STATUS, 01h
	OR DS:PORTA_VAL, 10h
	JMP M_TXN_POLL2FAR

M_TXN_IDLE_NOTPRI:
	; Secondary keypad
	CMP BL, 02h
	JNE M_TXN_POLL2FAR

	CMP AL, KEY_HASH
	JNE M_TXN_IDLE_NOTHASH

	MOV DS:TXN_STATE, TXN_STATE_HASH
	JMP M_TXN_POLL2FAR

M_TXN_HASH2FAR:
	JMP M_TXN_HASH3FAR

M_TXN_STAR2FAR:
	JMP M_TXN_STAR3FAR

M_TXN_IDLE_NOTHASH:

	CMP AL, KEY_STAR
	JNE M_TXN_IDLE_NOTSTAR

	MOV DS:TXN_STATE, TXN_STATE_STAR
	JMP M_TXN_POLL2FAR

M_TXN_IDLE_NOTSTAR:
	; Update item
	MOV DS:TXN_ITEM, AL
	CALL NEAR PTR SHOW_TXNSTATE

	OR DS:TXN_STATUS, 02h
	OR DS:PORTA_VAL, 20h
	JMP M_TXN_POLL2FAR

M_TXN_POLL3FAR:
	JMP M_TXN_POLL2FAR

M_TXN_HAVEBOTH:
	CMP DS:TXN_STATUS, 03h
	JNE M_TXN_POLL3FAR

	; Commit single item
	XOR BH, BH
	MOV BL, DS:TXN_ITEM
	MOV AL, DS:TXN_QTYS[BX]
	ADD AL, DS:TXN_QTY
	MOV DS:TXN_QTYS[BX], AL

	MOV AL, DS:TXN_QTY
	CALL NEAR PTR ADD_SOUND_QUEUE

	MOV DS:TXN_ITEM, 0
	MOV DS:TXN_QTY, 0

	MOV DS:TXN_STATUS, 0h
	AND DS:PORTA_VAL, 0CFh

	CALL NEAR PTR SHOW_SUBTTL

	MOV DS:TXN_STATE, TXN_STATE_IDLE
	JMP M_TXN_POLL2FAR

; State Hash
M_TXN_HASH3FAR:
	; Secondary Keypad
	CMP BL, 02h
	JNE M_TXN_POLL3FAR

	MOV DS:TXN_STATE, TXN_STATE_IDLE

	; Hash
	CMP AL, KEY_HASH
	JNE M_TXN_POLL3FAR

	; Double hash
	; Commit everything
	XOR BH, BH
	MOV BL, 10
M_TXN_NEXTPROD:
	MOV AL, DS:TXN_QTYS[BX][-1]
	SUB DS:ITEM_INVENTORY[BX][-1], AL
	MOV DS:TXN_QTYS[BX][-1], 0
	DEC BX
JNZ M_TXN_NEXTPROD

	CALL NEAR PTR SHOW_SUBTTL

	MOV AX, DS:TXN_TALLY
	ADD DS:SESSION_TALLY, AX

	JMP M_TXN_SKIP

; State Star
M_TXN_STAR3FAR:
	; Secondary Keypad
	CMP BL, 02h
	JNE M_TXN_POLL3FAR

	; Star
	CMP AL, KEY_STAR
	JNE M_TXN_DEL1

	; Double star
	XOR BH, BH
	MOV BL, 10
M_TXN_RMNEXTPROD:
	MOV DS:TXN_QTYS[BX][-1], 0
	DEC BX
	JNZ M_TXN_RMNEXTPROD

	MOV DS:TXN_STATE, TXN_STATE_IDLE
	JMP M_TXN_SKIP

M_TXN_DEL1:
	XOR BH, BH
	MOV BL, AL
	MOV DS:TXN_QTYS[BX], 0

	MOV DS:TXN_STATE, TXN_STATE_IDLE
	CALL NEAR PTR SHOW_SUBTTL
	JMP M_TXN_POLL3FAR

M_TXN_SKIP:
	POP CX
	POP BX
	POP AX
	RET
MODE_TRANSACTION ENDP

ADD_SOUND_QUEUE PROC NEAR
	PUSH BX

	XOR BH, BH
	MOV BL, DS:SOUND_QUEUE_HEAD
	MOV DS:SOUND_QUEUE[BX], AL
	INC DS:SOUND_QUEUE_HEAD

	POP BX
	RET
ADD_SOUND_QUEUE ENDP

ADD_SQ_NUM PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX

	XOR CX, CX

	CMP AX, 19
	JA SQ_ABOVE19

	CALL NEAR PTR ADD_SOUND_QUEUE
	JMP SQ_DONE

SQ_ABOVE19:
	; AX / 10
	XOR DX, DX
	MOV BX, 10
	DIV BX
	; Push remainder
	PUSH DX
	INC CX
	CMP AX, 0
	JA SQ_ABOVE19

SQ_PROCESS:
	POP AX

	CMP CX, 5
	JNE SQ_NOTTENTHOU

	CMP AL, 1
	JNE SQ_NOTTEENTHOU

	TEST AL, AL
	JZ SQ_LOOPDONE

	; AL * 10 + BL
	MOV BL, 10
	MUL BL
	POP BX
	DEC CX
	ADD AL, BL
	CALL NEAR PTR ADD_SOUND_QUEUE ;10-19
	MOV AL, SOUND_THOUSAND
	CALL NEAR PTR ADD_SOUND_QUEUE ;thousand
	JMP SQ_LOOPDONE
SQ_NOTTEENTHOU:

	SUB AL, 2
	ADD AL, SOUND_TWENTY
	CALL NEAR PTR ADD_SOUND_QUEUE ;20,30,40... (thousand)
	JMP SQ_LOOPDONE
SQ_NOTTENTHOU:

	CMP CX, 4
	JNE SQ_NOTTHOUSAND

	TEST AL, AL
	JZ SQ_LOOPDONE

	CALL NEAR PTR ADD_SOUND_QUEUE ;1-9
	MOV AL, SOUND_THOUSAND
	CALL NEAR PTR ADD_SOUND_QUEUE ;thousand
	JMP SQ_LOOPDONE
SQ_NOTTHOUSAND:

	CMP CX, 3
	JNE SQ_NOTHUNDRED

	TEST AL, AL
	JZ SQ_LOOPDONE

	CALL NEAR PTR ADD_SOUND_QUEUE ;1-9
	MOV AL, SOUND_HUNDRED
	CALL NEAR PTR ADD_SOUND_QUEUE ;hundred
	JMP SQ_LOOPDONE
SQ_NOTHUNDRED:

	CMP CX, 2
	JNE SQ_NOTTENS

	CMP AL, 1
	JNE SQ_NOTTEEN

	TEST AL, AL
	JZ SQ_LOOPDONE

	; AL * 10
	MOV BL, 10
	MUL BL
	; Pop 2nd digit
	POP BX
	DEC CX
	ADD AL, BL
	CALL NEAR PTR ADD_SOUND_QUEUE ;10-19
	JMP SQ_LOOPDONE
SQ_NOTTEEN:

	; (n-2), 2->20, 3->21, 4->22
	SUB AL, 2
	ADD AL, SOUND_TWENTY
	CALL NEAR PTR ADD_SOUND_QUEUE ;20,30,40,...
	JMP SQ_LOOPDONE
SQ_NOTTENS:
	CALL NEAR PTR ADD_SOUND_QUEUE ;1-9

SQ_LOOPDONE:
	DEC CX
	JNZ SQ_PROCESS

SQ_DONE:
	POP DX
	POP CX
	POP BX
	POP AX
	RET
ADD_SQ_NUM ENDP

SHOW_TXNSTATE PROC NEAR

	PUSH AX
	PUSH BX

	MOV BX, 6
M_TXN_QTYCLR:
	MOV DS:LED_VALS[BX][-1], LED_BLANK
	DEC BX
	JNZ M_TXN_QTYCLR

	XOR AH, AH
	MOV AL, DS:TXN_QTY
	MOV DS:DISPLAY_PASS, AX
	CALL FAR PTR UPDATE_DISPLAY

	MOV AL, DS:TXN_ITEM
	MOV DS:LED_VALS[0], AL

	POP BX
	POP AX
	RET
SHOW_TXNSTATE ENDP

SHOW_SUBTTL PROC NEAR
	PUSH AX
	PUSH BX
	PUSH DX
	PUSH SI

	MOV DS:TXN_TALLY, 0

	MOV BX, 10
SUBTTL_LOOP:
	XOR AH, AH
	MOV AL, DS:TXN_QTYS[BX][-1]

	; DW LUTs require 2xIndex
	MOV SI, BX
	DEC SI
	ADD SI, SI
	MUL DS:ITEM_PRICE[SI]

	ADD DS:TXN_TALLY, AX

	DEC BX
	JNZ SUBTTL_LOOP

	MOV AX, DS:TXN_TALLY

	MOV DS:DISPLAY_PASS, AX
	CALL FAR PTR UPDATE_CASH_DISPLAY

	POP SI
	POP DX
	POP BX
	POP AX
	RET
SHOW_SUBTTL ENDP

MODE_QUERY PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX

	MOV AL, 'Q'
	CALL FAR PTR PRINT_CHAR

M_QRY_POLL:
	MOV AL, DS:KEYPAD_QUEUE_TAIL
	CMP AL, DS:KEYPAD_QUEUE_HEAD
	JAE M_QRY_POLL

	XOR BH, BH
	MOV BL, DS:KEYPAD_QUEUE_TAIL
	MOV AL, DS:KEYPAD_QUEUE[BX]
	INC DS:KEYPAD_QUEUE_TAIL

	;BL = KEYPAD ID, AL = VALUE
	MOV BL, AL
	MOV CL, 4
	SHR BL, CL
	AND AL, 0Fh

	CMP BL, 02h
	JNE M_QRY_POLL

	XOR BH, BH
	MOV BL, AL
	XOR AH, AH
	MOV AL, DS:ITEM_INVENTORY[BX][-1]

	MOV DS:DISPLAY_PASS, AX
	CALL NEAR PTR ADD_SQ_NUM

	; PRODUCT_NAMES FROM 41-50
	MOV AL, BL
	ADD AL, 40
	CALL NEAR PTR ADD_SOUND_QUEUE

	MOV AL, DS:PORTA_VAL
	AND AL, 00Fh
	MOV CL, 4
	SHL BL, CL
	OR AL, BL
	MOV DS:PORTA_VAL, AL

	CALL FAR PTR UPDATE_DISPLAY

	POP CX
	POP BX
	POP AX
	RET
MODE_QUERY ENDP

MODE_STATISTICS PROC NEAR
	PUSH AX

	MOV AL, 'S'
	CALL FAR PTR PRINT_CHAR

	MOV AX, DS:SESSION_TALLY

	MOV DS:DISPLAY_PASS, AX

	CALL FAR PTR UPDATE_CASH_DISPLAY
	CALL NEAR PTR ADD_SQ_CASH

	POP AX
	RET
MODE_STATISTICS ENDP

SERIAL_REC_ACTION PROC FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DS

	MOV BX, DATA_SEG		;initialize data segment register
	MOV DS, BX

	CMP AL, 'L'
	JE SER_LT

	JMP SER_RET

SER_LT:
	MOV AL, DS:PORTA_VAL
	MOV DS:PORTA_VAL, 0FFh
	MOV CX, 10h
SER_LT_PA:
	NOP
	LOOPNZ SER_LT_PA
	MOV DS:PORTA_VAL, AL

	MOV BX, 6
SER_LT_SSEG:
	MOV AL, DS:LED_VALS[BX][-1]
	MOV DS:LED_VALS[BX][-1], 18h

	MOV CX, 10h
SER_LT_SSEG_DEL:
	NOP
	LOOPNZ SER_LT_SSEG_DEL

	MOV DS:LED_VALS[BX][-1], AL

	DEC BX
	JNZ SER_LT_SSEG

;	CMP	AL,'<'
;	JNE	S_FAST

;	INC	DS:T_COUNT_SET
;	INC	DS:T_COUNT_SET

;	JMP	S_NEXT0
;S_FAST:
;	CMP	AL,'>'
;	JNE	S_RET

;	DEC	DS:T_COUNT_SET
;	DEC	DS:T_COUNT_SET

;S_NEXT0:
;	MOV	CX,22			;initialize counter for message
;	MOV	BX,0

;S_NEXT1:	MOV	AL,DS:REC_MESS[BX]	;print message
;	call	FAR ptr print_char
;	INC	BX
;	LOOP	S_NEXT1

;	MOV	AL,DS:T_COUNT_SET	;print current period of timer0
;	CALL	FAR PTR PRINT_2HEX
SER_RET:
	POP	DS
	POP	CX
	POP	BX
	POP AX
	RET
SERIAL_REC_ACTION	ENDP

TIMER2_ACTION	PROC	FAR
	PUSH AX
	PUSH DS

	; Restore DS
	MOV	AX,DATA_SEG
	MOV	DS,AX

	; Check if head == tail
	MOV AH, DS:SOUND_QUEUE_HEAD
	MOV AL, DS:SOUND_QUEUE_TAIL
	CMP AH, AL
	JE NO_SOUND

	; Speech synthesis
	CALL NEAR PTR SPEECH_SYNTH

NO_SOUND:

	; Counter to limit LED refresh rate
	DEC DS:LED_COUNTER
	JNZ LED_DONE

	; LED output
	CALL NEAR PTR DISPLAY_HANDLER

	MOV DS:LED_COUNTER, LED_COUNTER_MAX

LED_DONE:

	; Counter to limit keypad refresh rate
	DEC DS:KEYPAD_COUNTER
	JNZ NO_KEYPAD

	CMP DS:KEYR_PRI_READ, 0
	JNE K1_DONE

	; Keypad 1 input
	CALL NEAR PTR KEYPAD1_READER

K1_DONE:

	CMP DS:KEYR_SEC_READ, 0
	JNE K2_DONE

	; Keypad 2 input
	CALL NEAR PTR KEYPAD2_READER

K2_DONE:
	MOV DS:KEYPAD_COUNTER, KEYPAD_COUNTER_MAX

NO_KEYPAD:

	POP DS
	POP AX
	RET
TIMER2_ACTION	ENDP

SPEECH_SYNTH PROC NEAR
	PUSH AX
	PUSH BX
	PUSH DX
	PUSH SI
	PUSH DI

	CMP DS:SOUND_REM, 0
	JA PENDING_SOUND

	; Prep next clip
	XOR BH, BH
	MOV BL, DS:SOUND_QUEUE_TAIL
	MOV BL, DS:SOUND_QUEUE[BX]
	MOV SI, OFFSET SOUND_BASE_ADDR

	SHL BX, 2
	MOV AX, WORD PTR [BX][SI]

	MOV WORD PTR DS:SOUND_ADDR[0], AX

	MOV AX, WORD PTR 2[BX][SI]
	MOV WORD PTR DS:SOUND_ADDR[2], AX

	SHR BX, 1
	MOV SI, OFFSET SOUND_SIZE
	MOV AX, WORD PTR [BX][SI]

	; Set remaining sound samples
	MOV DS:SOUND_REM, AX
	; Increment tail
	; INC DS:SOUND_QUEUE_TAIL

PENDING_SOUND:

	MOV SI, WORD PTR DS:SOUND_ADDR[0]
	MOV DI, WORD PTR DS:SOUND_ADDR[2]

	ADD DI, 4h
	SHL DI, 12

	; Set DS to audio EEP segment
	MOV DS, DI

	MOV AL, DS:[SI]
	MOV DX, DAC_SELECT
	OUT DX, AL

	; Restore DS
	MOV AX, DATA_SEG
	MOV DS, AX

	; Increment current addr
	INC WORD PTR DS:SOUND_ADDR[0]
	JNZ SOUND_DONE

	INC WORD PTR DS:SOUND_ADDR[2]

SOUND_DONE:
	; decrement remaining samples
	DEC DS:SOUND_REM
	JNZ SOUND_REMAINS

	INC DS:SOUND_QUEUE_TAIL

	; Reset queue pointers if need be
	MOV AL, DS:SOUND_QUEUE_TAIL
	CMP AL, DS:SOUND_QUEUE_HEAD
	JB SOUND_REMAINS
	MOV DS:SOUND_QUEUE_TAIL, 0
	MOV DS:SOUND_QUEUE_HEAD, 0

SOUND_REMAINS:

	POP DI
	POP SI
	POP DX
	POP BX
	POP AX
	RET
SPEECH_SYNTH ENDP

DISPLAY_HANDLER PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH DS

	MOV AX, DATA_SEG
	MOV DS, AX

	; Simple LEDs
	MOV AL, DS:PORTA_VAL
	NOT AL
	MOV DX, PORTA
	OUT DX, AL

	; Output bit pattern for desired digit
	XOR BH, BH
	MOV BL, DS:LED_CURRENT
	MOV BL, DS:LED_VALS[BX]
	MOV AL, DS:LED_LUT[BX]
	MOV DX, DISPLAY_VAL
	OUT DX, AL

	; Select display
	MOV AL, 01h
	MOV CL, DS:LED_CURRENT
	SHL AL, CL

	MOV DX, DISPLAY_SELECT
	NOT AL
	OUT DX, AL

	; Switch segments
	INC DS:LED_CURRENT
	CMP DS:LED_CURRENT, 06h
	JB LED_CUR_SKIP

	MOV DS:LED_CURRENT, 00h

LED_CUR_SKIP:
	POP DS
	POP DX
	POP CX
	POP BX
	POP AX
	RET
DISPLAY_HANDLER ENDP

KEYPAD1_READER PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH DS

	MOV AX, DATA_SEG
	MOV DS, AX

KR1_COL1:

	MOV DX, PORTB
	MOV AL, 01h
	OUT DX, AL

	; BL = COL
	MOV BL, 01h

	; AL = ROW
	MOV DX, PORTC
	IN AL, DX

	; Clear fist 4 bits
	AND AL, 0Fh

	CMP AL, 00h
	JNE KR1_SUSPECT

KR1_COL2:

	MOV DX, PORTB
	MOV AL, 02h
	OUT DX, AL

	; BL = COL
	MOV BL, 02h

	; AL = ROW
	MOV DX, PORTC
	IN AL, DX

	; Clear fist 4 bits
	AND AL, 0Fh

	CMP AL, 00h
	JNE KR1_SUSPECT

KR1_COL3:

	MOV DX, PORTB
	MOV AL, 04h
	OUT DX, AL

	; BL = COL
	MOV BL, 03h

	; AL = ROW
	MOV DX, PORTC
	IN AL, DX

	; Clear fist 4 bits
	AND AL, 0Fh

	CMP AL, 00h
	JNE KR1_SUSPECT

	CMP DS:KEYR_PRI_SUSPECT, 00h
	JE KR1_DONE

	; Enqueue

	XOR BH, BH
	MOV BL, DS:KEYPAD_QUEUE_HEAD
	MOV AL, DS:KEYR_PRI_SUSPECT
	CMP AL, 0Bh
	JNE KR1_NOTZERO

	XOR AL, AL

KR1_NOTZERO:

	; Keypad ID
	OR AL, 10h

	MOV DS:KEYPAD_QUEUE[BX], AL
	INC DS:KEYPAD_QUEUE_HEAD

	MOV DS:KEYR_PRI_SUSPECT, 0

	JMP KR1_DONE

KR1_SUSPECT:


	MOV CL, 00h

KR1_ROWLOOP:

	INC CL
	SHR AL, 1
	JNZ KR1_ROWLOOP

	; VAL = 3*(row-1) + col
	MOV BH, CL
	MOV AL, 3
	DEC BH
	MUL BH

	ADD AL, BL

	MOV DS:KEYR_PRI_SUSPECT, AL

KR1_DONE:
	POP DS
	POP DX
	POP CX
	POP BX
	POP AX
	RET
KEYPAD1_READER ENDP

KEYPAD2_READER PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH DS

	MOV AX, DATA_SEG
	MOV DS, AX

KR2_COL1:

	MOV DX, PORTB
	MOV AL, 10h
	OUT DX, AL

	; BL = COL
	MOV BL, 01h

	; AL = ROW
	MOV DX, PORTC
	IN AL, DX

	; Clear fist 4 bits
	AND AL, 0Fh

	CMP AL, 00h
	JNE KR2_SUSPECT

KR2_COL2:

	MOV DX, PORTB
	MOV AL, 20h
	OUT DX, AL

	; BL = COL
	MOV BL, 02h

	; AL = ROW
	MOV DX, PORTC
	IN AL, DX

	; Clear fist 4 bits
	AND AL, 0Fh

	CMP AL, 00h
	JNE KR2_SUSPECT

KR2_COL3:

	MOV DX, PORTB
	MOV AL, 40h
	OUT DX, AL

	; BL = COL
	MOV BL, 03h

	; AL = ROW
	MOV DX, PORTC
	IN AL, DX

	; Clear fist 4 bits
	AND AL, 0Fh

	CMP AL, 00h
	JNE KR2_SUSPECT

	CMP DS:KEYR_SEC_SUSPECT, 00h
	JE KR2_DONE

	; Enqueue

	XOR BH, BH
	MOV BL, DS:KEYPAD_QUEUE_HEAD
	MOV AL, DS:KEYR_SEC_SUSPECT
	CMP AL, 0Bh
	JNE KR2_NOTZERO

	XOR AL, AL

KR2_NOTZERO:

	; Keypad ID
	OR AL, 20h

	MOV DS:KEYPAD_QUEUE[BX], AL
	INC DS:KEYPAD_QUEUE_HEAD

	MOV DS:KEYR_SEC_SUSPECT, 0

	JMP KR2_DONE

KR2_SUSPECT:

	MOV CL, 00h

KR2_ROWLOOP:

	INC CL
	SHR AL, 1
	JNZ KR2_ROWLOOP

	; VAL = 3*(row-1) + col
	MOV BH, CL
	MOV AL, 3
	DEC BH
	MUL BH

	ADD AL, BL

	MOV DS:KEYR_SEC_SUSPECT, AL

KR2_DONE:
	POP DS
	POP DX
	POP CX
	POP BX
	POP AX
	RET
KEYPAD2_READER ENDP

CODE_SEG	ENDS
END
