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

public	serial_rec_action, timer2_action
extrn	print_char:far, print_2hex:far, iodefine:far
extrn   set_timer2:far

STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS


DATA_SEG SEGMENT

	TIMER0_MESS			DB	10,13,'TIMER2 INTERRUPT    '
	T_COUNT				DB	2FH
	T_COUNT_SET			DB	2FH
	REC_MESS			DB	10,13,'Period of timer2 =     '

	HEXCHAR				DB	'0', '1', '2', '3', '4', '5', '6', '7'
						DB	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'

	
	LED_COUNTER			DB	20h
	KEYPAD_COUNTER		DW	3E8h
	
	NAME_EGGS			DB	'Eggs', 13, 10
	NAME_MILK			DB	'Milk', 13, 10
	NAME_APPLES			DB	'Apples', 13, 10
	NAME_COKE			DB	'Coke', 13, 10
	NAME_ORANGES		DB	'Oranges', 13, 10
	NAME_BATTERIES		DB	'Batteries', 13, 10
	NAME_CANDIES		DB	'Candies', 13, 10
	NAME_MELONS			DB	'Melons', 13, 10
	NAME_CARROTS		DB	'Carrots', 13, 10
	NAME_PENS			DB	'Pens', 13, 10

	PRODUCT_NAMES		DB	10 DUP(?)
	; Setup: LEA each string to PRODUCT_NAMES[i]

	ITEM_PRICE			DW	10 DUP(01010h) ;Default val hardcoded, update from serial
	ITEM_INVENTORY		DD	10 DUP(0FFh) ;Update from serial
	SESSION_TALLY		DW	0

	TXN_QTY				DB	10 DUP(0)
	TXN_TALLY			DW	0

	SOUND_ADDR			DD	0
	SOUND_REM			DW	0
	SOUND_BASE_ADDR		DD	51 DUP(0) ;LUT to be filled in when base addr available
	SOUND_SIZE			DW	51 DUP(0FFFFh) ;LUT to be filled in

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

	SOUND_PROD_BASE		EQU 41

	SOUND_QUEUE			DB	32 DUP(?)
	SOUND_QUEUE_HEAD	DW	0
	SOUND_QUEUE_TAIL	DW	0

	PORTA_VAL			DB	0
	LED_CURRENT			DB	0
	LED_VALS			DB	6 DUP(?)
	; .GFEDCBA	
	LED_LUT				DB	00111111B, 00000110B, 01011011B, 01001111B, 01100110B
						DB	01101101B, 01111101B, 00100111B, 01111111B, 01101111B

	KEYR_PRI_VAL		DB	0
	KEYR_PRI_READ		DB	0
	KEYR_SEC_VAL		DB	0
	KEYR_SEC_READ		DB	0

DATA_SEG ENDS


CODE_SEG	SEGMENT

	PUBLIC		START

ASSUME	CS:CODE_SEG, SS:STACK_SEG

START:
;initialize stack area
	MOV	AX,STACK_SEG		
	MOV	SS,AX
	MOV	SP,TOS

; Initialize the on-chip pheripherals
	CALL	FAR PTR	IODEFINE

	IOCWR_VAL		EQU	0081h
	MPCS			EQU	0FFA8h
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
	call set_timer2
	STI
	
	MOV AX, DATA_SEG
	MOV DS, AX
	
	MOV DS:LED_VALS[0], 1h
	MOV DS:LED_VALS[1], 2h
	MOV DS:LED_VALS[2], 3h
	MOV DS:LED_VALS[3], 4h
	MOV DS:LED_VALS[4], 5h
	MOV DS:LED_VALS[5], 6h
	
	;MOV BX, 00h

NEXT:

;	MOV BX, 0
;	MOV DS:SOUND_QUEUE[BX], 0
;	INC DS:SOUND_QUEUE_HEAD
;
;	MOV CX, 0FFFFh
;	DERP:
;	LOOP DERP

;	XOR CH, CH
;	MOV CL, DS:LED_VALS[BX]
;	MOV SI, CX
;	MOV AL, DS:LED_LUT[SI]
;	MOV DX, DISPLAY_VAL
;	OUT DX, AL
;	
;	MOV DX, DISPLAY_SELECT
;	MOV AL, 01h
;	MOV CL, BL
;	SHL AL, CL
;	NOT AL
;	OUT DX, AL
;	
;	MOV CX, 100h
;	DERP:
;	LOOP DERP
;	
;	INC BX
;	CMP BX, 06h
;	JBE HERP
;	
;	MOV BX, 00h
;
;	HERP:

JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^


SERIAL_REC_ACTION	PROC	FAR
		PUSH	CX
		PUSH 	BX
		PUSH	DS

		MOV	BX,DATA_SEG		;initialize data segment register
		MOV	DS,BX

;		CMP	AL,'<'
;		JNE	S_FAST

;		INC	DS:T_COUNT_SET
;		INC	DS:T_COUNT_SET

;		JMP	S_NEXT0
;S_FAST:
;		CMP	AL,'>'
;		JNE	S_RET

;		DEC	DS:T_COUNT_SET
;		DEC	DS:T_COUNT_SET

;S_NEXT0:
;		MOV	CX,22			;initialize counter for message
;		MOV	BX,0

;S_NEXT1:	MOV	AL,DS:REC_MESS[BX]	;print message
;		call	FAR ptr print_char
;		INC	BX
;		LOOP	S_NEXT1

;		MOV	AL,DS:T_COUNT_SET	;print current period of timer0
;		CALL	FAR PTR PRINT_2HEX
;S_RET:
		POP	DS
		POP	BX
		POP	CX
		RET
SERIAL_REC_ACTION	ENDP



TIMER2_ACTION	PROC	FAR
	PUSHA

	CLI
	
	MOV	AX,DATA_SEG
	MOV	DS,AX

	MOV SI, WORD PTR DS:SOUND_QUEUE_HEAD
	MOV DI, WORD PTR DS:SOUND_QUEUE_TAIL
	CMP SI, DI
	JE NO_SOUND
	
	;CALL FAR PTR SPEECH_SYNTH

	JMP HAS_SOUND

NO_SOUND:
	MOV DS:SOUND_QUEUE_TAIL, 0
	MOV DS:SOUND_QUEUE_HEAD, 0

HAS_SOUND:

	DEC DS:LED_COUNTER
	JNZ LED_DONE

	CALL FAR PTR DISPLAY_HANDLER

	MOV DS:LED_COUNTER, 20h

LED_DONE:

	DEC DS:KEYPAD_COUNTER
	JNZ NO_KEYPAD

	CMP DS:KEYR_PRI_READ, 0
	JNZ K1_DONE

	;CALL FAR PTR KEYPAD1_READER

K1_DONE:
	
	CMP DS:KEYR_SEC_READ, 0
	JNZ K2_DONE

	;CALL FAR PTR KEYPAD2_READER

K2_DONE:
	MOV DS:KEYPAD_COUNTER, 3E8h

NO_KEYPAD:

	STI
	
	POPA
	RET
TIMER2_ACTION	ENDP

SPEECH_SYNTH PROC FAR
	PUSHA
	
	MOV AX, DATA_SEG
	MOV DS, AX

	CMP DS:SOUND_REM, 0
	JNE PENDING_SOUND

	; Prep next clip
	MOV BX, DS:SOUND_QUEUE_TAIL
	XOR BH, BH
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
	MOV DS:SOUND_REM, AX
	INC DS:SOUND_QUEUE_TAIL

PENDING_SOUND:
	DEC DS:SOUND_REM
	
	MOV SI, WORD PTR DS:SOUND_ADDR[0]
	MOV DI, WORD PTR DS:SOUND_ADDR[2]
	
	ADD DI, 4h
	SHL DI, 12
	
	MOV DS, DI
	MOV AX, DS
	
	MOV AL, BYTE PTR [SI]
	MOV DX, DAC_SELECT
	OUT DX, AL
	
	MOV AX, DATA_SEG
	MOV DS, AX
	
	INC WORD PTR DS:SOUND_ADDR[0]
	JNZ SOUND_NOT_DONE
	
	INC WORD PTR DS:SOUND_ADDR[2]

	JNZ SOUND_NOT_DONE

	INC DS:SOUND_QUEUE_TAIL

SOUND_NOT_DONE:

	POPA
	RET
SPEECH_SYNTH ENDP

DISPLAY_HANDLER PROC FAR
	PUSHA
	
	MOV AX, DATA_SEG
	MOV DS, AX
	
	MOV AL, 01h
	MOV CL, DS:LED_CURRENT
	SHL AL, CL
	
	MOV DX, DISPLAY_SELECT
	NOT AL
	OUT DX, AL
	
	XOR BH, BH
	MOV BL, DS:LED_CURRENT
	MOV BL, DS:LED_VALS[BX]
	MOV AL, DS:LED_LUT[BX]
	MOV DX, DISPLAY_VAL
	OUT DX, AL
	
	INC DS:LED_CURRENT
	CMP DS:LED_CURRENT, 06h
	JBE LED_CUR_SKIP
	
	MOV DS:LED_CURRENT, 00h
	
LED_CUR_SKIP:
	POPA
DISPLAY_HANDLER ENDP


CODE_SEG	ENDS
END
