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

INT_VEC_SEG	SEGMENT		AT 	0H
; Define the interrupt vector locations
; System reserved interrupts
		ORG	0000H
	DIV_ZERO	DD	?	;not defined yet
	SINGLE_STEP	DD	?	;not defined yet
	NMI_INPUT	DD	?	;start of downloaded program
	BRK_3_VEC	DD	?	;not defined yet
	OVERFLOW	DD	?	;not defined yet
	ARRAY_BND	DD	?	;Array Bounds
                ORG     020H
    TIMER0_VEC      DD      ? ;route for timer 0
; Interrupt control unit
		ORG	030H
	INTP0		DD	SERIAL_INTR
	INTP1		DD	?       ;external, not used yet
	INTP2		DD	?	;external, not used yet
	INTP3		DD	?	;external, not used yet
    NUMERICS    DD      ?       ;
    RSVED       DD      ?       ;system reserved
    TIMER1_VEC  DD      ?       ;route for timer 1
    TIMER2_VEC  DD      TIMER2_INTR       ;Timer2 Route
    ;Reserved from 050H to 080H
	            ORG     080H
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



INT_RAM_AREA	SEGMENT
	QUEUE_LEN	EQU	128

	QUEUE_TRANS	DB	QUEUE_LEN DUP(?)
	QUEUE_HEAD	DW	0H
	QUEUE_TAIL	DW	0H
INT_RAM_AREA	ENDS


$include(80188.inc)


MISC_ROUTINE	SEGMENT
ASSUME CS:MISC_ROUTINE

; ---This procdeure initialize the system I/O area and on-chip devices-----
IODEFINE	PROC	FAR
		PUSH	AX
        PUSH	DX

; Initialize SCU for operation
		MOV	AL,SMD_DATA
		OUT	SMD,AL
; Enable serial interrupts
		MOV	AL,S_INT_ENA
		OUT	SIER,AL
; =============== INITIALIZATION OF INTERRUPT CONTROL UNIT =============
; Initialize ICU for operation

; Mask all interrupts except SCU
                ;disable TX interrupt,ENABLE RX.
		MOV	AL,1
		OUT	SIER,AL
; SCU use INT0, enable INT0
	        MOV     DX, INT0_CTRL
  		XOR	AX,AX
         	OUT	DX,AL
; Mask other Int
                CLI
                MOV	DX,IMKW
		MOV	AX,0EEH
		OUT 	DX,AL
		POP	DX
		POP	AX
		RET
IODEFINE	ENDP


; ----------------Start of procedure PRINT_2HEX ------------------------
PRINT_2HEX 	PROC	FAR
	QUE_BASE	EQU	OFFSET QUEUE_TRANS
; The byte to be printed as 2 HEX number is put into AL.
; This procedure is then called.
		CALL	FAR PTR CHAR2HEX
; Result is return in AX
		PUSH 	AX
		MOV	AL,AH
		CALL	FAR PTR PRINT_CHAR
		POP	AX
		CALL	FAR PTR PRINT_CHAR
		RET
PRINT_2HEX	ENDP




; ---------------- Start of procedure PRINT_CHAR ------------------------
PRINT_CHAR	PROC	FAR
; This procedure is called to put a character into queue for transmission
; through the serial port.
; The data to be transmitted is put in AL before the procedure is called.
; Data is put at the tail. Queue_tail is then inc to point to next loc.
; Data is taken from the head. Queue_head is then inc to point to next data.

		PUSH	BX			;Save BX
		PUSH	ES

		PUSH	AX

		MOV	BX,SEG QUEUE_TAIL	;Init segment register.
		MOV	ES,BX			;ES now contains seg of INT_RAM_AREA

		IN	AL,SIER 		;disable TX interrupt.
		AND	AL,11111101B
		OUT	SIER,AL

		POP	AX
		MOV	BX,ES:QUEUE_TAIL
		MOV	ES:QUE_BASE[BX],AL	;Put data to queue_tail.
		INC	ES:QUEUE_TAIL		;Increment queue_tail
		CMP	ES:QUEUE_TAIL,QUEUE_LEN	;and wrap around
		JL	L_PRINT1		;to zero if needed.
		MOV	ES:QUEUE_TAIL,0
L_PRINT1:
		IN	AL,SIER			;enable TX interrupt
		OR	AL,00000010B
		OUT	SIER,AL

		POP	ES
		POP	BX
		RET
PRINT_CHAR	ENDP




;------------------Start of Procedure CHAR2HEX ----------------------------
CHAR2HEX	PROC	FAR
; Char to be converted to HEX is put in AL before calling this procedure.
; HEX version is return in AX.
		MOV	AH,AL
		AND	AL,00001111B
		CMP	AL,9
		JG	GT9_1
		OR	AL,00110000B
		JMP 	DIGIT2
GT9_1:		SUB	AL,9
		OR	AL,01000000B
DIGIT2:
		SHR	AH,4
		CMP	AH,9
		JG	GT9_2
		OR	AH,00110000B
		JMP	DONE
GT9_2:		SUB	AH,9
		OR	AH,01000000B
DONE:
		RET
CHAR2HEX	ENDP

Set_timer2      proc Far
	push ax
	push dx
	;Initialize Timer2
	mov ax, 0;
	mov dx, T2_CNT;
	OUT DX, AL

	MOV AX, 180;
	MOV DX, T2_CA;
	OUT DX, AL

	MOV AX,0E001H
	MOV DX, T2_CON
	OUT DX, AL

	MOV DX, TIMER_CTRL
	MOV AL, 01H
	OUT DX, AL
	pop dx
	pop ax

ret
Set_timer2 endp
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
		PUSH	AX			;save registers
		PUSH	BX
        PUSH    DX




		IN	AL,IIR			;read in serial INT ID
        AND AL,00000111B
		CMP AL,00000100B		;check if rx interrupt
		JE	RECEIVE_INTR

		CMP AL,00000010B		;check if tx interrupt
		JE	TRANSMIT_INTR


;RESET_INT_CTL
        MOV DX, EOI
        MOV AX, 12
        OUT DX, AL

        POP     DX
		POP	BX			;false serial interrupt
		POP	AX
		IRET				;return

RECEIVE_INTR:

		IN	AL,SRB
; Information received will be used by user routine
; Action to be taken will be contained in SERIAL_REC_ACTION
		CALL	FAR PTR SERIAL_REC_ACTION

		MOV DX, EOI
        MOV AX, 12
        OUT DX, AL
		POP     DX
		POP	BX			;false serial interrupt
		POP	AX
		IRET

TRANSMIT_INTR:

		PUSH	ES			;save ES
		MOV	BX,SEG QUEUE_TAIL	;set ES to SERIAL_Q_SEG
		MOV	ES,BX
		MOV	BX,ES:QUEUE_TAIL
		CMP	BX,ES:QUEUE_HEAD	;more data to be transmitted?
		JE	L_TX2
		MOV	BX,ES:QUEUE_HEAD	;get data from queue
		MOV	AL,ES:QUE_BASE[BX]
		OUT	STB,AL			;tx data
		INC	ES:QUEUE_HEAD		;point to next data
		CMP	ES:QUEUE_HEAD,QUEUE_LEN ;wrap around if necessary
		JL	L_TX1
		MOV	ES:QUEUE_HEAD,0
L_TX1:
		MOV	BX,ES:QUEUE_TAIL
		CMP	BX,ES:QUEUE_HEAD	;more data to be transmitted?
		JNE	L_TX3
L_TX2:
		IN	AL,SIER			;no more, disable TX interrupt.
		AND    	AL,11111101B
		OUT	SIER,AL
L_TX3:

;RESET_INT_CTL
	    MOV DX, EOI
	    MOV AX, 12
	    OUT DX, AL
  		POP	ES			;restore original ES(transmit)

        POP     DX
        POP	BX			;return serial interrupt
		POP	AX
		IRET
; **************** End of SERIAL_INTR service routine ************************



; **************** Start of TIMER0_INTR service routine ******************
TIMER2_INTR:
		PUSH 	AX

; Action to be taken on timer0 interrupt to be written by user
		CALL	FAR PTR TIMER2_ACTION

		POP	AX		;return interrupt
        ;RESET_INT_CTL
        MOV DX, EOI
        MOV AX, 8
        OUT DX, AL
		IRET
; **************** End of TIMER2_INTR service routine ************************

MISC_ROUTINE	ENDS


STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS

; --------------- Cash Register Begins --------------------

DATA_SEG SEGMENT

	TIMER0_MESS			DB	10,13,'TIMER2 INTERRUPT    '
	T_COUNT				DB	2FH
	T_COUNT_SET			DB	2FH
	REC_MESS			DB	10,13,'Period of timer2 =     '

	HEXCHAR				DB	'0', '1', '2', '3', '4', '5', '6', '7'
						DB	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'


	LED_COUNTER			DB	10h
	KEYPAD_COUNTER		DW	3E8h

	NAME_APPLES			DB	'Apples', 13, 10
	NAME_BATTERIES		DB	'Batteries', 13, 10
	NAME_CANDIES		DB	'Candies', 13, 10
	NAME_CARROTS		DB	'Carrots', 13, 10
	NAME_COKE			DB	'Coke', 13, 10
	NAME_EGGS			DB	'Eggs', 13, 10
	NAME_MILK			DB	'Milk', 13, 10
	NAME_MELONS			DB	'Melons', 13, 10
	NAME_ORANGES		DB	'Oranges', 13, 10
	NAME_PENS			DB	'Pens', 13, 10

	PRODUCT_NAMES		DB	10 DUP(?)
	; Setup: LEA each string to PRODUCT_NAMES[i]

	ITEM_PRICE			DW	10 DUP(01010h) ;Default val hardcoded, update from serial
	ITEM_INVENTORY		DB	10 DUP(0FFh) ;Update from serial
	SESSION_TALLY		DW	300

	TXN_QTYS			DB	10 DUP(0)
	TXN_TALLY			DW	0

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

	MODE_CUR			DB	0

	MODE_IDLE			EQU 00h
	MODE_TXN			EQU	01h
	MODE_QRY			EQU	02h
	MODE_EOD			EQU 04h
	MODE_232			EQU 08h

	PORTA_VAL			DB	0
	LED_CURRENT			DB	0
	LED_VALS			DB	6 DUP(?)
	; .GFEDCBA
	LED_LUT				DB	00111111B, 00000110B, 01011011B, 01001111B, 01100110B
						DB	01101101B, 01111101B, 00100111B, 01111111B, 01101111B
						DB	11110111B, 11111111B, 10111001B, 10111111B, 11111001B
						DB	11111001B, 00000000B, 11111111B

	KEYR_PRI_SUSPECT	DB	0
	KEYR_PRI_READ		DB	0
	KEYR_SEC_SUSPECT	DB	0
	KEYR_SEC_READ		DB	0
	KEYPAD_QUEUE		DB	16 DUP(?)
	KEYPAD_QUEUE_HEAD	DB	0
	KEYPAD_QUEUE_TAIL	DB	0
	; Keypad Queue pushing Heisenbug
	KEY_RUBBISH			DB	0

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
	call set_timer2
	STI

	MOV AX, DATA_SEG
	MOV DS, AX

	MOV DS:LED_VALS[0], 10h
	MOV DS:LED_VALS[1], 10h
	MOV DS:LED_VALS[2], 10h
	MOV DS:LED_VALS[3], 10h
	MOV DS:LED_VALS[4], 10h
	MOV DS:LED_VALS[5], 10h

	MOV DS:PORTA_VAL, 00h

	MOV DS:SOUND_QUEUE_HEAD, 0
	MOV DS:SOUND_QUEUE_TAIL, 0

	MOV BX, 0
	MOV DS:SOUND_QUEUE[BX], SOUND_APPLES
	INC DS:SOUND_QUEUE_HEAD

	XOR BH, BH
	MOV BL, DS:SOUND_QUEUE_HEAD
	MOV DS:SOUND_QUEUE[BX], SOUND_AND
	INC DS:SOUND_QUEUE_HEAD

	XOR BH, BH
	MOV BL, DS:SOUND_QUEUE_HEAD
	MOV DS:SOUND_QUEUE[BX], SOUND_ORANGES
	INC DS:SOUND_QUEUE_HEAD

	MOV DS:KEYPAD_QUEUE_HEAD, 0
	MOV DS:KEYPAD_QUEUE_TAIL, 0

NEXT:

	MOV AL, DS:KEYPAD_QUEUE_TAIL
	CMP AL, DS:KEYPAD_QUEUE_HEAD
	JAE KEY_DONE

	XOR BH, BH
	MOV BL, DS:KEYPAD_QUEUE_TAIL
	MOV AL, DS:KEYPAD_QUEUE[BX]
	INC DS:KEYPAD_QUEUE_TAIL

	MOV BL, AL
	MOV CL, 4
	SHR BL, CL

	AND AL, 0Fh

	CMP BL, 1
	JNE NOT_PRIMARY
	CMP AL, 0Ah 
	JNE KEY_DONE
	MOV DS:MODE_CUR, MODE_EOD

	MOV CL, AL
	MOV DX, PORTA
	MOV AL, 0FEh
	OUT DX, AL
	MOV AL, CL

	JMP CONTINUE_HERE_ALWAYS

NOT_PRIMARY:	
	CMP BL, 2
	JNE CONTINUE_HERE_ALWAYS
	CMP AL, 0Ah 
	JNE KEY_DONE
	MOV DS:MODE_CUR, MODE_QRY

	MOV DX, PORTA
	MOV AL, 0FDh
	OUT DX, AL

	;MOV DS:LED_VALS[BX], AL
CONTINUE_HERE_ALWAYS:
	MOV AL, DS:KEYPAD_QUEUE_HEAD
	CMP AL, DS:KEYPAD_QUEUE_TAIL
	JA KP_NORST
	MOV DS:KEYPAD_QUEUE_HEAD, 0
	MOV DS:KEYPAD_QUEUE_TAIL, 0
KP_NORST:

KEY_DONE:

JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^

MODE_QUERY PROC NEAR
		PUSHA

		MOV AX, DATA_SEG
		MOV DS, AX

		CMP DS:MODE_CUR, MODE_QRY
		JNE M_QUERY_SKIP

		MOV AL, 01h
		MOV DX, PORTA
		OUT DX, AL

MODE_QUERY_LOOP:
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
		JNE MODE_QUERY_LOOP

		MOV BL, AL
		MOV AL, DS:ITEM_INVENTORY[BX][-1]

		CALL NEAR PTR UPDATE_DISPLAY
		MOV DS:MODE_CUR, MODE_IDLE

M_QUERY_SKIP:
		POPA
		RET
MODE_QUERY ENDP

MODE_STATISTICS PROC NEAR
		PUSHA

		MOV AX, DATA_SEG
		MOV DS, AX

		CMP DS:MODE_CUR, MODE_EOD
		JNE M_STATISTICS_SKIP

		MOV AL, 02h
		MOV DX, PORTA
		OUT DX, AL

		MOV AX, DS:SESSION_TALLY

		CALL NEAR PTR UPDATE_CASH_DISPLAY
		MOV DS:MODE_CUR, MODE_IDLE

M_STATISTICS_SKIP:
		POPA
		RET
MODE_STATISTICS ENDP

UPDATE_DISPLAY PROC NEAR
		PUSHA

		MOV BX, 06h
UPDATE_DISPLAY_LOOP:

		XOR DX, DX
		MOV CX, 10
		DIV CX

		MOV DS:LED_VALS[BX][-1], DL

		CMP AX, 0
		JE LEADING_ZEROS_START

		DEC BX
		JNZ UPDATE_DISPLAY_LOOP

LEADING_ZEROS_START:

		CMP BX, 0
		JE UPDATE_DISPLAY_END

LEADING_ZEROS:

		MOV DS:LED_VALS[BX][-1], 10h
		DEC BX
		JNZ LEADING_ZEROS


UPDATE_DISPLAY_END:
		POPA
		RET
UPDATE_DISPLAY ENDP

UPDATE_CASH_DISPLAY PROC NEAR
		PUSHA

		MOV BX, 06h
UPDATE_CASH_DISPLAY_LOOP:

		XOR DX, DX
		MOV CX, 10
		DIV CX

		MOV DS:LED_VALS[BX][-1], DL

		CMP AX, 0
		JE LEADING_ZEROS_CASH_START

		DEC BX
		JNZ UPDATE_CASH_DISPLAY_LOOP

LEADING_ZEROS_CASH_START:

		CMP BX, 0
		JE UPDATE_CASH_DISPLAY_END

		CMP BX, 05h
		JB LEADING_ZEROS_CASH
		DEC BX
		MOV DS:LED_VALS[BX][-1], 0

LEADING_ZEROS_CASH:

		MOV DS:LED_VALS[BX][-1], 10h
		DEC BX
		JNZ LEADING_ZEROS_CASH

		MOV AL, DS:LED_VALS[3]
		OR AL, 80h
		MOV DS:LED_VALS[3], AL

UPDATE_CASH_DISPLAY_END:
		POPA
		RET
UPDATE_CASH_DISPLAY ENDP

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
	PUSH AX
	PUSH DS

	CLI

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

	; Skip queue head/tail reset
	JMP HAS_SOUND

NO_SOUND:
	MOV DS:SOUND_QUEUE_TAIL, 0
	MOV DS:SOUND_QUEUE_HEAD, 0

HAS_SOUND:

	STI

	; Counter to limit LED refresh rate
	DEC DS:LED_COUNTER
	JNZ LED_DONE

	; LED output
	CALL NEAR PTR DISPLAY_HANDLER

	MOV DS:LED_COUNTER, 10h

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
	MOV DS:KEYPAD_COUNTER, 100h

NO_KEYPAD:

	POP DS
	POP AX
	RET
TIMER2_ACTION	ENDP

SPEECH_SYNTH PROC NEAR
	PUSH AX
	PUSH BX
	PUSH DS
	PUSH SI
	PUSH DI

	MOV AX, DATA_SEG
	MOV DS, AX

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

SOUND_REMAINS:

	POP DI
	POP SI
	POP DS
	POP BX
	POP AX
	RET
SPEECH_SYNTH ENDP

DISPLAY_HANDLER PROC NEAR
	PUSH AX
	PUSH BX
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
	MOV DS:KEYPAD_QUEUE[BX], AL
	INC DS:KEYPAD_QUEUE_HEAD

	; HEISENBUG ALERT
	MOV AL, DS:KEYPAD_QUEUE_HEAD
	MOV DS:KEY_RUBBISH, AL

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

	; Keypad ID
	OR AL, 10h

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
	MOV BL, DS:KEYPAD_QUEUE_HEAD
	MOV AL, DS:KEYR_SEC_SUSPECT
	MOV DS:KEYPAD_QUEUE[BX], AL
	INC DS:KEYPAD_QUEUE_HEAD

	; HEISENBUG ALERT
	MOV AL, DS:KEYPAD_QUEUE_HEAD
	MOV DS:KEY_RUBBISH, AL

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

	; Keypad ID
	OR AL, 20h

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
