$mod186
NAME EG0_COMP
; Bootstrap loader for 80188 based microcomputer system
;
; Author:       A/Prof Tay Teng Tiow
; Address:      Department of Electrical Engineering 
;               National University of Singapore
;               10, Kent Ridge Crescent
;               Singapore 0511. 
; Date:         May 2000
;
; This file contains proprietory information and cannot be copied 
; or distributed without prior permission from the author.
;---------------------------------------------------------------------------

DATA_SEG        SEGMENT 
		ORG     0008H
; The 4 bytes at 00008H are reserved for the NMI vector.
; The start address of the downloaded program is to be stored at this address.
; Depressing the RESET button: Cause the downloader to be run.
; Depressing the NMI button  : Cause the downloaded program to be re-run.

	START_ADDRESS   DD      ?

DATA_SEG        ENDS


Reset_Seg   SEGMENT
    MOV DX, UMCR
    MOV AX, 03E07H
    OUT DX, AL

JMP far PTR start
Reset_Seg  ends

MESSAGE_SEG     SEGMENT
	MESSAGE_START   DB      10,13,'Ram-based Loader by TAY TENG TIOW    ',10,13
	MESSAGE_END     DB      10,13,'END OF DOWNLOAD                      ',10,13
	MESSAGE_TX_ERR  DB      10,13,'ERROR IN TRANSMISSION - ABORTING     ',10,13
	MESSAGE_RAM_ERR DB      10,13,'RAM VERIFY ERROR - ABORTING          ',10,13
	MESSAGE_CS      DB      10,13,'CHECKSUM ERROR                       ',10,13
MESSAGE_SEG     ENDS



CODE_SEG        SEGMENT
       
	PUBLIC          START
	START_OFF_L     EQU     0008H           
	START_OFF_H     EQU     0009H   
	START_SEG_L     EQU     000AH   
	START_SEG_H     EQU     000BH   

	SYNCH           EQU     3AH

ASSUME  CS:CODE_SEG

START:

; This file setup the serial port for communication with the PC.
$include(80188.inc)

; This file contains the macros used in the downloading program.
$include(boot86mc.inc)

SELECT_7SEG EQU 100H
SELECT_DISPLAY EQU 180H

;initialize LMCS 
    MOV DX, LMCR
    MOV AX, 01C4H  ; Starting address 1FFFH, 8K, No waits, last shoud be 5H for 1 waits      
    OUT DX, AL

; BEGINNING OF DOWNLOADER PROGRAM
; Send out start message to terminal.

		%PUT_MESSAGE (MESSAGE_START)

; The program does the following. 
; Polls the serial port for a downloaded program in the INTEL HEX format. 
; The data received are placed in memory.
; At the end of the downloading, it makes a jump to the beginning
; of the downloaded program and transfers control.

; The INTEL HEX format is define as follows:
;
; Col   #1      #2      #3      #4      #5                      #6
;--------------------------------------------------------------------
;       3A      xx      yyyy    00      zzzzzzzzzzzzzzzzzzzzzzz cs
;       3A      00      0000    01                              cs
;       3A      02      0000    02      ssss                    cs
;       3A      04      0000    03      aaaaaaaa                cs
;
; #1:   synch character.
; #2:   number of bytes to be sent in the records.
; #3:   offset address of data sent.
; #4:   type of record sent.
;       type 00: data streams.
;       type 01: end of file record.
;       type 02: the 02 bytes are the segment address for data of type 00.
;       type 03: the 04 bytes are the segment & offset of 1st instruction.
; #5:   data.
; #6:   checksum.


; The following program does not use any memory locations except 00008H which 
; is the NMI vector location. 

; Note that any downloaded program should normally not use this location 
; except writing an exception routine for NMI. 

; The internal registers are used as follows.
; AX    :       Scrap register.
; BX    :       Contains the checksum.
; CX    :       To store the number of bytes sent in each record.
; DX    :       As scrap data register, OUT and IN commands etc.
; DS    :       As segment address of data sent.
; ES    :       As scrap segment register for messages etc.
; BP    :       Store offset address of data sent.
; SI    :       As scrap register for intermediate values.
; DI    :       As scrap offset address register for messages etc.

LED_DISPLAY:
		
		MOV	DX, SELECT_DISPLAY
		MOV AX, 00FFH
		OUT DX, AL
		
		MOV BX, 02H
TEST_7_SEG_1:
		MOV	DX, SELECT_7SEG
		MOV AX, 0000H
		OUT DX, AL	
		MOV AL, 02H
		DEC AL
		JNZ TEST_7_SEG_1

		MOV BX, 02H
		
TEST_7_SEG_2:
		MOV	DX, SELECT_7SEG
		MOV AX, 00FDH
		OUT DX, AL	
		MOV AL, 02H
		DEC AL
		JNZ TEST_7_SEG_2
		
		MOV BX, 02H
		
TEST_7_SEG_3:
		MOV	DX, SELECT_7SEG
		MOV AX, 00FBH
		OUT DX, AL	
		MOV AL, 02H
		DEC AL
		JNZ TEST_7_SEG_3

		MOV BX, 02H
		
TEST_7_SEG_4:
		MOV	DX, SELECT_7SEG
		MOV AX, 00F7H
		OUT DX, AL	
		MOV AL, 02H
		DEC AL
		JNZ TEST_7_SEG_4
		
		MOV BX, 02H
		
TEST_7_SEG_5:
		MOV	DX, SELECT_7SEG
		MOV AX, 00EFH
		OUT DX, AL	
		MOV AL, 02H
		DEC AL
		JNZ TEST_7_SEG_5
		
		MOV BX, 02H
		
TEST_7_SEG_6:
		MOV	DX, SELECT_7SEG
		MOV AX, 00DFH
		OUT DX, AL	
		MOV AL, 02H
		DEC AL
		JNZ TEST_7_SEG_6

	JMP LED_DISPLAY


; Col #1, check for the synch character.
GET_REC:        XOR     BX,BX           ;clear BX for checksum
		mov     dh,0ffh         ;echo back character
		%GET_CHAR
		XOR     AL,SYNCH
		JNZ     GET_REC         ;check for synch character

; Col #2, get the number of bytes to be sent. This is stored in CX.
		mov     dh,0ffh         ;echo back characters
		%GET_2HEX
		MOV     CL,AL   
		XOR     CH,CH           ;clear CH

; Col #3, get offset address of data sent. This is stored in BP.
		mov     dh,0ffh         ;echo back characters
		%GET_2HEX
		MOV     AH,AL           ;put the top 8 bits into
		MOV     AL,0H           ;into BP
		MOV     BP,AX
		mov     dh,0ffh         ;echo back characters
		%GET_2HEX
		MOV     AH,0H           ;put the bottom 8 bits into
		OR      BP,AX           ;BP

; Col #4, get the type of record sent.
		mov     dh,0ffh         ;echo back characters
		%GET_2HEX

; type 0 ? (data)
		CMP     AL,00H  
		JZ      L_TYPE0
; type 2 ? (segment address)
		CMP     AL,02H
		JNZ     CONT0
		JMP     L_TYPE2
CONT0:
; type 3 ? (start offset and segement address)
		CMP     AL,03H
		JNZ     CONT1
		JMP     L_TYPE3
CONT1:

; Service for type 01, (end of file). 
; Make a jump to start address.
; Send out message to terminal to indicate end of download and then
; do a indirect branch to the start of the downloaded program.

		%PUT_MESSAGE (MESSAGE_END)

ASSUME  ES:DATA_SEG
		MOV     AX,DATA_SEG
		MOV     ES,AX
		JMP     START_ADDRESS

; Service for type 00, get the data.
L_TYPE0:        mov     dh,00h          ;do not echo back characters
		%GET_2HEX
		MOV     DS:[BP],AL
		MOV     AH,DS:[BP]      ;to verify correct transfer to RAM
		XOR     AL,AH
		JZ      NO_RAM_ERR
		JMP     RAM_ERROR
NO_RAM_ERR:     INC     BP
		LOOP    L_TYPE0
		JMP     CHECKSUM

; Service for type 02, get the segment address and put in DS.
L_TYPE2:        mov     dh,00h          ;do not echo back characters    
		%GET_2HEX
		MOV     AH,AL
		MOV     AL,00H
		MOV     SI,AX
		mov     dh,00h          ;do not echo back characters    
		%GET_2HEX
		MOV     AH,00H
		OR      AX,SI
		MOV     DS,AX
		JMP     CHECKSUM

; Service for type 03, get start address & put in START_OFFSET/START_SEGMENT
L_TYPE3:        MOV     AX,DATA_SEG
		MOV     ES,AX
		mov     dh,00h          ;do not echo back characters    
		%GET_2HEX               ;Get high byte start segment address.
		MOV     DI,START_SEG_H
		MOV     ES:[DI],AL
		mov     dh,00h          ;do not echo back characters    
		%GET_2HEX               ;Get low byte of start segment address
		MOV     DI,START_SEG_L
		MOV     ES:[DI],AL
		mov     dh,00h          ;do not echo back characters    
		%GET_2HEX               ;Get high byte start offset address.
		MOV     DI,START_OFF_H
		MOV     ES:[DI],AL
		mov     dh,00h          ;do not echo back characters    
		%GET_2HEX               ;Get low byte start offset address.
		MOV     DI,START_OFF_L
		MOV     ES:[DI],AL
		JMP     CHECKSUM

CHECKSUM:       mov     dh,0ffh         ;echo back characters   
		%GET_2HEX               ;This is the checksum byte
		XOR     BL,00H
		JNZ     CHECKSUM_ERR
		mov     dh,0ffh         ;echo back characters
		%GET_CHAR               ;to get /n
		JMP     GET_REC

; To send transmission error message to PC and abort download
ERROR_ROUTINE:  
		%PUT_MESSAGE (MESSAGE_TX_ERR)
		HLT
;
CHECKSUM_ERR:   %PUT_MESSAGE (MESSAGE_CS)
		HLT
;
; To send RAM verify error message to PC and abort download
RAM_ERROR:      %PUT_MESSAGE (MESSAGE_RAM_ERR)
		HLT
;

CODE_SEG        ENDS
END
