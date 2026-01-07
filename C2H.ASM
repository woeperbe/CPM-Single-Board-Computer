;
;***************************************************************
;*       UPLOAD COM FILE TO INTEL MDS                          *
;*       WRITTEN BY MARC BUFFET 2025                           *
;*       PUBLIC DOMAIN SOFTWARE                                *
;***************************************************************
;
;

REBOOT	EQU	0
BDOS	EQU	5
;
CONIN	EQU	1
CONOUT	EQU	2
PSTRING	EQU	9
RSTRING	EQU	10
VERSION	EQU	12
OPENF	EQU	15
CLOSEF	EQU	16
DELETEF	EQU	19
READS	EQU	20
WRITES	EQU	21
MAKEF	EQU	22
SETDMA	EQU	26
READR	EQU	33
WRITER	EQU	34
;
;
FCB		EQU		005CH
BUFFER	EQU		080H
TPA		EQU		0100H
;
CR		EQU		13
LF		EQU		10
;
UARTD	EQU		20H			;Data address
UARTC	EQU		21H			;Control address
UARTS	EQU		21H			;Status address
;
RTSON  	EQU 	00100111B	; RTS ON -  RX and TX enable DTR on
RTSOFF 	EQU 	00000111B	; RTS OFF -  RX and TX enable DTR on
;
;
; PERSONAL BIOS CALLS
;
	ORG	0100H
;
;				
;---------------------------------------------------------------------------
;       DUMP COM FILE AS INTEL MDS
;---------------------------------------------------------------------------
;
GO:		LXI		SP,STACK	; Local stack
		CALL	USTEXT		; signon	
		DB		"COM to HEX  Ver 2.3",CR,LF,0
;
		MVI		C,OPENF		;open the file
		LXI		D,FCB
		CALL	BDOS
		INR		A			; test if OK ?
		JNZ		WRTHX		; OK GO
;
		CALL	USTEXT
		DB		CR,LF,"Unable to open file",0
		JMP		REBOOT
;
;-----------------------------------------------------------------------
;       READ INTEL MDS OR HEX DUMP FILE FROM PORT
;-----------------------------------------------------------------------
;
WRTHX:	LXI		H,TPA		; start of ddress for HEX file
		SHLD	MEMW		; at storage		
		LXI		H,BUFFER	; standard buffer
		SHLD	MEMR		; Local buffer
		XCHG				; to DE
		MVI		C,SETDMA	; set DMA
		CALL	BDOS		; for bdos	
		LXI		D,FCB		; Buffer ( 128 bytes )
		MVI		C,READS		; write sequential
		CALL	BDOS		; to the open file
		INR		A			; OK ?
		JZ		REBOOT		; No error
WRTHX0:	LHLD    MEMR		; get local buffer
		MOV		A,H			; get high
		CPI		01H			; is it at end of buffer ?
		JNZ		WRTHX1		; no go on	
		LXI		H,BUFFER	; reset 	
		SHLD	MEMR		; buffer pointer
		LXI		D,FCB		; point to file
		MVI		C,READS		; read next record
		CALL	BDOS		; via bdos
		CPI		0			; OK ?
		JNZ		WRTHX3		; NO end of file
WRTHX1:	MVI     C,':'		; start hex dump
		CALL	USOUT		;
		XRA		A			; CLEAR CRC
		STA		CRC			;
		MVI     A,32		; 20 hex bytes / line
		CALL    HXPOUT      ; OUTPUT RECORD LENGTH
		LHLD    MEMW		; HEX dump address pointer
		MOV     A,H
		CALL    HXPOUT      ; OUTPUT ADDRESS HIGH
		MOV     A,L
		CALL    HXPOUT      ; OUTPUT ADDRESSS LOW
		MVI     A,0
		CALL    HXPOUT      ; OUTPUT RECORD TYPE
		MVI     A,32        ; SETUP COUNTER
		STA		COUNT
WRTHX2:	LHLD	MEMR
		MOV     A,M
		INX     H
		SHLD    MEMR
		CALL    HXPOUT      ; OUTPUT BYTE
		LDA		COUNT
		DCR		A
		STA		COUNT
		CPI		0
		JNZ     WRTHX2      ; LOOP UNTIL 20 BYTES DONE
		LDA     CRC
		CMA
		INR     A
		CALL    HXPOUT      ; OUTPUT CRC BYTE
		MVI     C,13        ; AND NEW LINE
		CALL    USOUT       ; AS CARRIAGE RETURN
		MVI     C,10        ; AND LINEFEED
		CALL    USOUT
		LHLD	MEMW
		LXI		D,20H
		DAD		D
		SHLD	MEMW
		JMP     WRTHX0
;
WRTHX3:	CALL    USTEXT      ; SEND EOF RECORD
		DB      ':00000001FF',13,10,26,0
		MVI		C,CLOSEF	; close the
		LXI		D,FCB		; output file
		CALL	BDOS		;
		JMP		REBOOT


;
;-----------------------------------------------------------------------------
;       HEX BYTE OUT TO PORT  AND UPDATE CRC
;-----------------------------------------------------------------------------
;
HXPOUT: PUSH    PSW			; SAVE VALUE
		MOV     C,A			; TO C
		LDA     CRC			; GET CRC
		ADD     C			; ADD NEW BYTE
		STA     CRC			; SAVE BACK
		MOV     A,C			; GET BACK BYTE
		RLC
		RLC			
		RLC
		RLC
		CALL    HXOUT1		; OUTPUT TO PORT
		POP     PSW			; GET BACK VALUE
HXOUT1:	ANI     15			; GET LEFT BEET
		ADI     '0'         ; MAKE DIGIT 0 - 9
		CPI     '9'+1
		MOV     C,A         ; TO C
		JC      USOUT
		ADI     7           ; ADJUST FOR A - F
		MOV     C,A         ; TO E
		JMP     USOUT      ; GO ECHO IT
;
;------------------------------------------------------------------------------
;       CHARACTER  TO CONSOLE
;------------------------------------------------------------------------------
;
USOUT:	PUSH	B			; To be shure
		PUSH	D			; save all registers 
		PUSH	H			; :-)
		MOV		E,C			; char to E
		MVI		C,CONOUT	; tell Char Out	
		CALL	BDOS		; work bdos ....
		POP		H			; restore 
		POP		D
		POP		B
		RET
;
;
;------------------------------------------------------------------------------
;       TEXT TO CONSOLE
;------------------------------------------------------------------------------
;
USTEXT: XTHL				; GET STRING ADDRESS
UTXT1: 	MOV     A,M			; GET BYTE FROM IT
		ORA     A			; TEST IF EOS
		JZ      UTXT2		; OK END OF STRING
		MOV     C,A			; TO C
		CALL    USOUT		; OUTPUT IT
		INX     H			; POINTER++
		JMP     UTXT1		; JUMP ON
UTXT2:	INX     H			; ADJUST FOR RETURN
		XTHL				; RESET RETURN ADDRESS
		RET					; GO ON
;
;----------------------------------------------------------------------
;		STORAGE AREA AND STACK
;----------------------------------------------------------------------
;
;
COUNT:	DS		1
CRC:	DS		1
MEMR:	DS		2
MEMW:	DS		2
;			
		DS		256,0
STACK	EQU		$
;
		END		0
;
