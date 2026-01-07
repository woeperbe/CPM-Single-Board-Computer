;
;***************************************************************
;*       UPLOAD INTEL MDS FILE TO COM FILE                     *
;*       WRITTEN BY MARC BUFFET 2025                           *
;*       PUBLIC DOMAIN SOFTWARE                                *
;***************************************************************
;

REBOOT	EQU		0
BDOS	EQU		5
;
CONIN	EQU		1
CONOUT	EQU		2
PSTRING	EQU		9
RSTRING	EQU		10
VERSION	EQU		12
OPENF	EQU		15
CLOSEF	EQU		16
DELETEF	EQU		19
READS	EQU		20
WRITES	EQU		21
MAKEF	EQU		22
SETDMA	EQU		26
READR	EQU		33
WRITER	EQU		34
;
FCB		EQU		005CH
BUFFER	EQU		080H
;
CR		EQU		13
LF		EQU		10
;
UARTD	EQU		20H					;Data address
UARTC	EQU		21H					;Control address
UARTS	EQU		21H					;Status address
;
RTSON  	EQU 	00100111B		 	; RTS ON -  RX and TX enable DTR on
RTSOFF 	EQU 	00000111B		 	; RTS OFF -  RX and TX enable DTR on
;
;
		ORG		0100H
;				
;---------------------------------------------------------------------------
;       GET HEX INTEL MDS AND WRITE TO FILE
;---------------------------------------------------------------------------
;
GO:		LXI		SP,STACK
		CALL	PRTMSG
		DB		"HEX to COM  Ver 3.5.2",0
;
		MVI		C,OPENF
		LXI		D,FCB
		CALL	BDOS
		INR		A
		JNZ		MDS
		MVI		C,MAKEF
		LXI		D,FCB
		CALL	BDOS
		INR		A
		JNZ		MDS
;
		CALL	PRTMSG
		DB		CR,LF,"Unable to create file",0
		JMP		REBOOT
;			
WRTERR:	CALL	PRTMSG
		DB		CR,LF,"Unable to write to file",0
		JMP		REBOOT
;
;-----------------------------------------------------------------------
;       READ INTEL MDS OR HEX DUMP FILE FROM PORT
;-----------------------------------------------------------------------
;

MDS:    PUSH	PSW
		MVI		A,RTSON
		OUT     UARTC
		POP		PSW
				
		CALL	PRTMSG
		DB		CR,LF,"Start uploading HEX file"
		DB		CR,LF,"End of file must be :00000001FF",CR,LF,0
;			
		LXI		H,080H		; standard buffer
		SHLD	MEMR
				
;
MDS01:	CALL	CIN	 	    ; read sync byte
		ANI		7FH			; strip off msb
		CPI		':'			; should be :
		JNZ		MDS01		; Wait until so
		XRA		A			; clear crc
		STA		CRC	
		CALL	GETB		; get byte count
		STA		COUNT		; save count value
		CPI		0
		JZ		MDS04
		CALL	GETW		; get address
;			
		CALL	GETB		; get record type
		CPI		1			; eof record ?
		JZ		MDS04		; ok done exit
;
MDS02:	CALL	GETB		; get one byte
		LHLD	MEMR
		MOV		M,A			; save in memory
		INX		H			; increment pointer
		SHLD	MEMR
		MOV		A,H
		CPI		01H
		JNZ		MDS9
;
		LXI		D,FCB		; Buffer full ( 128 bytes )
		MVI		C,WRITES	; write sequential
		CALL	BDOS		; to the open file
		INR		A			; OK ?
		JZ		WRTERR		; No error
;
		LXI		H,080H		; Reset Buffer pointer
		SHLD	MEMR		; for next hit
				
MDS9:	LDA		COUNT		; get counter
		DCR		A			; decrement it
		STA		COUNT		; save again
		ORA		A			; test it
		JNZ		MDS02		; loop until all done
		CALL	GETB		; get checksum bytes
		LDA		CRC			; get the final checksum
		ORA		A
		JNZ		MDS03		;error exit
;
		JMP		MDS01		; get next record
;
MDS03:	CALL	PRTMSG
		DB		CR,LF,"Checksum error !",0	; error 
		RET			
;
MDS04:	CALL	CIN			; get overflowing characters
		CPI		CR			; unti CR
		JNZ		MDS04		; not yet
		CALL	PRTMSG		; display end message
		DB		CR,LF,"Upload complete.",0	; OK done  
;				
		LXI		D,FCB		; write last buffer even not full
		MVI		A,WRITES	; at end of file	
		CALL	BDOS		;
;			
		MVI		C,CLOSEF	; close the
		LXI		D,FCB		; output file
		CALL	BDOS		;
		INR		A			; OK ?
		JNZ		FSAV		; yes exit
;
		CALL	PRTMSG		; No can't close the file
		DB		CR,LF,"Error closing file !",CR,LF,0
		JMP		REBOOT
;
FSAV:	MVI		A,RTSON
		OUT     UARTC
		XRA		A
		JMP		REBOOT
;
;---------------------------------------------------------------------------
;       CHKSM RAM FROM HL THRU DE IN 16 BIT VALUE
;---------------------------------------------------------------------------
;
CHKSM:  LXI     B,0          ; save checksum on stack
		PUSH    B
;
CHKSM0: MOV     A,H          ; compare high
		CMP     D            ; with end
		JNZ     CHKSM1       ; not ok continue
		MOV     A,L          ; compare low
		CMP     E            ; With end
		JNZ     CHKSM1       ; not ok
		POP     H            ; ok get chacksum
		RET                  ; and return
;
CHKSM1: MOV     A,M          ; get byte to add
		INX     H            ; bump pointer
		XTHL                 ; exchange pointer and cs
		PUSH    D            ; save end address
		MOV     E,A          ; value to add to  de
		MVI     D,0          ; so d to zero
		DAD     D            ; 16 bit add
		POP     D            ; restore de
		XTHL                 ; get back pointer on stack
		JMP     CHKSM0       ; jump for next
;				
;---------------------------------------------------------------------------
;       GET NUMBER 0 TO 255 FROM PORT IN ACU AND UPDATE CRC
;---------------------------------------------------------------------------
;
GETB:   PUSH    B           ; save bc
		CALL    CIN         ; read first len byte
		CALL    MKBIN       ; convert digit
		RLC
		RLC
		RLC
		RLC
		MOV     B,A         ; save in b
		CALL    CIN         ; Read next character
		CALL    MKBIN
		ORA     B           ; Combine byte
		MOV     B,A         ; Save back in b
		LDA     CRC         ; Get crc
		ADD     B           ; Add it
		STA     CRC         ; Save back crc
		MOV     A,B
		POP     B           ; Restore bc
		RET               	
;---------------------------------------------------------------------------
;       MAKE BINAIRY
;---------------------------------------------------------------------------
;
MKBIN:  SUI     '0'			; remove ascii 0
		CPI     10			; Test if a-f range
		RM					; No ok return
		SUI     7			; ELse adjust it
		RET					; And return
;
;---------------------------------------------------------------------------
;       GET ADDRESS FROM PORT IN DE
;---------------------------------------------------------------------------
;
GETW:   CALL    GETB		; Get one byte
		MOV     D,A			; to d
		CALL    GETB		; get another byte
		MOV     E,A			; to e
		RET					; now de holds address
;
;----------------------------------------------------------------------
;  READ CHARACTER FROM CONSOLE WITH HANDSHAKE
;----------------------------------------------------------------------
;
CIN:	PUSH	PSW
		MVI		A,RTSON
		OUT     UARTC
		POP		PSW
;
CIN0:	IN      UARTS		; Get UART status
		ANI		00000010B	; receiver ready?
		JZ		CIN0		; No wait until character recieved
		IN 		UARTD		; Get it now
;
		PUSH	PSW
		MVI		A,RTSOFF
		OUT     UARTC
		POP		PSW
		RET			
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
PRTMSG: XTHL				; get string address
UTXT01: MOV     A,M			; get byte from it
		ORA     A			; test if eos
		JZ      UTXT02		; ok end of string
		MOV     C,A			; to c
		CALL    USOUT		; output it
		INX     H			; pointer++
		JMP     UTXT01		; Jump on
UTXT02: INX     H			; Adjust for return
		XTHL				; Reset return address
		RET					; go on

;
;----------------------------------------------------------------------
;		STORAGE AREA AND STACK
;----------------------------------------------------------------------
;
;
COUNT:	DS		1
CRC:	DS		1
MEMR:	DS		2
;			
		DS		64
STACK	EQU		$
;
		END		0
;			