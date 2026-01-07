;
;
;	CCP source from CP/M 
;	Dissasembled from CP/M and modified by MBU 1986
;	
;	Enhanced version of CCP   version 0.4
;
;	PATCH COMMENTS:
;	REL 0.5	(07/03/87)
;		in LDTPA swap the call setdisk and call rsloc4
;		to boot back on previous drive if bdos select error
;
		ORG		0CE00H
;
;
VERS	EQU	0
REVN	EQU	5
;
;	Resident commands added or changed 
;
;		LABEL X: xyz	write volumelabel xyz to drive X:
;				if xyz is omitted volumelabel is deleted
;
;		DIR X:		List directory is changed so that the 
;				volumelabel is listed if present and
;				the remaining disk free space , blocksize
;				status and filecount  of the disk
;		
;		SHOW xyz	show file to screen FF surpressed
;
;		DIRS X:		show system files on disk 
;
;		PUBLIC		toggle the public flag ( needs bdos patch )
;
;		SIZE	xyz	display file size to screen	
;
;		COPY  X Y       copy file to file 
;				x may not contain ambigious filenames
;				y may also be only the drive
;				designator as B:
;	
;		RESET X:	reset drive after disk change without ^C
;
;		.COM		when loading an tpa program the operator can
;				abort execution before run with the ESC key
;
;		CLS		send formfeed character to screen
;
;		PROMPT XYZ	change prompt in any string where :
;				$U displays the active user
;				$D displays the current drive
;				$S displays the current disk free space
;				$P displays the public active or not flag
;				$E echoes a escape to console
;				all other characters are normal echoed 
;				
;		
;
;			you can patch a specific prompt for you on disk
; 
;			For the rest the messages are much more explicite
;			to be more user friendly.
; 
;	(C) Copyright	MBU 1986 September 
;
;
;	DISCOVERED THINGS:
;
;	BDOS function 13 ( reset disk system ) returns in acu
;		0FFH	is file present with $ in first position
;		0	if not
; 
;	The next 4 lines are only for debugging remove if not needed ! 
;
;
;
BDOS 	EQU 	5		; where bios stores jmp to bdos
;
;
;
;
TPA		EQU	0100H		; TPA start address
FCB5C	EQU	005CH		; Default FCB setup by CCP
DEFDMA	EQU	080H		; default dma buffer
RECLEN	EQU	128		; normal CP/M can not do else !
;
PUBLIC	EQU	040H		; bdos public flag
;
CR		EQU	13		; needs no explanation
LF		EQU	10
FF  	EQU	12
ESC		EQU	27
;
; ** CCP ENTRY POINTS
;
STRTCCP:JMP 	CCP			; CCP entry point for coldboot
		JMP 	CCPCLR		; CCP entry point for warmboot
;
; ** CCP COMMAND LINE BUFFER
;
CCPK:	DB	127		; max char in ccp buffer
CCPKC:	DB	0		; character count in buffer 
CCPBUF:	DS	128,0		; buffer 128 bytes
;
PBCCP:	DW	CCPBUF		; pointer after words in ccp buffer process
PBCCPA:	DW	CCPBUF		; pointer before word in process	
;
; ** OUTPUT CHARACTER
;
COUT:	MOV 	E,A		; character to e
 	MVI 	C,2		; opcode 2
 	JMP 	BDOS		; call bdos
;
; ** OUTPUT CHARACTER AND SAVE BC	
;
COUTSB:	PUSH	B		; save bc
 	CALL	COUT		; output character
 	POP 	B		; restore bc
 	RET 			; return to caller
;
; ** OUTPUT CR LF
;
CRLF:	MVI 	A,CR		; setup cr
 	CALL	COUTSB		; output
 	MVI 	A,LF		; setup linefeed
 	JMP 	COUTSB		; output
;
; ** OUTPUT SPACE
; 
SPACE:	MVI 	A,' '		; setup a space
 	JMP 	COUTSB		; output
;
; ** OUTPUT STRING POINTED BY BC UNTIL ZERO AFTER CRLF
;
OUTSTR:	PUSH	B		; save bc
 	CALL	CRLF		; new line
 	POP 	H		; pointer in hl
;
; ** OUTPUT STRING AT HL UNTIL ZERO
;
OUTSHL:	MOV 	A,M		; get a character
 	ORA 	A		; zero ?
 	RZ			; yes all done
 	INX 	H		; point to next
 	PUSH	H		; save pointer
 	CALL	COUT		; output it
 	POP 	H		; restore pointer
	JMP 	OUTSHL		; loop for next
;
; ** RESET DISK SYSTEM
;
RSTDSK:	MVI 	C,13		; reset disk opcode
 	JMP 	BDOS		; call bdos
;
; ** SELECT DISK IN A
;
SELDSK:	MOV 	E,A		; drive to e
 	MVI 	C,14		; select disk opcode
 	JMP 	BDOS		; call bdos
;
; ** GENERAL PURPOSE BDOS CALL
;
BDOSCL:	CALL	BDOS		; call bdos
 	STA 	BDSERR		; save return error code
 	INR 	A		; increment to put 0FFH to 0
 	RET 			; return
;
; ** SET ATTRIBUTES
;
SETATR:	MVI	C,30		; set attributes opcode
	JMP	BDOSCL		; call bdos
;
; ** OPEN DEFAULT FCB FILE
; 
OPNDEF:	XRA 	A		; clear acu
 	STA 	CURREC		; clear current record number
 	LXI 	D,DEFFCB	; point to defaultfcb	
; 	JMP 	OPENFL		; open the file
;
; ** OPEN FILE 
;
OPENFL:	MVI 	C,15		; open file opcode
 	JMP 	BDOSCL		; call bdos
;
; ** CLOSE FILE
;
CLOSFL:	MVI 	C,16		; close file opcode
 	JMP 	BDOSCL		; call bdos
;
; ** SEARCH FIRST DIR ENTRY
;
SHFRST:	MVI 	C,17		; search first opcode 
 	JMP 	BDOSCL		; call bdos
;
; ** SEARCH FOR NEXT DIR ENTRY
;
SHNEXT:	MVI 	C,18		; search next opcode
 	JMP 	BDOSCL		; call bdos
;
; ** FIND FIRST STARTING WITH DEFAULT FCB
;
FDFFCB:	LXI 	D,DEFFCB	; point to default fcb
 	JMP 	SHFRST		; call bdos to try to find it
;
; ** DELETE FILE
;
DELFL:	MVI 	C,19		; delete file opcode 	
 	JMP 	BDOS		; call bdos
;
; ** BDOS CALL ENTRY
;
BDOSCA:	CALL	BDOS		; call bdos
 	ORA 	A		; test return code
 	RET 			; return to caller
;
; ** READ FILE SEQUENTIAL
;
RDSEQ:	MVI 	C,14H		; read sequential opcode
 	JMP 	BDOSCA 		; call bdos
;
; ** READ DEFAULT FCB FILE SEQUENTIAL
;
RDDFFC:	LXI 	D,DEFFCB	; point to default fcb
 	JMP 	RDSEQ		; read next sequential record
;
; ** WRITE SEQUENTIAL
;
WRTSEQ:	MVI 	C,15H		; write sequential opcode
 	JMP 	BDOSCA		; call bdos
;
; ** CREATE FILE
;
CREAFL:	MVI 	C,16H		; create file opcode
 	JMP 	BDOSCL		; call bdos
;
; ** RENAME FILE
;
RNMEFL:	MVI 	C,17H		; rename file opcode
 	JMP 	BDOS		; call bdos
;
; ** GET USER NUMBER
;
GETUSR:	MVI 	E,0FFH		; 0FFH to get user number 
;
; ** USER NUMBER CALL 
;
GPUSER:	MVI 	C,32		; get/put user number opcode
 	JMP 	BDOS		; call bdos
;
; ** GET DISK FREE SPACE
;
FREESZE:MVI	C,31		; get disk parms
	CALL	BDOSCA		; thru bdos
	INX 	H		; point to
	INX	H		; block size
	MOV	D,M		; get it 
	INX	H		; point
	INX	H		; to
	INX	H		; block count
	MOV	A,M		; get low byte
	INX	H		; point to high
	MOV	H,M		; get it
	MOV	L,A		; low to l
	INX	H		; adjust range 0 n-1 to 1 n
	SHLD 	MAXBLK		; save max count
	MOV	A,D		; size to a
	STA	BLKSIZE		; save it
	MVI	C,27		; get allocation table address
	CALL	BDOSCA		; thru bdos
	PUSH	H		; pointer to bit table on stack
	LHLD	MAXBLK		; get max block	
	MOV	C,L		; copy it
	MOV	B,H		; to bc
	LXI	H,0		; hl to zero
	SHLD	COUNTER		; free counter to zero	
	XCHG			; DE is bit position reference init to 0
FREE03:	MOV	A,B		; at end of table 
	ORA	C		; test if counter of it is zero
	JZ	FREE06		; yes done has result
	DCX	B		; decrement counter if not yet zero
	MOV	A,E		; get bit in byte counter
	ANI	7		; mask to 8 
	JNZ	FREE04		; no get next bit
	XTHL			; else get first new byte
	MOV	A,M		; from
	INX	H		;address on stack of bit table
	XTHL			; and restore it
	MOV	L,A		; new byte to l
FREE04:	MOV	A,L		; get l
	RAL			; shift bit in carry
	MOV	L,A		; resave
	JC	FREE05		; if carry bit set block used
	PUSH	H		; save L
	LHLD	COUNTER		; get counter
	INX	H		; increment
	SHLD	COUNTER 	; save back				
	POP	H		; restore L
FREE05:	INX	D		; next block
	JMP	FREE03		; loop 
FREE06:	POP	H		; clear stack ( pointer to table )
	LHLD	COUNTER		; get counter
	LDA	BLKSIZE		; get block size
	SUI	2		; minus 2
	PUSH	PSW		; save it
FREE08:	DCR	A		; decrement it
	JZ	FREE09		; now zero then ok 
	DAD	H		; else double count
	JMP	FREE08		; loop again	
FREE09:	SHLD	COUNTER		; save it back
	POP	PSW		; get back blksize
	LXI	H,1024		; setup 1 k blocks
FREE10:	DCR	A		; decrement count
	JZ	FREE11		; zero now
	DAD	H		; no then double block size
	JMP	FREE10		; loop again
FREE11:	SHLD	BLKSIZE		; save it
	RET 			; return
;
; ** BINAIR TO ASCII  5 DIGITS
;
DECOUT:	MVI	B,80H		; flag to no print
	LXI	D,-10000	; setup 10000
	CALL	DECNO		; see how many
	LXI	D,-1000		; setup 1000
	CALL	DECNO		; see howmany
	LXI	D,-100		; setup 100
	CALL	DECNO		; see how many
	LXI	D,-10		; setup 10
	CALL	DECNO		; see howmany
	MVI	B,0		; clear inhibit print flag
	LXI	D,-1		; setup 1
DECNO:	INR	B		; see howmany
	DAD	D		; substract it
	JC	DECNO		; carry ?
	DCR	B		; yes adjust count
	XCHG			; to de
	CALL	NEGRP		; negate
	XCHG			; back to hl
	DAD	D		; readd
	MOV	A,B		; get no print flag
	CPI	80H		; still set
	RZ			; yes then return
	MOV	A,B		; get flag
	ANI	7FH		; mask out flag
	ORI	'0'		; make ascii  
	PUSH	H		; save hl
	CALL	COUT		; echo it
	POP	H		; restore hl
	MVI	B,0		; disable flag now
	RET			; and return
;
;	NEGATE REGISTER PAIR HL
;
NEGRP:	XRA	A		; clear carry clear acu
	SUB	L		; substarct l
	MOV	L,A		; save l
	MVI	A,0		; save carry but a to 0
	SBB	H		; substract h
	MOV	H,A		; save h
	RET			; ret
;
;
; ** COMBINE NEW USER WITH CURRENT DISK FOR BYTE AT LOC 4
;
LOC4:	CALL	GETUSR		; get user number
	ADD 	A		; shift
 	ADD 	A		; four
 	ADD 	A		; bits
 	ADD 	A		; left
 	LXI 	H,CURDSK	; point to current disk
 	ORA 	M		; combine
 	STA 	4		; save at location 4
 	RET 			; return
;
; ** RESET LOACATION 4 TO DRIVE CURRENTLY ACTIVE
;
RSLOC4:	LDA 	CURDSK		; get current acive disk
 	STA 	4		; save at location 4
 	RET 			; return to caller
;
; ** MAKE ASCII UPPERCASE
;
UPPERC:	CPI 	'a'		; below a
 	RC			; yes ok
 	CPI 	'z'+1		; above z
 	RNC 			;yes ok
 	ANI 	01011111B	; else make uppercase
 	RET 			; and return
;
; ** GET COMMAND STRING FROM KEYBORD OR DISK $$$.SUB
;
GNXTCM:	LDA 	DOLLAR		; get $ file present flag from bdos
 	ORA 	A		; test it
 	JZ	RDLIN		; zero then keybord entry
 	LDA 	CURDSK		; else submitfile maybe present
 	CALL	SELDSK		; select current disk
 	LXI 	D,BATCHFL	; point to fcb batch file
 	CALL	OPENFL		; open it
 	JZ	RDLIN		; open not ok go on
 	LDA 	RECCNT		; get current record		
 	DCR 	A		; one less
 	STA 	BTHCUR		; set it up as last record
 	LXI 	D,BATCHFL	; read it
 	CALL	RDSEQ		; sequential
 	JNZ 	RDLIN		; error ? yes done
 	LXI 	D,CCPKC		; point to ccp buffer 
 	LXI 	H,DEFDMA	; point to readed buffer
 	MVI 	B,RECLEN	; count
 	CALL	MOVE		; move it
 	LXI 	H,RECSVE	; point to record count into fcb
 	MVI 	M,0		; write there zero
 	INX 	H		; point to current record position
 	DCR 	M		; delete last record
 	LXI 	D,BATCHFL	; close file with one record less
 	CALL	CLOSFL		; in it    
 	JZ	RDLIN		; done error ? yes done
 	LDA 	CURDSK		; get current disk
 	CALL	SELDSK		; no select correct one
 	LXI 	H,CCPBUF	; point to ccp buffer	
 	CALL	OUTSHL		; and print it on the screen
 	CALL	KBHIT		; keybord hit ?
 	JZ	RDL01		; no process the batchline
 	CALL	DELBTH		; delete batch file	
 	JMP 	PROMPT		; ask back prompt
;
; ** READ CONSOLE LINE COMMAND AND MAKE UPPERCASE
;
RDLIN:	CALL	DELBTH		; delete batch file
 	CALL	LOC4		; reset location 4
 	MVI 	C,10		; setup read console buffer 
 	LXI 	D,CCPK		; point to it
 	CALL	BDOS		; read buffer from console
 	CALL	RSLOC4		; reset location 4 
RDL01:	LXI 	H,CCPKC		; point to count 
 	MOV 	B,M		; get character count
RDL02:	INX 	H		; point to first
 	MOV 	A,B		; get count
 	ORA 	A		; test it
 	JZ	RDL03		; zero ok all done
 	MOV 	A,M		; get first byte
 	CALL	UPPERC		; to uppercase
 	MOV 	M,A		; back to memory
 	DCR 	B		; decrement count
 	JMP 	RDL02		; loop until all so
RDL03:	MOV 	M,A		; ok all uppercase put a zero after buffer
 	LXI 	H,CCPBUF	; point to start of buffer	
 	SHLD	PBCCP		; and set pointer to where whe are at start
 	RET 	
;
; ** CHECK CONSOLE STATUS AND GET CHAR IF ONE THERE
;
KBHIT:	MVI 	C,11		; get console stauts
 	CALL	BDOS		; thru bdos
 	ORA 	A		; test return
 	RZ			; zero nothing there
 	MVI 	C,1		; get character 
 	CALL	BDOS		; thru bdos
 	ORA 	A		; test flags
 	RET 			; return
;
; ** GET CURRENT DISK
;
GETDSK:	MVI 	C,19H		; get current disk opcode
 	JMP 	BDOS		; call bdos
;
; ** SET DMA ADDRESS TO 80H
;
DMA80:	LXI 	D,DEFDMA	; de to 080H
;
; ** SET DMA ADDRESS
;
SETDMA:	MVI 	C,1AH		; set dma opcode
 	JMP 	BDOS		; call bdos
;
; ** DELETE BATCH FILE IS NO MORE ACTIVE
;
DELBTH:	LXI 	H,DOLLAR	; get dollar file active flag
 	MOV 	A,M		; get it
 	ORA 	A		; test 
 	RZ			; not active ok
 	MVI 	M,0		; clear it
 	LDA	CURDSK		; clear acu
 	CALL	SELDSK		; select drive zero
 	LXI 	D,BATCHFL	; point to batchfile
 	CALL	DELFL		; delete it
 	LDA 	CURDSK		; get current disk
 	JMP 	SELDSK		; and select it back
;
; ** TOGGLE PUBLIC FLAG
;
TGLPUB:	LXI 	H,PUBLIC	; point to public flag
 	MOV 	A,M		; get it
 	XRI 	01		; invert
 	MOV 	M,A		; restore
 	JMP 	PROMPT		; backto prompt
;
; ** DISPLAY ERROR MESSAGE AND RETURN PROMPT
;
ERREXT:	LXI	B,ERRMSG9	; error message setup
	CALL	OUTSTR		; print it
 	LHLD	PBCCPA		; current active command part
ERR01:	MOV 	A,M		; get first
 	CPI 	' '		; space ?? 
 	JZ	ERR02		; yes then done
 	ORA 	A		; test it
 	JZ	ERR02		; zero then done
 	PUSH	H		; save pointer to it
 	CALL	COUT		; to console
 	POP 	H		; restore pointer
 	INX 	H		; increment pointer
 	JMP 	ERR01		; loop again
ERR02: 	CALL	DELBTH		; delete batch file
 	CALL	CRLF		; new line
	JMP 	PROMPT		; prompt for anaother command
;
ERRMSG9:DB	'Invalid parameter : ',0
;
; ** VALID ASCII CHARACTER ?
;
VALASC:	LDAX	D		; get it
 	ORA 	A		; test it
 	RZ			; return if zero ok 
 	CPI 	' '		; control character ?
 	JC	ERREXT		; yes error
 	RZ			; if space ok
 	CPI 	'='		; if = not ok
 	RZ	
 	CPI 	'_'		; if _ not ok
 	RZ	
 	CPI 	'.'		; if . not ok
 	RZ	
 	CPI 	':'		; if : not ok
 	RZ	
 	CPI 	';'		; if ; not ok
 	RZ	
 	CPI 	'<'		; if < not ok
 	RZ	
 	CPI 	'>'		; if > not ok
 	RZ	
 	RET 			; all other ok	
;
; ** GET ADDRESS OF FILE FOUND IN DMA BUFFER
;
GETADR:	LDA 	BDSERR		; get dir return code from bdos ( 0 1 2 3 )
 	RRC 			; calculate 
 	RRC 			; offset address
 	RRC 			; into dma buffer
 	ANI 	60H		; mask out offset
 	MOV 	C,A		; save to c
	RET			; return
;
; ** FIND FIRST ZERO CHAR OR NON SPACE IN STREAM POINTED BY DE
; 
NONSPC:	LDAX	D		; get a byte
 	ORA 	A		; test it
 	RZ			; zero exit
 	CPI 	' '		; space ?
 	RNZ 			; no ok found
 	INX 	D		; else increment de
 	JMP 	NONSPC		; and check next
;
; ** ADD OFFSET TO GET ADRESS CORRECT  
; 
ADDOFF:	ADD 	L		; add a to l 
 	MOV 	L,A		; restore
 	RNC 			; carry no exit
 	INR 	H		; else increment h
 	RET 			; to adjust hl and return
;
; ** SETUP DEFAULT FCB UP WITH FILE OUT COMMAND LINE
;
SETFCB:	MVI 	A,0		; no offset
SETFCBA:LXI 	H,DEFFCB	; point hl to default fcb 
 	CALL	ADDOFF		; add offset
 	PUSH	H		; save pointer
 	PUSH	H		; twice
 	XRA 	A		; clear acu
 	STA 	TMPDSK		; to clear active drive
 	LHLD	PBCCP		; pointer first char after command 
 	XCHG			; to de	
 	CALL	NONSPC		; find first non space
 	XCHG			; back to hl
 	SHLD	PBCCPA		; save it
 	XCHG			; to de
 	POP 	H		; get fcb address
 	LDAX	D		; get first byte of command line ( after cmd )
 	ORA 	A		; test it ?
 	JZ	SFCB01		; zero ?? yes then go on 
 	SBI 	'A'-1		; else prepare already new drive	
 	MOV 	B,A		; save to b
 	INX 	D		; point to next byte 
 	LDAX	D		; get it
 	CPI 	':'		; should be a :
 	JZ	SFCB02		; yes then ok go on
 	DCX 	D		; no then restore de to first 
SFCB01:	LDA 	CURDSK		; get current disk
 	MOV 	M,A		; save it into fcb 
 	JMP 	SFCB03		; continue job
SFCB02:	MOV 	A,B		; else get new drive
 	STA 	TMPDSK		; save in ccp ( why ??? )
 	MOV 	M,B		; and setup this as drive in fcb at hl 
 	INX 	D		; point to next in stream
SFCB03:	MVI 	B,8		; 8 bytes to move
SFCB04:	CALL	VALASC		; check if next is valid punctuation
	JZ	SFCB08		; all characters done fill spaces
 	INX 	H		; point to next fcb byte
 	CPI 	'*'		; is character a *
 	JNZ 	SFCB05		; no skip
 	MVI 	M,'?'		; else replace now by a ?
 	JMP 	SFCB06		; go on
SFCB05:	MOV 	M,A		; move byte as it is in fcb
 	INX 	D		; increment pointer to stream
SFCB06:	DCR 	B		; decrement count
 	JNZ 	SFCB04		; loop until all 8 done
SFCB07:	CALL	VALASC		; next a character ? ( 9 char if file )
 	JZ	SFCB09		; yes ok
 	INX 	D		; no try next
 	JMP 	SFCB07		; until one found	
SFCB08:	INX 	H		; increment fcb pointer
 	MVI 	M,' '		; space there
 	DCR 	B		; decrement count
 	JNZ 	SFCB08		; until all filled up
SFCB09:	MVI 	B,3		; ok 8 done try 3 latest
 	CPI 	'.'		; now it should just after the point
 	JNZ 	SFCB14		; no ok we get the next char
 	INX 	D		; else after increment it is so
SFCB10:	CALL	VALASC		; valid terminator
 	JZ	SFCB14		; yes 
 	INX 	H		; no point to next fcb char
 	CPI 	'*'		; is it maybe a *
 	JNZ 	SFCB11		; no normal caharacter
 	MVI 	M,'?'		; is whas * put ? in file
 	JMP 	SFCB12		; go on
SFCB11:	MOV 	M,A		; then put it into fcb
 	INX 	D		; point to next in stream
SFCB12:	DCR 	B		; decrement count
 	JNZ 	SFCB10		; until all done
SFCB13:	CALL	VALASC		; valid ascii char
 	JZ	SFCB15		; yes loop again
 	INX 	D		; next char
 	JMP 	SFCB13		; until one found
SFCB14:	INX 	H		; ncrement hl ( pointer to fcb )
 	MVI 	M,' '		; space there
 	DCR 	B		; decrement count
 	JNZ 	SFCB14		; until flled up
SFCB15:	MVI 	B,3		; setup 3
SFCB16:	INX 	H		; next 3 bytes in fcb
 	MVI 	M,0		; to zero
 	DCR 	B		; loop
 	JNZ 	SFCB16		; until all
 	XCHG			; now address to fcb in de
 	SHLD	PBCCP		; save it
 	POP 	H		; restore pointer to
 	LXI 	B,11		; b to zero c to 11
SFCB17:	INX 	H		; next
 	MOV 	A,M		; get it
 	CPI 	'?'		; is there a ?
 	JNZ 	SFCB18		; no check next
 	INR 	B		; else increment b
SFCB18:	DCR 	C		; decrement count
 	JNZ 	SFCB17		; check next
 	MOV 	A,B		; b to a
 	ORA 	A		; if ? found not zero
 	RET 			; return to caller
;
; ** COMMAND TABLE ASCII WORDS 
;
CMDTBL:	DB	'DIR   '	; directory command
	DB	'DIRS  '	; system files  dir command
	DB	'DEL   '	; deleye file(s) command
	DB	'SHOW  '	; type file to console command
	DB	'SAVE  '	; save tpa area to disk
	DB	'REN   '	; rename file command
	DB	'USER  '	; change active user command
	DB	'PUBLIC'	; toggle public flag command
	DB	'LABEL '	; change volume label command
	DB	'SIZE  '	; compute file size
	DB	'COPY  '	; single file to file copy
	DB	'VERS  '	; display version of CCP
	DB	'RESET '	; reset drive 
	DB	'PROMPT'	; prompt modification
	DB	'CLS   '	; clear screen
;
; ** CHECK RESIDENT CCP COMMAND ASKED 
;
CHKCMD:	LXI 	H,CMDTBL	; point to command table
 	MVI 	C,0		; count to zero
CHKCM1:	MOV 	A,C		; get count
 	CPI 	15		; 15 commands in table
 	RNC 			; if at end exit
 	LXI 	D,DEFFCBN	; point to deffcb
 	MVI 	B,6		; resident commands only 6 long
CHKCM2:	LDAX	D		; get first of command
 	CMP 	M		; compare with table
 	JNZ 	CHKCM3		; not that take next
 	INX 	D		; increment table
 	INX 	H		; and command
 	DCR 	B		; decrement count long
 	JNZ 	CHKCM2		; check next
 	LDAX	D		; get last
 	CPI 	' '		; space
 	JNZ 	CHKCM4		; no tpa command
 	MOV 	A,C		; count to a 
 	RET 			; return with offset in table
CHKCM3:	INX 	H		; next byte from table
 	DCR 	B		; until count zero
 	JNZ 	CHKCM3		; to point to first of next commmand
CHKCM4:	INR 	C		; next command number
 	JMP 	CHKCM1		; loop again
;
; ** CLEAR SCREEN
;
CLS:	MVI	A,FF		; clear screen character
	CALL	COUT		; to console
	JMP	PROMPT		; back to prompt
;
; ** OUTPUT PUBLIC FLAG
;
PROMP:	LDA	PUBLIC		; get public flag
	ORA	A		; test it
	MVI	A,'U'		; setup User active 	
	JZ	COUT		; not active then return
	MVI	A,'P'		; setup Public active
	JMP	COUT		; to console
;
; ** OUTPUT USER NUMBER
;
PROMU:	CALL	GETUSR		; get user number
	MOV	L,A		; move it to l
	MVI	H,0		; h to zero
	JMP	DECOUT		; decimal to screen
;
; ** OUTPUT ESCAPE FOR PROMPT
;
PROME:	MVI	A,ESC		; setup escape char
	JMP	COUT		; echo it
;
; ** OUTPUT DISK FREE SPACE
;
PROMS:	CALL	FREESZE		; calculate free size of disk
	LHLD	COUNTER		; get it
	CALL	DECOUT		; display it
	MVI	A,'k'		; kilobyte char
	JMP	COUT		; output it
;
; ** OUTPUT CURRENT DISK
;
PROMD: 	CALL	GETDSK		; get current disk
 	ADI 	'A'		; make ascii
 	JMP	COUT		; to console
;
; ** ENTER CCP CLEAR RESIDENT COMMAND
; 
CCPCLR:	XRA 	A		; clear ccp command buffer
 	STA 	CCPKC		; to ignore coldboot command
;
; ** ENTER AND INIT CCP DRIVE IN C
;
CCP:	LXI 	SP,STACK	; clear stack
 	PUSH	B		; save drive and user
 	MOV 	A,C		; get it
 	RAR 			; shift
 	RAR 			; 4
 	RAR 			; bits
 	RAR 			; to get user number
 	ANI 	15		; mask out
 	MOV 	E,A		; to e
 	CALL	GPUSER		; set user
 	CALL	RSTDSK		; reset disk system
 	STA 	DOLLAR		; save return code in dollar file active flag
 	POP 	B		; restore drive number
 	MOV 	A,C		; get it
 	ANI 	15		; mask out
 	STA 	CURDSK		; save for ccp
 	CALL	SELDSK		; select that disk
 	LDA 	CCPKC		; get ccp command buffer count
 	ORA 	A		; test it
 	JNZ 	GOCCP		; if not zero execute that command
;
; ** CLEAR STACK SETUP AND OUTPUT PROMPT 
;
PROMPT:	LXI 	SP,STACK	; clear stack
 	CALL	CRLF		; new line
	LXI	H,PROMSTR-1	; point to prompt string buffer -1
PROMP2:	INX	H		; next character
	PUSH	H		; save this address
	MOV	A,M		; get an byte
	ORA	A		; zero yes end of prompt
	JZ	PROMP3		; exit prompt printing
	CPI	'$'		; dollar sign ???
	JNZ	PROMP8		; normal character echo it
	POP	H		; get back
	INX	H		; get next char
	PUSH	H		; save back
	MOV	A,M		; to acu
	CPI	'P'		; public flag demand ?
	CZ	PROMP		; yes do it
	CPI	'S'		; size demand ?
	CZ	PROMS		; yes do it
	CPI	'U'		; user demand ?
	CZ	PROMU		; yes do it
	CPI	'D'		; drive demand ?
	CZ	PROMD		; yes do it
	CPI	'E'		; escape demended
	CZ	PROME		; print escape
	JMP	PROMP9		; next char
PROMP8:	CALL	COUT		; output character normal
PROMP9:	POP	H		; restore pointer
	JMP	PROMP2		; loop again
;
PROMP3:	CALL	GNXTCM		; get next command line full
;
GOCCP:	CALL	DMA80		; set dma to 80
 	CALL	GETDSK		; get current disk 
 	STA 	CURDSK		; save in ccp
 	CALL	SETFCB		; set default fcb
 	CNZ 	ERREXT		; not zero error
 	LDA 	TMPDSK		; get new disk from fcb setup
 	ORA 	A		; test it	
 	JNZ 	LDTPA		; new one demanded ? yes then force TPA load
 	CALL	CHKCMD		; resident command ? 
;
; ** GET ADDRESS OF COMMAND
;
 	LXI 	H,CMDTAB	; hl point to table
 	MOV 	E,A		; count in e
 	MVI 	D,0		; d to zero
 	DAD 	D		; add two times to
 	DAD 	D		; get the address
 	MOV 	A,M 		; get low byte
 	INX 	H		; point to low
 	MOV 	H,M		; get high byte
 	MOV 	L,A		; low to l
 	PCHL			; jump to there
;
; ** COMMAND TABLE
;
CMDTAB:	DW	DIR		; dir command
	DW	DIRS		; dir for hidden files	
 	DW	DEL		; delete file command
 	DW	SHOW		; show file command
 	DW	SAVE		; save tpa erea command
 	DW	REN		; rename file command
 	DW	USER		; change user command
 	DW	TGLPUB		; toggle public flag command
	DW	VOL		; change volume label command
	DW	SIZE		; compute file size
	DW	COPY		; single file to file copy  
	DW	VER		; display version to screen	
	DW	RSET		; reset drive
	DW	CHPROM		; change prompt
	DW	CLS		; clear screen
	DW	LDTPA		; load program from disk routine
;
; ** OUTPUT NO FILE ERROR
;
ERRFL:	LXI 	B,FLERR		; point to no file error message
 	JMP 	OUTSTR		; output it
FLERR:	DB	'File(s) not found',CR,LF,0
;
; ** CONVERT DECIMAL ASCII TO BINAIR
;
DECBIN:	CALL	SETFCB		; setup default fcb wwith next part
 	LDA 	TMPDSK		; temp disk demanded ?
 	ORA 	A		; test it
 	JNZ 	ERREXT		; yes then error
 	LXI 	H,DEFFCBN	; point to name of it	
 	LXI 	B,11		; B=0 C=11 
DECBN1:	MOV 	A,M		; get first
 	CPI 	' '		; space ?
 	JZ	DECBN2		; yes then all done
 	INX 	H		; point already to next
 	SUI 	'0'		; substract 0
 	CPI 	10		; nummeric ?
 	JNC 	ERREXT		; no then error
 	MOV 	D,A		; to d
 	MOV 	A,B		; get count
 	ANI 	0E0H		; ??
 	JNZ 	ERREXT		; then error
 	MOV 	A,B		; get byte value
 	RLC 			; *2
 	RLC 			; *4
 	RLC 			; *8
 	ADD 	B		; *9
 	JC	ERREXT		; overflow then error
 	ADD 	B		; *10
 	JC	ERREXT		; overflow then error
 	ADD 	D		; add to result
 	JC	ERREXT		; overflow 255 then error
 	MOV 	B,A		; new count to b
 	DCR 	C		; loop count down
 	JNZ 	DECBN1		; not all done loop again
 	RET 			; job finished
DECBN2:	MOV 	A,M		; get next byte 
 	CPI 	' '		; space ?
 	JNZ 	ERREXT		; no error
 	INX 	H		; point to next
 	DCR 	C		; decrement count
 	JNZ 	DECBN2		; test next
 	MOV 	A,B		; get back char 
 	RET 			; return
;	
;
; ** SHIFT HL ONE TO RIGHT
;
SHFTHL:	ORA	A		; clear carry
	MOV	A,H		; get l	
	RAR			; shift
	MOV	H,A		; save
	MOV	A,L		; get h
	RAR			; shift
	MOV	L,A		; save
	RET			; return	
;
; ** MOVE BY DEFAULT 3 BYTES
;
MOVE3:	MVI 	B,3		; count to 3 and move
;
; ** MOVE HL TO DE COUNT B
;
MOVE:	MOV 	A,M		; get a byte
 	STAX	D		; save it
 	INX 	H		; bump source
 	INX 	D		; bump destination
 	DCR 	B		; decrement count
 	JNZ 	MOVE		; loop until all done
 	RET 			; return to caller
;
; ** GET BYTE FROM DMA BUFFER FOR DIR COMMAND AFTER SEARCH
;
GETBS:	LXI 	H,DEFDMA	; dma buffer at 80H	
 	ADD 	C		; add offset
 	CALL	ADDOFF		; get address
 	MOV 	A,M		; get the byte
 	RET 			; return
;
; ** FORCE OTHER DRIVE THAN DEFAULT AND SELECT DRIVE
;
SETND:	XRA 	A		; force default disk in fcb
 	STA 	DEFFCB		; do it
 	LDA 	TMPDSK		; get alternate drive 
 	ORA 	A		; test it
 	RZ			; ok go on
 	DCR 	A		; else decrement it
 	LXI 	H,CURDSK	; point to current disk saving byte	
 	CMP 	M		; compare
 	RZ			; same ok
 	JMP 	SELDSK		; else select it
;
; ** RESET DEFAULT DRIVE BACK
;
RESND:	LDA 	TMPDSK		; get default drive from fcb
 	ORA 	A		; test it
 	RZ			; whas it default ok exit
 	DCR 	A		; make good for setdsk	
 	LXI 	H,CURDSK	; point to current disk	
 	CMP 	M		; compare
 	RZ			; ok then exit
 	LDA 	CURDSK		; else restore default 
 	JMP 	SELDSK		; disk
;
; ** SAVE DRIVE OUT FCB OK FOR SELECT
;
SAVEDRV:LDA	TMPDSK		; get drive
	ORA	A		; test it 
	JZ	SVDR02		; zero ok get drive
	DCR	A		; make ok ( 0 for drive A )
SVDR01:	MVI	M,0		; default disk in fcb
	DCX	H		; decrement pointer
	MOV	M,A		; save it one below
	INX	H		; correct fcb pointer
	RET			; return
SVDR02:	PUSH	H		; save address of fcb
	CALL	GETDSK		; get current disk
	POP	H		; restore pointer to it
	JMP	SVDR01		; and save in pointer -1
;
; ** SET ALTERNATE DRIVE
;
ALTDRV:	DCX	D		; one lower
	LDAX	D		; get disk to select 
	INX	D		; restore pointer
	PUSH	D		; save it
	CALL	SELDSK		; set that disk 
	POP	D		; restore pointer
	RET 			; and return
;
; ** SEARCH FOR LABEL ON DIRECTORY
;
FNDLAB:	CALL	GETUSR		; get user number
	PUSH	PSW		; save it
	MVI	E,0		; setup user 0
	CALL	GPUSER		; set it
	LXI	D,ANYONE	; point to anyone fcb
	CALL	SHFRST		; find first entry
FNDLB1:	JZ	FNDLB5		; end of disk exit no label
	CALL	GETADR		; get address into dma buffer
	MVI	A,11		; byte 11 is volume label flag
	CALL	GETBS		; get it
	RAL			; flag in carry
	JC	FNDLB2		; yes is label 
	CALL	SHNEXT		; no try next
	JMP	FNDLB1		; if label is there		
FNDLB2:	MVI	B,1		; ok label filename point to first 
	LXI	D,LABEL		; point to recieve buffer
FNDLB3:	MOV	A,B		; counter to a
	CPI	12		; last one
	JZ	FNDLB4		; yes done get it
	CALL	GETBS		; get byte
	ANI	7FH		; mask of all flags
	STAX	D		; save in buffer
	INX	D		; bump index
	INR	B		; increment count
	JMP	FNDLB3		; loop for next
FNDLB4:	POP	PSW		; restore user number
	MOV	E,A		; to e
	CALL	GPUSER		; reset it
	MVI	A,00		; acu to zero no error
FNDLB6:	ORA	A		; set flags
	RET			; return 
FNDLB5:	POP	PSW		; restore user number
	MOV	E,A		; to e
	CALL	GPUSER		; reset it
	MVI	A,0FFH		; acu to -1
	JMP	FNDLB6		; set flags error 	
;
; ** DISPLAY VERSION TO SCREEN
;
VER:	LXI	B,VERMSG
	CALL	OUTSTR
	JMP	PROMPT
VERMSG:	DB	'CP/M 2.2 / enhanced CCP version '
	DB	VERS+'0','.',REVN+'0'
	DB	' running on the NotSoSimple8085 board',0
;
; ** RESET DRIVE 
;
RSET:	CALL	SETFCB		; get fcb to get drive wanted reset
	LDA	DEFFCB+1	; point to first of filename
	CPI	' '		; should be space
	JNZ	ERREXT		; no error
	LDA	TMPDSK		; get wanted disk
	ORA	A		; test it
	JNZ	RSET0		; not zero ok
	CALL	GETDSK		; else get current one
	INR	A		; adjust
RSET0:	PUSH	PSW		; save reset disk number
	LXI	H,1		; setup 1 for vector of A:
RSET1:	DCR	A		; decrement drive number
	JZ	RSET2		; zero vector is ok
	DAD	H		; shift bit 1 left
	JMP	RSET1		; loop next until correct vector set
RSET2:	XCHG			; vector to de
	MVI	C,37		; reset drive bdos opcode
	CALL	BDOS		; do it	
	CALL	GETDSK 		; get current disk
	INR	A		; adjust
	MOV	B,A		; save to b
	POP	PSW		; get resetted disk
	CMP	B		; compare
	JNZ	PROMPT		; not same ok prompt
	CALL	RSTDSK		; reset disk is current one force reset of it
	JMP	PROMPT		; and exit
;
; ** CHANGE PROMPT COMMAND
;
CHPROM:	LHLD	PBCCP		; point to parm string
	XCHG			; to de
	CALL	NONSPC		; first non space
	JZ	PROMPT		; not found exit
	LXI	H,PROMSTR	; prompt string saving	
CHPROM1:LDAX	D		; get a byte
	MOV	M,A		; to memory
	ORA	A		; test end of line
	JZ	PROMPT		; zero ok exit
	INX	D		; next character
	INX	H		; next saving
	JMP	CHPROM1		; loop again
;
; ** CHANGE VOLUME LABEL COMMAND
;
VOL:	CALL	GETUSR		; get user number
	PUSH	PSW		; save it
	MVI	E,0		; setup user 0
	CALL	GPUSER		; set it
	CALL	SETFCB		; set fcb to get drive
	CALL	SETND		; do it
	LDA	DEFFCBN		; get first character
	CPI	' '		; must be space
	JNZ	VOL9		; not so error	
	CALL	FNDLAB		; find label on disk 
	JNZ	VOL1		; not there skip
	LXI	D,DEFFCBN	; found
	LXI	H,LABEL		; get name
	MVI	B,11		; to default fcb
	CALL	MOVE		; move it
	LXI	D,DEFFCB	; point to fcb	
	CALL	DELFL		; delete it
VOL1:	LHLD	PBCCP		; point to after drive in ccp buffer 
	XCHG			; to de
	CALL	NONSPC		; first non space
	JZ	VOL5		; no label defined exit
	XCHG			; back to hl
	LXI	D,DEFFCBN	; point to deffcbn
	MVI	B,11		; 11 characters maximum
VOL2:	MOV	A,M		; get one
	ORA	A		; test it
	JZ	VOL3		; zero exit loop
	STAX	D		; save it
	INX	H		; next sourc
	INX	D		; next destination
	DCR	B		; decrement count
	JNZ	VOL2		; loop again
	JMP	VOL4		; skip space filling
VOL3:	MVI	A,' '		; fill up with spaces
	STAX	D		; until
	INX	D		; end of
	DCR	B		; fcb
	JNZ	VOL3		; to clean it up
VOL4:	LXI	D,DEFFCB	; point to fcb
	PUSH	D		; save once
	CALL	CREAFL		; create volume label
	LDA	DEFFCB+11	; get byte where volume flag bit is
	ORI	80H		; set it
	STA	DEFFCB+11	; resave it
	POP	D		; get pointer to fcb 
	CALL	SETATR		; set attributes
VOL5:	POP	PSW		; get back user number
	MOV	E,A		; to e
	CALL	GPUSER		; set back user number
	CALL	RESND		; reset temp drive
	JMP	PROMPT		; ignore rest of ccp buffer and go prompt
VOL9:	POP	PSW		; reset first user
	MOV	E,A		; number like above
	CALL	GPUSER		; before
	CALL	RESND		; reset new drive
	JMP	ERREXT		; to jump to error		
;
; ** COMPUTE FILE SIZE
;
SIZE:	CALL	SETFCB		; setup fcb
	JNZ	ERREXT		; unambigious filename ? no error
	CALL	SETND		; alternate drive
	CALL	OPNDEF		; open file
	CZ	ERRFL		; error ?
	JZ	PROMPT		; yes exit
	MVI	C,35		; setup file size opcode
	LXI	D,DEFFCB	; and def fcb
	CALL	BDOS		; get size
	LXI	B,SZMSG0	; pre message
	CALL	OUTSTR		; new line and message
	LHLD	PBCCPA		; point to filename enterd
	CALL	OUTSHL		; print file name
	LXI	H,SZMSG1	; output
	CALL	OUTSHL		; records message
	LHLD	RANREC		; get record number
	CALL	DECOUT		; output it
	LHLD	RANREC		; get it back
SIZE0:	MOV	A,L		; low to a
	ANI	7		; mask out to 8 ( 1Kb is 8 records )
	JZ	SIZE1		; until on kbyte boundary
	INX	H		; not yet increment
	JMP	SIZE0		; until so
SIZE1:	MVI	B,3		; divide by 8
SIZE2:	CALL	SHFTHL		; to shift 3 times hl
	DCR	B		; loop counter
	JNZ	SIZE2		; until zero
	PUSH	H		; save count now in kbyte
	LXI	H,SZMSG2	; output size
	CALL	OUTSHL		; message
	POP	H		; get back kbytes
	CALL	DECOUT		; output it
	LXI	H,SZMSG3	; pre message
	CALL	OUTSHL		; to screen
	LDA	DEFFCB+9	; get r/o flag
	RAL			; flag into carry
	JC	SIZE3		; carry yes then r/o
	MVI	A,'W'		; setup read write file
	JMP	SIZE4		; output it
SIZE3:	MVI	A,'O'		; read only string
SIZE4:	CALL	COUT		; output it
	JMP	RSTPRM		; exit to prompt
;
SZMSG0:	DB	'File ',0
SZMSG1:	DB	' records: ',0
SZMSG2:	DB	' / space(Kb): ',0  		
SZMSG3:	DB	' / access: R/',0
;
; ** COPY FILE TO FILE COMMAND
;
COPY:	XRA	A		; clear acu
	STA	OFFBUF		; clear end flag ( offbuf not used here )
;
	LXI	H,SRCFCB-1	; point to fcb area
	MVI	B,68		; 2 fcb's is 68 bytes ( 66+2 )
COPY0:	MVI	M,0		; clear it
	INX	H		; point to next
	DCR	B		; decrement count
	JNZ	COPY0		; loop until all zero
;
	CALL	SETFCB		; get first file name ( source )
	JNZ	ERREXT		; no ambigious file authorised
	LXI	H,DEFFCB	; point to fcb
	LXI	D,SRCFCB	; point to source fcb
	MVI	B,12		; 12 bytes
	CALL	MOVE		; move it
	LXI	H,SRCFCB	; repoint to fcb	
	CALL	SAVEDRV		; save correct drive in fcb-1
	LXI	D,DSTFCB	; and to destination
	MVI	B,12		; 12 bytes 
	CALL	MOVE		; move already if only drive entered in dest
;
	CALL	SETFCB		; get next fcb ( destination )
	LXI	H,DEFFCB+1	; point to first character of name
	MOV	A,M		; get it
	CPI	' '		; space ?
	JZ	COPY1		; yes only drive entered
	LXI	D,DSTFCB	; point to destination fcb
	DCX	H		; and hl to deffcb
	MVI	B,12		; move 12 bytes
	CALL	MOVE		; for name of it
;
COPY1:	LXI	H,DSTFCB	; destination
	CALL	SAVEDRV		; save drive of it
;
	LXI	H,DSTFCB-1	; point to destination file 
	LXI	D,SRCFCB-1	; poi,t to source file
	MVI	B,14		; 13 byte of file plus drive
COPY02:	LDAX	D		; get an byte 
	CMP	M		; compare 
	JNZ	COPY08		; not same ok copy
	INX	H		; take next
	INX	D		; on sourece and destination
	DCR	B		; decrement count
	JNZ	COPY02		; not at end go on
	JMP	PROMPT		; same file on same disk do nothing
;
COPY08:	LXI	D,SRCFCB	; point to source	
	CALL	ALTDRV		; set drive for it
	CALL	OPENFL		; open it
	JZ	COPY50		; error file not found
	LXI	D,DSTFCB	; point todestination
	CALL	ALTDRV		; alternate drive	  
	PUSH	D		; save arg
	CALL	DELFL		; delete it
	POP	D		; restore address
	CALL	CREAFL		; create it
	JZ	COPY49		; error no dir space
;			
COPY1A:	LXI	H,DEFDMA	; dma to 80
	PUSH	H		; save it on stack
	LXI	D,SRCFCB	; source file
	CALL	ALTDRV		; alternate drive
;
COPY2:	POP	H		; get address
	LXI	D,RECLEN	; setup  1 record length
	DAD	D		; add it
	PUSH	H		; save again
	LXI	D,STRTCCP	; point to max mem
	MOV	A,H		; check h
	CMP	D		; with high max address
	JNZ	COPY3		; not yet go on reading
	MOV	A,L		; get low 
	CMP	E		; compare with low max address
	JZ	COPY4	 	; save ok first write	
COPY3:	XCHG			; to de
	CALL	SETDMA		; set dma
	LXI	D,SRCFCB	; source file
	CALL	RDSEQ		; read a record	
	JZ	COPY2		; no error code returned read next
;
	MVI	A,1		; end flag on
	STA	OFFBUF		; use offset buffer byte for it 	
;
COPY4:	POP	H		; get end address
	SHLD	ADSAVE		; save it
	LXI	D,DSTFCB	; destnation fcb
	CALL	ALTDRV		; alternate drive	
	LXI	H,DEFDMA	; begin of buffer
	PUSH	H		; save on stack
COPY5:	POP	H		; get address
	LXI	D,RECLEN	; setup 128
	DAD	D		; add it
	PUSH	H		; save it back
	XCHG			; to de
	LHLD	ADSAVE		; get end address 
	MOV	A,H		; get high
	CMP	D		; compair
	JNZ	COPY6		; not same go on
	MOV	A,L		; get low
	CMP	E		; compair 
	JZ	COPY7		; same exit
COPY6:	CALL	SETDMA		; normal write set dma
	LXI	D,DSTFCB	; point to fcb	
	CALL	WRTSEQ		; write a sector
	JNZ	COPY48		; ok no then no more space
	JMP	COPY5		; loop again
;
COPY7:	POP	H		; clear stack
	LDA	OFFBUF		; get end flag
	ORA	A		; test it
	JZ	COPY1A		; not at end of file read next part
;
	LXI	D,DSTFCB	; point to destination fcb
	CALL	CLOSFL		; close it
	JZ	COPY46		; error can not close
;
COPY99:	LDA	CURDSK		; get current disk
	CALL	SELDSK		; select it back
	CALL	DMA80		; reset dma address
	JMP	PROMPT		; and exit to prompt
;
COPY50:	CALL	ERRFL		; file error
	JMP	COPY99		; and exit
;
COPY49:	LXI	B,NDRMSG	; no more  directory space
COPY49A:CALL	OUTSTR		; output string
	JMP	COPY99		; done exit
NDRMSG:	DB	'Directory full',CR,LF,0
;
COPY48:	LXI	B,MSNSP		; no space message
	JMP	COPY49A		; output and exit 
;
COPY46:	LXI	B,CNCMSG	; can not close message	
	JMP	COPY49A		; output and exit
CNCMSG:	DB	'Can',39,'t close file',CR,LF,0
;
; ** LIST DIRECTORY
;
DIRS:	MVI	A,1		; hidden file print
	JMP	DIR0		; and go
;
DIR:	MVI	A,0		; set normal dir flag
DIR0:	STA	DIRFLG		; save it
	LXI	H,0		; clear HL
	SHLD	FILECNT		; clear file count	
	CALL	SETFCB		; get fcb
 	CALL	SETND		; alternate drive demanded ?
	CALL	GETDSK		; get drive
	ADI	'A'		; make ascii 
	STA	DIRMSG1		; store in string
	LXI	B,DIRMSG	; point to message
	CALL	OUTSTR		; output it
	CALL	FNDLAB		; try to find label
	JNZ	DIR51		; not there ? 	
	LXI	H,LABOK		; print label message
	JMP	DIR52		; to screen
DIR51:	LXI	H,LABNOK	; print no label message
DIR52:	CALL	OUTSHL		; to screen
  	LXI 	H,DEFFCBN	; point to default fcb	
 	MOV 	A,M		; get first byte
 	CPI 	' '		; space 
 	JNZ 	DIR2		; no filename defined
 	MVI 	B,11		; else setup 11
DIR1:	MVI 	M,'?'		; to fill fcb
 	INX 	H		; with all ?
 	DCR 	B		; until
 	JNZ 	DIR1		; fcb full
DIR2:	MVI 	E,0		; e to zero
 	PUSH	D		; save it
 	CALL	FDFFCB		; search for first
 	CZ	ERRFL		; if not found no file message
DIR3:	JZ	DIR11		; and exit dir command
 	CALL	GETADR		; get address of file in dma buffer
	MVI	A,11		; get volume label flag offset
	CALL	GETBS		; get the byte
	RAL			; in carry
	JC	DIR10		; volume name yes skip
 	MVI 	A,10		; point to 10 for hidden file flag 
	CALL	GETBS		; get that byte
 	RAL 			; rotate flag in carry
 	PUSH	PSW		; save carry
	LDA	DIRFLG		; get dir flag
	ORA	A		; test it
	JZ	DIR35		; zero then normal dir
	POP	PSW		; restore carry
	JNC	DIR10		; hidden not set skip print
	JMP	DIR36		; hidden so print it
DIR35:	POP	PSW		; restore carry
	JC	DIR10		; skip print if hidden
DIR36:	POP 	D		; get count of files on a line
 	MOV 	A,E		; count to a
 	INR 	E		; increment e
 	PUSH	D		; save again
 	ANI 	3		; mask to 3
 	PUSH	PSW		; save
 	JNZ 	DIR4		; not at end of line go on skip crlf
 	CALL	CRLF		; new line
 	PUSH	B		; save bc
 	CALL	GETDSK		; get disk active
 	POP 	B		; restore bc
 	ADI 	'A'		; make ascii
 	CALL	COUTSB		; output it
 	MVI 	A,':'		; output :
 	CALL	COUTSB		; after it
 	JMP 	DIR5		; skip : only
DIR4:	CALL	SPACE		; space : space between files
 	MVI 	A,':'		; : 
 	CALL	COUTSB		; out
DIR5:	CALL	SPACE		; space after it
 	MVI 	B,1		; point to first character
	LHLD	FILECNT		; point to file count
	INX	H		; increment count
	SHLD	FILECNT		; save back
DIR6:	MOV 	A,B		; this to a c contains offset in dma buffer
 	CALL	GETBS		; call real address and get byte
 	ANI 	7FH		; mask to 7F
 	CPI 	' '		; space ?
 	JNZ 	DIR8		; no continue printing
 	POP 	PSW		; get files / line count
 	PUSH	PSW		; save it back
 	CPI 	3		; at last
 	JNZ 	DIR7		; no 
 	MVI 	A,9		; setup 9 for first of extend 
 	CALL	GETBS		; get that byte
 	ANI 	01111111B	; mask out
 	CPI 	' '		; space ?
 	JZ	DIR9		; yes 
DIR7:	MVI 	A,' '		; have to print spaces
DIR8:	CALL	COUTSB		; to console
 	INR 	B		; increment count of it
 	MOV 	A,B		; get count
 	CPI 	12		; if 12 position end of filename extend
 	JNC 	DIR9		; not yet
 	CPI 	9		; if 9 position end of filename part
 	JNZ 	DIR6		; continue printing filename
 	CALL	SPACE		; output space
 	JMP 	DIR6		; loop again until file printed
DIR9:	POP 	PSW		; clear stack
DIR10:	CALL	KBHIT		; keybord hit
 	JNZ 	DIR19		; yes exit
 	CALL	SHNEXT		; else search next 
 	JMP 	DIR3		; and loop again 
DIR11:	POP 	D		; clear stack
	CALL	FREESZE		; get disk free size
	LXI	B,FREMSG0	; output free size message
	CALL	OUTSTR		; to console
	LHLD	COUNTER		; get free size
	CALL	DECOUT		; display in decimal
	LXI	H,FREMSG1	; second message
	CALL	OUTSHL		; to screen 
	LHLD	BLKSIZE		; get block size
	CALL	DECOUT		; print it
	LXI	H,FREMSG2	; last message
	CALL	OUTSHL		; print it
	MVI	C,29		; setup get R/O vector bdos code
	CALL	BDOS		; bdos get it for us
	PUSH	H		; save it
	CALL	GETDSK		; get disk of dir command
	INR	A		; increment for loop test
	MOV	B,A		; set it in B acu used by shift
	POP	H		; restore vector
DIR12:	DCR 	B		; correct drive selected for bit in hl ?
	JZ	DIR13		; done get bit
	CALL	SHFTHL		; shift to get next
	JMP	DIR12		; and try iof correct bit now
DIR13:	MOV	A,L		; get ower vector
	ANI	1		; mask out the needed bit
	JNZ	DIR14		; zero no then R/O
	MVI	A,'W'		; setup R/W
	JMP	DIR15		; go echo it
DIR14:	MVI	A,'O'		; setup R/O
DIR15:	CALL	COUT		; echo it
	LXI	H,FREMSG3	; message 3
	CALL	OUTSHL		; to screen
	LDA	FILECNT		; get  file count
	MOV	L,A		; to l
	MVI	H,0		; h to zero
	CALL	DECOUT		; output count	
	JMP 	RSTPRM		; and exit dir command
DIR19:	POP	D		; clear stack
	JMP	RSTPRM		; exit dir
;
DIRMSG:	DB	'Directory listing of '
DIRMSG1:DB	' :',0 
;
FREMSG0:DB	CR,LF,'Space : ',0
FREMSG1:DB	' Kbyte / Blocksize : ',0	
FREMSG2:DB	' bytes / Status : R/',0
FREMSG3:DB	' / Files : ',0
;
LABOK:	DB	' has label '
LABEL:	DS	11,0
	DB	CR,LF,0
;
LABNOK:	DB	' has no label',CR,LF,0
;
;
; ** DELETE FILE(S)
;
DEL:	CALL	SETFCB		; get filename
 	CPI 	11		; 11 question marks
 	JNZ 	DEL1		; no go on
 	LXI 	B,ALLSTR	; yes ask if sure to delete all
 	CALL	OUTSTR		; output string
 	CALL	GNXTCM		; get answer
 	LXI 	H,CCPKC		; get count ertered
 	DCR 	M		; decrement
 	JNZ 	PROMPT		; now not zero then more than Y entered
 	INX 	H		; point to answer
 	MOV 	A,M		; get it
 	CPI 	'Y'		; 'Y'
 	JNZ 	PROMPT		; no exit
 	INX 	H		; clear next region to ccp buffer word
 	SHLD	PBCCP		; save it
DEL1:	CALL	SETND		; set alternate drive
 	LXI 	D,DEFFCB	; point to deffcb
 	CALL	DELFL		; delete files
 	INR 	A		; get return code to zero
 	CZ	ERRFL		; if so error
 	JMP 	RSTPRM		; done
ALLSTR:	DB	'Are you sure (Y/N) ? ',0
;
; ** PRINT ASCII FILE TO SCREEN
;
SHOW:	CALL	SETFCB		; set fcb
 	JNZ 	ERREXT		; ok no error
 	CALL	SETND		; force new drive ?
 	CALL	OPNDEF		; open it
 	JZ	SHOW4		; ok yes else erroe
 	CALL	CRLF		; new line
 	LXI 	H,OFFBUF	; point to offset in buffer
 	MVI 	M,0FFH		; init to -1
SHOW1:	LXI 	H,OFFBUF	; point to buffer 	
 	MOV 	A,M		; get it
 	CPI 	RECLEN		; at end
 	JC	SHOW2		; no go on
 	PUSH	H		; save pointer
 	CALL	RDDFFC		; read default fcb seq
 	POP 	H		; restore pointer
 	JNZ 	SHOW3		; error yes exit loop 
 	XRA 	A		; clear acu
 	MOV 	M,A		; pointer to start of buffer
SHOW2:	INR 	M		; increment pointer to first one or next
 	LXI 	H,DEFDMA	; setup 80h
 	CALL	ADDOFF		; add to offset
 	MOV 	A,M		; get that byte
 	CPI 	01AH		; eof ?
 	JZ	RSTPRM		; yes done exit to prompt
;
	CPI	FF		; formfeed ?
 	CNZ	COUT		; else output character
;
 	CALL	KBHIT		; keybord hit ?
 	JNZ 	RSTPRM		; yes exit loop
 	JMP 	SHOW1		; next character
SHOW3:	DCR 	A		; read code to 01 to zero
 	JZ	RSTPRM		; if so end of file
SHOW4:	CALL	RESND		; reset current drive
	CALL	ERRFL		; display file not present
 	JMP 	PROMPT		; and exit to promt
;
; ** SAVE TPA AREA TO DISK
;
SAVE:	CALL	DECBIN		; get count of pages
 	PUSH	PSW		; save it
 	CALL	SETFCB		; setup file
 	JNZ 	ERREXT		; not there error
 	CALL	SETND		; set new drive if asked
 	LXI 	D,DEFFCB	; point to fcb	
 	PUSH	D		; save pointer
 	CALL	DELFL		; delete file
 	POP 	D		; get pointer
 	CALL	CREAFL		; create file
 	JZ	SAVE5		; ok no error
 	XRA 	A		; clear acu
 	STA 	CURREC		; save it
 	POP 	PSW		; get count
 	MOV 	L,A		; to l
 	MVI 	H,00		; h to zero
 	DAD 	H		; double it
 	LXI 	D,TPA		; de to tpa
SAVE1:	MOV 	A,H		; get h
 	ORA 	L		; or with l
 	JZ	SAVE2		; done ? ok
 	DCX 	H		; next
 	PUSH	H		; save counter
 	LXI 	H,RECLEN	; hl to 128
 	DAD 	D		; add to offset
 	PUSH	H		; save offset
 	CALL	SETDMA		; set dma to there
 	LXI 	D,DEFFCB	; def fcb	
 	CALL	WRTSEQ		; write record
 	POP 	D		; get offset
 	POP 	H		; get count
 	JNZ 	SAVE3		; write ok no error
 	JMP 	SAVE1		; loop again
SAVE2:	LXI 	D,DEFFCB	; point to fcb	
 	CALL	CLOSFL		; close file
 	INR 	A		; test return code
 	JNZ 	SAVE4		; not ok error
SAVE3:	LXI 	B,MSNSP		; point to no space erroro	
SAVE9:	CALL	OUTSTR		; display message
SAVE4:	CALL	DMA80		; reset dma address to 80
 	JMP 	RSTPRM		; reset and new prompt
MSNSP:	DB	'Disk full',CR,LF,0
SAVE5:	LXI	B,NDRMSG	; no dir space
	JMP	SAVE9		; jump to output it
;
; ** RENAME FILE
;
REN:	CALL	SETFCB		; set destination fcb
 	JNZ 	ERREXT		; not there ? error
 	LDA 	TMPDSK		; get temp disk
 	PUSH	PSW		; save it
 	CALL	SETND		; set temp disk
 	CALL	FDFFCB		; try to find new name
 	JNZ 	REN5		; file exist error
 	LXI 	H,DEFFCB	; point to fcb +16 for rename file bdos
 	LXI 	D,DEFFCB2	; and start of fcb
 	MVI 	B,10H		; 16 bytes
 	CALL	MOVE		; to move
 	LHLD	PBCCP		; load pointer to buffer of ccp
 	XCHG			; to de
 	CALL	NONSPC		; find first non space
 	CPI 	'='		; is it =
 	JZ	REN1		; yes ok
 	CPI 	'_'		; is it _
 	JNZ 	REN4		; yes ???
REN1:	XCHG			; address back in hl
 	INX 	H		; increment
 	SHLD	PBCCP		; store in pointer
 	CALL	SETFCB		; set source fcb
 	JNZ 	REN4		; if not entered error
 	POP 	PSW		; save question marks count
 	MOV 	B,A		; and to b
 	LXI 	H,TMPDSK	; point to temp disk from destination
 	MOV 	A,M		; get it
 	ORA 	A		; test it ok same
 	JZ	REN2		; ok try next test to rename 
 	CMP 	B		; compair with destiantion drive
 	MOV 	M,B		; save it in fcb
 	JNZ 	REN4		; not same different drive error
REN2:	MOV 	M,B		; save drive in fcb
 	XRA 	A		; clear def drive
 	STA 	DEFFCB		; in fcb
 	CALL	FDFFCB		; find first source file
 	JZ	REN3		; not there error
 	LXI 	D,DEFFCB	; point to fcb
 	CALL	RNMEFL		; rename files
 	JMP 	RSTPRM		; and go to prompt
REN3:	CALL	ERRFL		; display error file
 	JMP 	RSTPRM		; prompt
REN4:	CALL	RESND		; reset new drive
 	JMP 	ERREXT		; error exit drive not the same
REN5:	LXI 	B,FLEXTS	; point to file exits message
 	CALL	OUTSTR		; display string
 	JMP 	RSTPRM		; exit
FLEXTS:	DB	'Filename exists ',CR,LF,0
;
; ** SETUP NEW USER NUMBER
;
USER:	CALL	DECBIN		; get decimal number from stream
 	CPI 	16		; must be < 15
 	JNC 	ERREXT		; greater then error
 	MOV 	E,A		; to e
 	LDA 	DEFFCBN		; point to first char of deffcb
 	CPI 	' '		; space ?
 	JZ	ERREXT		; yes then wrong filename entered
 	CALL	GPUSER		; set new user number
 	JMP 	CMDPEN		
;
; ** LOAD PROGRAM INTO TPA
; 
LDTPA: 	LDA 	DEFFCBN		; load first char of fcbname
 	CPI 	' '		; space then command to load
 	JNZ 	LDTP1		; do it
 	LDA 	TMPDSK		; swap drive demanded ( A: cr )
 	ORA 	A		; test it
 	JZ	CMDPEN		; exit to prompt same as whe are
 	DCR 	A		; decrement it
 	STA 	CURDSK		; save as current disk
 	CALL	SELDSK		; select new current active disk
	CALL	RSLOC4		; update location 4
 	JMP 	CMDPEN		; exit to prompt
LDTP1:	LXI 	D,DEFFCBE	; point to extend of fcb
 	LDAX	D		; get first byte
 	CPI 	' '		; space
 	JNZ 	ERREXT		; no then error
 	PUSH	D		; save pointer to it
 	CALL	SETND		; set new drive if needed
 	POP 	D		; restore pointer
 	LXI 	H,COMEXT	; point to COM string
 	CALL	MOVE3		; move it to fcb
 	CALL	OPNDEF		; open default file
 	JZ	FNOTF		; error file does not exist
 	LXI 	H,TPA		; hl point now to tpa
LDTP2:	PUSH	H		; save 
 	XCHG			; to de
 	CALL	SETDMA		; set dma to there
 	LXI 	D,DEFFCB	; point to fcb
 	CALL	RDSEQ		; read record
 	JNZ 	LDTP3		; error then end of file 
 	POP 	H		; restore pointer to address
 	LXI 	D,RECLEN	; setup 128
 	DAD 	D		; add it
 	LXI 	D,STRTCCP	; de to start of ccp 
 	MOV 	A,L		; l to a
 	SUB 	E		; substract e
 	MOV 	A,H		; h to a
 	SBB 	D		; substract d
 	JNC 	BIGERR		; hl > de no then error
 	JMP 	LDTP2		; read next
LDTP3:	POP 	H		; end of file restore stack
 	DCR 	A		; error code must be 1
 	JNZ 	BIGERR		; else error
 	CALL	RESND		; reset current active drive 
 	CALL	SETFCB		; set next fcb
 	LXI 	H,TMPDSK	; point to temp disk var
 	PUSH	H		; save pointer to it	
 	MOV 	A,M		; get temp drive	
 	STA 	DEFFCB		; store into fcb
 	MVI 	A,16		; next file name 16 bytes after
 	CALL	SETFCBA		; set fcb
 	POP 	H		; restore temp pointer	
 	MOV 	A,M		; get it	
 	STA 	DEFFCB2		; store into fcb2 as drive
 	XRA 	A		; clear acu	
 	STA 	CURREC		; to clear current record	
 	LXI 	D,FCB5C		; point to tpa fcb address	
 	LXI 	H,DEFFCB	; point to default fcb after command	
 	MVI 	B,21H		; 21 long 	
 	CALL	MOVE		; move it
 	LXI 	H,CCPBUF	; point to ccp buffer
LDTP4:	MOV 	A,M		; get a byte
 	ORA 	A		; test it
 	JZ	LDTP5		; zero then after command string file 
 	CPI 	' '		; space
 	JZ	LDTP5		; ok  
 	INX 	H		; increment pointer
 	JMP 	LDTP4		; until a ascii character found 
LDTP5:	MVI 	B,00		; count to zero
 	LXI 	D,DEFDMA+1	; start of command line after command
LDTP6:	MOV 	A,M		; get a byte
 	STAX	D		; store it
 	ORA 	A		; test it
 	JZ	LDTP7		; zero then done
 	INR 	B		; increment count
 	INX 	H		; increment pointer
 	INX 	D		; for source and destination
 	JMP 	LDTP6		; loop for next
LDTP7:	MOV 	A,B		; get count
 	STA 	DEFDMA		; store at buffer for length byte
 	CALL	CRLF		; new line
 	CALL	DMA80		; set dma to 80H
 	CALL	LOC4		; restore location 4
 	CALL	KBHIT		; keybord hit ?
	CPI	ESC		; escape character ?
	JZ	LDTP8		; yes then abort load
	CALL	TPA		; call program 
 	LXI 	SP,STACK	; on return restore stack
 	CALL	RSLOC4		; restore location 4
 	CALL	SELDSK		; select disk
 	JMP 	PROMPT		; put prompt
LDTP8:	LXI	H,ABRMSG	; point to abort message	 
	CALL	OUTSHL		; output it	
	JMP	PROMPT		; back to prompt
;
ABRMSG:	DB	'Program aborted', CR,LF,0 
;
FNOTF:	CALL	RESND		; reset temp drive 
	LXI	B,UNKMSG	; point to message
	CALL	OUTSTR		; print it
	JMP	PROMPT		; to prompt
UNKMSG:	DB	'Unknow command',CR,LF,0
;
BIGERR:	LXI 	B,TOBMSG	; point to to big message
 	CALL	OUTSTR		; to screen
 	JMP 	RSTPRM		; exit
TOBMSG:	DB	'Program too big',CR,LF,0
;
; ** TPA COM EXTEND FILENAME
;
COMEXT:	DB	'COM'		; tap program must have COM extend
;
; ** RESET CURRENT DRIVE AND EXIT TO PROMPT
;
RSTPRM:	CALL	RESND
;
; ** VERIFY OF NOTHING PENDING AFTER COMMAND THEN ERROR ELSE PROMPT
;
CMDPEN:	CALL	SETFCB		; try to set next fcb up
 	LDA 	DEFFCBN		; get first byte of name
 	SUI 	' '		; substract space so should be zero
 	LXI 	H,TMPDSK	; point to temp disk	
 	ORA 	M		; combine
 	JNZ 	ERREXT		; not zero then error
 	JMP 	PROMPT		; else new prompt
;
; ** STACK AREA
;
	DS	32,0		; 32 bytes stack level
STACK:	DS	0
;
;
DOLLAR:	DB	0		; DOLLAR file active flag 
;
; ** SUBMIT FILE FCB
;
BATCHFL:DB	0		; batch file name
 	DB	'$$$     SUB'	; is $$$.sub	
 	DS	2,0		; not used
RECSVE:	DB	0		; used for unknow usage
RECCNT:	DS	17,0		; group buffer
BTHCUR:	DB	0		; current record for reading
;
; ** DEFAULT WORKING FILE CONTROL BLOCK
;
DEFFCB:	DB	0		; default fcb drive
DEFFCBN:DS	8,0		; default fcb name of file
DEFFCBE:DS	3,0		; default fcb extend of file
     	DS  	4,0		; not used in fcb
DEFFCB2:DS  	16,0		; group storage or name is rename
CURREC:	DB  	0		; current record reading or writing
RANREC:	DS	3,0		; random record number bytes	
;
ANYONE:	DB	0,'???????????',0,0,0,0	; volume label search
	DB	0		; drive saving point		
SRCFCB:	DS	33,0		; source file for copy		
	DB	0		; drive saving point
DSTFCB:	DS	33,0		; destination file for copy 
;
; ** TEMPORARY STORAGE BYTES
;
BDSERR:	DB  	0		; temp storage
CURDSK:	DB  	0		; current active drive
TMPDSK:	DB  	0		; temp drive for working 
OFFBUF:	DB  	0		; offset in buffer for counting
;
COUNTER:DW	0		; block counter for size
MAXBLK:	DW	0		; max block on disk
BLKSIZE:DW	0		; block size buffer
ADSAVE:	DW	0		; address pointer saving area
DIRFLG:	DB	0		; dir or dirs flag	
FILECNT:DW	0		; file counter
PROMSTR:DB	'$P|$U|$S|$D:>',0	; default prompt string
			
;
;
     	END	STRTCCP
;
