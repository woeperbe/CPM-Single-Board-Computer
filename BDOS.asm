;
		ORG		0DE00H
;
;	CP/M 2.2 	BDOS  part 
;
;		Disassembled by Marc BUFFET 1986 
;
;	USER DEFINED EQUATES
;
PATCHPB	EQU	1		; public files access enabled
;
BIOS	EQU	0f000H		; bios start address
;
;
;
;
; ASCII EQUATES	
;		
CR	EQU	13		; carriage return
LF	EQU	10		; linefeed
BS	EQU	8		; backspace
VT	EQU	9		; horizontal tab
DEL	EQU	127		; DEL character ( rubout )
ESC	EQU	27		; escape character
;
; ** BIOS JUMP TABLE
;
IOCBT	EQU	BIOS		; cold boot
IOWBT	EQU	BIOS+3		; warm boot
IOSTS	EQU	BIOS+6		; console status
IOCIN	EQU	BIOS+9		; console in
IOCOUT	EQU	BIOS+12		; console out
IOLOUT	EQU	BIOS+15		; list device out
IOPOUT	EQU	BIOS+18		; punch device out
IORIN	EQU	BIOS+21		; reader input
IOHOME	EQU	BIOS+24		; disk home
IOSTDSK	EQU	BIOS+27		; set disk 
IOSTTRK	EQU	BIOS+30		; set track
IOSTSEC	EQU	BIOS+33		; set sector
IOSTDMA	EQU	BIOS+36		; set dma
IOREAD	EQU	BIOS+39		; read CP/M sector 128 bytes
IOWRITE	EQU	BIOS+42		; write sector
IOLSTST	EQU	BIOS+45		; list status ( not needed in bdos )
IOTRANS	EQU	BIOS+48		; sector translate routine
;
;
; **  PAGE ZERO REFERENCES
;
IOBYTE	EQU	3		; iobyte
;
DEFAULT	EQU	4		; default disk
;
;
; ** BDOS CODE STARTS HERE
;
	DS	6,0		; serial number 
;
BDOS:	JMP	BDOSE		; skip error vectors
;
; ** ERROR VECTORS
;
; vectors may be altered to user program a RET instruction
; ignores the error and continue bdos call ! be carefull
;
BADSEC:	DW	BDSEC		; bad sector error
SELECT:	DW	SELERR		; select error
DRIVERO:DW	DRVROE		; drive r/o error
FILERO:	DW	FLROER		; file r/o error
;
; ** BDOS ENTRY POINT
;
BDOSE:	XCHG 			; de to hl
	SHLD	BDOSDE		; save for later
	XCHG 			; restore
	MOV	A,E		; E to a
	STA	RGESVE		; save for later
	LXI	H,0		; hl to zero
	SHLD	BDSERR		; no error ( not yet )
	DAD	SP		; add stack pointer
	SHLD	OSTACK		; save it 
	LXI	SP,NSTACK	; setup save stack
	XRA	A		; clear acu
	STA	TMPDSK		; clear alternate drive
	STA	ADKFLG		; clear alternate drive flag 
	LXI	H,EXITBDOS	; setup return address
	PUSH	H		; to stack
	MOV	A,C		; get opcode demanded
	CPI	41		; overflow 
	RNC			; yes return to return code
	MOV	C,E		; e parm to c
	LXI	H,CMDADR	; point to table
	MOV	E,A		; opcode to e
	MVI	D,0		; d to zero
	DAD	D		; add offset
	DAD	D		; add offset
	MOV	E,M		; get low of address
	INX	H		; next 
	MOV	D,M		; get high of address
	LHLD	BDOSDE		; get initial de back
	XCHG 			; to de and address in hl
	PCHL 			; jump to there
;
; ** BDOS OPCODE JUMP TABLE 
;
CMDADR:	DW	IOWBT		; reset system
	DW	COIN		; console input
	DW	CHROUT		; console output
	DW	RDRIN		; reader input
	DW	IOPOUT		; punsh output
	DW	IOLOUT		; list output
	DW	DICOIO		; direct console io
	DW	GTIOBT		; get io byte
	DW	STIOBT		; put io byte
	DW	PRDSTR		; print string
	DW	RDCOBF		; read console buffer
 	DW	GTCOST		; get console status
	DW	VERSION		; get version number
	DW	RSTDISK		; reset disk system
	DW	BSETDSK		; select disk
	DW	OPENFILE	; open file
	DW	CLOSFILE	; close file
	DW	FIRST		; search for first
	DW	NEXT		; search for next
	DW	DELETE		; delete file
	DW	READSEQ		; read seqsuential
	DW	WRITESEQ	; write sequential
	DW	MAKEFILE	; make file
	DW	RENAME		; rename file
	DW	GETLOGIN	; get login vector
	DW	GETCDISK	; get current disk
	DW	SETDMAAD	; set dma address
	DW	GETALLOC	; get allocation vector
	DW	SDSKRO		; write protect disk
	DW	GETROVEC	; get R/O vector
	DW	SETFILEA	; set file attributes
	DW	GETDSKPRM	; get disk param address
	DW	GETSETUSR	; get/set user code
	DW	READRAND	; read random
	DW	WRITERAND	; write random
	DW	FILESIZE	; compute file size
	DW	SETRNDR		; set random record
	DW	RESETDISK	; reset drive
	DW	RTRN		; unused
	DW	RTRN		; unused
	DW	WRITEZF		; write random with 0 fill
;
; ** BAD SECTOR ERROR
;
BDSEC:	LXI	H,MSG3		; point to bad sector error 
	CALL	ERRSUB		; display error
	CPI	3		; ^C hit yes 
	JZ	0		; so warmboot
	RET			; else return and ignore
;
; ** SELECT ERROR
;
SELERR:	LXI	H,MSG4		; select error message
	JMP	DSPERR		; process
;
; ** DRIVE R/O ERROR
;
DRVROE:	LXI	H,MSG6		; drive r/o message
	JMP	DSPERR		; process
;
; ** FILE R/O ERROR
;
FLROER:	LXI	H,MSG5		; file r/o message
DSPERR:	CALL	ERRSUB		; process
	JMP	0		; and coldboot
;
MSG1:	DB	'Bdos Err On '
MSG2:	DB	' : $'
MSG3:	DB	'Bad Sector$'
MSG4:	DB	'Select$'
MSG5:	DB	'File '
MSG6:	DB	'R/O$'
;
; ** DISPLAY ERROR MESSAGE TO CONSOLE
;
ERRSUB:	PUSH	H		; save message address
	CALL	CRLF		; cr lf to console
	LDA	NDISK		; get current drive
	ADI	'A'		; make ascii
	STA	MSG2		; save into message
	LXI	B,MSG1		; point to general message part
	CALL	PRTSTR		; display it
	POP	B		; restore extend message address
	CALL	PRTSTR		; display it and drop into get character
;
; ** GET CHARACTER
;
GETCH:	LXI	H,PREVCH	; get previous one ( read on echo )
	MOV 	A,M		; to acu
	MVI	M,0		; clear it for next
	ORA	A		; test it
	RNZ			; there was one return
	JMP	IOCIN		; no one present get one
;
; GET CHAR AND ECHO IT
;
CHKECH:	CALL	GETCH		; get character
	CALL 	CONTRL		; check if control code
	RC 			; yes return 
	PUSH 	PSW		; no echo it save it
	MOV	C,A		; to c
	CALL 	CHROUT		; echo it
	POP	PSW		; pop it
	RET			; and return
;
; ** CHECK IF CHARACTER IS CONTROL CODE
;
CONTRL:	CPI	CR		; cr ok accepted
	RZ 			; 
	CPI	LF		; linefeed ok accepted 
	RZ 			;
	CPI	VT		; ht ok accepted
	RZ 			;
	CPI	BS		; backspace ok accepted
	RZ 			;
	CPI	' '		; check if < space
	RET			; return with carry set if so
;
; ** GET CONSOLE STATUS CHECK IF X-OFF HIT
;
XOFXON:	LDA	PREVCH		; get previous character
	ORA	A		; test it
	JNZ	XOF2		; one there skip test
	CALL 	IOSTS		; get status of keybord
	ANI	1		; mask out 
	RZ 			; no one there return
	CALL 	IOCIN		; get character
	CPI	'S'-40H		; X-off ?? 
	JNZ	XOF1		; no go on
	CALL 	IOCIN		; get next keybord hit
	CPI	'C'-40H		; ^C ?
	JZ 	0		; yes then warmboot
	XRA	A		; clear acu
	RET			; and return
XOF1:	STA	PREVCH		; save normal character in PREVIOUS BUFFER
XOF2:	MVI	A,1		; ACU to one
	RET			; and return
;
; ** ECHO CHARACTER TO CONSOLE AND PRINTER IF ASKED
;
ECHO:	LDA 	ENAECH		; output enable flag
	ORA	A		; test it
	JNZ	ECHO1		; ok ??
	PUSH 	B		; save character
	CALL 	XOFXON		; done exit asked ??
	POP	B		; restore
	PUSH 	B		; resave
	CALL 	IOCOUT		; bios cout
	POP	B		; restore
	PUSH 	B		; resave
	LDA	PRTON		; get printer active flag
	ORA	A		; test it
	CNZ	IOLOUT		; if not zero also to printer 
	POP	B		; restore char
ECHO1:	MOV	A,C		; to acu
	LXI	H,CHRPOS	; point to character count 
	CPI	7FH		; maximum 127
	RZ 			; zero ok return
	INR	M		; increment counter
	CPI	' '		; is it < space
	RNC			; yes 
	DCR	M		; resubstract it
	MOV	A,M		; get count
	ORA	A		; test it
	RZ 			; if zero return
	MOV	A,C		; get char
	CPI	BS		; bs ??
	JNZ	ECHO2		; no go on	
	DCR	M		; decrement count
	RET			; and return
ECHO2:	CPI	LF		; is it linefeed
	RNZ			; no exit
	MVI	M,0		; yes zero counter
	RET			; and return
;
; ** OUTPUT CHARACTER AS CONTROL CODE IF SO
;
CTRECH:	MOV	A,C		; character to acu
	CALL 	CONTRL		; check if control code
	JNC	CHROUT		; no normal echo
	PUSH 	PSW		; save it
	MVI	C,'^'		; setup ^
	CALL 	ECHO		; echo it
	POP	PSW		; get character back
	ORI	01000000B 	; ascii uppercase
	MOV	C,A		; to c to echo it
;
; ** ECHO CHARACTER NORMAL OR SPACES IF TAB 
;
CHROUT:	MOV	A,C		; get character
	CPI	VT		; ht ?
	JNZ	ECHO		; no echo it normal
CHROUT1:MVI	C,' '		; setup space
	CALL 	ECHO		; to console
	LDA	CHRPOS		; get pos count
	ANI	7		; mask out 3 lsb ( 8 positions )
	JNZ	CHROUT1		; at end no next space
	RET			; return
;
; ** BACKSPACE CHARACTER
;
BCKSPC:	CALL 	BSOUT		; output backspace
	MVI	C,' '		; setup space
	CALL 	IOCOUT		; output it
BSOUT:	MVI	C,BS		; setup backspace
	JMP	IOCOUT		; output and return
;
; ** PRINT # CRLF AND POSTION ON NEW LINE
;
NXTLN:	MVI	C,'#'		; setup #
	CALL 	ECHO		; output it
	CALL 	CRLF		; crlf
NXTLN1:	LDA	CHRPOS		; get current position
	LXI	H,POSSAV	; get saved position address
	CMP	M		; compare
	RNC			; greater than or equal
	MVI	C,' '		; setup space
	CALL 	ECHO		; display it
	JMP	NXTLN1		; and loop back
;
; ** OUTPUT CRLF TO CONSOLE
;
CRLF:	MVI	C,CR		; cr
	CALL 	ECHO		; output
	MVI	C,LF		; linefeed
	JMP	ECHO		; output
;
; ** PRINT $ TERMINATED STRING
; 
PRTSTR:	LDAX 	B		; get character
	CPI	'$'		; $ 
	RZ 			; yes return
	INX	B		; next
	PUSH 	B		; save pointer
	MOV 	C,A		; character to c 
	CALL 	CHROUT		; echo it
	POP	B		; restore pointer
	JMP	PRTSTR		; loop back
;
; ** READ CONSOLE BUFFER
;
RDCOBF:	LDA 	CHRPOS		; save old cursor position
	STA	POSSAV		; in possav
	LHLD 	BDOSDE		; get buffer
	MOV	C,M		; max count to c
	INX	H		; point to next byte
	PUSH 	H		; save this address
	MVI	B,0		; count to zero
RDCO01:	PUSH 	B		; save count
	PUSH 	H		; save address	
RDCO02:	CALL 	GETCH		; get a character
	ANI	01111111B	; strip off msb
	POP	H		; restore address
	POP	B		; restore count
;
	CPI	CR		; cr 
	JZ 	RDCO17		; yes done
;
	CPI	LF		; linefeed
	JZ 	RDCO17		; yes done
;
	CPI	BS		; backspace
	JNZ	RDCO03		; no skip backspace code
;
	MOV	A,B		; get count
	ORA	A		; test it
	JZ 	RDCO01		; at beginning yes ignore
	DCR	B		; ok decrement count
	LDA	CHRPOS		; get character position
	STA	ENAECH		; enable output
	JMP	RDCO10		; ?? 
;
RDCO03:	CPI	DEL		; is it DEL 
	JNZ	RDCO04		; no skip del code
	MOV	A,B		; get count
	ORA	A		; test it
	JZ 	RDCO01		; at start ignore
	MOV	A,M		; get byte to echo and delete
	DCR	B		; decrement count
	DCX	H		; decrement address
	JMP	RDCO15		; go echo it
;
RDCO04:	CPI	'E'-40H		; ^E		
	JNZ	RDCO05		; no skip code
	PUSH 	B		; save count
	PUSH 	H		; save address
	CALL 	CRLF		; new line
	XRA	A		; clear acu
	STA	POSSAV		; to clear saving position
	JMP	RDCO02		; go get next character
;
RDCO05:	CPI	'P'-40H		; ^P		
	JNZ	RDCO06		; no skip code
	PUSH 	H		; save address
	LXI	H,PRTON		; get printer flag
	MVI	A,1		; setup one
	SUB	M		; substract flag = to XOR it 	
	MOV	M,A		; restore
	POP	H		; get address
	JMP	RDCO01		; go get next character
;
RDCO06:	CPI	'X'-40H		; ^X	
	JNZ	RDCO08		; no skip code
	POP	H		; get address
RDCO07:	LDA	POSSAV		; get saved position
	LXI	H,CHRPOS	; point to current char position	
	CMP 	M		; compair it
	JNC	RDCOBF		; done reenter routine
	DCR	M		; decrement count
	CALL	BCKSPC		; backspace 
	JMP	RDCO07		; loop until all done
;
RDCO08:	CPI	'U'-40H		; ^U
	JNZ	RDCO09		; no skip code
	CALL 	NXTLN		; # and crlf
	POP 	H		; get address
	JMP	RDCOBF		; reenter routine from start 
;
RDCO09:	CPI	'R'-40H		; ^R	
	JNZ	RDCO14		; no skip code
RDCO10:	PUSH 	B		; save counter
	CALL 	NXTLN		; # new line
	POP	B		; get counter
	POP	H		; get address
	PUSH 	H		; resave it
	PUSH 	B		; save count
RDCO11:	MOV	A,B		; get count already entered to a
	ORA	A		; test it
	JZ 	RDCO12		; zero ok all done 
	INX	H		; increment address
	MOV	C,M		; get character
	DCR	B		; decrement count
	PUSH 	B		; save counter
	PUSH 	H		; save address
	CALL	CTRECH		; print character
	POP	H		; restore address
	POP	B		; restore counter
	JMP	RDCO11		; loop until all printed
RDCO12:	PUSH 	H		; save address
	LDA	ENAECH		; get enable flag
	ORA	A		; test it
	JZ 	RDCO02		; zero ignore echo
	LXI	H,CHRPOS	; get current position 
	SUB	M		; substract 
	STA	ENAECH		; save back
RDCO13:	CALL 	BCKSPC		; backspace
	LXI	H,ENAECH	; get enable flag 
	DCR	M		; decrement
	JNZ	RDCO13		; until back zero
	JMP	RDCO02		; get next entry
RDCO14:	INX	H		; increment address
	MOV	M,A		; and save new character in it
	INR 	B		; increment count
RDCO15:	PUSH 	B		; save counter
	PUSH 	H		; save address
	MOV	C,A		; to c
	CALL 	CTRECH		; display it as well
	POP	H		; get address
	POP	B		; get count
	MOV	A,M		; get character back
;
	CPI	'C'-40H		; ^C hit
	MOV	A,B		; get counter
	JNZ	RDCO16		; no ^C continue
	CPI	1		; whas it in first position
	JZ 	0		; yes then warmstart
RDCO16:	CMP 	C		; check count expered
	JC 	RDCO01		; no get next
RDCO17:	POP	H		; else pop address
	MOV	M,B		; move count to buffer +1
	MVI	C,CR		; cr
	JMP	ECHO		; and echo to terminate
;
; ** CONSOLE IN
;
COIN:	CALL 	CHKECH		; get keybord character
	JMP	BDEXTA		; and exit
;
; ** READER IN
;
RDRIN:	CALL 	IORIN		; get reader character
	JMP	BDEXTA		; save and exit
;
; ** DIRECT CONSOLE IO
;
DICOIO:	MOV	A,C		; get parm
	INR	A		; increment
	JZ 	DICOIO1		; whas it 0FFH yes then cin
	INR	A		; if 0FEH THEN STATUS DEMAND ??? 
	JZ 	IOSTS		; get console status
	JMP	IOCOUT		; echo it
DICOIO1:CALL 	IOSTS		; get status
	ORA	A		; test it
	JZ 	EXIT01		; not ready ??? NO return direct
	CALL 	IOCIN		; get character
	JMP	BDEXTA		; exit
;
; ** GET IO BYTE
;
GTIOBT:	LDA	IOBYTE		; get io byte
	JMP	BDEXTA		; and exit	
;
; ** SET IO BYTE
;
STIOBT:	LXI	H,IOBYTE	; point to io byte location
	MOV	M,C		; save it
	RET			; return
;
; ** PRINT STRING ROUTINE
;
PRDSTR:	XCHG 			; address of string in hl
	MOV	C,L		; copy hl
	MOV	B,H		; to bc	
	JMP	PRTSTR		; go print it
;
; ** GET CONSOLE STATUS
;
GTCOST:	CALL 	XOFXON		; get status
;
; ** RETURN FROM IO ROUTINE
;
BDEXTA:	STA	BDSERR		; return it to bdos caller in acu 
RTRN:	RET			; and return
;
; ** 
;
RTRN1:	MVI	A,1		; return 1 
	JMP 	BDEXTA		; and exit
;
ENAECH:	DB 	0		; echo enable ( for backspace )
POSSAV:	DB	0		; current cursor position saving ( ^X )
CHRPOS:	DB	0		; current position
PRTON:	DB	0		; printer enable flag
PREVCH:	DB 	0		; character buffer que
OSTACK:	DW	0		; bdos caller stack address
	DS	48,0		; bdos stack space
NSTACK	EQU	$		; bdos stack
CUSER:	DB	0		; curent active user number
NDISK:	DB	0		; current disk active
BDOSDE:	DW	0		; DE parm from bdos call
BDSERR:	DW 	0		; return code for bdos
;
; ** OUTPUT SELECT ERROR
;
SLERROR:LXI	H,SELECT	; point to select error
;
; ** ERROR MESSAGE INDIRECT JUMP ROUTINE
;
ERROR:	MOV	E,M		; get low address
	INX 	H		; point to next
	MOV 	D,M		; get high addres 
	XCHG 			; to de
	PCHL 			; jump to it
;
; ** MOVE ROUTINE FROM DE TO HL COUNT IN C
;
MOVE:	INR	C		; adjust maybe zero
MOVE01:	DCR	C		; decrement count
	RZ 			; done return
	LDAX 	D		; get a byte from source	
	MOV	M,A		; move to destination
	INX	D		; increment 
	INX	H		; source and destination
	JMP	MOVE01		; loop again
;
; ** 	SET DISK THRU BIOS 
;
SETDSK:	LDA	NDISK		; get disk
	MOV	C,A		; to c
	CALL 	IOSTDSK		; call bios setdisk
	MOV	A,H		; get return address
	ORA	L		; is it zero
	RZ 			; yes return error 
	MOV	E,M		; get low of translation table
	INX	H		; 
	MOV	D,M		; get high of translation table
	INX	H		; save addressesof 3 words in scratch area
	SHLD 	CSTEST		; address of word of number of cs entries 
	INX	H		; on this diskette directory
	INX	H		;
	SHLD 	SAVTRK		; get address of previous track seeked
	INX	H		; increment 
	INX	H		; 2 bytes
 	SHLD 	SAVSAL		; to save previous seeked sector aligned
	INX	H		; increment twoce
	INX 	H		; to get dpb 
	XCHG 			; address in de
	SHLD 	SKEWTB		; save TRANSLATION VECTOR
	LXI	H,DIRBUF	; point to internal buffer
	MVI	C,8		; for DIRBUF,CSV,ALV
	CALL 	MOVE		; move them
	LHLD 	DPB		; load address of disk parm block
	XCHG 			; to de
	LXI	H,SPT		; point to internal dpb block	
	MVI	C,15		; 15 bytes
	CALL 	MOVE		; move it
	LHLD 	DSM		; get number of blocks on disk
	MOV	A,H		; get high byte of it to a
	LXI	H,BLK255	; point to more than 255 block flag
	MVI	M,0FFH		; set it to -1
	ORA	A		; test H
	JZ 	STDSK01		; ok less than 255
	MVI	M,0		; set greater than 255
STDSK01:MVI	A,0FFH		; acu to FF
	ORA	A		; to indicate ok setdisk
	RET			; return to caller
;
; ** CLEAR INTERNAL DRIVE PARMS
;
CLRPRM:	CALL 	IOHOME		; home the drive 
	XRA 	A		; clearacu
	LHLD 	SAVTRK		; load address of SAVED TRACK 
	MOV	M,A		; clear first byte		
	INX	H		; point to next	
	MOV	M,A		; clear next		
	LHLD 	SAVSAL		; point to address of SAVED SECTOR ALIGNED
	MOV	M,A		; clear it
	INX	H		; point to next	
	MOV	M,A		; clear it
	RET			; return
;
; ** GENEREAL READ FILE
;
RDSEC:	CALL 	IOREAD		; read sector
	JMP	RWDONE		; jump to end of job
;
; ** GENERAL WRITE FILE
;
WTSEC:	CALL 	IOWRITE		; write sector and drop in end of job
;
; ** CLEAN UP END PROCESS READ OR WRITE
;
RWDONE:	ORA	A		; test return code
	RZ 			; zero ok
	LXI	H,BADSEC	; point to bad sector error msg vector 
	JMP	ERROR		; display it
;
; ** SEEK FOR DIRECTORY READ WRITE
;
DSEEK:	LHLD 	CNTER		; take dir entry ( 32 bytes )
	MVI 	C,2		; multiply be 4 to make sector offset 
	CALL	ARHL		; do it	
	SHLD 	CURBLK		; save as sector number
	SHLD 	CURCSP		; save as current checksum position
;
; ** SEEK TO SELECTED SECTOR 
;
SEEK:	LXI	H,CURBLK	; get sector to seek
	MOV	C,M		; low to c
	INX 	H 		; bump
	MOV	B,M		; high to b
	LHLD	SAVSAL		; get address of previous track aligned sector 
	MOV	E,M		; to de
	INX 	H		; bump
	MOV 	D,M		; and get high
	LHLD	SAVTRK		; get address to previous seek track
	MOV 	A,M		; get low
	INX 	H		; bump
	MOV 	H,M		; get high
	MOV 	L,A		; low to l
SEEK1:	MOV 	A,C		; get sector to seek
	SUB 	E		; substract
	MOV	A,B		; previous track aligned sector
	SBB	D		; to check if greater than
	JNC	SEEK2		; new is greater	
	PUSH 	H		; save track
	LHLD 	SPT		; get sector/track
	MOV 	A,E		; substract 
	SUB	L		; it
	MOV	E,A		; to get 
	MOV	A,D		; back track aligned 
	SBB	H		; sector
	MOV	D,A		; on one track lower
	POP	H		; get back track		
	DCX	H		; decrement
	JMP	SEEK1		; test again if still less loop again
SEEK2:	PUSH 	H		; sector is greater save track
	LHLD 	SPT		; get spt
	DAD	D		; add aligned sector on track
	JC 	SEEK3		; overflow ok track is ok
	MOV	A,C		; sector to read 
	SUB	L		; minus 
	MOV	A,B		; aligned
	SBB	H		; overflow
	JC 	SEEK3		; no go seek
	XCHG 			; set back ok for next iritation
	POP	H		; get track		
	INX	H		; next track
	JMP	SEEK2		; try next addition of one track
SEEK3:	POP	H		; get tarck
	PUSH 	B		; save sector to read
	PUSH 	D		; save new aligned sector on track just below
	PUSH 	H		; save track
	XCHG 			; track to de
	LHLD 	OFFSET		; get track offset
	DAD	D		; add it
	MOV	B,H		; copy to 
	MOV	C,L		; bc
	CALL 	IOSTTRK		; call settrack in bios
	POP	D		; get new track
	LHLD	SAVTRK		; point to bios scratch area 
	MOV	M,E		; save it
	INX	H		; for next
	MOV 	M,D		; seek
	POP	D		; get track
	LHLD 	SAVSAL		; get new aligned sector on track
	MOV	M,E		; save 
	INX	H		; it for
	MOV 	M,D		; next seek
	POP 	B		; get sector to read
	MOV	A,C		; minus
	SUB	E		; track
	MOV	C,A		; aligned sector
	MOV	A,B		; results
	SBB	D		; in sector
	MOV	B,A		; to read on this track
	LHLD 	SKEWTB		; get translation pointer
	XCHG 			; to de
	CALL 	IOTRANS		; translate thru bios
	MOV	C,L		; copy 
	MOV	B,H		; to bc
	JMP	IOSTSEC		; set sector
;
; ** CALCULATE BLOCKNUMBER OFFSET FROM RECORD NUMBER AND EXTEND 
;
BLKOFS:	LXI	H,BSH		; get block shift factor
	MOV	C,M		; get it
	LDA	RWRECO		; get record
BLKOFS1:ORA	A		; clear carry
	RAR			; rotate to divide	
	DCR	C		; until shiftfactor is zero
	JNZ	BLKOFS1		; loop until so
	MOV	B,A		; save to b
	MVI	A,8		; 8 records in 1kb
	SUB	M		; substract block shift factor
	MOV	C,A		; to c
	LDA	EXTMSK		; get exent masked
BLKOFS2:DCR	C		; decrement
	JZ 	BLKOFS3		; zero ok done
	ORA	A		; clear carry
	RAL			; multiply
	JMP	BLKOFS2		; until zero ????
BLKOFS3:ADD 	B		; add to get offset for blocknumber
	RET
;
; ** GET BLOCKNUMBER FROM USER FCB OFFSET IN BC
;
GETBLK:	LHLD 	BDOSDE		; get user fcb
	LXI	D,16		; setup offset to get block numbers
	DAD	D		; add it
	DAD	B		; add block offset demanded
	LDA	BLK255		; get more than 255 blocks flag
	ORA	A		; test it
	JZ 	GETBLK0		; yes special case
	MOV	L,M		; get block number
	MVI	H,0		; h to zero of course
	RET			; and return
GETBLK0:DAD	B		; add twice demanded offset 2 bytes for 1 blk
	MOV	E,M		; get low blocknumber
	INX	H		; point to next
	MOV	D,M		; get high blocknumber
	XCHG 			; set it in hl
	RET			; and return
;
; ** GET CURRENT ACTIVE BLOCKNUMBER
;
ACTBLK:	CALL 	BLKOFS		; get blocknumber offset
	MOV	C,A		; move offset to c
	MVI	B,0		; b to zero
	CALL 	GETBLK		; get that block number
	SHLD 	CURBLK		; store it for seek 
	RET			; and return
;
; ** TEST IF BLOCK NUMBER IS ZERO ( NOT YET ALLOCATED )
;
BLKZR:	LHLD 	CURBLK		; get blocknumber 
	MOV	A,L		; low to a
	ORA	H		; or with h
	RET			; and return
;
; ** CALCULATE RELATIVE RECORD NUMBER AND ADJUST BLOCKNUMBER
;
CALREC:	LDA	BSH		; get blockshift factor
	LHLD 	CURBLK		; get current block
CALREC1:DAD	H		; shift
	DCR	A		; decrement count
	JNZ	CALREC1		; until record number found
	SHLD 	CURBLS		; save it
	LDA	BLM		; get block mask
	MOV	C,A		; to c
	LDA	RWRECO		; get current record
	ANA	C		; mask out
	ORA	L		; combine with offset
	MOV	L,A		; save again
	SHLD 	CURBLK		; save in current block
	RET			; return
;
; ** GET EXTEND NUMBERADDRESS FROM USER FCB
;
GETEXT:	LHLD 	BDOSDE		; get user fcb address
	LXI	D,12		; setup extend number offset
	DAD	D		; add it
	RET			; and return
;
; ** GET FILE AND USER RECORD NUMBER
;
ADRREC:	LHLD 	BDOSDE		; get uset fcb
	LXI	D,15		; setup offset for record number current ext
	DAD	D		; add it
	XCHG 			; to hl
	LXI	H,17		; offset for record number to read or write
	DAD	D		; add it
	RET			; return
;
; ** GET RECORD NUMBERS AND EXTEND 
;
GETREC:	CALL 	ADRREC		; get record counters 
	MOV	A,M		; get record to read or write
	STA	RWRECO		; save it
	XCHG 			; get address of current record in extent
	MOV	A,M		; get it
	STA	CRECXT		; save it
	CALL 	GETEXT		; get extend number
	LDA	EXM		; get mask
	ANA	M		; mask it ??
	STA 	EXTMSK		; save it
	RET			; return
;
; ** UPDATE USER FCB RECORD NUMBER ( ADD MODE FLAG 0 OR 1 IF NEEDED ) 
;
UPDFCB:	CALL	ADRREC		; get record address
	LDA	MODE		; get mode
	CPI	2		; zero fill ?
	JNZ 	UPDFCB1		; no go on
	XRA	A		; no record update
UPDFCB1:MOV	C,A		; get code
	LDA	RWRECO		; get current record pointed to
	ADD	C		; add new one if needed
	MOV	M,A		; save back
	XCHG 			; get address of extend	
	LDA	CRECXT		; get current extend if new
	MOV	M,A		; save it in fcb
	RET			; and return
;
; ** SHIFT HL reg C BITS RIGHT
;
ARHL:	INR	C		; adjust 
ARHL01:	DCR	C		; decrement count
	RZ 			; ok return
	MOV	A,H		; get h
	ORA	A		; clear carry
	RAR			; shift one right
	MOV	H,A		; save h
	MOV	A,L		; get l
	RAR			; shift one right
	MOV	L,A		; save l
	JMP	ARHL01		; loop again
;
; ** CHECKSUM THE DIRECTORY BUFFER
;
CHKSUM:	MVI	C,128		; dir buffer is 128 bytes
	LHLD 	DIRBUF		; load address of it
	XRA	A		; clear acu initial cs
CHKSM1:	ADD 	M		; add memory
	INX	H		; next byte
	DCR	C		; decrement count
	JNZ	CHKSM1		; loop back
	RET			; return
;
; ** SHIFT HL reg C BITS LEFT
;
ALHL:	INR	C		; adjust c
ALHL01:	DCR	C		; decrement c
	RZ 			; return if all done
	DAD 	H		; shift hl
	JMP	ALHL01		; loop again
;
; ** SET DRIVE BIT VECTOR ON initial in BC new in HL 
;
SETBIT:	PUSH 	B		; save vector
	LDA	NDISK		; get current disk
	MOV	C,A		; to c
	LXI	H,1		; hl to 1 for a
	CALL 	ALHL		; shift until correct bit set
	POP	B		; restore vector
	MOV	A,C		; get c
	ORA	L		; or with new bit
	MOV	L,A		; save in l
	MOV	A,B		; get b 
	ORA	H		; or with new bit
	MOV	H,A		; save 
	RET			; return result in hl
;
; ** GET DRIVE BIT IF R/O OR R/W ( 1 is R/O / 0 is R/W )
;
GETBIT:	LHLD 	ROVECT		; get bit vector
	LDA 	NDISK		; get current disk
	MOV	C,A		; to c
	CALL 	ARHL		; get correct bit
	MOV	A,L		; get it
	ANI	1		; mask out
	RET			; return
;
; ** WRITE PROTECT DISK AND SET DIR CHECK SIZE WORD TO MAXIMUM IN DPE
;
SDSKRO:	LXI	H,ROVECT	; get r/o vector
	MOV	C,M		; get low
	INX	H		; point to high
	MOV	B,M		; get high byte
	CALL 	SETBIT		; set bit on in C
	SHLD 	ROVECT		; save it back
	LHLD 	DRM		; get maximum dir entries 
	INX	H		; one more
	XCHG 			; to de
	LHLD 	CSTEST		; point to dir check count address for this
	MOV	M,E		; move there low
	INX	H		; increment
	MOV	M,D		; move there high
	RET
;
; ** FILE READ ONLY ??
;
FLRO:	CALL 	GFCBAD		; get address of FILE in directory
FLRO1:	LXI	D,9		; byte 9 msb is r/o flag
	DAD	D		; add
	MOV	A,M		; get that byte
	RAL			; in carry
	RNC			; not set ok
	LXI	H,FILERO	; point to file R/O error
	JMP	ERROR		; go display it
;
; ** CHECK DISK R/O AND DISPLAY IF SO
;
DSKRO: 	CALL 	GETBIT		; get r/o bit
	RZ 			; zero ok return
	LXI	H,DRIVERO	; point to disk R/O vector
	JMP	ERROR		; go display it
;
; ** GET ADDRESS OF FILE FCB IN DIRECTORY BUFFER
;
GFCBAD:	LHLD 	DIRBUF		; load dir buffer address
	LDA	DIROFF		; load offset in dir
GFCBAD1:ADD	L		; add l
	MOV	L,A		; save back
	RNC			; check if page overflow
	INR	H		; if so adjust h
	RET			; return
;
; ** GET FILE STATUS BYTE IN USER FCB 
;
GETSTS:	LHLD 	BDOSDE		; get user fcb address
	LXI	D,14		; setup offset for file status byte
	DAD	D		; add offset
	MOV	A,M		; get that byte
	RET			; return
;
; ** CLEAR FILE STATUS BYTE
;
STSCLR:	CALL 	GETSTS		; get address and byte
	MVI	M,0		; clear it
	RET			; and return
;
; ** SET FILE STATUS FLAG ON
;
STSSET:	CALL 	GETSTS		; get address and byte
	ORI	80H		; set msb
	MOV	M,A		; save back
	RET			; and return
;
; ** AT END OFF PREVIOUS CHECKED DIRECTORY ENTIES ( CHECKSUM )
;
CSEND:	LHLD 	CNTER		; get current poistion 
	XCHG 			; to de
	LHLD 	CSTEST		; get previous count checked address
	MOV	A,E		; get low of it
	SUB	M		; substract current pos
	INX	H		; nextbyte
	MOV	A,D		; get high
	SBB	M		; substract current position 
	RET			; carry set if cnter greater
;
; ** CHECK IF DIR ENTRY IS ALREADY SAVED IN COUNT ELSE SET IT( CHECKSUM )
;
CSOK:	CALL 	CSEND		; check if already setup
	RC 			; carry ok return
	INX	D		; one more ?? 
	MOV	M,D		; save new cnter value
	DCX	H		; into dir 
	MOV	M,E		; check size saving area
	RET			; and return
;
; ** SUBSTRACT HL FROM DE RESULT IN HL
;
SUBHLD:	MOV	A,E		; get e
	SUB	L		; substract l
	MOV	L,A		; save l 
	MOV	A,D		; get d
	SBB	H		; substract h
	MOV	H,A		; save h
	RET			; return
;
; ** BUILD OR CHECK DIRECTORY CHECKSUM TABLE C=FF CALCULATE C=0 VERIFY
;
CCHKSM:	MVI	C,0FFH		; setup calculate checksum 
;
VCHKSM:	LHLD 	CURCSP		; get current dir scan position 
	XCHG 			; to de
	LHLD 	CKS		; load check size entries
	CALL 	SUBHLD		; substract
	RNC			; no more to check
	PUSH 	B		; save entry flag
	CALL 	CHKSUM		; checksum sector
	LHLD 	CSV		; get checksum buffer address	
	XCHG 			; to de
	LHLD 	CURCSP		; get current position 
	DAD	D		; add offset
	POP	B		; get flag back
	INR	C		; increment
	JZ 	CHKSUM1		; zero then it whas set vector demand
	CMP	M		; else compair checksum
	RZ 			; ok return
	CALL 	CSEND		; at end of previous checked entries count
	RNC			; no carry ok return
	CALL 	SDSKRO		; set disk R/O
	RET			; and return
CHKSUM1:MOV	M,A		; set demand write checksum in buffer
	RET
;
; ** WRITE IN DIRECTORY
;
WRTDIR:	CALL 	CCHKSM		; verify dir checksum ( disk changed )
	CALL 	DIRDMA		; set dma to dir buffer
	MVI	C,1		; set write to dir code for blocking
	CALL 	WTSEC		; rewrite sector
	JMP	DEFDMA		; reset dma
;
; ** READ INTO DIRECTORY
;
RDDIR: 	CALL 	DIRDMA		; set dma to dir buffer
	CALL 	RDSEC		; read the sector
;
; ** SET DMA ADDRESS ON ASKED ONE
;
DEFDMA:	LXI	H,DMAADR	; get def dma address	
	JMP	SETDMA		; set it
;
; ** SET DMA ON DIRECTORY BUFFER
;
DIRDMA:	LXI	H,DIRBUF	; load dir buffer address
;
; ** SET DMA ADDRESS INDIRECT THRU HL
;
SETDMA:	MOV	C,M		; low to c
	INX	H		; point to next
	MOV	B,M		; high to b
	JMP	IOSTDMA		; set dma in bios
;
; ** MOVE DIRECTORY BUFFER TO USER DMABUFFER 
;
MOVDIR:	LHLD 	DIRBUF		; load address of dir buffer
	XCHG 			; to de
	LHLD 	DMAADR		; load dma buffer address
	MVI	C,128		; 128 bytes
	JMP	MOVE		; move it
;
; ** END OFF DIRECTORY ( ZERO RETURN IF CNT = FFFF )
;
ENDOFF:	LXI	H,CNTER		; point to cnter
	MOV	A,M		; get low
	INX	H		; point to high
	CMP	M		; compare
	RNZ			; not FFFF return non zero value
	INR	A		; set zero acu and flag
	RET
;
; ** RESET DIRECTORY COUNTER ( NOT FOUND )
;
RSTCNT:	LXI	H,-1		; HL to 0FFFFH
	SHLD 	CNTER		; reset cnter
	RET
;
; ** GET NEXT DIRECTORY ENTRY
;
NXTDIR:	LHLD 	DRM		; load dir max entries
	XCHG 			; to de
	LHLD 	CNTER		; current countscan to hl
	INX	H		; increment
	SHLD 	CNTER		; resave for later
	CALL 	SUBHLD		; at end ?? ( all get )
	JNC	NXTDIR1		; no go on
	JMP	RSTCNT		; go reset count return with carry set
NXTDIR1:LDA	CNTER		; get count
	ANI	3		; mask 2 bits ( 4 entries in 1 sector )
	MVI	B,5		; setup 5
NXTDIR2:ADD 	A		; add 5 times to itself
	DCR	B		; to get 
	JNZ	NXTDIR2		; address offset
	STA	DIROFF		; save in ram
	ORA	A		; test it 
	RNZ			; not at end of sector return
	PUSH 	B		; save bc
	CALL 	DSEEK		; prepare seek
	CALL 	RDDIR		; read next dir sector
	POP	B		; restore bc
	JMP	VCHKSM		; go verify directory checksum
;
; ** GET ALV TABLE BYTE AND ADDRESS FROM BLOCK NUMBER in BC
;		on return d is rotate count 
;		to be back correct address in hl 
;
GALVB:	MOV	A,C		; gety low
	ANI	7		; mask out to 7 ( 8 blocks in an byte ) 
	INR	A		; adjust count
	MOV	E,A		; to e as counter
	MOV	D,A		; return count for rotate
	MOV	A,C		; get back low
	RRC			; ignore 3 lsb's 
	RRC			; 
	RRC			;
	ANI	1FH		; mask out what we need
	MOV	C,A		; to c
	MOV	A,B		; high byte to a
	ADD	A		;
	ADD	A
	ADD	A
	ADD	A
	ADD	A	
	ORA	C		;
	MOV	C,A		;
	MOV	A,B		;
	RRC			;
	RRC			;
	RRC			;
	ANI	1FH		;
	MOV	B,A		; now in bc address offset
	LHLD 	ALV		; get allocation table address
	DAD	B		; add offset calculated	
	MOV	A,M		; get byte
GALVB1:	RLC			; rotate 
	DCR	E		; until correct bit is in lsb
	JNZ	GALVB1		;
	RET			; return
;
; ** SET or RESET ALLOCATION BIT IN ALV ( C=1 set / C=0 reset )
;
SRALV:	PUSH 	D		; save flag (set or reset bit)
	CALL 	GALVB		; get address and byte of alv table 
	ANI	11111110B	; mask out lsb
	POP 	B		; get flag ( new value )
	ORA	C		; combine
SRALV1:	RRC			; rotate in correct position
	DCR	D		; given in d
	JNZ	SRALV1		; until back in correct order
	MOV	M,A		; and resave in allocation table
	RET			; return to aller
;
; ** ALLOCATION VECTOR BUILDER ( SCANS 16 ALLOC BYTES (RE)SET BITS IN TABLE )
;
BLDVEC:	CALL 	GFCBAD		; get address of file in dir
	LXI	D,16		; setup 16 bytes
	DAD 	D		; add it
	PUSH	B		; to get first of block area 
	MVI	C,17		; loop 16 times 
BLDVEC1:POP	D		; restore de	
	DCR	C		; decrement loop counter
	RZ 			; zero ok return
	PUSH 	D		; save set or reset flag
	LDA	BLK255		; more than 255 blocks on disk
	ORA	A		; test it
	JZ 	BLDVEC2		; no skip
	PUSH 	B		; save counter
	PUSH 	H		; save dir address
	MOV	C,M		; get blocknumber
	MVI	B,0		; b to zero of course
	JMP	BLDVEC3		; go do the job
BLDVEC2:DCR	C		; ok decrement back each block is 2 bytes
	PUSH 	B		; save counter
	MOV	C,M		; get low byte ( more than 255 blocks )
	INX 	H		; point to next
	MOV	B,M		; get high byte
	PUSH 	H		; save dir address
BLDVEC3:MOV	A,C		; get low 
	ORA	B		; combine with high
	JZ 	BLDVEC4		; block number zero = entry not yet used
	LHLD 	DSM		; get max block nmbr for drive
	MOV	A,L		; to l
	SUB	C		; substract that found
	MOV	A,H		; low 
	SBB	B		; and high
	CNC	SRALV		; in range yes set bit in allocation table
BLDVEC4:POP	H		; restore dir address
	INX	H		; point to next
	POP	B		; restore counter 
	JMP	BLDVEC1		; and loop for next block 
;
; ** LOG ON DRIVE READ DIR AND BUILD ALLOCATION BIT VECTOR
;
LOGDSK:	LHLD 	DSM		; get total disk capacity
	MVI	C,3		; setup 3
	CALL 	ARHL		; divide each block 2 bits
	INX	H		; increment to be sure to get all
	MOV	B,H		; copy
	MOV	C,L		; to bc
	LHLD 	ALV		; get allocation space 
LOGDSK1:MVI	M,0		; write there zero
	INX	H		; to clear
	DCX	B		; complete
	MOV	A,B		; allocation
	ORA	C		; bits 
	JNZ	LOGDSK1		; vector
	LHLD 	AL0AL1		; number of dir blocks 
	XCHG 			; to de
	LHLD 	ALV		; first allocation word
	MOV	M,E		; set to dir reserve blocks
	INX	H		; this are 
	MOV	M,D		; maximum 16 blocks ( see manual alter guide )
	CALL 	CLRPRM		; clear internal drive parms
	LHLD 	CSTEST		; get address of checked size saving word
	MVI	M,3		; setup 3
	INX	H		; into his word
	MVI	M,0		; to force at least one complete dir sector
	CALL 	RSTCNT		; reset cnter
LOGDSK2:MVI	C,0FFH		; mark as no any match to do
	CALL 	NXTDIR		; get next dir entry	
	CALL 	ENDOFF		; check of at end of dir ?
	RZ 			; yes return
	CALL 	GFCBAD		; get fcb addres in dir 
	MVI	A,0E5H		; setup erased or free entry
	CMP	M		; compair
	JZ 	PATCH1		; yes erased check next if also erased
	LDA	CUSER		; get current user
	CMP	M		; same 
	JNZ	LOGDSK3		; no next
	INX	H		; point to first char of filename
	MOV	A,M		; get it
	SUI	'$'		; substract $
	JNZ	LOGDSK3		; not zero now next
	DCR	A		; acu is zero and now FF
	STA	BDSERR		; save for return in ccp ( bdos opcode 13 )
LOGDSK3:MVI	C,1		; set c to set bit in alv
	CALL 	BLDVEC		; build up alv for this file	
	CALL 	CSOK		; add this fcb for dir checksum counting
	JMP	LOGDSK2		; and check next
;
; ** EXIT BDOS
;
EXITBD:	LDA	RETCOD		; get return code
	JMP	BDEXTA		; exit bdos
;
; ** CHECK EXTEND NUMBER FROM DIR FCB
;
CHECKXT:PUSH 	B		;
	PUSH 	PSW		;
	LDA	EXM		;
	CMA			;
	MOV	B,A		;
	MOV	A,C		;
	ANA	B		;
	MOV	C,A		;
	POP	PSW		;
	ANA	B		;
	SUB	C		;
	ANI	1FH		;
	POP	B		;
	RET			;
;
; ** RESET DIRECTORY FOR SEARCH OF FILE 
;
RSTDIR:	MVI	A,0FFH		; mark dir whas reset
	STA	RETCOD		; at this byte
	LXI	H,MATCH		; setup save location
	MOV	M,C		; save there count to check
	LHLD 	BDOSDE		; get fcb address from bdos caller
	SHLD 	FCBTMP		; save in new location
	CALL 	RSTCNT		; reset cnterr
	CALL 	CLRPRM		; clear internal drive parms ??
;
; ** TRY TO FIND FCB IN DIR THAT MATCH FCB FILE FROM BDOS
;
SCNDIR:	MVI	C,0		; start from begin of dir
	CALL 	NXTDIR		; get next entry
	CALL 	ENDOFF		; at end of it
	JZ 	BDEROR		; yes exit not found
	LHLD 	FCBTMP		; get file fcb address 
	XCHG 			; to de
	LDAX 	D		; get first byte
	CPI	0E5H		; erased
	JZ 	SCNDIR1		; yes next
	PUSH 	D		; save address
	CALL 	CSEND		; at end of checksum check
	POP	D		; restore
	JNC	BDEROR		; end exit
SCNDIR1:CALL 	GFCBAD		; get addres
	LDA	MATCH		; get saved count for match
	MOV	C,A		; to c
	MVI	B,0		; counter to zero 
SCNDIR2:MOV	A,C		; get count scan
	ORA	A		; test it
	JZ 	SCNDIR5		; found exit routine
	LDAX 	D		; get byte
	CPI	'?'		; wildchart char
	JZ 	SCNDIR4		; yes ignore
;
; ** PUBLIC FILES PATCH  
;
	IF	PATCHPB
	LDA	040H		; get public flag
	ORA	A		; test it
	JZ	SCNDIR6		; zero not active skip patch	
	MOV	A,B		; get position in test	
	ORA	A		; test it
	JNZ	SCNDIR6		; not first skip code
	LDAX	D		; ok get first byte in fcb from dir
	CPI	0E5H		; erased one
	JZ	SCNDIR6		; yes skip patch
	MOV	A,M		; get user number from user fcb 
	ORA	A		; test it 
	JZ	SCNDIR4		; if zero force character match
	ENDIF
;
SCNDIR6:MOV	A,B		; end of filename ?
	CPI	13		; is byte 13 in fcb
	JZ 	SCNDIR4		; yes done
	CPI	12		; extend byte ?
	LDAX 	D		; get it
	JZ 	SCNDIR3		; yes ok
	SUB	M		; compare character
	ANI	01111111B	; mask of all flags in msb
	JNZ	SCNDIR		; not same try next dir entry
	JMP	SCNDIR4		; continue routine
SCNDIR3:PUSH 	B		; save loop counter
	MOV	C,M		; get extend 
	CALL 	CHECKXT		; check extend number
	POP	B		; restore counter
	JNZ	SCNDIR		; not good check next 
SCNDIR4:INX	D		; next in file fcb
	INX	H		; next in directory
	INR	B		; next ??
	DCR	C		; loop for filename scan 
	JMP	SCNDIR2		; go check next character
SCNDIR5:LDA	CNTER		; get current dir entry pointer
	ANI	3		; mask out only 3 lsb ( 0 1 2 3 )
	STA	BDSERR		; save dir return code for caller to bdos
	LXI	H,RETCOD	; point to error code
	MOV	A,M		; get it
	RAL			; msb in carry
	RNC			; no carry error ????
	XRA	A		; clear acu
	MOV	M,A		; and reset error code	
	RET			; return
;
; ** BDOS ERROR RETURN 
;
BDEROR:	CALL 	RSTCNT		; reset counter
	MVI	A,0FFH		; error bdos return code
	JMP	BDEXTA		; exit bdos
;
; ** MARK FILE IN DIR AS DELETED ( set first bit to E5 and RELEASE ALV )
;
RELFIL:	CALL 	DSKRO		; check disk r/o
	MVI	C,12		; 12 characters to match
	CALL 	RSTDIR		; reset diretory end find file
RELFIL1:CALL 	ENDOFF		; not found return
	RZ 			; Z=0 if no more extends there
	CALL 	FLRO		; check file is R/O
	CALL 	GFCBAD		; get address 
	MVI	M,0E5H		; set first byte to E5 = deleted
	MVI	C,0		; set C to reset alv
	CALL 	BLDVEC		; reset alv buffer for this file
	CALL 	WRTDIR		; rewrite sector
	CALL 	SCNDIR		; scan next maybe extend there
	JMP	RELFIL1		; loop again
;
; ** GET BC  AVAILABLE BLOCKS IN ALV AND RESERVE THEM 
;
FREEBLK:MOV	D,B		; copy count to de for next loop
	MOV	E,C		;
GTBLK1:	MOV	A,C		; get count to
	ORA	B		; check if zero
	JZ 	GTBLK2		; yes job done
	DCX	B		; decrement count
	PUSH 	D		; save to find count 
	PUSH 	B		; save loop counter
	CALL 	GALVB		; get allocation bit
	RAR			; rotate into carry
	JNC	GTBLK3		; not used 
	POP	B		; restore bc
	POP	D		; and de
GTBLK2:	LHLD 	DSM		; get max drive capacity
	MOV	A,E		; test
	SUB	L		; if
	MOV	A,D		; beond
	SBB	H		; end of disk 
	JNC	GTBLK4		; yes error
	INX	D		; next block
	PUSH 	B		; save bc
	PUSH 	D		; save de
	MOV	B,D		; copy de
	MOV	C,E		; to bc ??
	CALL 	GALVB		; get allocation bit
	RAR			; rotate in carry 
	JNC	GTBLK3		; not used go on
	POP	D		; restore de
	POP	B		; restore bc
	JMP	GTBLK1		; loop back
GTBLK3:	RAL			; set back
	INR	A		; increment to set bit on
	CALL 	SRALV1		; rotate back in good position and update ALV
	POP	H		; get new block number
	POP	D		; clear stack 	
	RET			; return
GTBLK4:	MOV	A,C		; loop count done
	ORA	B		; test it
	JNZ	GTBLK1		; no loop again
	LXI	H,0		; no free block found error return
	RET			; return
;
;** MOVE NEW FCB IN DIRECTORY BUFFER AND REWRITE 
;
MOVFCBD:MVI	C,0		; no offset in fcb
	MVI	E,32		; new entry is 32 bytes
MOVFCB:	PUSH 	D		; ENTRY FOR RENAME OR RECOPY save count
	MVI	B,0		; b to zero never more than 255 bytes offset
	LHLD 	BDOSDE		; get user fcb
	DAD	B		; add offset 
	XCHG 			; to de
	CALL 	GFCBAD		; get dir fcb address
	POP	B		; get count
	CALL 	MOVE		; move a copy to user fcb
RWRDIR:	CALL 	DSEEK		; prepare seek
	JMP	WRTDIR		; rewrite directory
;
; ** RENAME FILE 
;
RENAM:	CALL 	DSKRO		; is disk r/o
	MVI	C,12		; only file name to match
	CALL 	RSTDIR		; get it
	LHLD 	BDOSDE		; get user fcb
	MOV	A,M		; get drive byte
	LXI	D,16		; offset to new name
	DAD	D		; add it
	MOV	M,A		; get new name drive number
RENAM1:	CALL 	ENDOFF		; whas file found ?
	RZ 			; no error return
	CALL 	FLRO		; is file r/O
	MVI	C,16		; offset to new
	MVI	E,12		; filename length
	CALL 	MOVFCB		; move new name to dir buffer and rewrite
	CALL 	SCNDIR		; next dir entry
	JMP	RENAM1		; next extend check and rename
;
; ** SET FILE ATTRIBUTES
;
SETATR:	MVI	C,12		; 12 long disk and filename
	CALL 	RSTDIR		; find it
SETATR1:CALL 	ENDOFF		; at end not found
	RZ 			; no return
	MVI	C,0		; no offset
	MVI	E,12		; 12 long
	CALL 	MOVFCB		; move and rewrite 
	CALL 	SCNDIR		; other extend present ?
	JMP	SETATR1		; go check it and rewrite if needed
;
; ** OPEN FILE AND COPY FCB TO USER
;
OPNFCB:	MVI	C,15		; 15 characters
	CALL 	RSTDIR		; reset dir and find it
	CALL 	ENDOFF		; at end ??
	RZ 			; yes not found exit
OPEN01:	CALL 	GETEXT		; get extend byte address of user
	MOV	A,M		; get it
	PUSH 	PSW		; save it
	PUSH 	H		; and save address
	CALL 	GFCBAD		; get file address in dir
	XCHG 			; to de
	LHLD 	BDOSDE		; user fcb in hl
	MVI	C,32		; 32 bytes
	PUSH 	D		; save dir fcb address
	CALL 	MOVE		; move already fcb to user
	CALL 	STSSET		; mark file open and ok
	POP	D		; restore dir fcb
	LXI	H,12		; setup extend offset
	DAD	D		; add it
	MOV	C,M		; get extend
	LXI	H,15		; setup record count offset
	DAD	D		; add fcb adress
	MOV	B,M		; record to b
	POP	H		; extend byte of user address
	POP	PSW		; exend
	MOV	M,A		; restore in user fcb
	MOV	A,C		; get dir extend number
	CMP	M		; compaire with user
	MOV	A,B		; get alrea dy record count 
	JZ 	OPEN02		; same extend go on
	MVI	A,0		; set up zero 
	JC 	OPEN02		; extend dir > extend user
	MVI	A,80H		; extend dir < extend user
OPEN02:	LHLD 	BDOSDE		; get user fcb
	LXI	D,15		; setup record count offset
	DAD	D		; add it
	MOV	M,A		; get record count 0 80 or number
	RET			; save it there and return
;
; ** COMMON CLOSE EXTEND BLOCK CHECK AND WRITE FOR DISKS > 256 BLOCKS
;
CSBLK25:MOV	A,M		;
	INX	H		;
	ORA	M		;
	DCX	H		;
	RNZ			;
	LDAX 	D		;
	MOV	M,A		;
	INX	D		;
	INX	H		;
	LDAX 	D		;
	MOV	M,A		;
	DCX	D		;
	DCX	H		;
	RET			;
;
; ** CLOSE UP FILE or EXTEND 
;
CLOFCB:	XRA	A		; clear acu
	STA	BDSERR		; no error ( not yet )
	STA	CNTER		; clear cnter
	STA	CNTERH		; 2 bytes
	CALL 	GETBIT		; get ro bit for this drive
	RNZ			; yes error
	CALL 	GETSTS		; get file flags
	ANI	80H		; mask out msb
	RNZ			; file not been written return
	MVI	C,15		; 15 characters to check
	CALL 	RSTDIR		; find file fcb in dir
	CALL 	ENDOFF		; at end of dir
	RZ 			; yes bnot found
	LXI	B,16		; 16 bytes
	CALL 	GFCBAD		; get address of it
	DAD	B		; add 16 to get block table
	XCHG 			; to DE=DIR FCB BLOCK START
	LHLD 	BDOSDE		; user fcb
	DAD	B		; TO HL=USER FCBBLOCK START
	MVI	C,16		; setup 16 bytes long
CLOS01:	LDA	BLK255		; get more than 255 block flag 
	ORA	A		; test it
	JZ 	CLOS04		; greater go take special care
	MOV	A,M		; get user block
	ORA	A		; test it
	LDAX 	D		; get already fcb block
	JNZ	CLOS02		; user not used it
	MOV	M,A		; used set it in dir fcb
CLOS02:	ORA	A		; test fcb block
	JNZ	CLOS03		; whas not zero
	MOV	A,M		; get user block
	STAX 	D		; save in fcb 
CLOS03:	CMP	M		; compiar is it whas the same	
	JNZ	CLOS07		; no then ERROR
	JMP	CLOS05		; process next
CLOS04:	CALL 	CSBLK25		; 
	XCHG 			;
	CALL 	CSBLK25		;
	XCHG 			;
	LDAX 	D		;
	CMP	M		;
	JNZ	CLOS07		;
	INX	D		;
	INX	H		;
	LDAX 	D		;
	CMP	M		;
	JNZ	CLOS07		;
	DCR	C		;
CLOS05:	INX	D		; point to next block from fcb
	INX	H		; point to next block of dir
	DCR	C		; counter decremented
	JNZ	CLOS01		; al 16 done no
	LXI	B,0FFECH	; setup offset to repoint to extend 
	DAD	B		; user fcb
	XCHG 			; and
	DAD	B		; dir fcb
	LDAX 	D		; get it from user
	CMP	M		; compair with dir
	JC 	CLOS06		; not same ok close this extend
	MOV	M,A		; write new ?? extend in dir fcb
	LXI	B,3		; repoint to record count
	DAD	B		; add to user fcb
	XCHG 			; and	
	DAD	B		; to dir fcb
	MOV	A,M		; get record count 
	STAX 	D		; copy it in dir
CLOS06:	MVI	A,0FFH		; ?
	STA	UNKNOW		; ?
	JMP	RWRDIR		; go rewrite the directory
CLOS07:	LXI	H,BDSERR	; point to return flag
	DCR	M		; set to 0FFh ( reset by call entry in bdos)
 	RET			; and return
;
; ** MAKE NEW DIRECTORY ENTRY
;
MAKFCB:	CALL 	DSKRO		; disk r/o	
	LHLD 	BDOSDE		; get fcb from user
	PUSH 	H		; save it
	LXI	H,DELFCB	; point to E5 byte erased fcb to find
	SHLD 	BDOSDE		; new fcb
	MVI	C,1		; one byte must match E5
	CALL 	RSTDIR		; reset dir and search
	CALL 	ENDOFF		; at end ?	
	POP	H		; restore user fcb
	SHLD 	BDOSDE		; at de pointer
	RZ 			; return if not found
	XCHG 			; fcb user in de
	LXI	H,15		; point to byte 15 in fcb
	DAD	D		; add this
	MVI	C,17		; 1 record number and 16 blocks bytes	
	XRA	A		; clear acu
MAKE01:	MOV	M,A		; clear rest of fcb
	INX	H		; bump 
	DCR	C		; decrement count
	JNZ	MAKE01		; loopuntil all cleared
	LXI	H,13		; point to extend number
	DAD	D		; add fcb address
	MOV	M,A		; clear it also
	CALL 	CSOK		; adjust dir checked size
	CALL 	MOVFCBD		; write entry to directory
	JMP	STSSET		; mark file maked 
;
; ** CLOSE CURRENT EXTEND AND CREATE NEW ONE
;
NEXTXT:	XRA	A		; clear acu
	STA	UNKNOW		; ???
	CALL 	CLOFCB		; close current fcb
	CALL 	ENDOFF		; not found
	RZ 			; error return
	LHLD 	BDOSDE		; get user fcb
	LXI	B,12		; offset to extend
	DAD	B		; add it
	MOV	A,M		; get extend number
	INR	A		; next ( haha )
	ANI	1FH		; mask out to max 1F
	MOV	M,A		; save back
	JZ 	NEXT01		; zero ????
	MOV	B,A		; to b
	LDA	EXM		; get extend mask
	ANA	B		; and it
	LXI	H,UNKNOW		; ??
	ANA	M		; ??
	JZ 	NEXT02		; ??
	JMP	NEXT03		; ??
NEXT01:	LXI	B,2		; ??
	DAD	B		; ??
	INR	M		; ??
	MOV	A,M		; ??
	ANI	0FH		; ??
	JZ 	NEXT05		; ??
NEXT02:	MVI	C,15		; 15 characters to match 
	CALL 	RSTDIR		; scan dir if extend is there
	CALL 	ENDOFF		; at end
	JNZ	NEXT03		; no extend extists
	LDA	RWFLAG		; get read/write flag
	INR	A		; test it
	JZ 	NEXT05		; read then exit 
	CALL 	MAKFCB		; ok make it
	CALL 	ENDOFF		; normal created
	JZ 	NEXT05		; no error
	JMP	NEXT04		; exit
NEXT03:	CALL 	OPEN01		; ok open it then
NEXT04:	CALL 	GETREC		; get record numbers  
	XRA	A		; clear return code	
	JMP	BDEXTA		; end exit
NEXT05:	CALL 	RTRN1		; set error return code
	JMP	STSSET		; set file no written flag
;
; ** READ SEQUENTIAL
;
READS:	MVI	A,1		; acu to 1
	STA	MODE		; to set sequential mode
READG:	MVI	A,0FFH		; acu to ff  
	STA	RWFLAG		; to set read flag
	CALL 	GETREC		; get record to read
	LDA	RWRECO		; get it
	LXI	H,CRECXT	; get record in current extend	
	CMP 	M		; compair it
	JC 	READ01		; still not at end of extend go read
	CPI	080H		; test if at end	
	JNZ	READ02		; error exit 
	CALL 	NEXTXT		; get next extend
	XRA	A		; clear acu
	STA	RWRECO		; reset record number in extend	
	LDA	BDSERR		; get return code from search next extend
	ORA	A		; test it	 	
	JNZ	READ02		; error ? yes exit		
READ01:	CALL 	ACTBLK		; calculate and get active block for record
	CALL 	BLKZR		; is it zero ( not written )
	JZ 	READ02		; yes error try to read byeond file
	CALL 	CALREC		; get physical record to read
	CALL 	SEEK		; seek to it
	CALL 	RDSEC		; and read sector
	JMP	UPDFCB		; clean up 
READ02:	JMP	RTRN1		; error exit
;
; ** WRITE SEQUENTIAL
;
WRITES:	MVI	A,1		; acu to one
	STA	MODE		; set write sequential mode
WRITEG:	MVI	A,0		; acu to 0
	STA	RWFLAG		; set write mode
	CALL 	DSKRO		; is disk R/O ? yes report it
	LHLD 	BDOSDE		; get user fcb
	CALL 	FLRO1		; is file R/O yes error
	CALL 	GETREC		; get record to write
	LDA	RWRECO		; get record in extend
	CPI	80H		; at end of extend
	JNC	RTRN1		; error exit		
	CALL 	ACTBLK		; get active block for this
	CALL 	BLKZR		; zero ? not yet written and allocated
	MVI	C,0		; c to zero ????
	JNZ	WRITE05		; yes allocated
	CALL 	BLKOFS		; get block offset
	STA	BLKOFST		; save in ram
	LXI	B,0		; bc to zero 
	ORA	A		; test block offset
	JZ 	WRITE01		; zero ok go on
	MOV	C,A		; offset to c
	DCX	B		; and adjust to find previous
	CALL 	GETBLK		; get previous blocknumber 
	MOV	B,H		; to bc
	MOV	C,L		; and
WRITE01:CALL 	FREEBLK		; get next available block after this ???
	MOV	A,L		; number to a	
	ORA	H		; or h		
	JNZ	WRITE02		; not zero ok get one	
	MVI	A,2		; error code 2 
	JMP	BDEXTA		; disk full ERROR
WRITE02:SHLD 	CURBLK		; save new block 
	XCHG 			; to DE
	LHLD 	BDOSDE		; get user fcb
	LXI	B,16		; setup offset to blocks 
	DAD	B		; add offset
	LDA	BLK255		; get more than 255 block flag
	ORA	A		; test it
	LDA	BLKOFST		; get new blocknumber offset
	JZ 	WRITE03		; zero then more than 255
	CALL 	GFCBAD1		; get new offset
	MOV	M,E		; save new block in fcb of user
	JMP	WRITE04		; go on
WRITE03:MOV	C,A		; get offset
	MVI	B,0		; b to zero
	DAD	B		; add offset
	DAD	B		; twice 2 bytes for a block	
	MOV	M,E		; low to fcb
	INX	H		; next byte
	MOV	M,D		; high to fcb
WRITE04:MVI	C,2		; setup 2 in C ?????
WRITE05:LDA	BDSERR		; get error code
	ORA	A		; rest it
	RNZ			; not zero return	
	PUSH 	B		; 
	CALL 	CALREC		;
	LDA	MODE		;
	DCR	A		; test if in zero fill mode
	DCR	A		;
	JNZ	WRITE08		; no go
	POP	B		;
	PUSH 	B		;
	MOV	A,C		;
	DCR	A		;
	DCR	A		;
	JNZ	WRITE08		; skip fill code for write withzero fill
	PUSH 	H		; save 
	LHLD 	DIRBUF		; point to dir buffer
	MOV	D,A		; acu is zero ( 3 lines higher )
WRITE06:MOV	M,A		; clear byte
	INX	H		; point to next 
	INR	D		; increment count
	JP 	WRITE06		; will become negative on 80h ( 128 bytes )
	CALL 	DIRDMA		; set dir dma to bios
	LHLD 	CURBLS		; get current block
	MVI	C,2		; setup write to unallocated
WRITE07:SHLD 	CURBLK		; store it
	PUSH 	B		; save bios write code
	CALL 	SEEK		; seek to it
	POP	B		; restore bios write code
	CALL 	WTSEC		; write the sector tozero fill block
	LHLD 	CURBLK		; get current block
	MVI	C,0		; c to 0 
	LDA	BLM		; block mask
	MOV	B,A		; tob
	ANA	L		; mask
	CMP	B		; compare
	INX	H		; next sector
	JNZ	WRITE07		; fill until all block written
	POP	H		; get back current block
	SHLD 	CURBLK		; save in pointer	 
	CALL 	DEFDMA		; back default dma
WRITE08:CALL 	SEEK		; seek to sector
	POP	B		; get code for write  back
	PUSH 	B		; save back
	CALL 	WTSEC		; write sector asked to do
	POP	B		; restore
	LDA	RWRECO		; 
	LXI	H,CRECXT	;
	CMP	M		;
	JC 	WRITE09		;
	MOV	M,A		;
	INR	M		;
	MVI	C,2		;
WRITE09:DW	0		;	
	LXI	H,0		;
	PUSH 	PSW		;
	CALL 	GETSTS		; get status of file
	ANI	7FH		; set file written	
	MOV	M,A		; restore it
	POP	PSW		;
	CPI	7FH		;
	JNZ	WRITE11		;
	LDA	MODE		; if sequential mode 
	CPI	1		; test it
	JNZ	WRITE11		; exit
	CALL 	UPDFCB		; 
	CALL 	NEXTXT		;
	LXI	H,BDSERR	;
	MOV	A,M		;
	ORA	A		;
	JNZ	WRITE10		;
	DCR	A		;
	STA	RWRECO		;
WRITE10:MVI	M,0		;
WRITE11:JMP	UPDFCB		; exit and update user fcb for next record
;
; ** POSITION FILE FOR RANDOM RECORD C=0 FOR WRITE C=0FFH FOR READ 
;
SETRND:	XRA	A		; clear mode flag	
	STA	MODE		; to set random mode
SETRND1:PUSH 	B		; save entry flag 
	LHLD 	BDOSDE		; get user fcb
	XCHG 			; to de
	LXI	H,21H		; setup offset to random record number
	DAD	D		; add it
	MOV	A,M		; get it
	ANI	7FH		; mask to 1 extend
	PUSH 	PSW		; save it
	MOV	A,M		; get it back
	RAL			; msb in carry	
	INX	H		; mext byte
	MOV	A,M		; get it
	RAL			; carry in
	ANI	1FH		; mask out to 1f
	MOV	C,A		; to c
	MOV	A,M		; get back byte	
	RAR			; shift 4 bits
	RAR			;
	RAR			;
	RAR			;
	ANI	0FH		; mask out
	MOV	B,A		; to b
	POP	PSW		; restore lsb
	INX	H		; next byte
	MOV	L,M		; to l
	INR	L		; test it 
	DCR	L		; if zero
	MVI	L,6		; already setup seek past disk size 
	JNZ	SETRND5		; go report error 	
	LXI	H,20H		; offset to record number in seq mode 
	DAD	D		; add fcb
	MOV	M,A		; set there lsb masked to 80
	LXI	H,12		; offset to extend
	DAD	D		; add fcb
	MOV	A,C		; get calculated extend
	SUB	M		; test with in fcb
	JNZ	SETRND2		; no next extend
	LXI	H,14		;
	DAD	D		;
	MOV	A,B		;
	SUB	M		;
	ANI	7FH		;
	JZ 	SETRND3		;
SETRND2:PUSH 	B		;
	PUSH 	D		;
	CALL 	CLOFCB		; close current extend
	POP	D		;
	POP	B		;
	MVI	L,3		; already setup can not close current extend
	LDA	BDSERR		; save it
	INR	A		; test return code
	JZ 	SETRND4		; error 
	LXI	H,12		; offset to extend
	DAD	D		; add it
	MOV	M,C		; set there new extend   
	LXI	H,14		; offset to record number	
	DAD	D		; add it
	MOV	M,B		; set there new record number
	CALL 	OPNFCB		; open fcb
	LDA	BDSERR		; get error code
	INR	A		; test it
	JNZ	SETRND3		; not zero now error
	POP	B		; reset entry flag
	PUSH 	B		; save back
	MVI	L,4		; already setup seek to unwritten record
	INR	C		; test flag 
	JZ 	SETRND4		; reading skip create code
	CALL 	MAKFCB		; make new extend
	MVI	L,5		; already setup dir full error 
	LDA	BDSERR		; save it
	INR	A		; get return code from create 
	JZ 	SETRND4		; error 
SETRND3:POP	B		; clear stack 
	XRA	A		; clear acu
	JMP	BDEXTA		; exit routine no error
SETRND4:PUSH 	H		; save 
	CALL 	GETSTS		; get status
	MVI	M,0C0H		; set there more than 1 extend created ????
	POP	H		; restore
SETRND5:POP	B		; clear stack 
	MOV	A,L		; set error code in return byte error code
	STA	BDSERR		; do it
	JMP	STSSET		; clear file written status
;
; ** READ RANDOM
;
READR:	MVI	C,0FFH		; setup reading
	CALL 	SETRND		; position file
	CZ 	READG		; records exist read it 
	RET	
;
; ** WRITE RANDOM
;
WRTRAND:MVI	C,0		; qsetup write
	CALL 	SETRND		; position file
	CZ 	WRITEG		; ok ?? then read
	RET	
;
; ** GET RANDOM RECORD NUMBER IN ABC FROM USER FCB
;
GETABC:	XCHG 			;
	DAD	D		;
	MOV	C,M		;
	MVI	B,0		;
	LXI	H,12		;
	DAD	D		;
	MOV	A,M		;
	RRC			;
	ANI	80H		;
	ADD	C		;
	MOV	C,A		;
	MVI 	A,0		;
	ADC 	B		;
	MOV 	B,A		;
	MOV 	A,M		;
	RRC 	 		;
	ANI	15		;
	ADD	B		;
	MOV	B,A		;
	LXI	H,14		;
	DAD	D		;
	MOV	A,M		;

	ADD	A		;
	ADD	A		;
	ADD	A		;
	ADD	A		;
	
	PUSH 	PSW		;
	ADD	B		;
	MOV	B,A		;
	PUSH 	PSW		;
	POP	H		;
	MOV	A,L		;
	POP	H		;
	ORA	L		;
	ANI	1		;
	RET			;
;
; ** COMPUTE FILE SIZE
;
SIZE:	MVI	C,12		; match filename only
	CALL 	RSTDIR		; scan dir to find
	LHLD 	BDOSDE		; get user fcb
	LXI	D,0021H		; offset to random record number
	DAD	D		; add offset
	PUSH 	H		; save address of it
	MOV	M,D		; d is zero clear
	INX	H		; the 
	MOV	M,D		; 3
	INX	H		; bytes
	MOV	M,D		; for random record number
SIZE01:	CALL 	ENDOFF		; file found ?
	JZ 	SIZE03		; no exit
	CALL 	GFCBAD		; get file fcb in dir
	LXI	D,15		; 
	CALL 	GETABC		; get random record number in ABC
	POP	H		; restore fcb address
	PUSH 	H		; save again
	MOV	E,A		; a to e
	MOV	A,C		; substract found yet
	SUB	M		; to 	
	INX	H		; check	
	MOV	A,B		; if 	 
	SBB	M		; this found
	INX	H		; is greater
	MOV	A,E		; than
	SBB	M		; previous found
	JC 	SIZE02		; no not greater skip saving code and exit
	MOV	M,E		; save new found
	DCX	H		; to random
	MOV 	M,B		; record
	DCX	H		; bytes
	MOV	M,C		; in fcb from user
SIZE02:	CALL 	SCNDIR		; next directory entry for this file
	JMP	SIZE01		; test again
SIZE03:	POP	H		; clear stack
	RET			; and exit	
;
; ** SET RANDOM RECORD
;
SETRNDR:LHLD 	BDOSDE		; get user fcb
	LXI	D,0020H		; offset to current record number
	CALL 	GETABC		; get current record in abc for random format
	LXI	H,0021H		; point to saving area fior it	
	DAD	D		; add this offset
	MOV	M,C		; low to it
	INX	H		; next byte
	MOV	M,B		; high to it
	INX	H		; next byte
	MOV	M,A		; overflow to it
	RET			; return
;
; ** LOG DRIVE ON ( SET BIT IN LOGIN VECTOR AND SELECT IT )
;
ONDISK:	LHLD 	LOGIN		; get login vector
	LDA	NDISK		; get new disk
	MOV	C,A		; disk to c	
	CALL 	ARHL		; get login bit for this drive
	PUSH 	H		; save this bit
	XCHG 			; to de
	CALL 	SETDSK		; set disk
	POP	H		; restore
	CZ 	SLERROR		; zero return from setdisk ERROR select
	MOV	A,L		; get bit
	RAR			; in carry
	RC 			; if set already logged on
	LHLD 	LOGIN		; else get login
	MOV	C,L		; copy to
	MOV	B,H		; bc
	CALL 	SETBIT		; set bit for this drive
	SHLD 	LOGIN		; and save again
	JMP	LOGDSK		; go read dir and set alv
;
; ** SELECT DISK
; 
BSETDSK:LDA	RGESVE		; get register e from call
	LXI	H,NDISK		; point to new disk
	CMP	M		; compare
	RZ 			; same ok return
	MOV	M,A		; save as new disk
	JMP	ONDISK		; and selectit 
;
; ** SET ALTERNATE DRIVE 
;
ALTDSK:	MVI	A,0FFH		; set alternate drive access
	STA	ADKFLG		; save it
	LHLD 	BDOSDE		; get user fcb
	MOV	A,M		; get first byte
	ANI	1FH		; mask out drive
	DCR	A		; adjust
	STA	RGESVE		; save it temp
	CPI	1EH		; is it current disk 
	JNC	ALTDSK1		; yes select no other
	LDA	NDISK		; get current disk
	STA	OLDDSK		; save it for later
	MOV	A,M		; get new
	STA	TMPDSK		; save it
	ANI	0E0H		; mask out user number
	MOV	M,A		; save it back
	CALL 	BSETDSK		; set new disk
ALTDSK1:LDA	CUSER		; get current user number
	LHLD 	BDOSDE		; get drive byte
	ORA	M		; combine with user number
	MOV	M,A		; save back
	RET			; and return
;
; ** RETURN VERSION NUMBER
;
VERSION:MVI	A,22H		; version 2.2
	JMP	BDEXTA		; exit it
;
; ** RESET DISK SYSTEM
;
RSTDISK:LXI	H,0		; clear hl
	SHLD 	ROVECT		; clear disk R/O vector
	SHLD 	LOGIN		; no one logged in
	LDA	DEFAULT		; clear acu
	ANI	15		; mask out user number
	STA	NDISK		; select disk 0 (this make that fool A: access)
	LXI	H,80H		; dma to 80h
	SHLD 	DMAADR		; save it
	CALL 	DEFDMA		; read directory in	
	JMP	ONDISK		; and logon drive
;
; ** OPEN FILE
;
OPENFILE:CALL 	STSCLR		; clear second bdos byte in fcb
	CALL 	ALTDSK		; set correct drive 
	JMP	OPNFCB		; go open file
;
; ** CLOSE FILE 
;
CLOSFILE:CALL 	ALTDSK		; set correct drive
	JMP	CLOFCB		; close file
;
; ** SEARCH FOR FIRST 
;
FIRST:	MVI	C,0		; clear c ??
	XCHG 			; fcb user in hl
	MOV	A,M		; get first one
	CPI	'?'		; ? entered
	JZ 	FIRST01		; yes special case
	CALL 	GETEXT		; get address of extend byte
	MOV	A,M		; get it	
	CPI	'?'		; also ?
	CNZ	STSCLR		; no clear extend number
	CALL 	ALTDSK		; alternate drive
	MVI	C,15		; 15 characters to match 
FIRST01:CALL 	RSTDIR		; reset directory and find it
	JMP	MOVDIR		; go move the dir dma buffer
;
; ** SEARCH FOR NEXT
;
NEXT:	LHLD 	FCBTMP		; get previous fcb pointer
	SHLD 	BDOSDE		; for now also
	CALL	ALTDSK		; alternate drive ?
	CALL	SCNDIR		; scan directory 
	JMP 	MOVDIR		; and move directory
;
; ** DELETE FILE
;
DELETE:	CALL 	ALTDSK		; alternate drive
	CALL 	RELFIL		; delete file
	JMP	EXITBD		; exit
;
; ** READ SEQUENTIAL
;
READSEQ:CALL 	ALTDSK		; alternate drive 
	JMP	READS		; go read sequential
;
; ** WRITE SEQUENTIAL
;
WRITESEQ:CALL 	ALTDSK		; alternate drive
	JMP	WRITES		; go write sequential
;
; ** MAKE FILE
;
MAKEFILE:CALL 	STSCLR		; clear file status byte 
	CALL 	ALTDSK		; alternate drive
	JMP	MAKFCB		; go make file in dir
;
; ** RENAME FILE
;
RENAME:	CALL 	ALTDSK		; alternate drive
	CALL 	RENAM		; rename file
	JMP	EXITBD		; exit
;
; ** GET LOGIN VECTOR
;
GETLOGIN:LHLD 	LOGIN		; get login vector
	JMP	BDEXTDE		; return
;
; **GET CURRENT DISK
;
GETCDISK:LDA	NDISK		; get disk current in use
	JMP	BDEXTA		; exit bdos
;
; ** SET DMA ADDRESS
;
SETDMAAD:XCHG 			; dma address into hl 
	SHLD 	DMAADR		; store it
	JMP	DEFDMA		; and set it in bios
;
; ** GET ALLOCATION VECTOR
;
GETALLOC:LHLD 	ALV		; get alv address
	JMP	BDEXTDE		; exit
;
; ** GET R/O VECTOR
;
GETROVEC:LHLD 	ROVECT		; get r/o vector
	JMP	BDEXTDE		; exit
;
; ** SET FILE ATTRIBUTES
;
SETFILEA:CALL 	ALTDSK		; alternate drive
	CALL 	SETATR		; set attributes
	JMP	EXITBD		; exit
;
; ** GET DISK PARAMETERS
;
GETDSKPRM:LHLD 	DPB		; get parms and drop in bdextde
;
; ** RETURN FROM BDOS WITH DE PARAMETER
;
BDEXTDE:SHLD 	BDSERR		; save address
	RET			; and exit
;
; ** GET SET USER NUMBER
;
GETSETUSR:LDA	RGESVE		; get e register from call
	CPI	0FFH		; is it FF
	JNZ	USER01		; yes get demanded
	LDA	CUSER		; get user 
	JMP	BDEXTA		; exit and report it
USER01:	ANI	1FH		; mask to 1F ?? ( wetenwe nu ook )
	STA	CUSER		; save in current user number
	RET			; and return
;
; ** READ RANDOM
;
READRAND:CALL 	ALTDSK		; alternate drive
	JMP	READR		; go read random
;
; ** WRITE RANDOM
;
WRITERAND:CALL 	ALTDSK		; alternate drive
	JMP	WRTRAND		; go write random
;
; ** COMPUTE FILE SIZE
;
FILESIZE:CALL 	ALTDSK		; alternate drive
	JMP	SIZE		; compute file size
;
; ** RESET DRIVE
;
RESETDISK:LHLD 	BDOSDE		; get asked reset vector
	MOV	A,L		; to a
	CMA			; invert it
	MOV	E,A		; to e
	MOV	A,H		; get h
	CMA			; invert
	LHLD 	LOGIN		; get login vector
	ANA	H		; and with vector inverted to reset bit
	MOV	D,A		; save it to d
	MOV	A,L		; get l
	ANA	E		; same reset bits to delog drive 
	MOV	E,A		; save it to e
	LHLD 	ROVECT		; get ro vector	
	XCHG 			; swap
	SHLD 	LOGIN		; hl now resetted login vector 
	MOV	A,L		; get l
	ANA	E		; reset also ro flag
	MOV	L,A		; save back
	MOV	A,H		; get other 8
	ANA	D		; reset also
	MOV	H,A		; save back
	SHLD 	ROVECT		; restore it
	RET			; return to caller
;
; ** CLEAN UP BDOS AND EXIT BDOS TO USER	
;
EXITBDOS:LDA	ADKFLG		; get alternate drive flag
	ORA	A		; test it
	JZ 	EXIT01		; no file read write done skip code
	LHLD 	BDOSDE		; get user fcb
	MVI	M,0		; reset drive there and user
	LDA	TMPDSK		; get temp disk
	ORA	A		; test it
	JZ 	EXIT01		; default return ok
	MOV	M,A		; restore alternate drive in fcb
	LDA	OLDDSK		; get back old disk
	STA	RGESVE		; save it ? 
	CALL 	BSETDSK		; reset old disk active
EXIT01:	LHLD 	OSTACK		; get user stack back
	SPHL 			; to it
	LHLD 	BDSERR		; get returncode
	MOV	A,L		; l to a
	MOV	B,H		; h to b( see manual )
	RET			; return to user
;
; ** WRITE RANDOM + ZERO FILL
;
WRITEZF:CALL 	ALTDSK		; alternate drive
	MVI	A,2		; setup zero fill mode
	STA	MODE		; save it in flag
	MVI	C,0		; setup write
	CALL 	SETRND1		; position extend and record
	CZ 	WRITEG		; ok then write record
	RET			; and return to user
;
; ** DELETED FILE FCB BLOCK
;
DELFCB:	DB	0E5H		; deleted fcb for create file 
;
; ** RAM SCRATCH AREA
;
ROVECT:	DW	0		; r/o vector
LOGIN:	DW	0		; logged in vector
DMAADR:	DW	80H		; dma address
CSTEST:	DW	0		; checksummed dir enties address word
SAVTRK:	DW	0		; address of previous track seeked
SAVSAL:	DW	0		; address of track aligned sector seeked
DIRBUF:	DW	0		; dir buffer address
DPB:	DW	0		; dpb address
CSV:	DW	0		; checksum table start address
ALV:	DW	0		; allocation buffer start address
;
; ** DISK PARAMETER BLOCK
;
SPT:	DW	0		; sectors / track
BSH:	DB	0		; block shift factor
BLM:	DB	0		; block maskfactor
EXM:	DB	0		; extent mask factor
DSM:	DW	0		; total storage capacity
DRM:	DW	0		; directory entries
AL0AL1:	DW	0		; blocks allocated for directory
CKS:	DW	0		; dir check vector
OFFSET:	DW	0		; track offset
;
SKEWTB:	DW	0		; address of skew table
UNKNOW:	DB	0		; don't now yet ?????
RWFLAG:	DB	0		; R/W flag 0=WRITE/FF=READ
RETCOD:	DB	0		; return code from
MODE:	DB	0		; R/W mode 0=RND/1=SEQ/2=RND+ZF 
RGESVE:	DB	0		; bdos call reg E saving
BLKOFST:DB	0		; offset of blocknumber in dir record 0-15
MATCH:	DB	0		; match count saving in dir search
FCBTMP:	DW	0		; previous fcb address from user
	DW	0		; not used !
BLK255:	DB	0		; more than 255 blocks on disk
ADKFLG:	DB	0		; alternate drive used flag 
OLDDSK:	DB	0		; previous default disk
TMPDSK:	DB	0		; new temporary disk
CRECXT:	DB	0		; current record in this extend
EXTMSK:	DB	0		; mask of user fcb extend on extend mask
RWRECO:	DW	0		; current read write record
CURBLK:	DW	0		; current block
CURBLS:	DW	0		; current block saving temporary
DIROFF:	DB	0		; pointer in directory scan
CNTER: 	DB	0		; LOW counter in dir scan 1 for 32 byte
CNTERH:	DB	0		; HIGH of it
CURCSP:	DW	0		; current checksum calc pointer
;
;
; *** PATCH AREA
;
; ** logon disk patch if next byte of fcb also E5 rest of disk not used
;
PATCH1:	INX	H		; get next dir fcb byte 
	CMP	M		; also E5
	JNZ	LOGDSK2		; no check next
	RET			; return all other entries never used
;
	DS	12,0		; free patch area
;
	END	BDOS
; 
