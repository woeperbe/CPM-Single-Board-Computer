;	
;
;
;***************************************************************
;*                                                             *
;*       BIOS FOR THE NOTSOSIMPLE8085 SINGLE BOARD COMPUTER    *
;*       WRITTEN BY MARC BUFFET 2025                           *
;*       PUBLIC DOMAIN SOFTWARE                                *
;*                                                             *
;*       DOUBLE DENSITY VERSION FOR 26 SECTORS OF 128 BYTES    *
;*                                                             *
;***************************************************************
;
;
;		MEMORY LAYOUT
;
;		
; 		BIOS	F000-FFFF
;		RAM		EE00-EFFF
;		BDOS	DE00-EDFF
;		CCP		CE00-DDFF
; 		MONITOR BE00-CDFF (If Loaded)
;		TPA		0000-CDFF
;
;
;  DIP SITCH : 	1 - Baud Rate
;				2 - Baud Rate
;				3 - Baud Rate
;				4 - 8E2 / 7N1
;				5 - Formfeed at poweron
;				6 - A: disk is 40 Tracks/ 80 Tracks
;				7 - Boot Monitor or Selection Menu
;				8 - Clear ram at Power ON ( Only )
;
;
				PAGE	
				.8085							; I have a 8085 CPU :-)				
;
; COMMON P8251 BIT PATTERNS
;	
RX_MASK 		EQU 	00000010B         	; RXRDY FLAG BIT IN STATUS BYTE
TX_MASK 		EQU 	00000001B         	; TXRDY FLAG BIT IN STATUS BYTE
RD_MASK			EQU 	00000100B		  	; TX Empty flag
DT_MASK 		EQU 	10000000B         	; DSR FLAG BIT IN STATUS BYTE
RTS_ON  		EQU 	00100111B		 	; RTS ON -  RX and TX enable DTR on
RTS_OFF 		EQU 	00000111B		 	; RTS OFF -  RX and TX enable DTR on
TE_MASK 		EQU     00000100B
DS_MASK 		EQU 	10000000B			; DSR flag bit in status byte
RTS_ON  		EQU     00100111B			; RTS ON -  RX and TX enable DTR on
RTS_OFF			EQU 	00000111B			; RTS OFF -  RX and TX enable DTR on
;
; WD2797 COMMANDS
;
WDSTPRATE		EQU		01B
WDHOME			EQU		00001100B+WDSTPRATE
WDSEEK			EQU		00011100B+WDSTPRATE
WDREADH0		EQU		10001000B			; Read sector Side 0
WDREADH1		EQU		10001010B			; Read sector Side 1
WDWRITEH0		EQU		10101000B			; Write sector Side 0
WDWRITEH1		EQU		10101010B			; Write sector Side 1

WDFORMH0		EQU		11110000B			; Format track side 0
WDFORMH1		EQU		11110010B			; format track side 1
;
; DIP SWITCHES 
;
SWTCH			EQU		00H				 	; DIP Switch port
;
; CONSOLE UART
;
UARTD			EQU		20H					;Data address
UARTC			EQU		21H					;Control address
UARTS			EQU		21H					;Status address
;
; READER PUNCHER UART IF PRESENT 
;
UBRTD			EQU		40H					;Data address
UBRTC			EQU		41H					;Control address
UBRTS			EQU		41H					;Status address
;
; PPI Controller for FDC
;
PPIPA 			EQU 	10H              	; 8255A PORT A	--> Input lines for FDC
PPIPB 			EQU 	11H               	; 8255A PORT B	--> Output line for FDC
PPIPC 			EQU 	12H               	; 8255A PORT C	--> LED Register
PPICTRL 		EQU 	13H               	; 8255A CONTROL REGISTER
;
; PPI CONTROLLER FOR EPROM
;
PPIRA 			EQU 	70H              	; 8255A PORT A	--> Address low
PPIRB 			EQU 	71H               	; 8255A PORT B	--> Address High
PPIRC 			EQU 	72H               	; 8255A PORT C	--> Data Bus
PPIRTRL 		EQU 	73H               	; 8255A CONTROL REGISTER
;
; COUNTER TIMER	
;
CTCC0 			EQU 	30H               	; P8254 COUNTER 0	--> UART CONSOLE
CTCC1 			EQU 	31H               	; P8254 COUNTER 1	--> UART PUNCHER READER
CTCC2 			EQU 	32H               	; P8254 COUNTER 2	--> TIMER FOR MOTOR ON
CTCCMD 			EQU 	33H               	; P8254 CONTROL REGISTER
;
;------------- FDC REGISTERS -------------
;
FDC_CMD     	EQU 	60H					; FDC Controller command register
FDC_TRACK   	EQU 	61H					; track register
FDC_SECTOR  	EQU 	62H					; sector register
FDC_DATA    	EQU 	63H					; data register
;
;------------- DMA REGISTERS -------------
;
DMA_ADDR0		EQU 	50H					; FDC DMA controller ADDRESS
DMA_CNT0	   	EQU 	51H					; FDC DMA Controller byte count
DMA_CMD     	EQU 	58H					; FDC DMA Controller command register
;
;------------- FORMAT PARAMETERS ----------
;
SECTORS_PER_TRACK EQU 	26					; 26
SECTOR_SIZE       EQU 	128					; CP/M 2.2 restriction without blocking/deblocking
TRACK_COUNT       EQU 	41					; Track count on 5 inch floppy
SIDES             EQU 	2					; ouble side
;
DATA_FILL         EQU 	0E5H				; default bytes i sector
;
; ------------- GLOBAL PARAMETER
;
CR				EQU		0DH					; carriage return
LF				EQU		0AH					; Line feed
FF				EQU		0CH					; Form feed
;
; DISK SELECT AND SETTINGS
;	
DSKSEL0			EQU 	00100101B			; Drive 0 (A:) for FM Single density
DSKSEL1			EQU		00110010B			; Drive 1 (B:) for FM Single density
;
DSKSEL0DD		EQU 	00000101B			; Drive 0 (A:) for MFM Double density
DSKSEL1DD		EQU		00010010B			; Drive 1 (B:) for MFM Double density
;
;
BIOS			EQU		0F000H				; Where i put the BIOS
TPA				EQU		00100H				; Start of TPA area in RAM
LOADER			EQU		TPA					; BIOS Floppy Loader
;
;
CCP				EQU		0CE00H				; CCP  LOCATION ( ENHANCED ONE )
CCPEND			EQU		CCP+4096				
;
BDOS			EQU		0DE00H				; BDOS	LOCATION
BDOSEND			EQU		BDOS+4096-256		
;
RAM				EQU		0EE00H				; Bios RAM Area	
;
;	GLOBAL VARIABLES				
;
PUBLIC			EQU		040H				; Where public flag is
DEFDSK			EQU		4					; Current selected disk
;
RAMSIG			EQU		000CH				; Where i put RAM CLEAR signature
;
; 	MONITOR LOCATIONS
;
MONITORS		EQU		0BE00H
MONITORE		EQU		0CE00H
;
; 	BOOTROM CP/M AND MONOTOR LOCATIONS
;
LOCCCPE			EQU		5000H				; Enhanced CCP AT 0CE00H
LOCBDOSE		EQU		4000H				; Enhanced BDOS AT 0DE00H
;
LOCCCPS			EQU		7000H				; Standard CCP	AT 0CE00H		
LOCBDOSS		EQU		6000H				; Standard BDOS AT 0DE00H
;
MONINROM		EQU		00000H				; Monitor (4K)
;
;
;	ROM SIZE DEFINITION
;
ROMSTART		EQU		0E000H				; Since i use 8K EEPROMS for now
;
; MACROS DEFINITIONS
;
				LIST	MACRO
;
				MACRO	LED	 L,S			; Led Macro LED,ON/OFF
				PUSH	PSW
				PUSH	B					; Save BC
				MVI		B,L					; Move led to B
				MVI		C,S					; Led ON/OFF to C
				CALL	SETLED				; call led process	
				POP		B					; restore BC
				POP		PSW
				ENDM

;
;-----------------------------------------------------------------------
; MONITOR CODE START
;-----------------------------------------------------------------------
;
;
				ORG		ROMSTART			; Boot mechanism for the SN74LS74 system
				JMP		BIOS				; jump for RAM/ROM SWAPPING FLIP/FLOP reset	
				DS		BIOS-$		  		; ROM EMPTY FROM 0000 TO 3FFF
;				
;
;-----------------------------------------------------------------------
; BIOS CODE START
;-----------------------------------------------------------------------				
;
				ORG		BIOS				; Big job starts here
;	
				JMP		CBOOT				;-3: Cold start routine
				JMP		WBOOT				; 0: Warm boot - reload command processor
				JMP		CONST				; 3: Console status
				JMP		CONIN				; 6: Console input
				JMP		CONOUT				; 9: Console output
				JMP		LIST				;12: Printer output
				JMP		PUNCH				;15: Paper tape punch output
				JMP 	READER				;18: Paper tape reader input
				JMP		HOME				;21: Move disc head to track 0
				JMP		SETDISK				;24: Select disc drive
				JMP		SETTRK				;27: Set track number
				JMP		SETSEC				;30: Set sector number
				JMP		SETDMA				;33: Set DMA address
				JMP		READ				;36: Read a sector
				JMP		WRITE				;39: Write a sector
				JMP		LISTST				;42: Status of list device
				JMP		SECTRAN				;45: Sector translation for skewing
;
				JMP		MOVETABLE			;48: Move table for DPB block to RAM
				JMP		MOVESYS				;51: Load CP/M Image in RAM
				JMP		PRTMSG				;54: Print message following this call
				JMP		FORMAT_DISK			;57: Format disk subroutine
				JMP		LOCATION			;60: BC = CCP start DE = BDOS Start
;
;
;
;-----------------------------------------------------------------------
; COLD BOOT LOAD SYSTEM
;-----------------------------------------------------------------------
;
CBOOT:			MVI		A,10010000B			; PPI PORTS  A INPUT  B AND C DEFINED AS OUTPUT
				OUT		PPICTRL				; INIT PPI
				MVI		A,10001001B			; PPI PORT EPROM A and B Output C Input 
				OUT		PPIRTRL				; Set control register
				MVI		A,0FFH				; All bits 1
				OUT		PPIRA				; to powerdown
				OUT		PPIRB				; totaly EPROM
;
;-----------------------------------------------------------------------
;	CLEAR RAM IF ACTIVATED
;-----------------------------------------------------------------------
;		
				IN		SWTCH				; Read the switches
				ANI		10000000B			; Read switch 8
				CPI		0					; Switch is OFF ?
				JZ		SETBDS				; Skip Ram clear
				LDA		RAMSIG				; Load signature location low
				CPI		055H				; See if it is 55
				JNZ		INIT0				; nope init ram after power on
				LDA		RAMSIG+1			; get signature location high
				CPI		0AAH				; see it it is AA
				JNZ		INIT0				; noper must be power on 
				JMP		SETBDS				; skip copyright message and RUN basic
INIT0:			LXI		H,0					; init HL to 0
				MVI		A,00				; Acu to 0
INIT4:			MOV		M,A					; clear memory byte
				CMP		M					; check again
				JNZ		INIT5
				INX		H					; increment pointer
				JMP		INIT4
;
INIT5:			SHLD	EOM					; Store End Of Memory pointer			
				MVI		A,055H				; prepare signature	low
				STA		RAMSIG				; store it
				MVI		A,0AAH				; prepare signature high
				STA		RAMSIG+1			; store it
;
SETBDS:			LXI		SP,STACK			; Initial Stack pointer
				CALL	INIT				; Init all UART stuff
				CALL	SWEEPLEDS			;
				XRA		A
				STA		BOOTMODE
				STA		DEFDSK				; drive A
				STA		PUBLIC				; public flag OFF
				INR		A
				STA		NEEDHOME
;			
				IN		SWTCH				; Read SWITCH position
				ANI		01000000B			; Switch 7 ON ?
				JZ		STARTMON			; Jump to Monitor
;				
CBOOT0:			IN		SWTCH				; Read the switches
				ANI		00010000B			; Read switch 5
				CPI		0					; Switch is OFF ?
				JZ		CBOOT07				; Skip sreen clear
				CALL	PRTMSG				; Clear screen before signon
				DB		FF,0				; Formfeed
CBOOT07:		CALL	PRTMSG				; Display Signon message
				DB		CR,LF,"WOEPER System Menu Ver 3.5.2",0
;
				IN		SWTCH				; Read the switches
				ANI		10000000B			; Read switch 8
				CPI		0					; Switch is OFF ?
				JZ		CBOOT8				; Skip Ram display message
				CALL	PRTMSG				; Display ram space message
				DB		CR,LF,"Ram space is ",0
				LHLD	EOM					; Get EOM value
				MOV		A,H					; high to A
				RRC							; divide by 4
				RRC							; by shift 2 times
				MOV		L,A					; value to L
				MVI		H,0					; and H to 0
				CALL	DECOUT				; print number of KB to console	
				CALL	PRTMSG				; End of message
				DB		" Kb",0				; display KB
;				
CBOOT8:			CALL	PRTMSG				; Print drive parameter
				DB		CR,LF,"Drive A: is set for ",0
				IN		SWTCH
				ANI		00100000B			; Switch 6 - Drive A = 80 track ?
				JZ		CBOOT01				; YES  80 tracks
				LXI 	H,40				; no 40
				JMP		CBOOT02
CBOOT01:		LXI		H,80				; Else 80
CBOOT02:		CALL	DECOUT				; Display it
				CALL	PRTMSG				; display selection menu
				DB		" tracks",CR,LF
				DB		"1 - Boot from Floppy A:",CR,LF
				DB		"2 - Load CP/M 2.2 Enhanced",CR,LF
				DB		"3 - Load CP/M 2.2 Standard",CR,LF
				DB		"4 - Warm Boot",CR,LF
				DB		"5 - Load INTEL MDS file and warm boot",CR,LF
				DB		"6 - Format floppy disk",CR,LF
				DB		"7 - Load Monitor",CR,LF
				DB		"Select : ",0
;
CBOOTL:			CALL	CONIN				; get command
				MOV		C,A					; to C
				CPI		'1'					; between 1
				JC		CBOOTL				; no ignore
				CPI		'9'+1				; and 9
				JNC		CBOOTL				; no ignore
;						
				CALL	CONOUT				; echo it valid command
				MOV		A,C					; mov C to A again
				ANI		0FH					; make binary
				MOV		E,A					; to DE	
				MVI		D,0					;
				LXI		H,CMDTAB-2			; HL point to table with offset 1
				DAD		D					; add twice offset
				DAD		D					
				MOV		E,M					; Get routine address low
				INX		H					; bump index
				MOV		D,M					; get routine address high
				XCHG						; address to HL
				PCHL						; and jump to it
				
CMDTAB:			DW		BTFLP				; Boot from Floppy Disk
				DW		EPROME				; Boot from EPROM Enhanced
				DW		EPROMS				; Boot from EPROM Standard
				DW		WBOOT				; Warm boot
				DW		INTMDS				; Load Intel MDS and warm boot
				DW		FORMAT				; Format disk
				DW		STARTMON			; load from EPROM and jump to monitor 
				DW		CBOOTL				; Not used yet
				DW		CBOOTL				; not used yet
;			
;
;---------------------------------------------------------------------
; BOOT FROM FLOPPY DISK LOAD TRACK 0 SECTOR 1 AT 0100 AND JUMP TO IT
;---------------------------------------------------------------------
;
BTFLP:			MVI		A,1
				STA		BOOTMODE			; Save how it all started
				CALL	PRTMSG
				DB		CR,LF,"Booting from floppy ..."
				DB		CR,LF,"Digital Research 1979",CR,LF		
				DB		"CP/M 2.2 Enhanced",CR,LF,0
				CALL	MOVETABLE			; Move disk table to RAM
				CALL	LOADSYS
				MVI		A,0					; Load Full
				JMP		LOADER				; Loop again
;	
;---------------------------------------------------------------------
;  LOAD SYSTEM BOOT SECTOR AND JUMP TO IT
;---------------------------------------------------------------------		
;	
LOADSYS:		LED		7,1					; Motor ON LED on
				LXI		B,0					; Disk A:
				CALL	SETDISK				; Set it
				CALL	HOME				; Home seekdel 1  drive	
				LXI		B,0					; track 0
				CALL	SETTRK				; set it
				LXI		B,1					; Sector 1
				CALL	SETSEC				; Set it	
				LXI		B,LOADER			; Point to Loader ram area
				CALL	SETDMA				; set DMA	
				CALL	READ				; Go read the boot sector
				CPI		0					; OK ?
				JNZ		LOADSYS1			; NO error start over
				LDA		LOADER+128-2		; Get first byte of signature	( writen by SYSGEN )
				CPI		055H				; Is it 55H ?
				JNZ		LOADSYS0			; No bootloader present
				LDA		LOADER+128-1		; Get next byte of signature
				CPI		0AAH				; is it AAH
				JNZ		LOADSYS0			; No bootloader present
				RET
;				
LOADSYS0:		CALL	PRTMSG
				DB		CR,LF,"Non system disk.",CR,LF,0
				CALL	CONIN
				JMP		CBOOT
LOADSYS1:		CALL	PRTMSG
				DB		CR,LF,"Boot loader disk error.",CR,LF,0
				CALL	CONIN
				JMP		CBOOT
;
;-----------------------------------------------------------------------
; BOOT CP/M FROM EPROM ENHANCED OR STANDARD
;-----------------------------------------------------------------------
;				
EPROME:			MVI		A,2
				STA		BOOTMODE			; Save how it all started
				CALL	CBOOTMSG
				CALL	PRTMSG
				DB		"CP/M 2.2 Enhanced",CR,LF,0
				MVI		C,0					; Load CCP and BDOS
				MVI		B,0					; Select ENHANCED mode
;				
CBOOT5:			CALL	MOVESYS				; Move CP/M from EPROM
				CALL	MOVETABLE			; Move disk table to RAM
				JMP		CCP					; Start cpm
;				
EPROMS:			MVI		A,3
				STA		BOOTMODE
				CALL	CBOOTMSG
				CALL	PRTMSG
				DB		"CP/M 2.2 Standard",CR,LF,0
				MVI		C,0					; Load CCP and BDOS
				MVI		B,1					; Select STANDARD mode
				JMP		CBOOT5				; Go do it
;
CBOOTMSG:		CALL	PRTMSG
				DB		CR,LF,"Booting from EPROM ..."
				DB		CR,LF,"Digital Research 1979",CR,LF,0	
				RET
;
;---------------------------------------------------------------------
; BOOT FROM STORAGE SYSTEM EPROM 
;  C=0 BDOS + CCP / C=1 CCP ONLY / B=0 Enhanced / B=1 Standard
;---------------------------------------------------------------------
;
MOVESYS:		LED		3,1
				MVI		A,10001001B			; A and B Output C Input 
				OUT		PPIRTRL				; Set control register
				PUSH	B	
				MOV		A,C					; Get job code
				CPI		0					; Full move ?
				JNZ		MOVECCP				; No CCP only
				LXI		H,BDOS				; Point to start of BDOS
				MOV		A,B					; Get version flag
				CPI		1					; Standard CP/M
				JZ		MOVESYS1			; Yes go select correct EPROM location		
				LXI		D,LOCBDOSE			; Start of CP/M core on EPROM
				JMP		MOVEBDOS			; Go move it to RAM	
MOVESYS1:		LXI		D,LOCBDOSS			; Point to standard version in EPROM
;				
MOVEBDOS:		MOV		A,E					; Get EPROM address pointer low 
				OUT		PPIRA				; to A Port	
				MOV		A,D					; Prepare pointer High
				OUT		PPIRB				; To port B
				IN		PPIRC				; Get data byte
				MOV		M,A					; move it to RAM
				INX		H					; bump index
				INX		D					; bump eprom address
				LXI		B,BDOSEND			; load BDOSEND
				MOV		A,H					; Compare if done
				CMP		B					; High
				JNZ		MOVEBDOS			; not yet continue
				MOV		A,L					; get low
				CMP		C					; compare
				JNZ		MOVEBDOS			; not yet
;
MOVECCP:		POP		B					; get back version flag 
				MOV		A,B					; to ACU
				CPI		1					; Standard version ?	
				JZ		MOVECCP3			; Yes go select it
				LXI		D,LOCCCPE			; Where Enhanced CCP is in EPROM
				JMP		MOVECCP5			; go move it		
MOVECCP3:		LXI		D,LOCCCPS			; prepare standard CCP	
MOVECCP5:		LXI		H,CCP				; Where CCP is in RAM		
				
MOVECCP0:		MOV		A,E					; Address low to A
				OUT		PPIRA				; output to port A
				MOV		A,D					; high to A
				OUT		PPIRB				; to port B
				IN		PPIRC				; Get data byte
				MOV		M,A					; move to memory
				INX		H					; bump index
				INX		D					; increment eprom address
				LXI		B,CCPEND			; prepare endlocation
				MOV		A,H					; high to A
				CMP		B					; Compare
				JNZ		MOVECCP0			; not yet continue
				MOV		A,L					; low to A
				CMP		C					; compare
				JNZ		MOVECCP0			; not yet
				MVI		A,0FFH				; All bits 1
				OUT		PPIRA				; to powerdown
				OUT		PPIRB				; totaly EPROM
				LED		3,0
				RET							; all done return
;
;-----------------------------------------------------------------------
; LOAD MONITOR IN RAM AND JUMP TO IT
;-----------------------------------------------------------------------
;
STARTMON:		LXI		D,MONINROM			; prepare eprom location	
				LXI		H,MONITORS			; Where MONITOR is in RAM		
MOVMON0:		MOV		A,E					; Address low to A
				OUT		PPIRA				; output to port A
				MOV		A,D					; high to A
				OUT		PPIRB				; to port B
				IN		PPIRC				; Get data byte
				MOV		M,A					; move to memory
				INX		H					; bump index
				INX		D					; increment eprom address
				LXI		B,MONITORE			; prepare endlocation
				MOV		A,H					; high to A
				CMP		B					; Compare
				JNZ		MOVMON0				; not yet continue
				MOV		A,L					; low to A
				CMP		C					; compare
				JNZ		MOVMON0				; not yet
				MVI		A,0FFH				; All bits 1
				OUT		PPIRA				; to powerdown
				OUT		PPIRB				; totaly EPROM
				JMP		MONITORS
;
;-----------------------------------------------------------------------
; LOAD INTEL MDS FILE IN RAM
;-----------------------------------------------------------------------
;
INTMDS:			CALL	INTELMDS			; Go load the INTEL MDS HEX File
				MVI		A,2					; force bootmode 2
				STA		BOOTMODE
				JMP		WBOOT				; Jump to CCP	
;
;-----------------------------------------------------------------------
; RESTART TO CCP
;-----------------------------------------------------------------------
;
WBOOT:			LXI		SP,STACK			; Restore local Stack
				LDA		BOOTMODE			; Get initial bootmode
				CPI		1					; from floppy ?
				JZ		WBOOT4				; yes process it
				CPI		2					; From EPROM Enhanced ?
				JZ		WBOOT2				; Yes do it else drop into WBOOT1 for standard version
;				CPI		3
;				JZ		WBOOT1
;				
WBOOT1:			MVI		C,1					; Only CCP
				MVI		B,1					; Standard version
				CALL	MOVESYS				; Go load the system
				JMP		WBOOT3				; exit to it
;
WBOOT2:			MVI		C,1					; Only CCP
				MVI		B,0					; Enhanced version
				CALL	MOVESYS				; Go load it and drop into WBOOT3
;
WBOOT3:			CALL	MOVETABLE
				XRA		A
				STA		PUBLIC
				LDA		DEFDSK				; GET CURRENT DISK
				ANI		0FH
				MOV		C,A					; TO C
				JMP		CCP					; Start the magic
;
WBOOT4:			CALL	LOADSYS				; Go load bootsector
				MVI		A,1					; Reload CCP only --> ACU = 1 see SYSGEN.ASM
				JMP		LOADER				; Go execute that loader
				
;
;-----------------------------------------------------------------------
; LOCATION RETURN DE = BDOS start BC = CCP start HL = RAM 
;-----------------------------------------------------------------------
;
LOCATION:		LXI		H,RAM				; HL to RAM
				LXI		D,BDOS				; DE is start of BDOS
				LXI		B,CCP				; BC is start of CCP
				RET

;
;-----------------------------------------------------------------------
; CONSOLE STATUS 0 - NONE / FF - GOT CHARACTER
;-----------------------------------------------------------------------
;
CONST:			CALL	MOTOROFF			; Motor OFF ?
				IN 		UARTS               ; Get 8251 status
				ANI 	RX_MASK             ; Test for ready
				RZ 							; no char return 0
				XRA		A					; clear acu
				DCR		A					; set to FF
				RET							; return
;
;----------------------------------------------------------------------
;  READ CHARACTER FROM CONSOLE
;----------------------------------------------------------------------
;
CIN:			IN      UARTS				; Get UART status
				ANI		00000010B			; receiver ready?
				JZ		CIN					; No wait until character recieved
				IN 		UARTD				; Get it now
				RET							; and return
;
;-----------------------------------------------------------------------
; GET CHARACTER FROM CONSOLE FOR MOTOR CONTROL
;-----------------------------------------------------------------------
;
CONIN:			CALL	MOTOROFF			; stop motors ?
				IN 		UARTS               ; Get 8251 status
				ANI 	RX_MASK             ; Test for ready
				JZ		CONIN               ; No char yet
				IN 		UARTD               ; Get 8251 data
				CPI		02H					; Control-B ?
				JZ		BIOS				; Reset system
				CPI		17H					; Control-W ?
				JZ		STARTMON
				RET							; return to BDOS
;
;-----------------------------------------------------------------------
; OUTPUT CHARACTER TO CONSOLE
;-----------------------------------------------------------------------
;
CONOUT:			PUSH 	PSW                 ; Save acu
CONOUT1: 		IN 		UARTS               ; Get 8251 status
				ANI     TX_MASK             ; Test TX Ready bit
				JZ 		CONOUT1             ; Not ready
				MOV		A,C					; get char to print
				OUT 	UARTD               ; Write 8251 data
CONOUT2:		IN      UARTS				; Get 8251 statusGF
				ANI		RD_MASK				; Test TX empty 
				JZ     	CONOUT2				; Wait until send
				POP		PSW					; restore psw
				RET							; return
;
;
;-----------------------------------------------------------------------
;  CHARACTER TO PRINTER OVER PORT UBRT
;-----------------------------------------------------------------------
;
LIST:			IN		UBRTS				; Get status
				ANI		DS_MASK				; DSR pin ?
				JZ		LIST				; wait until ready
LIST1:			IN		UBRTS				; get status
				ANI		TX_MASK				; transmitter ready?
				JZ		LIST1				; no terminate currect char first
				MOV		A,C					; restore character
				OUT		UBRTD				; transmit
LIST2:			IN		UBRTS				; get status
				ANI		TE_MASK				; check if transmitter is empty	
				JZ		LIST2				; wait until so
				RET							; return to BDOS
;
;-----------------------------------------------------------------------
; LIST DEVICE STATUS
;-----------------------------------------------------------------------
;
LISTST:			IN		UBRTS				; Get status
				ANI		DS_MASK				; List device ready ?
				JZ		LISTST1				; No JUMP 	
				MVI		A,0					; return 0
				RET							; to BDOS
LISTST1:		MVI		A,0FFH				; Return -1 
				RET							; to bdos
;
;
;-----------------------------------------------------------------------
; OUTPUT CHARACTER TO PUNCHER ( UBRT )
;-----------------------------------------------------------------------
;
PUNCH:			IN		UBRTS				; Get UART status
				ANI		TX_MASK				; transmitter ready?
				JZ		PUNCH				; Wait until OK
				MOV		A,C					; restore character
				OUT		UBRTD				; transmit
PUNCH2:			IN		UBRTS				; get status
				ANI		TE_MASK				; check if transmitter is empty	
				JZ		PUNCH2				; wait until so
				RET
;
;
;-----------------------------------------------------------------------
; GET CHARACTER FROM READER (UBRT)
;-----------------------------------------------------------------------
;
READER:			IN		UBRTS				; Read status
				ANI		RX_MASK				; char ready ?
				JZ		READER				; No wait for it
				IN		UBRTD				; read char in A
				RET							; return to caller
;
;-----------------------------------------------------------------------
; SELECT DISK AND RETURN DCB 
;-----------------------------------------------------------------------
;
SETDISK:		XRA		A					; Clear acu
				STA		MOTSTATUS			; motor status OFF
				MOV		A,C					; Disk to A
				STA		SELDISK				; save for later
				CPI 	0          			; Only support drive 0 and 1
				JZ		SETDISK0			; Disk 0 selected
				CPI		1					; 1 ?
				JZ		SETDISK1			; disk 1 selected
				LED		1,0
				LED		2,0
				LXI 	H,0					; return error to BDOS
				RET							;
;
SETDISK0:		LED		1,1
				LED		2,0
				IN		SWTCH				; Read dip switches
				ANI		00100000B			; Switch 6 Disk A: = 80 track ?
				JZ		SETDISK2			; YES return DPB1 for 80 tracks
				LXI 	H,DPH0				; point to Disk Parameter Block
				RET							; return
SETDISK2:		LXI 	H,DPH2				; point to Disk Parameter Block
				RET	
;				
SETDISK1:		LED		1,0
				LED		2,1
				LXI 	H,DPH1				; point to disk parameter block
				RET							; and return to BDOS
;
;-----------------------------------------------------------------------
; RESTORE FLOPPY TO TRACK ZERO
;-----------------------------------------------------------------------
;
HOME:		   	CALL	MOTORON
				XRA		A
				STA		NEEDHOME
				LED		4,1					; Turn seek led ON
				MVI		A,00001101B			; Prepare HOME command
				OUT		FDC_CMD				; To FDC
				CALL	FDC_BUSY			; Still busy so wait
				IN		FDC_CMD				; Get status 		
				ANI		00000100B			; Track Zero reached ?
				CPI		00000100B			; Test it
				JZ		HOME0				; OK at track Zero -- > Exit
				CALL	PRTMSG				; Track zero not found
 				DB		0Dh,0Ah,"BIOS: Track 0 not found",0Dh,0Ah,0
				RET							; return
HOME0:			LED		4,0					; Seek led OFF
				RET							; and return
;
;
;-----------------------------------------------------------------------
; SET TRACK NUMBER
;-----------------------------------------------------------------------
;
SETTRK:			MOV		A,C					; Get tracknumber ( 0 - 80 )
				ORA		A					; Clear Carry
				RAR							; Calculate real track number
				STA		SELTRACK			; store it
				MOV		A,C					; get tracknumber again
				ANI		1					; mask out head selection
				STA		SELHEAD				; store selected head
				RET							; return to caller
;
;
;-----------------------------------------------------------------------
; SET SECTOR NUMBER
;-----------------------------------------------------------------------
;
SETSEC:			MOV		A,C					; Get sector
				STA		SELSECTOR			; save it
				RET							; return to BDOS
;
;-----------------------------------------------------------------------
; SET DMA TRANSFER ADDRESS in BC
;-----------------------------------------------------------------------
;
SETDMA:			PUSH	H					; Save H
				PUSH	B
				MOV		L,C					; Move dma address to HL
				MOV		H,B					;
				SHLD	SELDMA				; Store it
				POP		B
				POP		H					; restore H
				RET							; return
;
;-----------------------------------------------------------------------
; READ THE SECTOR
;-----------------------------------------------------------------------
;
READ:			CALL	MOTORON				; make sure motor is on
				LED		5,1					; Read LED ON
				LDA		SELTRACK			; Get track number
				MOV		C,A					; to C
				IN		FDC_TRACK			; Get current track we are on
				CMP		C					; compare
				CNZ		SEEK				; Not Good Seek to it
				LDA		SELSECTOR			; Get sector number
				OUT		FDC_SECTOR			; to Register
				LDA		SELTRACK			; Get Track
				OUT		FDC_TRACK
				LHLD	SELDMA				; Get DMA Address
				MVI		C,128				; and 1 sector count
				CALL	SETDMAR				; init DMA for REad from Ram
				LDA		SELHEAD				; get head
				CPI		1					; head 1
				JZ		READ0				; prepare command for head 1
				MVI		A,10001000B			; Read sector Side 0
				JMP		READ1				; Do the command
READ0:			MVI		A,10001010B			; Read sector Side 1
READ1:			OUT		FDC_CMD				; Execute the READ
				CALL	FDC_BUSY			; until done
				IN		FDC_CMD				; Get status
				ANI		10011100B			; Isolate error bits ( 
				JZ		READ2				; All OK
				MVI		A,0					; Report READ error
				CALL	SHWERR			;
				MVI		A,1					; Return error
				RET
READ2:			LED		5,0					; Set READ Led OFF
				MVI		A,0						
				RET
;
;
;-----------------------------------------------------------------------
; WRITE THE SECTOR
;-----------------------------------------------------------------------
;
WRITE:			CALL	MOTORON				; make sure motor is on
				LED		6,1					; Write led ON
				LDA		SELTRACK			; Get Track
				MOV		C,A					; To C	
				IN		FDC_TRACK			; Get current track
				CMP		C					; compare
				CNZ		SEEK				; Not equal call seek
				LDA		SELSECTOR			; get sector
				OUT		FDC_SECTOR			; to sector register
				LDA		SELTRACK			; Get Track
				OUT		FDC_TRACK
				LHLD	SELDMA				; Load Transfer address
				MVI		C,128				; count one sector
				CALL	SETDMAW				; init DMA for write
				LDA		SELHEAD				; Get head needed
				CPI		1					; Head 1
				JZ		WRITE0				; Yes jump
			 	MVI		A,10101000B			; Write sector Side 0
				JMP		WRITE1				; continue
WRITE0:			MVI		A,10101010B			; Write sector Side 1
WRITE1:			OUT		FDC_CMD				; Execute command 
				CALL	FDC_BUSY			; Wait until ready ?
				IN		FDC_CMD				; Get FDC Status
				ANI		01000000B			; write protect ?
				JNZ		WRITE3				; yes report it
				IN		FDC_CMD
				ANI		10011100B			; rnf/lost data/CRC
				JNZ		WRITE2				; Not OK process error
				LED		6,0
				MVI		A,0					; Return OK
				RET
WRITE2:			MVI		A,1					; Report WRITE error	
				CALL	SHWERR				; do it	
				MVI		A,1					; CRC / RNF / LOST DATA Error
				LED		6,0
				RET
WRITE3:			MVI		A,2					; Write protected disk
				LED		6,0
				RET
;
;-----------------------------------------------------------------------
; PROCESS ERROR A=1 -> WRITE A=0 -> READ
;-----------------------------------------------------------------------
;				
SHWERR:			CPI		1
				JZ		SHWERR1		
				CALL	PRTMSG
				DB		CR,LF,"BIOS: Read Error ",0
				JMP		SHWERR2
SHWERR1:		CALL	PRTMSG
				DB		CR,LF,"BIOS: Write Error ",0
SHWERR2:		IN		FDC_CMD
				ANI		00010000B
				JZ		SHWERR3
				CALL	PRTMSG
				DB		"RNF ",0
SHWERR3:		IN		FDC_CMD
				ANI		00001000B
				JZ		SHWERR4
				CALL	PRTMSG
				DB		"CRC ",0
SHWERR4:		IN		FDC_CMD
				ANI		00000100B
				JZ		SHWERR5	
				CALL	PRTMSG
				DB		"LOST DATA ",0
SHWERR5:		CALL	PRTMSG
				DB		"on track ",0
				IN		FDC_TRACK			; Get Track
				MOV		L,A					; to L	
				MVI		H,0
				CALL	DECOUT
				CALL	PRTMSG
				DB		" sector ",0
				IN		FDC_SECTOR			; Get Sector
				MOV		L,A					; to L	
				MVI		H,0
				CALL	DECOUT
				CALL	PRTMSG
				DB		CR,LF,0
				RET
;
;-----------------------------------------------------------------------
; SECTOR TRANSLATE INTERLEAVE   SECTOR - BC TABLE - DE NEW SECTOR IN HL
;-----------------------------------------------------------------------
;
SECTRAN:		MVI		B,0					; Sector in C prepare BC by B=0
				XCHG						; swap to DE TO HL
				DAD		B					; Add offset		
				MOV		L,M					; get new sector number	
				MVI		H,0					; H to zero HL = Sector number
				RET							; return
;
;-----------------------------------------------------------------------
; WAIT UNTIL FDC NOT BUSY
;-----------------------------------------------------------------------
;
FDC_BUSY:		IN		PPIPA				; IN PPIPA
				ANI		00000100B			; Isolate INTQ line
				JZ		FDC_BUSY			; still low wait	
				RET	
FDC_BUSY0:		IN   	FDC_CMD				; Get fdc contrroller status
				ANI  	00000001B			; isolate busy bit
				JNZ  	FDC_BUSY0			; loop until done
				RET							; return
				
;-----------------------------------------------------------------------
; SEEK TO TRACK IN C
;-----------------------------------------------------------------------	
;
SEEK:			LED		4,1					; Seek LED ON
				LXI		H,SEEKCNT			; point to retry counter
				MVI		M,0					; clear it
SEEK0:			MOV		A,C					; Get track to seek
				OUT		FDC_DATA			; to FDC data register
				MVI		A,00011101B		; execute the seek command and set veify flag
				OUT		FDC_CMD				; do it
				CALL	FDC_BUSY			; check if still busy
				IN		FDC_CMD				; get status
				ANI		00010000B			; Check if seek error
				JZ		SEEK1				; No all OK
				PUSH	B
				CALL	HOME				; Restore head to Track 0
				POP		B
				LXI		H,SEEKCNT			; point to retry counter
				MOV		A,M					; get it
				CPI		3					; 3 times ?
				JZ		SEEK2				; Yes report error
				INR		M					; not yet 3 increment it
				JMP		SEEK0				; and go retry
SEEK2:			CALL	PRTMSG				; Print error
				DB		0Dh,0Ah,"BIOS: Track not found",0Dh,0Ah,0			
SEEK1:			LED		4,0					; Seek LED OFF
				RET							; Return to caller
;
;-----------------------------------------------------------------------
; INIT DMA CHANNEL 0 READ SECTOR ADDRESS HL COUNT C
;-----------------------------------------------------------------------
;
SETDMAR:		MVI		A,01000000B
				OUT		DMA_CMD
				MOV  	A,L					; Address low 
				OUT  	DMA_ADDR0			; to DMA controller
				MOV  	A,H					; Address High
				OUT  	DMA_ADDR0			; to DMA controller
				MOV		A,C					; Count (sector length) 
				OUT  	DMA_CNT0			; to DMA controller
				MVI  	A,01000000B			; set DMA Read mode
				OUT  	DMA_CNT0			; To DMA controller
				MVI		A,01000001B			; Arm the DMA controller
				OUT		DMA_CMD				; To command register
				RET							; DMA ready go ahead

;-----------------------------------------------------------------------
; INIT DMA CHANNEL 0 WRITE SECTOR ADDRESS HL COUNT C
;-----------------------------------------------------------------------
;
SETDMAW:		MVI		A,01000000B
				OUT		DMA_CMD
				MOV  	A,L					; Address low 
				OUT  	DMA_ADDR0			; to DMA controller
				MOV  	A,H					; Address High
				OUT  	DMA_ADDR0			; to DMA controller
				MOV		A,C					; Count (sector length) 
				OUT  	DMA_CNT0			; to DMA controller
				MVI  	A,10000000B			; set DMA Read mode
				OUT  	DMA_CNT0			; To DMA controller
				MVI		A,01000001B			; Enable DMA
				OUT		DMA_CMD				; To command register
				RET							; DMA ready go ahead
;
;-----------------------------------------------------------------------
; INIT DMA CHANNEL 0 FOR FORMAT
;-----------------------------------------------------------------------
;
SETDMAFULL:		MVI		A,01000000B			; Reset full dma
				OUT  	DMA_CMD				; execute it
				LXI		H,FORMATBUF			; Address for track image
				MOV  	A,L					; low to DMA
				OUT  	DMA_ADDR0			;
				MOV  	A,H					; High to DMA
				OUT  	DMA_ADDR0			;
;
				MVI		A,0FFH				; Maximum count	
				OUT  	DMA_CNT0			; to DMA
				MVI  	A,10111111B			; and read from RAM
				OUT  	DMA_CNT0			;
    
				MVI		A,01000001B			; ARM Enable DMA
				OUT		DMA_CMD				; Go !
				RET
;
;
;-----------------------------------------------------------------------
; MOTOR ON START TIMER
;-----------------------------------------------------------------------
;
MOTORON:		LDA		MOTSTATUS			; Get motor status
				ORA		A					; check zero
				RNZ							; no motor is still on
				MVI		A,1					; Set motor status
				STA		MOTSTATUS			; to running
				LED		7,1					; Motor LED ON
				MVI		A,10010000B			; prepare timer
				OUT		CTCCMD				; start timer
				MVI		A,35				; for 50 index rotation count ( 10 Seconds )
				OUT		CTCC2				; to the register
				LDA		SELDISK				; get current disk
				CPI		0					; disk 0
				JNZ		MOTORON1			; no start disk 1	
				MVI		A,DSKSEL0DD			; Disk 0	
				OUT		PPIPB				; start it
				CALL	HOME
				RET							; return to caller
MOTORON1:		MVI		A,DSKSEL1DD			; disk 1
				OUT		PPIPB				; start it
				CALL	HOME
				RET							; return
;
;-----------------------------------------------------------------------
; MOTOR OFF AFTER TIMEOUT
;-----------------------------------------------------------------------
;
MOTOROFF:		IN		PPIPA				; Read timer status 
				ANI		1					; on PA0
				RZ							; Still zero then motor must stay on
				LED		7,0					; Motor LED Off
				XRA		A					; Clear status
				STA		MOTSTATUS			; Motor must be set OFF
				MVI		A,00111111B			; Prepare command
				OUT		PPIPB				; Motors OFF
				RET							; Return
;
;
;-----------------------------------------------------------------------
; STOP ALL MOTORS
;-----------------------------------------------------------------------
;
INIT:			MVI		A,00111111B			; Off all disk selections Motor OFF DDEN DISABLE
				OUT		PPIPB	
;
;
;-----------------------------------------------------------------------
; INITIALIZE THE UARTS
;-----------------------------------------------------------------------
;
;	
				MVI		A,0					; insure not setup mode
				OUT		UARTC				; write once
				OUT		UARTC				; write again (now in operate mode)
				OUT		UARTC				; write again (now in operate mode)	
				MVI		A,040H				; return to setup mode
				OUT		UARTC				; write it
				IN      SWTCH				; read switches
				ANI     08H					; get switch 4 7n1 or 8e2
				JNZ		SETBD3				; 
				MVI		A,11111110B			; 8 data, 2 stop, even parity ,clock 16 time
				JMP		SETBD4
SETBD3:			MVI		A,01001010B			; 7 data, 1 stop, no parity ,clock 16 time
SETBD4:			OUT 	UARTC
				MVI		A,00100111B			; rts,dtr,enable rx and tx
				OUT		UARTC				; write it
				IN      SWTCH				; read switch
				ANI		07H					; select speed bits
SETBD0:			LXI		H,SPDTAB			; table
				LXI		D,2					; offset
SETBD1:			DCR		A					; at end of it
				JM		SETBD2				; yes found
				DAD		D					; add offset
				JMP		SETBD1				; next hit
SETBD2:			MVI		A,00110110B			; counter zero mode 3
				OUT		CTCCMD				; 
				MOV		A,M					; get low
				OUT		CTCC0				; to pit
				INX		H					; get high
				MOV		A,M
				OUT		CTCC0				; to pit
;			
				MVI		A,0					; insure not setup mode
				OUT		UBRTC				; write once
				OUT		UBRTC				; write again (now in operate mode)
				OUT		UBRTC				; write again (now in operate mode)	
				MVI		A,40H				; return to setup mode
				OUT		UBRTC				; write it
				MVI		A,11111110B			; 8 data, 2 stop, even parity ,clock 16 time
SETBD6:			OUT 	UBRTC
				MVI		A,RTS_ON			; rts,dtr,enable rx and tx
				OUT		UBRTC				; write it
SETBD9:			MVI		A,01110110B			; 9600 Baud always for URTB counter one mode 3		
				OUT		CTCCMD				; 
				MVI		A,20				; get low
				OUT		CTCC1				; to pit
				MVI		A,0					; Get high
				OUT		CTCC1				; to pit
				RET
;
;-------------------------------------------------------------------------
; SET LED IN B to VALUE IN C
;-------------------------------------------------------------------------

SETLED:			MOV		A,C					; Led status to A
				ANI		1					; Isolate 1/0
				MOV		C,A					; back to C
				MOV		A,B					; Get LED number
				ANI		7					; Isolate number
				RLC							; Rotate LEFT
				ORA		C					; Combine with status
				ANI		15					; Clean up
				OUT		PPICTRL				; Set or Reset that LED	
				RET							; And return
;
;------------------------------------------------------------------------ 
; PRINT HL AS HEX 
;------------------------------------------------------------------------				
;
PRNHL:			PUSH	H					; Save HL
				PUSH	D					; Save DE
				PUSH	B					; Save BC
				PUSH	PSW					; Save PSW
				MOV 	A,H                	; Get HIGH byte
				CALL 	PRNHL1              ; Output it
				MOV 	A,L                	; Get LOW byte
				CALL	PRNHL1				; Output it
				MVI		C,'.'
				CALL	CONOUT
				POP		PSW					; Restore PSW
				POP		B					; Restore BC
				POP		D					; Restore DE
				POP		H					; Restore HL
				RET							; Return to caller
PRNHL1: 		PUSH 	PSW                 ; Save low digit
				RRC                         ; Shift
				RRC                         ; high
				RRC                        	; digit
				RRC                         ; into low
				CALL 	PRNHL2              ; Display a single digit
				POP 	PSW                 ; Restore low digit
				CALL	PRNHL2				; Display a single digit
				RET							; Return from subroutine
PRNHL2:			ANI 	0Fh                	; Remove high digit
				CPI	 	10                  ; Convert to ASCII
				SBI 	2Fh					; Adjust
				DAA							; Digital adjust acu
				MOV		C,A
				CALL	CONOUT				; Output the digit
				RET
;
;-----------------------------------------------------------------------
; ** BINAIR TO ASCII  5 DIGITS IN HL
;-----------------------------------------------------------------------
;
DECOUT:			MVI		B,80H				; flag to no print
				LXI		D,-10000			; setup 10000
				CALL	DECNO				; see how many
				LXI		D,-1000				; setup 1000
				CALL	DECNO				; see howmany
				LXI		D,-100				; setup 100
				CALL	DECNO				; see how many
				LXI		D,-10				; setup 10
				CALL	DECNO				; see howmany
				MVI		B,0					; clear inhibit print flag
				LXI		D,-1				; setup 1
DECNO:			INR		B					; see howmany
				DAD		D					; substract it
				JC		DECNO				; carry ?
				DCR		B					; yes adjust count
				XCHG						; to de
				XRA		A					; clear carry clear acu
				SUB		L					; substarct l
				MOV		L,A					; save l
				MVI		A,0					; save carry but a to 0
				SBB		H					; substract h
				MOV		H,A					; save h
				XCHG						; back to hl
				DAD		D					; readd
				MOV		A,B					; get no print flag
				CPI		80H					; still set
				RZ							; yes then return
				MOV		A,B					; get flag
				ANI		7FH					; mask out flag
				ORI		'0'					; make ascii  
				PUSH	H					; save hl
				MOV		C,A	
				CALL	CONOUT				; echo it
				POP		H					; restore hl
				MVI		B,0					; disable flag now
				RET	
;
;-----------------------------------------------------------------------
; DISPLAY MESSAGE [PC]
;-----------------------------------------------------------------------
;
PRTMSG: 		POP 	H                    ; Get address
				CALL 	PRTSTR               ; Output message
				PCHL                         ; And return
PRTSTR: 		MOV 	A,M              	 ; Get byte from message
				INX 	H                    ; Advance to next
				ANA 	A                    ; End of message?
				RZ                           ; Yes, exit
				MOV		C,A
				CALL 	CONOUT               ; Output the character
				JMP 	PRTSTR               ; And proceed
;
;	
;-----------------------------------------------------------------------
; SWEEP LEDS ON START
;-----------------------------------------------------------------------
;
SWEEPLEDS:		MVI		A,0					; Prepare table offset
				PUSH	PSW					; save on stack
SWEEP0:			XRA		A					; clear ACU
				OUT		PPIPC				; to clear all LEDS
				POP		PSW					; Get offset
				MOV		E,A					; move to DE
				MVI		D,0					; so D must be 0
				LXI		H,LEDTABLE			; HL now led table
				DAD		D					; Add offset
				MOV		B,M					; Get led to set
				MVI		C,1					; ondicate ON
				PUSH	PSW					; save offset
				CALL	SETLED				; go set the led
				LXI		D,1FFFH				; prepare delay counter
SWEEP1:			DCR		E					; decrement LSB
				JNZ		SWEEP1				; not yet 0 redo it
				DCR		D					; decrement MSB
				JNZ		SWEEP1				; not yet go continue
				POP		PSW					; Get offset
				INR		A					; Next led
				PUSH	PSW					; saver counter
				CPI		13					; end of table
				JNZ		SWEEP0				; no next led sequence
				POP		PSW					; drop offset
				XRA		A					; clear ACU
				OUT		PPIPC				; to clear all LEDS
				RET							; and return
LEDTABLE:		DB		1,2,3,7,6,5,4,5,6,7,3,2,1
;
;-----------------------------------------------------------------------
;       READ INTEL MDS OR HEX DUMP FILE FROM PORT
;-----------------------------------------------------------------------
;
INTELMDS:      	CALL	PRTMSG
				DB		CR,LF,CR,LF,"Start uploading INTEL MDS file to TPA"
				DB		CR,LF,"End of file = :00000001FF",CR,LF,0
;				
;
INTELMDS01:		CALL	CIN	 	      			; read sync byte
				ANI		7FH						; strip off msb
				CPI		':'						; should be :
				JNZ		INTELMDS01				; Wait until so
				XRA		A						; clear crc
				STA		CRC	
				CALL	GET_B					; get byte count
				STA		COUNT					; save count value
				CPI		0
				JZ		INTELMDS04
				CALL	GET_W					; get address
				XCHG							; to hl
				SHLD	MEM_R					; save for later
				CALL	GET_B					; get record type
				CPI		1						; eof record ?
				JZ		INTELMDS04				; ok done exit
;
				LHLD	MEM_R					; get address where to save
INTELMDS02:		CALL	GET_B					; get one byte
				MOV		M,A						; save in memory
				INX		H						; increment pointer
				LDA		COUNT					; get counter
				DCR		A						; decrement it
				STA		COUNT					; save again
				ORA		A						; test it
				JNZ		INTELMDS02				; loop until all done
				SHLD	MEM_R					; save address again
				CALL	GET_B					; get checksum bytes
				LDA		CRC						; get the final checksum
				ORA		A
				JNZ		INTELMDS03				; error exit
;
				JMP		INTELMDS01				; get next record
;
INTELMDS03:		CALL	PRTMSG
				DB		"Checksum error !",0	; error 
				RET			
;
INTELMDS04:		CALL	CONIN					; get overflowing characters
				CPI		CR						; unti CR
				JNZ		INTELMDS04				; not yet
				CALL	PRTMSG					; display end message
				DB		"Upload complete.",CR,LF; OK done  
				DB		"Use SAVE ",0
				LHLD	MEM_R					; get latest address
				MOV		L,H						; page number to L	
				MVI		H,0						; H to 0
				CALL	DECOUT					; print number of pages to save
				CALL	PRTMSG					; terminate message
				DB		" FILENAME.COM",CR,LF,0
				RET								; and return
;
;---------------------------------------------------------------------------
;       CHECKSUM RAM FROM HL THRU DE IN 16 BIT VALUE
;---------------------------------------------------------------------------
;
CHECK_SUM:      LXI     B,0                     ; save checksum on stack
				PUSH    B
;
CHECK_SUM0:     MOV     A,H                     ; compare high
				CMP     D                       ; with end
				JNZ     CHECK_SUM1              ; not ok continue
				MOV     A,L                     ; compare low
				CMP     E                       ; With end
				JNZ     CHECK_SUM1              ; not ok
				POP     H                       ; ok get chacksum
				RET                             ; and return
;
CHECK_SUM1:     MOV     A,M                     ; get byte to add
				INX     H                       ; bump pointer
				XTHL                            ; exchange pointer and cs
				PUSH    D                       ; save end address
				MOV     E,A                     ; value to add to  de
				MVI     D,0                     ; so d to zero
				DAD     D                       ; 16 bit add
				POP     D                       ; restore de
				XTHL                            ; get back pointer on stack
				JMP     CHECK_SUM0              ; jump for next
;				
;---------------------------------------------------------------------------
;       GET NUMBER 0 TO 255 FROM PORT IN ACU AND UPDATE CRC
;---------------------------------------------------------------------------
GET_B:          PUSH    B                       ; save bc
				CALL    CIN                     ; read first len byte
				CALL    MAKE_BIN                ; convert digit
				RLC
				RLC
				RLC
				RLC
				MOV     B,A                     ; save in b
				CALL    CIN                     ; Read next character
				CALL    MAKE_BIN
				ORA     B                       ; Combine byte
				MOV     B,A                     ; Save back in b
				LDA     CRC                     ; Get crc
				ADD     B                       ; Add it
				STA     CRC                     ; Save back crc
				MOV     A,B
				POP     B                       ; Restore bc
				RET               	
;---------------------------------------------------------------------------
;       MAKE BINAIRY
;---------------------------------------------------------------------------
MAKE_BIN:       SUI     '0'						; remove ascii 0
				CPI     10						; Test if a-f range
				RM								; No ok return
				SUI     7						; ELse adjust it
				RET								; And return
;
;---------------------------------------------------------------------------
;       GET ADDRESS FROM PORT IN DE
;---------------------------------------------------------------------------
GET_W:          CALL    GET_B					; Get one byte
				MOV     D,A						; to d
				CALL    GET_B					; get another byte
				MOV     E,A						; to e
				RET								; now de holds address
;
;---------------------------------------------------------------------------
;       FORMAT DISK BIOS VERSION 
;---------------------------------------------------------------------------
FORMAT:			CALL		PRTMSG
				DB			0DH,0AH,"ROM Format Disk DD Version 1.1",0DH,0AH,"Select drive (A/B):",0
				CALL		CONIN
				ANI			05FH
				MOV			C,A
				CALL		CONOUT
				CPI			'A'
				JZ			FORMATA
				CPI			'B'
				JZ			FORMATB
				LXI			B,0
				JMP			CBOOT
;
FORMATA:		MVI			A,0
				STA			SELDISK
				LED			2,0
				LED			3,1
				IN			SWTCH					; Read dip switches
				ANI			00100000B				; Drive A = 80 track ?
				JNZ			FORMATA1				; NO 40 tracks 
;
				MVI			A,80
				STA			MAXTRACK
				JMP			FORM
;				
FORMATA1:		MVI			A,42
				STA			MAXTRACK
				JMP			FORM
;			
FORMATB:		MVI			A,1
				STA			SELDISK
				LED			2,1
				LED			3,0
				MVI			A,80
				STA			MAXTRACK
;
FORM:			CALL		PRTMSG
				DB			0DH,0AH,"Are you sure (Y/N):",0
				CALL		CONIN
				ANI			05FH
				MOV			C,A
				CALL		CONOUT
				CPI			'Y'
				JNZ			CBOOT
				
				CALL		PRTMSG
				DB			0DH,0AH,"Starting format",0Dh,0Ah,0
				CALL 		FORMAT_DISK			
				JMP			CBOOT	
;				
;-----------------------------------------------------------------------
; MAIN FORMAT DISK
;-----------------------------------------------------------------------
;
FORMAT_DISK:	XRA		A						; ACU to zero
				STA		MOTSTATUS				; tell motor is OFF
				CALL	MOTORON					; make sure motor in now ON
				CALL	HOME					; restore drive to track 0
;					
				XRA		A						; prepare zero
				STA		CURTRACK				; store it
;			
FORMAT0:		LED		6,1						; Write led ON
				MVI		A,0						; Set side to 0
				STA		CURSIDE					; save it
				CALL 	BUILD_FORMATBUF			; Build format buffer for this track
				CALL	SETDMAFULL			; Start DMA controller	
				LDA		CURTRACK				; get current track
				OUT		FDC_TRACK				; to track register
				CALL	FORMATS					; Display the message
				MVI  	A,11110000B				; Write track Head 0
				OUT  	FDC_CMD					; execute the command	
				CALL 	FDC_BUSY				; wait until done
				MVI		A,1						; select other side
				STA		CURSIDE					; save it to
				CALL 	BUILD_FORMATBUF			; Build format buffer for this track
				CALL	SETDMAFULL			; Start DMA controller		
				LDA		CURTRACK				; load currect track
				OUT		FDC_TRACK				; to track register
				CALL	FORMATS					; display the message
				MVI  	A,11110010B				; Write track Head 1
				OUT  	FDC_CMD					; execute command
				CALL 	FDC_BUSY				; wait until done
				LED		6,0						; Write LED OFF	
				LDA		CURTRACK				; get current track
				INR		A						; increment
				MOV		C,A						; to C
				LDA		MAXTRACK				; load maximum
				CMP		C						; compare it
				JZ		FORMAT1					; ok we are done
				LDA		CURTRACK				; get current track
				INR		A						; increment
				STA		CURTRACK				; store again
				LED		4,1						; Seek lED On
				MVI 	A,01000010B				; Step in command
				OUT  	FDC_CMD					; step command execute
				CALL 	FDC_BUSY				; wait until ready	
				LED		4,0						; Seek LED Off
				JMP		FORMAT0					; go format next track
FORMAT1:		CALL	PRTMSG					; final message
				DB		0Dh,0Ah,"Format done",0Dh,0Ah,0
				RET
FORMATS:		CALL	PRTMSG					; display progress	
				DB		"Format track:",0
				LDA		CURTRACK				; track
				MOV		L,A						; to HL
				MVI		H,0
				CALL	DECOUT					; print it
				CALL	PRTMSG					; side ?
				DB		" Side:",0
				LDA		CURSIDE					; get side
				MOV		L,A						; to hl
				MVI		H,0	
				CALL	DECOUT					; print it
				CALL	PRTMSG					; final CR
				DB		0DH,0
				RET
;
;			
;-----------------------------------------------------------------------
; BUILD FORMAT BUFFER
;-----------------------------------------------------------------------
;
;
BUILD_FORMATBUF:LXI  	H,FORMATBUF     		; DE = dest
				MVI		A,1
				STA		CURSECTOR
				MVI		C,80		
GAP1_LOOP0:		MVI		M,04EH					;---- INDEX GAP 80   4E
				INX  	H
				DCR  	C
				JNZ  	GAP1_LOOP0
				MVI  	C,12
GAP1_LOOP1:		MVI		M,0
				INX  	H
				DCR  	C
				JNZ  	GAP1_LOOP1
				MVI  	M,0F6H					;---- SYNC (3 × F6 1 x FC) 
				INX  	H
				MVI  	M,0F6H			 
				INX  	H
				MVI  	M,0F6H			 
				INX  	H
				MVI  	M,0FCH			 
				INX  	H
				MVI  	C,50
GAP1_LOOP2:		MVI		M,04EH
				INX  	H
				DCR  	C
				JNZ  	GAP1_LOOP2
SECTOR_LOOP0:	MVI  	C,12			  		;---- ID FIELD ----
GAP1_LOOP3:		MVI		M,0
				INX  	H
				DCR  	C
				JNZ  	GAP1_LOOP3
				MVI  	M,0F5H
				INX  	H
				MVI  	M,0F5H
				INX  	H
				MVI  	M,0F5H
				INX  	H
				MVI		M,0FEH
				INX		H
				LDA		CURTRACK			   	; Track
				MOV  	M,A
				INX  	H
				LDA		CURSIDE					; Side
				MOV  	M,A
				INX  	H
 				LDA		CURSECTOR				; Sector #
				MOV  	M,A
				INX  	H
				MVI  	M,00H					; Size code
				INX  	H
 				MVI  	M,0F7H					; CRC marks
				INX  	H
				MVI  	C,22					;---- GAP 2 (11×FF) ----
GAP2_LOOP:		MVI		M,04Eh
				INX  	H
				DCR  	C
				JNZ  	GAP2_LOOP
				MVI  	C,12
GAP3_LOOP:		MVI		M,0
				INX  	H
				DCR  	C
				JNZ  	GAP3_LOOP			
				MVI		M,0F5H					 ;---- DATA FIELD ----
				INX		H
				MVI		M,0F5H
				INX		H
				MVI		M,0F5H
				INX		H
				MVI  	M,0FBH
				INX  	H
				MVI  	C,128					; 128 bytes of data
DATA_LOOP:		MVI  	M,0E5H
				INX  	H
				DCR  	C
				JNZ  	DATA_LOOP
				MVI  	M,0F7H						; CRC marks
				INX  	H
				MVI  	C,22						; End gap	
GAP4_LOOP:		MVI		M,04EH
				INX  	H
				DCR  	C
				JNZ  	GAP4_LOOP	
				LDA		CURSECTOR
				INR		A
				STA		CURSECTOR
				CPI		SECTORS_PER_TRACK+1
    			JNZ  	SECTOR_LOOP0
				MVI  	C,598
GAP5_LOOP:		MVI		M,04EH
				INX  	H
				DCR  	C
				JNZ  	GAP5_LOOP	
				RET
;
;-----------------------------------------------------------------------
; SPEED SETUP TABLE AND DISPLAY MESSAGES
;-----------------------------------------------------------------------
;
SPDTAB:			DW		5					; 38400 BAUD
				DW	 	10					; 19200 BAUD
				DW	 	20					; 9600 BAUD
				DW		40					; 4800 BAUD
				DW		80					; 2400 BAUD
				DW		160					; 1200 BAUD
				DW		320					; 600 BAUD
				DW		640					; 300 BAUD
;
;----------------------------------------------------------
; Disk Parameter Table
;----------------------------------------------------------

DPB0:			DW    	26          		; SPT - 26 logical sectors per track (128B)
				DB    	3           		; BSH - 1K block = 2^3 * 128
				DB    	7           		; BLM - block mask
				DB    	0           		; EXM - extent mask
				DW    	254         		; DSM - total blocks - 1
				DW    	63          		; DRM - 64 dir entries
				DB    	192        			; AL0 - dir allocation bitmap
				DB    	0           		; AL1
				DW   	16          		; CKS - (DRM+1)/4
				DW    	3           		; OFF - reserved tracks
;
XLTBL0:			DB		1,4,7,10,13,16,19,22,25,2,5,8,11,14,17,20,23,26,3,6,9,12,15,18,21,24
;
DPB1:			DW    	26          		; SPT - 16 logical sectors per track (128B)
				DB    	4           		; BSH - 2K block = 2^4 * 128
				DB    	15          		; BLM - block mask
				DB    	0          			; EXM - extent mask
				DW    	254         		; DSM - total blocks - 1
				DW    	63          		; DRM - 64 dir entries
				DB    	128         		; AL0 - dir allocation bitmap
				DB    	0           		; AL1
				DW   	16          		; CKS - (DRM+1)/4
				DW    	3           		; OFF - reserved tracks
;
XLTBL1:			DB		1,4,7,10,13,16,19,22,25,2,5,8,11,14,17,20,23,26,3,6,9,12,15,18,21,24
;
DPB2:			DW    	26          		; SPT - 16 logical sectors per track (128B)
				DB    	4           		; BSH - 2K block = 2^4 * 128
				DB    	15          		; BLM - block mask
				DB    	0           		; EXM - extent mask
				DW    	254         		; DSM - total blocks - 1
				DW    	63          		; DRM - 64 dir entries
				DB    	128         		; AL0 - dir allocation bitmap
				DB    	0           		; AL1
				DW   	16          		; CKS - (DRM+1)/4
				DW    	3           		; OFF - reserved tracks
;
XLTBL2:			DB		1,4,7,10,13,16,19,22,25,2,5,8,11,14,17,20,23,26,3,6,9,12,15,18,21,24
;
DPB3:			DW    	32          		; SPT - 16 logical sectors per track (128B)
				DB    	4           		; BSH - 2K block = 2^4 * 128
				DB    	15          		; BLM - block mask
				DB    	0           		; EXM - extent mask
				DW    	511         		; DSM - total blocks - 1
				DW    	127         		; DRM - 64 dir entries
				DB    	192         		; AL0 - dir allocation bitmap
				DB    	0           		; AL1
				DW   	32	         		; CKS - (DRM+1)/4
				DW    	0	        		; OFF - reserved tracks
;
;
;
;----------------------------------------------------------
; Disk Parameter Table to RAM
;----------------------------------------------------------

MOVETABLE:		LXI		H,XLTBL0			; translate table 0
				SHLD	DPH0				; to ram for DPB0	
				LXI		H,XLTBL1			; translate table 1
				SHLD	DPH1				; to ram for DPB1
				LXI		H,XLTBL2			; translate table 1
				SHLD	DPH2				; to ram for DPB2
				LXI		H,0					; translate table 1
				SHLD	DPH3				; to ram for DPB3
				LXI		H,DIRBUF			; Directory buffer
				SHLD	DPH01				; for both of 
				SHLD	DPH11				; the DPB
				SHLD	DPH21				; the DPB
				SHLD	DPH31				; the DPB
				
				LXI		H,DPB0				; DPB 0
				SHLD	DPH02				; pointer to RAM
				LXI		H,CSV0				; CSV0
				SHLD	DPH03				; to RAM
				LXI		H,ALV0				; Allocation vector table
				SHLD	DPH04				; To RAM
				
				LXI		H,DPB1				; DPB1
				SHLD	DPH12				; pointer to RAM 
				LXI		H,CSV1				; CSV1
				SHLD	DPH13				; To RAM	
				LXI		H,ALV1				; Alocation vector table
				SHLD	DPH14				; To RAM
				
				LXI		H,DPB2				; DPB1
				SHLD	DPH22				; pointer to RAM 
				LXI		H,CSV2				; CSV1
				SHLD	DPH23				; To RAM	
				LXI		H,ALV2				; Alocation vector table
				SHLD	DPH24				; To RAM
				
				LXI		H,DPB3				; DPB1
				SHLD	DPH32				; pointer to RAM 
				LXI		H,CSV3				; CSV1
				SHLD	DPH33				; To RAM	
				LXI		H,ALV3				; Alocation vector table
				SHLD	DPH34				; To RAM
;
				LXI		H,BDOS+6			; BDOS start
				SHLD	6					; AT 6
				LXI		H,BIOS+3			; BIOS start 
				SHLD	1					; AT 1
				MVI		A,0C3H				; JMP OPCODE
				STA		0					; AT 0
				STA		5					; AND AT 5
				RET
;
;
;-----------------------------------------------------------------------
;			RAM SPACE STORAGE
;-----------------------------------------------------------------------
;
				ORG		RAM
;
				
DIRBUF:			DS 128						; Directory buffer
;
ALV0:   		DS 32         				; Allocation vector 
CSV0:   		DS 16          				; Check vector
ALV1:   		DS 32           			; Allocation vector 
CSV1:   		DS 16           			; Check vector
ALV2:   		DS 32           			; Allocation vector 
CSV2:   		DS 16           			; Check vector
ALV3:   		DS 64           			; Allocation vector 
CSV3:   		DS 32           			; Check vector
;
;
DPH0:       	DW XLTBL0      				; XLT
				DW 0            			; SCR1
				DW 0            			; SCR2
				DW 0            			; SCR3
DPH01:			DW DIRBUF
DPH02:			DW DPB0         			; DPB
DPH03:			DW CSV0         			; CSV
DPH04:			DW ALV0         			; ALV
;
DPH1:			DW XLTBL1       			; XLT
				DW 0            			; SCR1
				DW 0           				; SCR2
				DW 0            			; SCR3
DPH11:			DW DIRBUF
DPH12:			DW DPB1         			; DPB
DPH13:			DW CSV1         			; CSV
DPH14:			DW ALV1         			; ALV
;
DPH2:			DW XLTBL2       			; XLT
				DW 0            			; SCR1
				DW 0            			; SCR2
				DW 0            			; SCR3
DPH21:			DW DIRBUF
DPH22:			DW DPB2         			; DPB
DPH23:			DW CSV2         			; CSV
DPH24:			DW ALV2         			; ALV
;
DPH3:			DW 0       					; XLT
				DW 0            			; SCR1
				DW 0            			; SCR2
				DW 0            			; SCR3
DPH31:			DW DIRBUF
DPH32:			DW DPB3         			; DPB
DPH33:			DW CSV3         			; CSV
DPH34:			DW ALV3         			; ALV
;
;-----------------------------------------------------------------------
; DATA AREA 
;-----------------------------------------------------------------------
;
CURTRACK:		DS		1
CURSECTOR:		DS		1
CURSIDE:		DS		1
MAXTRACK:		DS		1
;
SELDISK			DS		1
SELTRACK:		DS		1
SELSECTOR:		DS		1
SELHEAD:		DS		1
SELDMA:			DS		2
CRC:			DS		2
MEM_R:			DS		2
COUNT:			DS		1
BOOTMODE:		DS		1
MOTSTATUS:		DS		1
SEEKCNT:		DS		1
EOM:			DS		2
NEEDHOME:		DS		1
;		
;
;-----------------------------------------------------------------------
; STACK AREA 
;-----------------------------------------------------------------------
				DS			58
STACK 			EQU			$
;
				ORG 		TPA+256
;
FORMATBUF:  	DS 			6000       ; buffer for track format

				END

