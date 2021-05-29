; *******************************************************************
; *** This software is copyright 2005 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

#define DEBUG

include    bios.inc
include    kernel.inc

O_HIMEM    equ     0442h               ; himem value

; RD - Data page register
; RC - Stack frame pointer
; RB - Local stack pointer

; pstate values:
;   1 - last code was an abbreviation 1
;   2 - last code was an abbreviation 2
;   3 - last code was an abbreviation 3
;  10 - last code was start of extended sequence
;  11 - last code was hi 5-bits of extended sequence

           org     8000h
           lbr     0ff00h
#ifdef DEBUG
           db      'zrun3d',0
#else
           db      'zrun3',0
#endif
           dw      9000h
           dw      endrom+7000h
           dw      2000h
           dw      endrom-2000h
           dw      2000h
           db      0

           org     2000h
           br      start

include    date.inc
include    build.inc
           db      'Written by Michael H. Riley',0
          
data:
ip:        db      0,0,0
reg_rc:    dw      0
reg_rb:    dw      0
reg_r2:    dw      0
hm_ptr:    db      0,0,0
pstate:    db      0
lfsr:      db      1,2,3,4
alpha:     db      0
ext_code:  db      0
itype:     db      0
opcode:    db      0
num_args:  db      0
arg1:      dw      0
arg2:      dw      0
arg3:      dw      0
arg4:      dw      0
arg5:      dw      0
result:    db      0                   ; variable for result
retval:    dw      0
wrd_addr:  db      0
wrd_len:   db      0
tkn_cnt:   db      0
max_tkn:   db      0
static:    dw      0
line_pos:  db      0
#ifdef DEBUG
tab:       db      0
#endif
next_page: db      0
page_tab:  db      0,0,0               ; holds page number of static-hi mem
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
           db      0,0,0
fildes:    db      0,0,0,0
           dw      dta
           db      0,0
           db      0
           db      0,0,0,0
           dw      0,0
           db      0,0,0,0
fildes2:   db      0,0,0,0
           dw      0
           db      0,0
           db      0
           db      0,0,0,0
           dw      0,0
           db      0,0,0,0


start:     lda     ra                  ; move past any spaces
           smi     ' '
           lbz     start
           dec     ra                  ; back to non-space character
           ldn     ra                  ; was an argument given
           lbnz    start1              ; jump if so
           sep     scall               ; otherwise display usage
           dw      f_inmsg
           db      'Usage: zrun3 file',10,13,0
           lbr     o_wrmboot           ; and return to os
start1:    ldi     high data           ; setup pointer for data page
           phi     rd
           ghi     ra                  ; copy argument address to rf
           phi     rf
           glo     ra
           plo     rf
loop1:     lda     ra                  ; look for first <= space
           smi     33
           bdf     loop1
           dec     ra                  ; backup to char
           ldi     0                   ; and terminate filename
           str     ra
           ldi     low fildes          ; point to file descriptor
           plo     rd
           ldi     0                   ; no special flags
           plo     r7
           sep     scall               ; open the story file
           dw      o_open
           lbnf    opened              ; jump if file opened correctly
           ldi     high err_1          ; point to error message
           phi     rf
           ldi     low err_1
           plo     rf
msg_ret:   sep     scall               ; and display it
           dw      o_msg
           lbr     o_wrmboot           ; return to Elf/OS
opened:    ldi     low fildes          ; be sure we have file descriptor
           plo     rd
           ldi     high header         ; address for header
           phi     rf
           ldi     low header
           plo     rf
           ldi     0                   ; need to read 64 bytes
           phi     rc
           ldi     64
           plo     rc
           sep     scall               ; read file header
           dw      o_read
           ldi     high header         ; address for header
           phi     rf
           ldi     low header
           plo     rf
           ldn     rf                  ; get version
           plo     r7                  ; keep a copy
           smi     3                   ; must be 3
           lbz     ver_ok              ; jump if version is ok
           ldi     high err_2          ; point to error message
           phi     rf
           ldi     low err_2
           plo     rf
           sep     scall               ; display the error
           dw      o_msg
           glo     r7                  ; get needed version number
           adi     30h                 ; convert to ascii
           sep     scall               ; display it
           dw      o_type
           ldi     high crlf           ; display a crlf
           phi     rf
           ldi     low crlf
           plo     rf
           lbr     msg_ret             ; display and return
; *****************************************
; *** Setup header and startup settings ***
; *****************************************
ver_ok:    inc     rf                  ; move to header[1]
           ldn     rf                  ; retrieve flags
           ani     8fh                 ; clear writable flags
           ori     10h                 ; signal no status line
           str     rf                  ; put back into header
           inc     rf                  ; move to header[6]
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           ldi     low ip              ; point to ip
           plo     rd                  ; setup data pointer
           ldi     0                   ; high byte is zero
           str     rd
           inc     rd                  ; point to middle byte
           lda     rf                  ; read high byte, move to header[7]
           str     rd                  ; store middle of initial ip
           inc     rd
           lda     rf                  ; read low byte, move to header[8]
           str     rd                  ; initial ip is now set
           inc     rf                  ; header[9]
           inc     rf                  ; header[A]
           inc     rf                  ; header[B]
           inc     rf                  ; header[C]
           inc     rf                  ; header[D]
           inc     rf                  ; header[E]
           ldi     low static          ; need to get static address
           plo     rd
           lda     rf                  ; read high byte, move to header[F]
           str     rd                  ; store high byte of static address
           inc     rd
           lda     rf                  ; read low byte, move to header[10]
           str     rd
           ldn     rf                  ; get flags2 byte
           ani     0feh                ; clear lowest bit
           str     rf                  ; and write it back
           glo     rf                  ; add 14, moves to header[1E]
           adi     0eh
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           ldi     18                  ; user 18 for interpreter versino
   ldi   1
           str     rf
           inc     rf                  ; move to header[1F]
           ldi     2                   ; use 2
           str     rf
           inc     rf                  ; move to header[20]
           ldi     24                  ; set screen height to 24
           str     rf
           inc     rf                  ; move to header[21]
           ldi     80                  ; set screen width to 80
           str     rf
           inc     rf                  ; move to header[22]
           ldi     low header          ; move to memory just beyond header
           adi     64
           plo     rf
           ldi     high header
           adci    0
           phi     rf
           ldi     low static          ; need count of dynamic data
           plo     rd
           lda     rd                  ; read it
           phi     rc                  ; into rc
           ldn     rd
           plo     rc
           ldi     low fildes          ; point to file descriptr
           plo     rd
           sep     scall               ; read dynamic data into memory
           dw      o_read
           glo     rc                  ; add count to header address
           adi     low header
           plo     rf                  ; and place into rf
           ghi     rc
           adci    high header
           phi     rf
           ldi     low page_tab        ; setup page 0 has holding nothing
           plo     rd
           load    rc,O_HIMEM
page_loop: ldi     0                   ; mark as nothing in page
           str     rd
           inc     rd
           ghi     rf                  ; get page address
           str     rd                  ; and place into table entry
           inc     rd
           glo     rf
           str     rd
           inc     rd
           ghi     rf                  ; get address
           adi     2                   ; add 512 byte
           phi     rf
           adi     3                   ; loop until 3 pages below himem
           sex     rc
           sm
           sex     r2
           lbnf    page_loop
           ldi     0                   ; place termination record
           str     rd
           inc     rd
           str     rd
           inc     rd
           str     rd
           glo     rd
           smi     5                   ; point to last real entry
           plo     re                  ; save this value
           ldi     next_page           ; need to set next page pointer
           plo     rd
           glo     re                  ; recover value
           str     rd                  ; and store
           
           inc     rc
           ldn     rc                  ; set main stack to top of memory
           plo     r2
           dec     rc
           ldn     rc
           phi     r2
           smi     1                   ; set game stack 1 page below TOM
           phi     rb
           inc     rc
           ldn     rc
           plo     rb
           plo     rc
           ghi     rb
           phi     rc
           ldi     15                  ; setup 15 local variables for main
           plo     re
           sex     rb
main_vl:   ldi     0                   ; all set to zero
           stxd
           stxd
           dec     re
           glo     re
           lbnz    main_vl
           sex     r2
           ldi     low line_pos        ; set line position
           plo     rd
           ldi     0
           str     rd
#ifdef DEBUG
           ldi     tab
           plo     rd
           ldi     0
           str     rd
#endif
           lbr     cycle               ; ok, ready to run


; *********************************
; *** Get address for an object ***
; *** D - object number         ***
; *** Returns: RF - address     ***
; *********************************
obj_addr:  smi     1                   ; reset origin
           str     r2                  ; save copy of number
           plo     rf                  ; also into address
           ldi     0
           phi     rf
           ldi     3                   ; need 2 shifts
           plo     re
obj_addrl: glo     rf                  ; multiply by 2
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           dec     re                  ; decrement count
           glo     re                  ; see if done
           lbnz    obj_addrl
           glo     rf                  ; add in original for x9
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           glo     rf                  ; move past defaults table
           adi     62
           plo     rf
           ghi     rf                  ; propagate carry
           adci    0
           phi     rf
           ldi     low header          ; point to header memory
           adi     0bh                 ; point to lsb of object table entry
           plo     r7                  ; place into r7
           ldi     high header         ; high byte of header address
           adci    0                   ; propagate carry
           phi     r7
           sex     r7                  ; point x to address
           glo     rf                  ; add it into rf
           add
           plo     rf
           dec     r7
           ghi     rf
           adc
           phi     rf
           sex     r2                  ; point x back to stack
           sep     sret                ; return to caller

; **************************************************
; *** Get address of first property of an object ***
; *** D - object number                          ***
; *** Returns: RF - address of first property    ***
; **************************************************
frst_prop: sep     scall               ; get address object
           dw      obj_addr
           inc     rf                  ; move to properties address
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           sep     scall               ; read address from memory
           dw      rdmem16w
           ghi     r7                  ; transfer address to rf
           phi     rf
           glo     r7
           plo     rf
           sep     scall               ; read text length
           dw      rdmem16
           shl                         ; multiply by 2
           plo     r7                  ; and into r7
           ldi     0                   ; clear high byte
           shlc                        ; r7 is now x2
           phi     r7
           inc     r7                  ; add in size byte
           glo     r7                  ; add to address in rf
           str     r2
           glo     rf
           add
           plo     rf
           ghi     r7
           str     r2
           ghi     rf
           adc
           phi     rf                  ; now pointing at properties
           sep     sret                ; return to caller

; *****************************************
; *** find a property                   ***
; *** arg1 - object number              ***
; *** arg2 - prop number                ***
; *** Returns: rf - address of property ***
; ***          DF=0 - property found    ***
; ***          DF=1 - not found         ***
; *****************************************
fnd_prop:  ldi     low arg1            ; point to arg1
           plo     rd                  ; set data pointer
           inc     rd                  ; move to object number
           ldn     rd                  ; and get it
           sep     scall               ; get address of first property
           dw      frst_prop
           inc     rd                  ; point to arg2, prop number
           inc     rd
           ldn     rd                  ; get it
           stxd                        ; keep on stack
prop_lp:   sep     scall               ; get size byte of current property
           dw      rdmem16
           lbz     prop_nm             ; jump if end of table
           plo     re                  ; keep a copy
           ani     1fh                 ; need only property number for now
           irx                         ; point to required number
           sm                          ; and compare
           dec     r2                  ; preserve value on stack
           lbz     prop_fnd            ; jump if property found
           glo     re                  ; recover size byte
           shr                         ; get lenght part
           shr
           shr
           shr
           shr
           adi     2                   ; compensate for bias and size byte
           str     r2                  ; add to current address
           glo     rf
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           lbr     prop_lp             ; keep looking
prop_fnd:  irx                         ; remove prop number from stack
           adi     0                   ; signal property found
           sep     sret                ; and return
prop_nm:   irx                         ; remove prop number from stack
           smi     0                   ; signal property not found
           sep     sret                ; and return

 

; ******************************************
; *** build attribute mask               ***
; *** Returns: RF - zmemory of attirubte ***
; ***           D - positive mask        ***
; ******************************************
bld_mask:  ldi     low arg1            ; point to object number
           plo     rd
           inc     rd
           ldn     rd                  ; and retrieve it
           sep     scall               ; get object address
           dw      obj_addr
           ldi     low arg2            ; now need attribute number
           plo     rd
           inc     rd
           ldn     rd                  ; get it
           shr                         ; divided by 2
           shr                         ; by 4
           shr                         ; by 8
           ani     3                   ; keep only bottom 2 bits
           str     r2                  ; place here for add
           glo     rf                  ; add into object address
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           ldn     rd                  ; get attribute number again
           ani     7                   ; keep bottom 3 bits
           plo     re                  ; put it here
           ldi     080h                ; initial mask
clr_lp:    str     r2                  ; save for a moment
           glo     re                  ; see if done
           lbz     clr_dn              ; jump if so
           dec     re                  ; otherwise decrement re
           ldx                         ; get mask
           shr                         ; shift by one bit
           lbr     clr_lp              ; loop back
clr_dn:    ldn     r2                  ; recover mask
           sep     sret                ; return to caller

; ***************************
; *** Add signed RF to IP ***
; ***************************
add_rf_ip: ghi     rf                  ; see if RF is negative
           shl                         ; by shifting high bit into DF
           lbdf    add_neg             ; jump if negative
           ldi     0                   ; otherwise positive
           lskp                        ; skip over next instruction
add_neg:   ldi     0ffh                ; sign extend the negative
           plo     re                  ; and store here
           ldi     low ip              ; point to ip
           plo     rd                  ; place into rd
           inc     rd                  ; point to lsb
           inc     rd
           sex     rd                  ; point x to ip data
           glo     rf                  ; add RE:RF to IP
           add
           str     rd                  ; saving result in IP
           dec     rd                  ; point to middle byte
           ghi     rf                  ; middle byte of offset
           adc
           str     rd
           dec     rd                  ; point to highest byte
           glo     re                  ; get highest byte of offset
           adc
           str     rd                  ; save result
           sex     r2                  ; point x back to stack
           sep     sret                ; return to caller

; **************************
; *** Subtract 2 from IP ***
; **************************
ip_sub_2:  ldi     low ip              ; point to IP
           plo     rd                  ; place into rd
           inc     rd                  ; point to lsb
           inc     rd
           ldn     rd                  ; get lsb
           smi     2                   ; subtract 2
           str     rd                  ; and store back
           dec     rd                  ; point to middle byte
           ldn     rd
           smbi    0                   ; propagate borrow
           str     rd
           dec     rd                  ; highest byte
           ldn     rd
           smbi    0
           str     rd                  ; save result
           sep     sret                ; return to calelr

; ******************************
; *** Handle branches        ***
; *** DF - result of compare ***
; ***      1 - true          ***
; ***      0 - false         ***
; ******************************
branch:    shrc                        ; move result into D
           ani     80h                 ; keep only high bit
           stxd                        ; save this
           sep     scall               ; need next byte from ip
           dw      readip
           plo     re                  ; save a copy
           ani     080h                ; keep only test bit
           irx                         ; point to result
           xor                         ; and xor results
           lbnz    bdone               ; no branch if they are not the same
           glo     re                  ; need to check branch type
           shl                         ; shift length bit into DF
           shl
           lbdf    br_short            ; jump if short form
           glo     re                  ; recover high 6 bits of address
           ani     03fh                ; strip off high bits
           plo     re                  ; save this
           ani     020h                ; see if value is negative
           lbz     br_pos              ; jump if positive
           glo     re                  ; recover value
           ori     0c0h                ; extend sign to end of byte
           plo     re
br_pos:    glo     re
           phi     rf                  ; place into rf
           sep     scall               ; get next byte from ip 
           dw      readip
           plo     rf                  ; rf now has full offset
           lbr     br_go               ; continue branch
br_short:  glo     re                  ; recover offset
           ani     03fh                ; strip high 2 bits 
           plo     rf                  ; place into rf
           ldi     0                   ; high byte is zero
           phi     rf
br_go:     ghi     rf                  ; make sure not 0 or 1
           lbnz    br_go2
           glo     rf
           lbz     rfalse              ; jump if zero
           smi     1                   ; check for true
           lbz     rtrue               ; jump if so
br_go2:    sep     scall               ; add in the offset
           dw      add_rf_ip
           sep     scall               ; minus 2
           dw      ip_sub_2
#ifdef DEBUG
           ldi     '*'
           sep     scall
           dw      o_type
#endif
           lbr     idone               ; done
bdone:     glo     re                  ; recover branch type
           ani     040h                ; see if 1 byte or 2
           lbnz    idone               ; jump if only 1
           sep     scall               ; read one more byte from ip
           dw      readip
           lbr     idone               ; then done

true:      smi     0                   ; set DF to show truth
           lbr     branch              ; then jump to branch

false:     adi     0                   ; reset DF to show false
           lbr     branch              ; then jump to branch

; *************************************
; *** Left to do on printing engine ***
; *** 1. handle abbreviations       ***
; *** 2. handle extended codes      ***
; *************************************
abbr1:     ldi     0                   ; first table
           lskp                        ; skip next
abbr2:     ldi     64                  ; second table
           lskp                        ; skip next
abbr3:     ldi     128                 ; third table
           str     r2                  ; prepare to add to offset
           glo     re                  ; recover offset
           shl                         ; multiply it by 2
           add                         ; and add to talbe number
           plo     r8                  ; and place into r8
           ldi     0                   ; reset printing state
           str     rd
           glo     rf                  ; save rf
           stxd
           ghi     rf
           stxd
           glo     r7                  ; save r7
           stxd
           ghi     r7
           stxd
           ldi     low header          ; need to get abbreviations table base
           adi     18h
           plo     r9
           ldi     high header
           adci    0                   ; propagate carry
           phi     r9
           inc     r9                  ; point to low byte
           glo     r8                  ; and add with offset
           str     r2
           ldn     r9                  ; get table address
           add
           plo     rf                  ; place into rf
           dec     r9                  ; point to lsb
           ldn     r9
           adci    0
           phi     rf                  ; rf now has address
           sep     scall               ; read address from memory
           dw      rdmem16w
           ldi     low hm_ptr          ; point to high memory pointer
           plo     rd                  ; setup data pointer
           inc     rd                  ; point to lsb
           inc     rd
           ldn     rd                  ; retrieve current value 
           stxd                        ; and place onto stack
           glo     r7                  ; get zstring address
           shl                         ; times 2
           str     rd                  ; and write to high memory pointer
           dec     rd                  ; point to middle byte
           ldn     rd                  ; read it 
           stxd                        ; and place on stack
           ghi     r7                  ; get high of zstring address
           shlc                        ; continue multiply by 2
           str     rd                  ; and store it
           dec     rd                  ; point to high byte
           ldn     rd                  ; read it
           stxd                        ; and save
           ldi     0                   ; build high byte
           shlc
           str     rd                  ; hm_ptr now ponits to zstring
           sep     scall               ; so print it
           dw      hm_print
           ldi     low hm_ptr          ; point back to hm_ptr
           plo     rd
           irx                         ; and retrieve original value
           ldxa
           str     rd
           inc     rd
           ldxa
           str     rd
           inc     rd
           ldxa
           str     rd
           ldxa                        ; recover saved r7
           phi     r7
           ldxa
           plo     r7
           ldxa                        ; recover saved rf
           phi     rf
           ldx
           plo     rf
           ldi     low pstate          ; reset printing state
           plo     rd
           ldi     0
           str     rd
           ldi     low alpha           ; reset alphabet
           plo     rd
           ldi     0
           str     rd
           sep     sret                ; return to caller

ext1:      ldi     low pstate          ; need to get pstate
           plo     rd
           ldi     11                  ; and set to 11
           str     rd
           ldi     low ext_code        ; now need to write ext_code
           plo     rd
           glo     re                  ; retrieve code
           shl                         ; shift left 5 bits
           shl
           shl
           shl
           shl
           str     rd                  ; and store it
           sep     sret                ; all done for now
ext2:      ldi     low pstate          ; need to reset pstate
           plo     rd
           ldi     0                   ; to zero
           str     rd
           ldi     low ext_code        ; now need extended code
           plo     rd
           sex     rd                  ; point x to it
           glo     re                  ; get new code
           or                          ; combine with previous
           sex     r2                  ; point x pack to stack
           stxd                        ; save the code
           sep     scall               ; output the code
           dw      z_type
           irx                         ; recover code
           ldx
           smi     13                  ; was it a CR
           lbnz    do_ret              ; jump if not
           ldi     10                  ; add a linefeed
           sep     scall
           dw      o_type
clr_lnp:   glo     rd                  ; save rd
           plo     re
           ldi     low line_pos        ; need to reset line position
           plo     rd
           ldi     0
           str     rd
           glo     re                  ; recover rd
           plo     rd
do_ret:    sep     sret                ; and return

z_type:    plo     re                  ; save bye
           smi     32                  ; check for space
           lbz     z_space             ; jump if so
           glo     re                  ; recover character
z_type_go: sep     scall               ; and output it
           dw      o_type
           glo     rd                  ; save rd
           plo     re
           ldi     low line_pos        ; need to increment line position
           plo     rd
           ldn     rd                  ; retrieve positoin
           adi     1                   ; increment it
           str     rd                  ; and put it back
           glo     re                  ; recover rd
           plo     rd
           sep     sret                ; return to caller
z_space:   glo     rd                  ; save rd
           plo     re
           ldi     low line_pos        ; get line position
           plo     rd
           ldn     rd                  ; retrieve it
           smi     60                  ; check for below 60
           lbdf    z_newline           ; jump if over
           glo     re                  ; recover rd
           plo     rd
           ldi     32                  ; need the space again
           lbr     z_type_go           ; and print it
z_newline: ldi     0                   ; reset line position
           str     rd
           glo     re                  ; recover rd
           plo     rd
           ldi     10                  ; send cr/lf
           sep     scall
           dw      o_type
           ldi     13                  ; send cr/lf
           sep     scall
           dw      o_type
           lbr     clr_lnp             ; clear line position

; *****************************
; *** output a 5-bit z-code ***
; *****************************
zchar:     plo     re                  ; save code
           ldi     low pstate          ; need to get printing state
           plo     rd                  ; set data pointer
           ldn     rd                  ; retrieve it
           lbz     zchr_0              ; jump if nothing special
           smi     1                   ; check for abbrev 1
           lbz     abbr1               ; jump if so
           smi     1                   ; check for abbrev 2
           lbz     abbr2               ; jump if so
           smi     1                   ; check for abbrev 3
           lbz     abbr3               ; jump if so
           smi     7                   ; check for extended code start
           lbz     ext1                ; jump if so
           smi     1                   ; check for extended code end
           lbz     ext2
zchr_0:    glo     re                  ; recover character
           lbz     zchar_sp            ; jump if it should be a space
           smi     1                   ; check for abbrev 1
           lbz     zchar_1             ; jump if so
           smi     1                   ; check for abbrev 2
           lbz     zchar_2             ; jump if so
           smi     1                   ; check for abbrev 3
           lbz     zchar_3             ; jump if so
           smi     1                   ; check for A1 switch
           lbz     zchar_a1
           smi     1                   ; check for A2 switch
           lbz     zchar_a2            ; jump if so
           smi     1                   ; in range now for characters
           str     r2                  ; store here for addition
           ldi     low alpha           ; get alpha switch
           plo     rd                  ; place into data pointer
           ldn     rd                  ; get alpha state
           lbz     need_a0             ; jump if unshifted
           smi     1                   ; check for A1
           lbz     need_a1
           ldi     high alpha2         ; setup pointer for alpha2
           phi     r8
           ldi     low alpha2
           plo     r8
           ldx                         ; get character code
           lbnz    zchar_cnt           ; jump if not 6 in alpha2
           ldi     low pstate          ; need to set pstate to extended
           plo     rd
           ldi     10
           str     rd                  ; store into print state
           sep     sret                ; then return
zchar_cnt: glo     r8                  ; add character offset
           add
           plo     r8
           ghi     r8                  ; propagate carry
           adci    0
           phi     r8
           ldn     r8                  ; get character
           smi     10                  ; check for cr/lf
           lbz     zchar_cr            ; jump if so
           ldn     r8                  ; recover character
           sep     scall               ; and display it
           dw      z_type
           ldi     low alpha           ; need to reset shift state
           plo     rd
           ldi     0
           str     rd
           sep     sret                ; return to caller
need_a0:   ldi     high alpha0         ; setup pointer for alpha0
           phi     r8
           ldi     low alpha0
           plo     r8
           lbr     zchar_cnt           ; continue
need_a1:   ldi     high alpha1         ; setup pointer for alpha1
           phi     r8
           ldi     low alpha1
           plo     r8
           lbr     zchar_cnt           ; continue
zchar_a1:  ldi     low alpha           ; point to alpha switch
           plo     rd                  ; set data pointer
           ldi     1                   ; select alpha 1
           str     rd                  ; place into switch
           sep     sret                ; then return
zchar_a2:  ldi     low alpha           ; point to alpha switch
           plo     rd                  ; set data pointer
           ldi     2                   ; select alpha 1
           str     rd                  ; place into switch
           sep     sret                ; then return
zchar_1:   ldi     low pstate          ; need to set pstate to abbrv 1
           plo     rd
           ldi     1
           str     rd                  ; store into print state
           sep     sret                ; then return
zchar_2:   ldi     low pstate          ; need to set pstate to abbrv 2
           plo     rd
           ldi     2
           str     rd                  ; store into print state
           sep     sret                ; then return
zchar_3:   ldi     low pstate          ; need to set pstate to abbrv 3
           plo     rd
           ldi     3
           str     rd                  ; store into print state
           sep     sret                ; then return
zchar_sp:  ldi     ' '                 ; get a space
           sep     scall               ; and output it
           dw      z_type
           sep     sret                ; and return
zchar_cr:  ldi     13                  ; print cr/lf
           sep     scall
           dw      o_type
           ldi     10                  ; print cr/lf
           sep     scall
           dw      o_type
           lbr     clr_lnp

; *******************************
; *** print 2-byte z-sequence ***
; *******************************
zprint:    ghi     r7                  ; get first character
           shr                         ; shift into position
           shr
           ani     1fh                 ; strip unneeded bits
           sep     scall               ; output code
           dw      zchar
           glo     r7                  ; get middle char
           shr                         ; shift into position
           shr
           shr
           shr
           shr
           ani     7                   ; strip unneeded bits
           str     r2                  ; store here
           ghi     r7                  ; other bits come from high byte
           ani     3                   ; clear unneeded bits
           shl                         ; shift remaining 2 bits into place
           shl
           shl
           or                          ; or with previous value
           sep     scall               ; output the code
           dw      zchar
           glo     r7                  ; get 3rd code
           ani     1fh                 ; strip unneeded bits
           sep     scall               ; output code
           dw      zchar
           sep     sret                ; return to calelr

; *******************************
; *** Handle printing from ip ***
; *******************************
ip_print:  sep     scall               ; read next byte from ip
           dw      readip 
           phi     r7                  ; place into rf
           sep     scall               ; read next byte from ip
           dw      readip
           plo     r7                  ; set as low byte
           sep     scall               ; print it
           dw      zprint
           ghi     r7                  ; see if done
           shl                         ; high bit determines end
           lbnf    ip_print            ; jump if not done
           sep     sret                ; return to caller

; *******************************
; *** Handle printing from RF ***
; *******************************
rf_print:  sep     scall               ; read next sequence
           dw      rdmem16w
           inc     rf                  ; increment pointer
           inc     rf
           sep     scall               ; print the sequence
           dw      zprint
           ghi     r7                  ; need to see if done
           shl                         ; high bit determines end
           lbnf    rf_print            ; jump if not done
           sep     sret                ; return to caller

; ****************************************
; *** Handle printing from high memory ***
; ****************************************
hm_print:  sep     scall               ; read next byte
           dw      readhm
           phi     r7                  ; set in high byte
           sep     scall               ; read next byte
           dw      readhm
           plo     r7
           sep     scall               ; print it
           dw      zprint
           ghi     r7                  ; need to see if done
           shl                         ; high bit determines end
           lbnf    hm_print            ; jump if not done
           sep     sret                ; otherwise return to caller

; ********************************
; *** Reset printing variables ***
; ********************************
rst_prnt:  ldi     low alpha           ; point to alpha switch
           plo     rd
           ldi     0                   ; need base alphabet
           str     rd
           ldi     low pstate          ; set printing state
           plo     rd
           ldi     0                   ; to nothing special
           str     rd
           sep     sret                ; return to caller

; ************************
; *** RF = ARG1 - ARG2 ***
; ************************
arg_sub:   ldi     low arg1            ; point to arg1
           plo     rd                  ; place into data pointer
           ldn     rd                  ; bias arg1 by 8000h
           adi     80h
           str     rd
           inc     rd
           inc     rd
           ldn     rd                  ; bias arg2 by 8000h
           adi     80h
           str     rd
           dec     rd
           dec     rd
           lda     rd                  ; retreive value
           phi     rf                  ; into rf
           lda     rd
           plo     rf
           inc     rd                  ; point to lsb or arg2
           sex     rd                  ; point x to data
           glo     rf                  ; perform the subtraction
           sm
           plo     rf
           dec     rd
           ghi     rf
           smb
           phi     rf
           sex     r2                  ; point x back to stack
           sep     sret                ; and return


; ********************************
; *** Get random bit from LFSR ***
; ********************************
fn_lfsr:   ldi     low lfsr
           plo     rd
           inc     rd                  ; point to lsb
           inc     rd
           inc     rd
           ldn     rd                  ; retrieve it
           plo     re                  ; put into re  ( have bit 0)
           shr                         ; shift bit 1 into first position
           str     r2                  ; xor with previous value
           glo     re
           xor
           plo     re                  ; keep copy
           ldn     r2                  ; get value
           shr                         ; shift bit 2 into first position
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           ldn     r2                  ; now shift to bit 4
           shr     
           shr     
           str     r2                  ; and combine
           glo     re
           xor     
           plo     re
           ldn     r2                  ; now shift to bit 6
           shr
           shr
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           dec     rd                  ; point to lfsr msb
           dec     rd
           dec     rd
           ldn     rd                  ; retrieve it
           shl                         ; shift high bit to low
           shlc
           str     r2                  ; combine with previous value
           glo     re
           xor
           shr                         ; shift new bit into DF
           ldn     rd                  ; now shift the register
           shrc
           str     rd
           inc     rd                  ; now byte 1
           ldn     rd                  ; now shift the register
           shrc
           str     rd
           inc     rd                  ; now byte 2
           ldn     rd                  ; now shift the register
           shrc
           str     rd
           inc     rd                  ; now byte 3
           ldn     rd                  ; now shift the register
           shrc
           str     rd
           shr                         ; shift result bit into DF
           sep     sret                ; and return

; *********************************************************************
; *** Read keyboard input and perform lexical analysis              ***
; *********************************************************************
; **************************************
; *** search dictionary              ***
; *** R8:R7 entry to search for      ***
; *** Returns: R9 - address of entry ***
; **************************************
srch_dict: glo     rf                  ; save consumed register
           stxd
           ghi     rf
           stxd
           glo     ra                  ; save consumed register
           stxd
           ghi     ra
           stxd
           ldi     low header          ; need to look in header
           adi     8                   ; dictionary address is at 8
           plo     rf
           ldi     high header
           adci    0                   ; propagate carry
           phi     rf
           lda     rf                  ; retrieve dictionary address
           plo     re
           lda     rf
           plo     rf                  ; into rf
           glo     re
           phi     rf
           sep     scall               ; read number of word sep characters
           dw      rdmem16
           str     r2                  ; add to current address
           inc     rf                  ; move past count byte
           glo     rf                  ; adjust address
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           sep     scall               ; now get entry length
           dw      rdmem16
           stxd                        ; keep this on the stack
           inc     rf                  ; now to get number of entries
           sep     scall               ; get msb
           dw      rdmem16
           phi     ra                  ; will use ra for count
           inc     rf                  ; point to lsb of entry count
           sep     scall               ; and read it
           dw      rdmem16
           plo     ra                  ; ra now has entry count
           inc     rf                  ; point rf at first entry
srch_lp:   glo     rf                  ; save position
           stxd
           ghi     rf
           stxd
           sep     scall               ; read byte from entry
           dw      rdmem16
           str     r2                  ; prepare for compare
           ghi     r8                  ; check high byte first
           sm
           lbnz    srch_no
           inc     rf                  ; point to next byte
           sep     scall               ; read byte from entry
           dw      rdmem16
           str     r2                  ; store for compare
           glo     r8                  ; check low byte
           sm
           lbnz    srch_no             ; jump if no match
           inc     rf                  ; point to next byte
           sep     scall               ; read byte from entry
           dw      rdmem16
           str     r2                  ; prepare for compare
           ghi     r7                  ; check high byte first
           sm
           lbnz    srch_no
           inc     rf                  ; point to next byte
           sep     scall               ; read byte from entry
           dw      rdmem16
           str     r2                  ; store for compare
           glo     r7                  ; check low byte
           sm
           lbnz    srch_no             ; jump if no match
           irx                         ; found, recover address
           ldxa
           phi     r9
           ldxa
           plo     r9
srch_ret:  irx                         ; recover original registers
           ldxa
           phi     ra
           ldxa
           plo     ra
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     sret                ; return to caller
srch_no:   irx                         ; recover address
           ldxa
           phi     rf
           ldxa                        ; and leave pointing at size
           add                         ; add in size
           plo     rf
           ghi     rf                  ; propagate carry
           adci    0
           phi     rf
           dec     r2                  ; befire size byte
           dec     ra                  ; decrement entry count
           glo     ra                  ; see if more entries
           lbnz    srch_lp
           ghi     ra                  ; check high byte as well
           lbnz    srch_lp
           irx                         ; remove size from stack
           ldi     0                   ; indicate entry not found
           phi     r9
           plo     r9
           lbr     srch_ret            ; and return

; ***************************************
; *** Shift 5 bit quantity into R8:R7 ***
; ***************************************
shift5:    ani     1fh                 ; make sure only 5 bits
           str     r2                  ; save for now
           ldi     5                   ; need to perform 5 shifts
           plo     re
shift5lp:  glo     r7                  ; shift the register
           shl
           plo     r7
           ghi     r7
           shlc
           phi     r7
           shlc                        ; carry bit 6 instead of 7 to R8
           glo     r8                  ; shift the register
           shlc
           plo     r8
           ghi     r8
           shlc
           phi     r8
           dec     re                  ; decrement count
           glo     re                  ; see if done
           lbnz    shift5lp            ; jump if not
           glo     r7                  ; now can add in new value
           or
           plo     r7                  ; put it back
           sep     sret                ; return to caller

; *************************************
; *** Check for alpha2 character    ***
; *** D - char to check - destroyed ***
; *** Returns: D - offset           ***
; ***          DF=1 is alhpa2       ***
; ***          DF=0 not alpha2      ***
; *************************************
enc_a2:    plo     re
           ghi     re                  ; save baud constant
           stxd
           ldi     high alpha2         ; point to alpha2 character set
           phi     rd
           ldi     low alpha2
           plo     rd
           inc     rd                  ; point to '0' character
           inc     rd
           ldi     8                   ; value of first char
           phi     re
           sex     rd                  ; point X here
enc_a2_lp: glo     re                  ; see if character matches
           sm
           lbz     enc_a2_f            ; jump if found
           inc     rd                  ; move to next char
           ghi     re                  ; get offset number
           adi     1                   ; increment it
           phi     re                  ; and put it back
           smi     20h                 ; see if end of alph
           lbnz    enc_a2_lp           ; jump if more to check
           adi     0                   ; signal not alpha2
enc_a2_rt: sex     r2                  ; point x back to stack
           plo     re                  ; save possible result
           irx                         ; recover baud constant
           ldx
           phi     re
           ldi     high data           ; reset rd
           phi     rd
           glo     re                  ; recover possible result
           sep     sret                ; and return to caller
enc_a2_f:  ghi     re                  ; get offset
           plo     re                  ; place into re
           smi     0                   ; signal alpha2 was found
           lbr     enc_a2_rt           ; and return

; *****************************************
; *** Enocde input                      ***
; *** RF - text to encode               ***
; ***      Physical, not zmemory        ***
; *****************************************
encode:    ldi     low wrd_addr        ; need to keep address of word
           plo     rd                  ; setup data pointer
           ldi     0                   ; set to beginning of buffer
           str     rd                  ; write to wrd_addr var
           ldi     low tkn_cnt         ; need to set token count to zero
           plo     rd                  ; setup data pointer
           ldi     0                   ; set to beginning of buffer
           str     rd                  ; write to wrd_addr var
           ldi     low arg2            ; need to get parse buffer
           plo     rd                  ; setup data pointer
           lda     rd                  ; read address
           phi     ra                  ; into ra
           ldn     rd
           plo     ra
           ldi     low header          ; add in header address
           str     r2
           glo     ra
           add
           plo     ra
           ldi     high header
           str     r2
           ghi     ra
           adc
           phi     ra
           ldi     low max_tkn         ; point to maximum tokens
           plo     rd                  ; setup data pointer
           lda     ra                  ; get maximum number of tokens
           str     rd                  ; and store into max_tkn var
           inc     ra                  ; point to first token slot
encode_lp: ldn     rf                  ; read byte from buffer
           smi     '$'                 ; check for token
           lbnz     not_dlr            ; jump if not a dollar
           ldi     5                   ; push in a 5
           sep     scall               ; to the zsequence
           dw      shift5
           ldi     6                   ; push in a 6
           sep     scall               ; to the zsequence
           dw      shift5
           ldi     1                   ; push in a 1
           sep     scall               ; to the zsequence
           dw      shift5
           ldi     3                   ; push in a 3
           sep     scall               ; to the zsequence
           dw      shift5
           ldi     5                   ; push in a 5
           sep     scall               ; to the zsequence
           dw      shift5
           lbr     token_go

not_dlr:   ldn     rf
           smi     'a'                 ; check for token
           lbdf    enc_tkn             ; found a token
           ldn     rf                  ; recover character
           lbz     encode_dn           ; jump if end of buffer
           sep     scall               ; see if alpha2
           dw      enc_a2
           lbnf    encode_n2           ; jump if not
           shl                         ; shift up 3 positions
           shl
           shl
           plo     r8                  ; and place into r8
           ldi     5                   ; put 5 into high byte
           phi     r8
           glo     r8                  ; now perform 2 16-bit shifts
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           glo     r8                  ; second 2 16-bit shifts
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           glo     r8                  ; need to setup low bits
           ori     5
           plo     r8
           ldi     094h                ; setup r7
           phi     r7
           ldi     0a5h
           plo     r7
           inc     rf                  ; move past character
           ldi     low wrd_addr        ; need word addres
           plo     rd                  ; in data pointer
           ldn     rd                  ; get address
           stxd                        ; and place onto stack
           ldi     low wrd_len         ; need to increment word length
           plo     rd
           ldi     1
           str     rd
           ldi     low wrd_addr        ; and address
           plo     rd
           ldn     rd
           adi     1
           str     rd
           lbr     token_go            ; and process token
encode_n2: inc     rf                  ; point to next position
           ldi     low wrd_addr        ; point to address variable
           plo     rd                  ; setup data poniter
           ldn     rd                  ; get position
           adi     1                   ; increment it
           str     rd                  ; put it back
           lbr     encode_lp           ; loop over junkspace
enc_tkn:   ldi     low wrd_addr        ; need word addres
           plo     rd                  ; in data pointer
           ldn     rd                  ; get address
           stxd                        ; and place onto stack
           ldi     6                   ; maximum of 6 chars allowd
           stxd                        ; place onto stack
           ldi     low wrd_len         ; point to word length var
           plo     rd                  ; setup data pointer
           ldi     0                   ; set to zero
           str     rd
tkn_lp:    ldn     rf                  ; get next byte from token
           lbz     token_dn            ; jump if at end
           plo     re                  ; save a copy
           smi     'a'                 ; check for below letters
           lbnf    token_dn            ; jump if at end of word
           smi     26                  ; check for top of letters
           lbdf    token_dn            ; jump if above letters
           glo     re                  ; recover letter
           smi     91                  ; convert to 5-bit zscii
           sep     scall               ; push into zsequence
           dw      shift5
           inc     rf                  ; move past character
           ldi     low wrd_len         ; need to increment word length
           plo     rd
           ldn     rd
           adi     1
           str     rd
           ldi     low wrd_addr        ; and address
           plo     rd
           ldn     rd
           adi     1
           str     rd
           irx                         ; get count
           ldx
           smi     1                   ; minus 1 character
           stxd                        ; otherwise put back on stack
           lbz     token_6             ; jump if 6 characters added to token
           lbr     tkn_lp              ; loop back for more
token_6:   ldn     rf                  ; get next byte from token
           lbz     token_dn            ; jump if end found
           smi     'a'                 ; check if below letters
           lbnf    token_dn            ; done if so
           smi     26                  ; check above letters
           lbdf    token_dn            ; jump if so
           inc     rf                  ; move past character
           ldi     low wrd_len         ; need to increment word length
           plo     rd
           ldn     rd
           adi     1
           str     rd
           ldi     low wrd_addr        ; and address
           plo     rd
           ldn     rd
           adi     1
           str     rd
           lbr     token_6
token_dn:  irx                         ; recover count
           ldx
           lbz     token_go            ; jump if have all 6 characters
           smi     1                   ; subtract 1 from count
           stxd                        ; save it
           ldi     5                   ; push in a 5
           sep     scall               ; to the zsequence
           dw      shift5
           lbr     token_dn            ; loop until 6 characters
token_go:  ghi     r8                  ; be sure high bit is not set
           ani     07fh
           phi     r8
           ghi     r7                  ; this one should have high bit set
           ori     080h
           phi     r7
           sep     scall               ; search dictionary
           dw      srch_dict
           ghi     r9                  ; write into record
           str     ra
           inc     ra
           glo     r9
           str     ra
           inc     ra
           ldi     low wrd_len         ; need word length
           plo     rd                  ; setup data pointer
           ldn     rd                  ; get length
           str     ra                  ; store into record
           inc     ra
           irx                         ; recover buffer position from stack
           ldx
           str     ra                  ; and store into record
           inc     ra
           ldi     low tkn_cnt         ; need to update token count
           plo     rd                  ; setup data pointer
           ldn     rd                  ; increment it
           adi     1
           str     rd                  ; and put it back
           str     r2                  ; save for a moment
           ldi     low max_tkn         ; need to compare against max
           plo     rd
           ldn     rd
           sm                          ; compare values
           lbz     encode_dn           ; jump if all tokens allowed
           lbr     encode_lp           ; loop back for more tokens
encode_dn: ldi     low arg2            ; need parse buffer address
           plo     rd                  ; setup data pointer
           lda     rd                  ; retrieve address
           phi     ra                  ; into ra
           ldn     rd
           plo     ra
           ldi     low header          ; add in header address
           str     r2
           glo     ra
           add
           plo     ra
           ldi     high header
           str     r2
           ghi     ra
           adc
           phi     ra
           inc     ra                  ; move to encoded tokens field
           ldi     low tkn_cnt         ; now need token count
           plo     rd                  ; setup data pointer
           ldn     rd                  ; retrieve token count
           str     ra                  ; and place into parse buffer
           sep     sret                ; return to caller




; ****************************
; *** Convert buffer to lc ***
; ****************************
tolc:      glo     rf                  ; save position
           stxd
           ghi     rf
           stxd
tolclp:    ldn     rf                  ; get byte from buffer
           lbz     tolcdn              ; jump if finished
           smi     'A'                 ; see if below uc
           lbnf    tolcno              ; jump if so
           smi     26                  ; see if <= Z
           lbdf    tolcno              ; jump if not
           ldn     rf                  ; recover character
           adi     32                  ; convert to lowercase
           str     rf                  ; and put back
tolcno:    inc     rf                  ; point to next position
           lbr     tolclp              ; and keep looking
tolcdn:    irx                         ; recover buffer position
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     sret                ; and return to caller

; *********************************************************************
; *** End of keyboard input section                                 ***
; *********************************************************************


; ********************************
; *** Add header address to rf ***
; ********************************
add_hdr:   ldi     low header          ; get header address
           str     r2                  ; need to add
           glo     rf                  ; to rf
           add
           plo     rf
           ldi     high header         ; now high byte
           str     r2
           ghi     rf
           adc
           phi     rf
           sep     sret                ; return to caller

; *********************************************************************
; *** Instruction decoder/dispatcher                                ***
; *********************************************************************
cycle:
#ifdef DEBUG
           ldi     low tab
           plo     rd
           ldn     rd
tab_lp:    lbz     tab_dn
           stxd
           ldi     ' '
           sep     scall
           dw      o_type
           irx
           ldx
           smi     1
           lbr     tab_lp
tab_dn:    ldi     low ip
           plo     rd
           ldn     rd
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ldn     rd
           ani     0fh
           sep     scall
           dw      hexdigit
           inc     rd
           ldn     rd
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ldn     rd
           ani     0fh
           sep     scall
           dw      hexdigit
           inc     rd
           ldn     rd
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ldn     rd
           ani     0fh
           sep     scall
           dw      hexdigit
           sep     scall
           dw      f_inmsg
           db      ': ',0
#endif
           sep     scall               ; read next instruction byte
           dw      readip
           plo     re                  ; save a copy
           ani     0c0h                ; check for var type instruction
           smi     0c0h
           lbz     decode_v            ; decode var
           glo     re                  ; recover code
           ani     0c0h                ; check for short form
           smi     080h
           lbz     decode_s            ; decode short
; **************************************
; *** Decode long format instruction ***
; **************************************
decode_l:  ldi     low num_args        ; long is always 2OP
           plo     rd                  ; setup data pointer
           ldi     2
           str     rd
           ldi     low itype           ; set instruction type to 2OP
           plo     rd
           ldi     2
           str     rd
           ldi     low opcode          ; need to store opcode
           plo     rd
           glo     re                  ; get opcode
           stxd                        ; keep a copy on the stack
           ani     1fh                 ; only bottom 5 bits
           str     rd                  ; store into opcode var
           ldi     low arg1            ; point to first argument
           plo     rd
           ldi     0                   ; high byte will be zero
           str     rd                  ; store it
           inc     rd                  ; point to low byte
           sep     scall               ; get next byte from ip
           dw      readip
           str     rd                  ; and store it
           irx                         ; get opcode
           ldx
           dec     r2                  ; and keep on stack
           ani     040h                ; check first argument type
           lbz     dec_l_a2            ; jump if supposed to be a constant
           ldn     rd                  ; get variable number
           dec     rd                  ; then point back to msb
           sep     scall               ; read variable
           dw      getvar
           ghi     rf                  ; save value into arg1
           str     rd
           inc     rd
           glo     rf
           str     rd
dec_l_a2:  ldi     low arg2            ; point to second argument
           plo     rd
           ldi     0                   ; high byte will be zero
           str     rd                  ; store it
           inc     rd                  ; point to low byte
           sep     scall               ; get next byte from ip
           dw      readip
           str     rd                  ; and store it
           irx                         ; get opcode
           ldx
           ani     020h                ; check second argument type
           lbz     run_inst            ; jump if supposed to be a constant
           ldn     rd                  ; get variable number
           dec     rd                  ; then point back to msb
           sep     scall               ; read variable
           dw      getvar
           ghi     rf                  ; save value into arg1
           str     rd
           inc     rd
           glo     rf
           str     rd
           lbr     run_inst            ; instruction is ready to run
; ***************************************
; *** Decode short format instruction ***
; ***************************************
decode_s:  ldi     low opcode          ; point to opcode storage
           plo     rd                  ; setup data pointer
           glo     re                  ; get opcode
           ani     0fh                 ; only bottom 4 bits are kept
           str     rd                  ; write to opcode var
           glo     re                  ; recover code
           ani     030h                ; check for 0OP
           smi     030h
           lbz     dec_s_0             ; jump if so
           ldi     low num_args        ; arguments will be 1
           plo     rd                  ; setup data pointer
           ldi     1
           str     rd
           ldi     low itype           ; instruction tyep will be 1OP
           plo     rd
           ldi     1
           str     rd
           ldi     low arg1            ; point to first argument
           plo     rd                  ; setup data pointer
           glo     re                  ; recover opcode
           shr                         ; shift operand type down
           shr
           shr
           shr
           ani     3                   ; keep only operand type bits
           lbnz    dec_s_nl            ; jump if not large constant
           sep     scall               ; get next ip byte
           dw      readip
           str     rd                  ; save into argument
           inc     rd
           sep     scall               ; now get low byte
           dw      readip
           str     rd
           lbr     run_inst            ; instruction is ready to run
dec_s_nl:  smi     1                   ; check for short constant
           lbnz    dec_s_ns            ; jump if not
           ldi     0                   ; high byte of arg is zero
           str     rd
           inc     rd
           sep     scall               ; get next ip byte
           dw      readip
           str     rd                  ; store into arg
           lbr     run_inst            ; instruction is ready to run
dec_s_ns:  sep     scall               ; get next byte
           dw      readip
           sep     scall               ; get variable
           dw      getvar
           ghi     rf                  ; get high of value
           str     rd                  ; store into argument
           inc     rd
           glo     rf                  ; get low of value
           str     rd                  ; store into argument
           lbr     run_inst            ; instruction is ready to run
dec_s_0:   ldi     low num_args        ; need to set number of args
           plo     rd                  ; setup data pointer
           ldi     0                   ; num args is 0
           str     rd
           ldi     low itype           ; instruction type is 0OP
           plo     rd                  ; setup data pointer
           ldi     0
           str     rd
           lbr     run_inst            ; instruction is ready to run

; *************************************
; *** Decode var format instruction ***
; *************************************
decode_v:  ldi     low opcode          ; need to write opcode
           plo     rd                  ; setup data pointer
           glo     re                  ; retrieve code
           ani     1fh                 ; opcode is bottom 5 bits
           str     rd                  ; write to opcode var
           glo     re                  ; recover opcode
           ani     20h                 ; see if 2OP or VAR
           lbz     dec_v_2             ; jump if 2OP
           ldi     low itype           ; need to set instruction type
           plo     rd
           ldi     3                   ; to VAR
           str     rd
           lbr     dec_v_cnt           ; now process arguments
dec_v_2:   ldi     low itype           ; need to set instruction type
           plo     rd
           ldi     2                   ; to 2OP
           str     rd
dec_v_cnt: ldi     low num_args        ; need to reset num_args
           plo     rd
           ldi     0                   ; to 0
           str     rd
           sep     scall               ; get operand types byte
           dw      readip
           stxd                        ; keep it on the stack
           ldi     low arg1            ; point to argument storage
           plo     ra
           ldi     high arg1
           phi     ra
           ldi     low num_args        ; point rd at num_args
           plo     rd
dec_v_lp:  irx                         ; recover operand type
           ldx
           plo     re                  ; make a copy
           shl                         ; shift over 2 bits
           shl
           ori     3                   ; bottom 2 bits need to be set
           stxd                        ; and put back on the stack
           glo     re                  ; shift high bits to low
           shl
           shlc
           shlc
           ani     3                   ; only want bottom 2 bits
           lbz     dec_v_lc            ; jump for long constant
           smi     1                   ; check for short constant
           lbz     dec_v_sc
           smi     1                   ; check for var
           lbz     dec_v_va
           irx                         ; done so remove type byte from stack
           lbr     run_inst            ; instruction is ready to run
dec_v_lc:  sep     scall               ; read next byte from ip
           dw      readip
dec_v_lo:  str     ra                  ; store into variable
           inc     ra
           sep     scall               ; now read low byte
           dw      readip
           str     ra
           inc     ra
dec_v_go:  ldn     rd                  ; increment arg_count
           adi     1
           str     rd                  ; write it back
           lbr     dec_v_lp            ; loop for more
dec_v_sc:  ldi     0                   ; high byte is zero in small constant
           lbr     dec_v_lo            ; save into argument
dec_v_va:  sep     scall               ; need to get variable number
           dw      readip
           sep     scall               ; then read the variable
           dw      getvar
           ghi     rf                  ; write to argument
           str     ra
           inc     ra
           glo     rf
           str     ra
           inc     ra
           lbr     dec_v_go            ; incrment num_args and continue



; **********************************************
; *** Instructino is decoded, now execute it ***
; **********************************************
run_inst:  ldi     low itype           ; need to get instruction type
           plo     rd                  ; setup data pointer
           ldn     rd                  ; get it
           lbz     disp_0              ; jump if a 0OP
           smi     1
           lbz     disp_1              ; jump if a 1OP
           smi     1
           lbz     disp_2              ; jump if a 2OP
           lbr     disp_v              ; otherwise VAR
disp_0:    ldi     high table_0        ; need opcode table 0
           phi     r7
           ldi     low table_0
           plo     r7
           lbr     dispatch            ; dispatch instruction
disp_1:    ldi     high table_1        ; need opcode table 1
           phi     r7
           ldi     low table_1
           plo     r7
           lbr     dispatch            ; dispatch instruction
disp_2:    ldi     high table_2        ; need opcode table 2
           phi     r7
           ldi     low table_2
           plo     r7
           lbr     dispatch            ; dispatch instruction
disp_v:    ldi     high table_v        ; need opcode table v
           phi     r7
           ldi     low table_v
           plo     r7
dispatch:  ldi     low opcode          ; get opcode
           plo     rd                  ; setup data pointer
           ldn     rd                  ; get opcode
           shl                         ; multiply by 2
           str     r2                  ; add to table
           glo     r7
           add 
           plo     r7
           ghi     r7                  ; propagate carry
           adci    0
           phi     r7
           ldi     high jump           ; get jump vector
           phi     rf
           ldi     low jump
           plo     rf
           inc     rf                  ; move past lbr code
           lda     r7                  ; write address
           str     rf
           inc     rf
           lda     r7
           str     rf
jump:      lbr     cycle               ; instruction jump vector

; ****************************************
; *** Instructions jump here when done ***
; ****************************************
idone:
#ifdef DEBUG
  ldi  10
  sep  scall
  dw   o_type
  ldi  13
  sep  scall
  dw   o_type
#endif
           lbr     cycle               ; process next instruction

table_0:   dw      rtrue               ; 0 - rtrue
           dw      rfalse              ; 1 - rfalse
           dw      op_print            ; 2 - print
           dw      op_printr           ; 3 - print_ret
           dw      op_nop              ; 4 - nop
           dw      op_save             ; 5 - save
           dw      op_rest             ; 6 - restore
           dw      restart             ; 7 - restart
           dw      ret_pop             ; 8 - ret_popped
           dw      op_pop              ; 9 - pop
           dw      op_quit             ; A - quit
           dw      op_nl               ; B - new_line
           dw      show_stat           ; C - show_status
           dw      op_verify           ; D - verify
           dw      op_nop              ; E - nop
           dw      op_nop              ; F - save

table_1:   dw      op_jz               ; 0 - jz
           dw      get_sibl            ; 1 - get_sibling
           dw      get_chld            ; 2 - get_child
           dw      get_prnt            ; 3 - get_parent
           dw      get_plen            ; 4 - get_prop_len
           dw      op_inc              ; 5 - inc
           dw      op_dec              ; 6 - dec
           dw      prt_addr            ; 7 - print_addr
           dw      op_nop              ; 8 - save
           dw      rem_obj             ; 9 - remove_obj
           dw      prt_obj             ; A - print_obj
           dw      return              ; B - ret
           dw      op_jump             ; C - jump
           dw      prt_paddr           ; D - print_paddr
           dw      op_load             ; E - load
           dw      op_not              ; F - not

table_2:   dw      op_nop              ; 0 - nop
           dw      op_je               ; 1 - je
           dw      op_jl               ; 2 - jl
           dw      op_jg               ; 3 - jg
           dw      op_decchk           ; 4 - dec_chk
           dw      op_incchk           ; 5 - inc_chk
           dw      op_jin              ; 6 - jin
           dw      op_test             ; 7 - test
           dw      op_or               ; 8 - or
           dw      op_and              ; 9 - and
           dw      test_attr           ; A - test_attr
           dw      set_attr            ; B - set_attr
           dw      clr_attr            ; C - clear_attr
           dw      op_store            ; D - store
           dw      ins_obj             ; E - insert_obj
           dw      op_loadw            ; F - loadw
           dw      op_loadb            ; 10 - loadb
           dw      get_prop            ; 11 - get_prop
           dw      get_paddr           ; 12 - get_prop_addr
           dw      get_nprop           ; 13 - get_next_prop
           dw      op_add              ; 14 - add
           dw      op_sub              ; 15 - sub
           dw      op_mul              ; 16 - mul
           dw      op_div              ; 17 - div
           dw      op_mod              ; 18 - mod
           dw      op_nop              ; 19 - undefined
           dw      op_nop              ; 1A - undefined
           dw      op_nop              ; 1B - undefined
           dw      op_nop              ; 1C - undefined
           dw      op_nop              ; 1D - undefined
           dw      op_nop              ; 1E - undefined
           dw      op_nop              ; 1F - undefined

table_v:   dw      op_call             ; 0 - call
           dw      op_storew           ; 1 - storew
           dw      op_storeb           ; 2 - storeb
           dw      put_prop            ; 3 - put_prop
           dw      op_sread            ; 4 - sread
           dw      print_chr           ; 5 - print_char
           dw      print_num           ; 6 - print_num
           dw      op_rnd              ; 7 - random
           dw      op_push             ; 8 - push
           dw      op_pull             ; 9 - pull
           dw      splt_wnd            ; A - split_window
           dw      set_wnd             ; B - set_window
           dw      op_nop              ; C - undefined
           dw      op_nop              ; D - undefined
           dw      op_nop              ; E - undefined
           dw      op_nop              ; F - undefined
           dw      op_nop              ; 10 - undefined
           dw      op_nop              ; 11 - undefined
           dw      op_nop              ; 12 - undefined
           dw      out_strm            ; 13 - output_stream
           dw      in_strm             ; 14 - input_stream
           dw      sound               ; 15 - sound
           dw      op_nop              ; 16 - undefined
           dw      op_nop              ; 17 - undefined
           dw      op_nop              ; 18 - undefined
           dw      op_nop              ; 19 - undefined
           dw      op_nop              ; 1A - undefined
           dw      op_nop              ; 1B - undefined
           dw      op_nop              ; 1C - undefined
           dw      op_nop              ; 1D - undefined
           dw      op_nop              ; 1E - undefined
           dw      op_nop              ; 1F - undefined


; *********************************************************************
; *** End of Instruction decoder/dispatcher                         ***
; *********************************************************************

; *********************************
; *** Find address for variable ***
; *** D - var name to find      ***
; *** Returns: RA - address     ***
; ***          DF=1 - physical  ***
; ***          DF=0 - zmemory   ***
; *********************************
var_addr:  plo     re                  ; save a copy
           smi     1                   ; origin from 0
           shl                         ; multiply by 2
           plo     ra                  ; and put into ra
           ldi     0                   ; create high byte of offset
           shlc
           phi     ra                  ; ra now has offset
           glo     re                  ; recover variable number
           ani     0f0h                ; see if local or global
           lbnz    var_glbl            ; jump if global variable
           glo     ra                  ; need to subtract RA from RC
           str     r2
           glo     rc
           sm
           plo     ra
           ghi     ra
           str     r2
           ghi     rc
           smb
           phi     ra
           dec     ra                  ; move to msb of value
           smi     0                   ; indicate physcial memory
           sep     sret                ; and return to caller
var_glbl:  ghi     ra                  ; place offset onto stack
           stxd
           glo     ra
           stxd
           ldi     low header          ; need to get position of globals
           adi     12
           plo     ra
           ldi     high header
           adci    0
           phi     ra
           lda     ra                  ; retreive global base
           plo     re
           ldn     ra
           plo     ra
           glo     re
           phi     ra
           sex     r2                  ; point x back to stack
           irx                         ; add in variable offset
           glo     ra
           add
           plo     ra
           irx
           ghi     ra
           adc
           phi     ra
           glo     ra                  ; subtract bias
           smi     30
           plo     ra
           ghi     ra
           smbi    0
           phi     ra                  ; now have address of var
           adi     0                   ; indicate zmemory
           sep     sret                ; and return
  
ex_ra_rf:  glo     rf                  ; exchange ra and rf
           plo     re
           glo     ra
           plo     rf
           glo     re
           plo     ra
           ghi     rf
           plo     re
           ghi     ra
           phi     rf
           glo     re
           phi     ra
           sep     sret                ; return to caller

; *******************************
; *** Save RF into variable D ***
; *******************************
setvar:
#ifdef DEBUG
           stxd
           ldi     '<'
           sep     scall
           dw      o_type
           irx
           ldx
           stxd
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           irx
           ldx
           stxd
           ani     0fh
           sep     scall
           dw      hexdigit
           ldi     '='
           sep     scall
           dw      o_type
           ghi     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ghi     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           glo     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           glo     rf
           ani     0fh
           sep     scall
           dw      hexdigit
 
           ldi     '>'
           sep     scall
           dw      o_type
           ldi     ' '
           sep     scall
           dw      o_type
           irx
           ldx
#endif
           lbnz    setvar1             ; jump if not stack access
           sex     rb                  ; point to game stack 
           glo     rf                  ; and push value onto it
           stxd
           ghi     rf
           stxd
           sex     r2                  ; point x back to main stack
           sep     sret                ; and return
setvar1:   plo     re                  ; save variable number
           glo     ra                  ; save consumed register
           stxd
           ghi     ra
           stxd
           glo     re                  ; recover variable number
           sep     scall               ; get address for variable
           dw      var_addr
           lbnf    setvar2             ; jump if zmemory
           ghi     rf                  ; save value into variable
           str     ra
           inc     ra
           glo     rf
           str     ra
setvar_rt: irx                         ; recover ra
           ldxa
           phi     ra
           ldx
           plo     ra
           sep     sret                ; return to caller
setvar2:   sep     scall               ; exchnage ra with rf
           dw      ex_ra_rf
           ghi     ra                  ; write high byte to memory
           sep     scall
           dw      wrmem16
           inc     rf                  ; increment address
           glo     ra                  ; and write low byte
           sep     scall
           dw      wrmem16
           sep     scall               ; swap ra and rf back
           dw      ex_ra_rf
           lbr     setvar_rt           ; and return
           

; ******************************
; *** Get variable D into RF ***
; ******************************
getvar:
#ifdef DEBUG
           stxd
           ldi     '('
           sep     scall
           dw      o_type
           irx
           ldx
           stxd
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           irx
           ldx
           stxd
           ani     0fh
           sep     scall
           dw      hexdigit
           irx
           ldx
#endif
           lbnz    getvar1             ; jump if not stack access
           sex     rb                  ; point x to game stack
           irx                         ; pop off a vluae
           ldxa
           phi     rf                  ; into rf
           ldx
           plo     rf
           sex     r2                  ; point x back to main stack
#ifdef DEBUG
           ldi     '='
           sep     scall
           dw      o_type
           ghi     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ghi     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           glo     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           glo     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           ldi     ')'
           sep     scall
           dw      o_type
           ldi     ' '
           sep     scall
           dw      o_type
#endif
           sep     sret                ; and return
getvar1:   plo     re                  ; save variable number
           glo     ra                  ; save consumed register
           stxd
           ghi     ra
           stxd
           glo     re                  ; recover variable number
           sep     scall               ; and get address for variable
           dw      var_addr
           lbnf    getvar2             ; jump if zmemory
           lda     ra                  ; read variable value
           phi     rf                  ; into rf
           ldn     ra
           plo     rf
getvar_go:
#ifdef DEBUG
           ldi     '='
           sep     scall
           dw      o_type
           ghi     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ghi     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           glo     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           glo     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           ldi     ')'
           sep     scall
           dw      o_type
           ldi     ' '
           sep     scall
           dw      o_type
#endif
           lbr     setvar_rt           ; recover ra and return
getvar2:   sep     scall               ; exchange ra with rf
           dw      ex_ra_rf
           sep     scall               ; read high byte
           dw      rdmem16
           phi     ra                  ; and place into ra
           inc     rf
           sep     scall               ; read low byte
           dw      rdmem16
           plo     ra
           sep     scall               ; exhange registers back
           dw      ex_ra_rf
           lbr     getvar_go           ; and return

; *************************************
; *** Read zmemory pointed to by RF ***
; *** Returns byte in D             ***
; *************************************
rdmem16:   glo     rd                  ; save rd
           stxd
           glo     rf                  ; save original rf
           stxd
           ghi     rf
           stxd
           ldi     low static          ; need to compare to static address
           plo     rd
           inc     rd                  ; starting from lsb
           sex     rd                  ; point x to static address
           glo     rf                  ; and perform subtraction
           sm
           dec     rd                  ; point to msb
           ghi     rf
           smb
           sex     r2                  ; point x back to stack
           lbdf    rdmem_st            ; jump if address is static
           ldi     low header          ; add in memory base
           str     r2
           glo     rf
           add
           plo     rf
           ldi     high header
           str     r2
           ghi     rf
           adc
           phi     rf                  ; rf now has address
           ldn     rf                  ; read byte from memory
           plo     re                  ; save it
           lbr     rdmem_rt            ; and return
rdmem_st:  glo     r7                  ; save r7
           stxd
           ghi     rf                  ; need to get page number
           shr                         ; which is high 7 bits
           plo     r7
           ghi     rf                  ; clear page number from offset
           ani     1
           phi     rf
           sep     scall               ; read from page
           dw      rd_page
           plo     re                  ; save read byte
           irx                         ; recover r7
           ldx
           plo     r7
           lbr     rdmem_rt            ; and return
           

; *************************************
; *** Read zmemory pointed to by RF ***
; *** Returns word in R7            ***
; *************************************
rdmem16w:  sep     scall               ; read high byte out of memory
           dw      rdmem16
           phi     r7                  ; put into r7
           inc     rf                  ; point to low byte
           sep     scall               ; read low byte out of memory
           dw      rdmem16
           plo     r7
           dec     rf                  ; put rf back
           sep     sret                ; and return


; **************************************
; *** Write zmemory pointed to by RF ***
; *** D - byte to write              ***
; **************************************
wrmem16:   plo     re                  ; save byte to write
           glo     rd                  ; save rd
           stxd
           glo     rf                  ; save original address
           stxd
           ghi     rf
           stxd
           ldi     low header          ; add in zmemory base
           str     r2
           glo     rf
           add
           plo     rf
           ldi     high header
           str     r2
           ghi     rf
           adc
           phi     rf
           glo     re                  ; recover byte to write
           str     rf                  ; and store it
rdmem_rt:  irx                         ; recover original rf
           ldxa
           phi     rf
           ldxa
           plo     rf
           ldx                         ; recover original rd
           plo     rd
           glo     re                  ; recover byte to write
           sep     sret                ; return to caller

; ********************************
; *** Read from a zmemory page ***
; *** R7 - page                ***
; *** RF - offset              ***
; *** Returns: D - bytes read  ***
; ********************************
rd_page:   glo     rd                  ; save current rd
           stxd
           ldi     low page_tab        ; get against loaded page
           plo     rd
           sex     rd                  ; set x to data pointer
rd_pg_fnd: glo     r7                  ; check against page entry
           sm                          ; compare
;           lbz     page_gon            ; jump if found correct page
           lbz     page_go             ; jump if found correct page
           inc     rd                  ; point to page address
           lda     rd                  ; see if end of table
           lbz     rd_pg_end           ; jump if so
           inc     rd                  ; move to next entry
           lbr     rd_pg_fnd           ; keep looking
page_go:   glo     rd                  ; see if at top page
           smi     low page_tab
           lbz     page_gon            ; jump if no need to bubble
           sex     r2                  ; be sure x points to stack
           glo     rf                  ; save rf
           stxd
           ghi     rf
           stxd
           ghi     rd                  ; copy rd to rf
           phi     rf
           glo     rd
           plo     rf
page_gol:  dec     rf                  ; make 1 entry higher up
           dec     rf
           dec     rf
           glo     rd                  ; see if at top slot
           smi     low page_tab
           lbz     page_go2            ; jump if at top
           ldn     rf                  ; exchange entries
           plo     re
           ldn     rd
           str     rf
           glo     re
           str     rd
           inc     rf                  ; point to next byte
           inc     rd
           ldn     rf                  ; exchange entries
           plo     re
           ldn     rd
           str     rf
           glo     re
           str     rd
           inc     rf                  ; point to next byte
           inc     rd
           ldn     rf                  ; exchange entries
           plo     re
           ldn     rd
           str     rf
           glo     re
           str     rd
           dec     rd                  ; back to beginning
           dec     rd
           dec     rf                  ; back to beginning
           dec     rf
           dec     rd                  ; back 1 whole entry
           dec     rd
           dec     rd
           lbr     page_gol            ; loop until bubbled to top
page_go2:  irx                         ; recover rf
           ldxa
           phi     rf
           ldx
           plo     rf

page_gon:  sex     rd                  ; be sure x points to page entry
           inc     rd                  ; point to lsb of address
           inc     rd
           glo     rf                  ; add it into rf
           add
           plo     rf
           dec     rd
           ghi     rf
           adc
           phi     rf
           sex     r2                  ; point x to stack
           ldn     rf                  ; read the byte
           plo     re                  ; set aside for a moment
           irx                         ; recover rd
           ldx
           plo     rd
           glo     re                  ; get read byte
           sep     sret                ; and return
rd_pg_end: ldi     low page_tab        ; need to find a page
           plo     rd
           sex     rd                  ; be sure stack points to table
rd_pg_el:  ldi     low next_page       ; need to get next usable page
           plo     rd
           ldn     rd                  ; read page address
           plo     re                  ; keep a copy

rd_pg_e2:  glo     re                  ; resetup rd
           plo     rd
           lbr     rd_need
           

rd_need:   sex     r2                  ; point x back to stack
           glo     r7                  ; save consumed registers
           stxd
           ghi     r7
           stxd
           glo     r8                  ; save consumed registers
           stxd
           ghi     r8
           stxd
           glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           glo     rf                  ; save consumed registers
           stxd
           ghi     rf
           stxd
           glo     rd                  ; save consumed registers
           stxd
           glo     r7                  ; save new page address
           str     rd
           glo     r7                  ; shift page number
           shl
           ani     0feh                ; strip lowest bit out
           phi     r7                  ; and put into r7
           ldi     0                   ; build high word
           phi     r8
           shlc 
           plo     r8
           ldi     0                   ; low byte of r7 is zero
           plo     r7                  ; R8:R7 now has file offset
           glo     rd                  ; save page pointer
           stxd
           ldi     low fildes          ; point to file descriptor
           plo     rd
           ldi     0                   ; seek from beginning
           plo     rc
           phi     rc
           sep     scall               ; perform the seek
           dw      o_seek
           irx                         ; recover page pointer
           ldx
           plo     rd
           inc     rd                  ; move to buffer address
           lda     rd                  ; retrieve puffer address
           phi     rf                  ; into rf
           lda     rd
           plo     rf
           ldi     2                   ; need to read 512 bytes
           phi     rc
           ldi     0
           plo     rc
           ldi     low fildes          ; point to file descriptor
           plo     rd
           sep     scall               ; read the bytes
           dw      o_read
           irx                         ; recover consumed registers
           ldxa
           plo     rd
           ldxa
           phi     rf
           ldxa
           plo     rf
           ldxa
           phi     rc
           ldxa
           plo     rc
           ldxa
           phi     r8
           ldxa
           plo     r8
           ldxa
           phi     r7
           ldx
           plo     r7
           lbr     page_go             ; now retrieve byte
           
; ******************************
; *** Read next byte from IP ***
; *** Returns: D - next byte ***
; ******************************
readip:    glo     rd                  ; want to save rd position
           stxd
           glo     rf                  ; save rf
           stxd
           ghi     rf
           stxd
           ldi     low ip              ; need high byte of ip
#ifdef DEBUG
   plo     rd
   ldn     rd                  ; get it
   lbnz    readip_l            ; jump if long address
   inc     rd                  ; point to middle byte of ip
   lda     rd                  ; and read it into rf
   phi     rf
   ldn     rd
   plo     rf
   sep     scall               ; read using 16-bit routine
   dw      rdmem16
   plo     re                  ; save answer
  sep  scall
  dw   hexout
   lbr inc_ip
#endif
read_17:   plo     rd
           ldn     rd                  ; get it
           lbnz    readip_l            ; jump if long address
           inc     rd                  ; point to middle byte of ip
           lda     rd                  ; and read it into rf
           phi     rf
           ldn     rd
           plo     rf
           sep     scall               ; read using 16-bit routine
           dw      rdmem16
           plo     re                  ; save answer
inc_ip:    ldn     rd                  ; get lsb
           adi     1                   ; increment it
           str     rd                  ; and put back
           dec     rd                  ; now middle byte
           ldn     rd
           adci    0
           str     rd
           dec     rd                  ; and now high byte
           ldn     rd
           adci    0
           str     rd
           lbr     rdmem_rt            ; return
readip_l:  glo     r7                  ; save r7
           stxd
           inc     rd                  ; point to middle byte
           ldn     rd                  ; and retrieve it
           shr                         ; get page number
           ori     080h                ; place high bit
           plo     r7                  ; and place into r7
           ldn     rd                  ; get byte again
           ani     1                   ; strip out page
           phi     rf                  ; and place into rf
           inc     rd                  ; point at lsb
           ldn     rd                  ; retrieve it
           plo     rf                  ; rf now has offset
           sep     scall               ; read the page
           dw      rd_page
           plo     re                  ; save copy result
           irx                         ; recover r7
           ldx
           plo     r7
           lbr     inc_ip              ; now increment pointer


; **********************************
; *** Read next byte from hm_ptr ***
; *** Returns: D - next byte     ***
; **********************************
readhm:    glo     rd                  ; want to save rd position
           stxd
           glo     rf                  ; save rf
           stxd
           ghi     rf
           stxd
           ldi     low hm_ptr          ; need high byte of ip
           lbr     read_17             ; then read from 17-bit address

; ******************************************************************
; ***                   0OP instruction handlers                 ***
; ******************************************************************
; *******************************
; *** 0 - Handle rtrue opcode ***
; *******************************
rtrue:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:RTRUE ',0
#endif
           ldi     0                   ; true is one 
           phi     rf                  ; place into return value
           plo     rf
           inc     rf                  ; make value true
           lbr     return_rf           ; and return

; ********************************
; *** 1 - Handle rfalse opcode ***
; ********************************
rfalse:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:RFALSE ',0
#endif
           ldi     0                   ; false is zero 
           phi     rf                  ; place into return value
           plo     rf
           lbr     return_rf           ; and return

; *******************************
; *** 2 - Handle print opcode ***
; *******************************
op_print:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:PRINT ',0
#endif
           sep     scall               ; reset printing engine
           dw      rst_prnt
           sep     scall               ; print string from ip
           dw      ip_print
           lbr     idone               ; then done

; ***********************************
; *** 3 - Handle print_ret opcode ***
; ***********************************
op_printr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:PRINT_RET ',0
#endif
           sep     scall               ; reset printing engine
           dw      rst_prnt
           sep     scall               ; print string from ip
           dw      ip_print
           ldi     10                  ; output a LF
           sep     scall
           dw      o_type
           ldi     13                  ; and a CR
           sep     scall
           dw      o_type
           ldi     low line_pos        ; need to clear line position
           plo     rd
           ldi     0 
           str     rd
           lbr     rtrue               ; then return true

; *****************************
; *** 4 - Handle nop opcode ***
; *****************************
op_nop:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:NOP ',0
#endif
           lbr     idone               ; does nothing

docrlf:    ldi     10                  ; output a LF
           sep     scall
           dw      o_type
           ldi     13                  ; and a CR
           sep     scall
           dw      o_type
           sep     sret

get_fname: sep     scall
           dw      f_inmsg
           db      'Enter filename: ',0
           ldi     high tbuffer        ; point to input buffer
           phi     rf
           ldi     low tbuffer
           plo     rf
           sep     scall               ; read input from user
           dw      o_input
           sep     scall               ; perform cr/lf
           dw      docrlf
           ldi     low next_page       ; need last page address for dta
           plo     rd
           lda     rd                  ; retrieve it
           plo     rd                  ; rd now pointing at page
           ldi     0                   ; set page byte to zero
           str     rd
           inc     rd                  ; now get address
           lda     rd
           phi     rf
           ldn     rd
           plo     rf
           ldi     low fildes2         ; now point to save fildes
           adi     4                   ; point to dta field
           plo     rd
           ghi     rf                  ; and write dta address
           str     rd
           inc     rd
           glo     rf
           str     rd
           ldi     low fildes2         ; now point to save fildes
           plo     rd                  ; fildes is now ready
           ldi     high tbuffer        ; point to filename
           phi     rf
           ldi     low tbuffer
           plo     rf
           sep     sret                ; return to caller


; ***********************
; *** 5 - Handle save ***
; ***********************
op_save:   ldi     low reg_rc          ; need to save important registers
           plo     rd
           ghi     rc
           str     rd
           inc     rd
           glo     rc
           str     rd
           inc     rd
           ghi     rb
           str     rd
           inc     rd
           glo     rb
           str     rd
           inc     rd
           ghi     r2
           str     rd
           inc     rd
           glo     r2
           str     rd
           inc     rd
           sep     scall               ; get fname and setup descriptor
           dw      get_fname
           ldi     3                   ; create, truncate if exists
           plo     r7
           sep     scall               ; open the file
           dw      o_open
           ldi     low ip              ; first will save registers
           plo     rf
           ghi     rd
           phi     rf
           ldi     0                   ; 9 bytes
           phi     rc
           ldi     9
           plo     rc
           sep     scall               ; write the bytes
           dw      o_write

           ldi     low static          ; need static address
           plo     rd
           lda     rd                  ; as next count
           phi     rc
           lda     rd
           plo     rd
           ldi     low fildes2         ; point rd back to fildes
           plo     rd
           ldi     high header         ; write from header
           phi     rf
           ldi     low header
           plo     rf
           sep     scall               ; write the bytes
           dw      o_write
           ldi     low reg_rb          ; next block starts at rb
           plo     rd
           lda     rd
           phi     rf
           ldn     rd
           plo     rf
           ldi     low fildes2         ; point rd back to fildes
           plo     rd
           glo     rf                  ; size is 7eff-RF
           sdi     0ffh
           plo     rc
           ghi     rf
           sdbi    07eh
           phi     rc
           sep     scall               ; write the bytes
           dw      o_write

           sep     scall               ; close the file
           dw      o_close
           ldi     low reg_rc          ; recover rc
           plo     rd
           lda     rd
           phi     rc
           ldn     rd
           plo     rc
           lbr     true

; *********************************
; *** 6 - Handle restore opcode ***
; *********************************
op_rest:   glo     rc                  ; save rc
           stxd
           ghi     rc
           stxd
           sep     scall               ; get fname and setup descriptor
           dw      get_fname
           ldi     0                   ; create, truncate if exists
           plo     r7
           sep     scall               ; open the file
           dw      o_open
           irx                         ; recover rc
           ldxa
           phi     rc
           ldx
           plo     rc
           lbdf    false               ; error if file could not be opened
           ldi     low ip              ; first will save registers
           plo     rf
           ghi     rd
           phi     rf
           ldi     0                   ; 9 bytes
           phi     rc
           ldi     9
           plo     rc
           sep     scall               ; read the bytes
           dw      o_read

           ldi     low static          ; need static address
           plo     rd
           lda     rd                  ; as next count
           phi     rc
           lda     rd
           plo     rd
           ldi     low fildes2         ; point rd back to fildes
           plo     rd
           ldi     high header         ; write from header
           phi     rf
           ldi     low header
           plo     rf
           sep     scall               ; read the bytes
           dw      o_read
           ldi     low reg_rb          ; next block starts at rb
           plo     rd
           lda     rd
           phi     rf
           ldn     rd
           plo     rf
           ldi     low fildes2         ; point rd back to fildes
           plo     rd
           glo     rf                  ; size is 7eff-RF
           sdi     0ffh
           plo     rc
           ghi     rf
           sdbi    07eh
           phi     rc
           sep     scall               ; read the bytes
           dw      o_read

           sep     scall               ; close the file
           dw      o_close
           ldi     low reg_rc          ; recover rc
           plo     rd
           lda     rd
           phi     rc
           lda     rd
           plo     rc
           lda     rd                  ; recover rb
           phi     rb
           lda     rd
           plo     rb
           lda     rd                  ; recover r2
           phi     r2
           lda     rd
           plo     r2
           ldi     low page_tab        ; need to clear page table
           plo     rd
           ldi     19                  ; 19 entries
           plo     re
clear_lp:  ldi     0                   ; clear entry
           str     rd
           inc     rd                  ; move to next entry
           inc     rd
           inc     rd
           dec     re                  ; decrement count
           glo     re                  ; see if done
           lbnz    clear_lp            ; jump if not done
           lbr     true

; *********************************
; *** 7 - Handle restart opcode ***
; *********************************
restart:   ldi     low fildes          ; need file descriptor
           plo     rd
           ldi     0                   ; point file back to beginning
           phi     r8
           plo     r8
           phi     r7
           plo     r7
           phi     rc                  ; seek from beginning
           plo     rc
           sep     scall               ; perform the seek
           dw      o_seek
           lbr     opened              ; start program from beginning

; ***************************************
; *** 8 - Handle return_popped opcode ***
; ***************************************
ret_pop:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:RETURN_POPPED ',0
#endif
           sex     rb                  ; point x to game stack 
           irx                         ; point to top of stack
           ldxa                        ; retreive top value
           phi     rf                  ; into rf
           ldx
           plo     rf
           sex     r2                  ; point x back to stack
           lbr     return_rf           ; and perform return

; *****************************
; *** 9 - Handle pop opcode ***
; *****************************
op_pop:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:POP ',0
#endif
           inc     rb                  ; remove top of game stack
           inc     rb
           lbr     idone               ; done

; ******************************
; *** A - Handle quit opcode ***
; ******************************
op_quit:   lbr     o_wrmboot           ; return to Elf/OS
        
; **********************************
; *** B - Handle new_line opcode ***
; **********************************
op_nl:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:NEW_LINE ',0
#endif
           ldi     10                  ; output a LF
           sep     scall
           dw      o_type
           ldi     13                  ; and a CR
           sep     scall
           dw      o_type
           ldi     low line_pos        ; reset line position
           plo     rd
           ldi     0
           str     rd
           lbr     idone               ; then continue

; *************************************
; *** C - Handle show_status opcode ***
; *************************************
show_stat:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:SHOW_STAT ',0
#endif
           lbr     idone               ; ignore this code

; ********************************
; *** D - Handle verify opcode ***
; ********************************
op_verify:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':0OP:VERIFY ',0
#endif
           lbr     true                ; not implemented

; ******************************************************************
; ***                   1OP instruction handlers                 ***
; ******************************************************************
; ****************************
; *** 0 - Handle jz opcode ***
; ****************************
op_jz:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:JZ ',0
#endif
           ldi     low arg1            ; point to arg1
           plo     rd                  ; set into data pointer
           lda     rd                  ; get high byte
           lbnz    false               ; jump if not zero
           ldn     rd                  ; check low byte
           lbnz    false               ; jump if not zero
           lbr     true                ; otherwise true

; *************************************
; *** 1 - Handle get_sibling opcode ***
; *************************************
get_sibl:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:GET_SIBLING ',0
#endif
           ldi     low arg1            ; point to arg1
           plo     rd 
           inc     rd                  ; need low byte
           ldn     rd                  ; now have object number
           sep     scall               ; get object address
           dw      obj_addr
           glo     rf                  ; point to sibling entry
           adi     5
get_go:    plo     rf
           ghi     rf                  ; propagate carry
           adci    0
           phi     rf
           sep     scall               ; read memory
           dw      rdmem16
           plo     rf                  ; save value
           ldi     0                   ; set high byte to zero
           phi     rf
           sep     scall               ; read result number from ip
           dw      readip
           sep     scall               ; save the result
           dw      setvar
           glo     rf                  ; branch if nonzero
           lbnz    true
           lbr     false               ; otherwise false

; ***********************************
; *** 2 - Handle get_child opcode ***
; ***********************************
get_chld:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:GET_CHILD ',0
#endif
           ldi     low arg1            ; point to arg1
           plo     rd 
           inc     rd                  ; need low byte
           ldn     rd                  ; now have object number
           sep     scall               ; get object address
           dw      obj_addr
           glo     rf                  ; point to child entry
           adi     6
           lbr     get_go              ; jump to finish

; ************************************
; *** 3 - Handle get_parent opcode ***
; ************************************
get_prnt:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:GET_PARENT ',0
#endif
           ldi     low arg1            ; point to arg1
           plo     rd 
           inc     rd                  ; need low byte
           ldn     rd                  ; now have object number
           sep     scall               ; get object address
           dw      obj_addr
           inc     rf                  ; move to parent
           inc     rf
           inc     rf
           inc     rf
           sep     scall               ; read memory
           dw      rdmem16
           plo     rf                  ; save value
           ldi     0                   ; set high byte to zero
           phi     rf
           sep     scall               ; read result number from ip
           dw      readip
           sep     scall               ; save the result
           dw      setvar
           lbr     idone               ; done

; **************************************
; *** 4 - Handle get_prop_len opcode ***
; **************************************
get_plen:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:GET_PROP_LEN ',0
#endif
           ldi     low arg1            ; need to get address
           plo     rd                  ; place into data pointer
           lda     rd                  ; get address
           phi     rf                  ; into rf
           ldn     rd
           plo     rf
           dec     rf                  ; move to length byte
           sep     scall               ; read the length byte
           dw      rdmem16
           shr                         ; get only size bits
           shr
           shr
           shr
           shr
           adi     1                   ; add bias back
           plo     rf                  ; place into rf
           ldi     0                   ; high byte is zero
           phi     rf
           lbr     res_save            ; save result
        
; *****************************
; *** 5 - Handle inc opcode ***
; *****************************
op_inc:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:INC ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           ldn     rd                  ; get variable number
           sep     scall               ; get value of variable
           dw      getvar
           inc     rf                  ; increment it
           ldn     rd                  ; get variable number
           sep     scall               ; and save new value
           dw      setvar
           lbr     idone               ; done

; *****************************
; *** 6 - Handle dec opcode ***
; *****************************
op_dec:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:DEC ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           ldn     rd                  ; get variable number
           sep     scall               ; get value of variable
           dw      getvar
           dec     rf                  ; decrement it
           ldn     rd                  ; get variable number
           sep     scall               ; and save new value
           dw      setvar
           lbr     idone               ; done

; ************************************
; *** 7 - Handle print_addr opcode ***
; ************************************
prt_addr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:PRINT_ADDR ',0
#endif
           sep     scall               ; reset printing engine
           dw      rst_prnt
           ldi     low arg1            ; need to get address
           plo     rd                  ; setup data pointer
           lda     rd                  ; read address
           phi     rf                  ; into rf
           ldn     rd
           plo     rf
           sep     scall               ; print string pointed to by RF
           dw      rf_print
           lbr     idone               ; then done

; ************************************
; *** 9 - Handle remove_obj opcode ***
; ************************************
rem_obj:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:REMOVE_OBJ ',0
#endif
           sep     scall               ; perform remove object
           dw      remove
           lbr     idone               ; done

remove:    ldi     low arg1            ; point to arg1
           plo     rd                  ; setup data pointer
           inc     rd                  ; need lsb
           ldn     rd                  ; retrieve it
           phi     r9                  ; save object number
           sep     scall
           dw      obj_addr            ; get object
           inc     rf                  ; point to parent
           inc     rf
           inc     rf
           inc     rf
           sep     scall               ; read parent
           dw      rdmem16   
           lbz     dosret              ; nothing to do if parent is 0
           phi     r8                  ; save parent object number
           ldi     0                   ; set parent to zero
           sep     scall               ; write to parent field
           dw      wrmem16
           inc     rf                  ; point to sibling field
           sep     scall               ; and retrieve it
           dw      rdmem16
           plo     r8                  ; save it
           ldi     0                   ; set it to zero
           sep     scall               ; write back to sibling field
           dw      wrmem16
           ghi     r8                  ; get parent object number
           sep     scall               ; and retrieve object address
           dw      obj_addr
           inc     rf                  ; point to child field
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           sep     scall                ; read child object number
           dw      rdmem16
           plo     r9                   ; place into I
           str     r2                   ; and memory for compare
           ghi     r9                   ; get OBJ
           sm                           ; and compare
           lbnz    rem_obj_1            ; jump if not first child
           glo     r8                   ; get S
           sep     scall                ; and write to child field of parent
           dw      wrmem16
           lbr     dosret               ; done
rem_obj_1: glo     r9                   ; get I
           lbz     dosret               ; jump if end of object tree
           sep     scall                ; get object I
           dw      obj_addr
           inc     rf                   ; point to sibling field
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           ghi     r9                   ; get OBJ
           stxd                         ; and place for compare
           sep     scall                ; and read it
           dw      rdmem16
           plo     r9                   ; write new i
           irx                          ; compare with previous I
           sm
           lbnz    rem_obj_1            ; jump if no match
           glo     r8                   ; get S
           sep     scall                ; and write it
           dw      wrmem16
dosret:    sep     sret                 ; done removing object

; ***********************************
; *** A - Handle print_obj opcode ***
; ***********************************
prt_obj:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:PRT_OBJ ',0
#endif
           sep     scall               ; reset printing engine
           dw      rst_prnt
           ldi     low arg1            ; need to get object number
           plo     rd                  ; setup data pointer
           inc     rd                  ; point to object number
           ldn     rd                  ; and retrieve it
           sep     scall               ; get object address
           dw      obj_addr
           inc     rf                  ; move to properties field
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           sep     scall               ; retrieve it
           dw      rdmem16w
           ghi     r7                  ; transfer back to rf
           phi     rf
           glo     r7
           plo     rf
           inc     rf                  ; move past size byte
           sep     scall               ; and print object name
           dw      rf_print
           lbr     idone               ; then done

; ********************************
; *** B - Handle return opcode ***
; ********************************
return:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:RETURN ',0
#endif
           ldi     low arg1            ; point to argument to return
           plo     rd
           lda     rd                  ; read value
           phi     rf                  ; save temporarily
           ldn     rd                  ; get low byte
           plo     rf
return_rf: irx                         ; point to previous ip
           ldi     low ip              ; point to instruction pointer
           plo     rd                  ; setup data pointer
           ldxa                        ; get highest byte of last ip
           str     rd                  ; and store
           inc     rd
           ldxa                        ; next highest byte
           str     rd
           inc     rd
           ldxa                        ; least significant byte
           str     rd                  ; previous ip is now restored
           ldxa                        ; get result var
           plo     re                  ; set aside for now
           ldxa                        ; and clear current stack frame
           phi     rc
           ldxa                        ; low byte as well
           plo     rc
           ldxa                        ; reset local stack address
           phi     rb
           ldx                         ; low byte as well
           plo     rb
           glo     re                  ; recover result var
           sep     scall               ; save the result
           dw      setvar
#ifdef DEBUG
           ldi     low tab
           plo     rd
           ldn     rd
           smi     2
           str     rd
#endif
           lbr     idone               ; done with instruction

; ******************************
; *** C - Handle jump opcode ***
; ******************************
op_jump:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:JUMP ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; place into data pointer
           lda     rd                  ; retrieve value
           phi     rf                  ; into rf
           ldn     rd
           plo     rf
           lbr     br_go               ; execute the jump

; *************************************
; *** D - Handle print_paddr opcode ***
; *************************************
prt_paddr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:PRINT_PADDR ',0
#endif
           sep     scall               ; reset printing engine
           dw      rst_prnt
           ldi     low arg1            ; need to get address
           plo     rd                  ; setup data pointer
           lda     rd                  ; read address
           phi     rf                  ; into rf
           ldn     rd
           plo     rf
           ldi     low hm_ptr          ; need to setup high memory pointer
           plo     rd
           inc     rd                  ; move to lowest byte
           inc     rd
           glo     rf                  ; multiply address by 2
           shl
           str     rd                  ; and store into hm_ptr
           dec     rd                  ; point to middle byte
           ghi     rf                  ; high byte of packed address
           shlc                        ; continue multiply by 2
           str     rd                  ; and store into hm_ptr
           dec     rd                  ; point to high byte
           ldi     0                   ; shift high bit in
           shlc
           str     rd                  ; hm_ptr is now setup
           sep     scall               ; print the string
           dw      hm_print
           lbr     idone               ; then done

; *******************************
; *** E - Handle load opcode  ***
; *******************************
op_load:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:LOAD ',0
#endif
           ldi     low arg1            ; point to arg1
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           ldn     rd                  ; get variable number
           sep     scall               ; retrieve variable contents
           dw      getvar
           lbr     res_save            ; save into destination var

; *********************************
; *** F - Handle the not opcode ***
; *********************************
op_not:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':1OP:NOT ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; place into data pointer
           lda     rd                  ; retrieve first argument
           xri     0ffh                ; flip the bits
           phi     rf                  ; into rf
           lda     rd                  ; low byte, point to arg2
           xri     0ffh                ; flip the bits
           plo     rf
           lbr     res_save            ; and save the results

; ******************************************************************
; ***                   2OP instruction handlers                 ***
; ******************************************************************
; ****************************
; *** 1 - Handle je opcode ***
; ****************************
op_je:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:JE ',0
#endif
           ldi     low num_args        ; need number of arguments
           plo     rd
           ldn     rd                  ; retrieve it
           smi     1                   ; minus 1 for first argument
           plo     re                  ; into re
           ldi     low arg1            ; need to get arg1
           plo     rd
           lda     rd                  ; retrieve it
           phi     r7                  ; into r7
           lda     rd                  ; get lsb and leave pointing to arg2
           plo     r7
op_je_lp:  glo     re                  ; see if done
           lbz     false               ; jump if so
           lda     rd                  ; get high byte of next arg
           str     r2                  ; prepare for compare
           ghi     r7                  ; first check high byte
           sm                          ; compare values
           lbnz    op_je_no            ; jump if not
           ldn     rd                  ; now check low byte
           str     r2
           glo     r7
           sm                          ; compare values
           lbz     true                ; true if a match found
op_je_no:  inc     rd                  ; point to msb of next arg
           dec     re                  ; decrement count
           lbr     op_je_lp            ; and keep checking

; ****************************
; *** 2 - Handle jl opcode ***
; ****************************
op_jl:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:JL ',0
#endif
           sep     scall               ; subtract arguments
           dw      arg_sub
           lbnf    true                ; jump if fist arg was smaller
           lbr     false               ; otherwise false

; ****************************
; *** 3 - Handle jg opcode ***
; ****************************
op_jg:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:JG ',0
#endif
           sep     scall               ; subtract argument
           dw      arg_sub
           lbnf    false               ; jump if first was smaller
           ghi     rf                  ; now must be nonzero to be true
           lbnz    true
           glo     rf                  ; check low byte
           lbnz    true
           lbr     false               ; false if they were equal

; *********************************
; *** 4 - Handle dec_chk opcode ***
; *********************************
op_decchk:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:DEC_CHK ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; set data pointer
           inc     rd                  ; get variable number
           ldn     rd                  ; get variable number
           sep     scall               ; get variable value
           dw      getvar
           dec     rf                  ; decrement value
           ldn     rd                  ; get variable number
           sep     scall               ; set variable value
           dw      setvar
           glo     rf                  ; write new value to arg1
           str     rd
           dec     rd                  ; high byte as well
           ghi     rf
           str     rd
           lbr     op_jl               ; branch if less than value

; *********************************
; *** 5 - Handle inc_chk opcode ***
; *********************************
op_incchk:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:INC_CHK ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; set data pointer
           inc     rd                  ; get variable number
           ldn     rd                  ; get variable number
           sep     scall               ; get variable value
           dw      getvar
           inc     rf                  ; increment value
           ldn     rd                  ; get variable number
           sep     scall               ; set variable value
           dw      setvar
           glo     rf                  ; write new value to arg1
           str     rd
           dec     rd                  ; high byte as well
           ghi     rf
           str     rd
           lbr     op_jg               ; branch if greater than value

; *****************************
; *** 6 - Handle jin opcode ***
; *****************************
op_jin:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:JIN ',0
#endif
           ldi     low arg1            ; need to get first object number
           plo     rd                  ; place into data pointer
           inc     rd                  ; need lsb
           ldn     rd                  ; get it
           sep     scall               ; get object address
           dw      obj_addr
           inc     rf                  ; move to parent field
           inc     rf
           inc     rf
           inc     rf
           inc     rd                  ; move data pointer to 2nd arg lsb
           inc     rd
           sep     scall               ; read parent entry
           dw      rdmem16
           sex     rd                  ; set as data position
           sm                          ; compare against second arg
           sex     r2                  ; point x back to stack
           lbz     true                ; jump if equal
           lbr     false               ; otherwise false

; ******************************
; *** 7 - Handle test opcode ***
; ******************************
op_test:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:TEST ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; place into data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd                  ; low byte, point to arg2
           plo     rf
           sex     rd                  ; use data pointer for X
           ghi     rf                  ; and with RF
           and
           phi     rf
           irx                         ; point to low byte
           glo     rf                  ; and perform and
           and
           plo     rf
           sex     r2                  ; point x back to stack
           dec     rd                  ; move back to arg1
           dec     rd
           glo     rf                  ; store anded value in arg1
           str     rd
           dec     rd
           ghi     rf
           str     rd
           lbr     op_je               ; and branch if equal

; ****************************
; *** 8 - Handle or opcode ***
; ****************************
op_or:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:OR ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; place into data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd                  ; low byte, point to arg2
           plo     rf
           sex     rd                  ; use data pointer for X
           ghi     rf                  ; or with RF
           or
           phi     rf
           irx                         ; point to low byte
           glo     rf                  ; and perform or
           or
           plo     rf
           sex     r2                  ; point x back to stack
res_save:  sep     scall               ; read result number from ip
           dw      readip
           sep     scall               ; save the result
           dw      setvar
           lbr     idone               ; done with instruction

; *****************************
; *** 9 - Handle and opcode ***
; *****************************
op_and:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:AND ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; place into data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd                  ; low byte, point to arg2
           plo     rf
           sex     rd                  ; use data pointer for X
           ghi     rf                  ; and with RF
           and
           phi     rf
           irx                         ; point to low byte
           glo     rf                  ; and perform and
           and
           plo     rf
           sex     r2                  ; point x back to stack
           lbr     res_save            ; save result

; ***********************************
; *** A - Handle test_attr opcode ***
; ***********************************
test_attr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:TEST_ATTR ',0
#endif
           ldi     low arg1            ; get object number
           plo     rd
           inc     rd
           ldn     rd
           lbz     false
           sep     scall               ; built attribute mask
           dw      bld_mask
           stxd                        ; save it
           sep     scall               ; read byte from memory
           dw      rdmem16
           irx                         ; point back to mask
           and                         ; and test bit
           lbnz    true                ; jump if bit was set
           lbr     false               ; otherwise no branch

; **********************************
; *** B - Handle set_attr opcode ***
; **********************************
set_attr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:SET_ATTR ',0
#endif
           sep     scall               ; built attribute mask
           dw      bld_mask
           stxd                        ; save it
           sep     scall               ; read byte from memory
           dw      rdmem16
           irx                         ; point back to mask
           or                          ; and set bit
           sep     scall               ; write back to memory
           dw      wrmem16
           lbr     idone               ; and return to caller

; ************************************
; *** C - Handle clear_attr opcode ***
; ************************************
clr_attr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:CLR_ATTR ',0
#endif
           sep     scall               ; built attribute mask
           dw      bld_mask
           xri     0ffh                ; flip the bits
           stxd                        ; save it
           sep     scall               ; read byte from memory
           dw      rdmem16
           irx                         ; point back to mask
           and                         ; and clear bit
           sep     scall               ; write back to memory
           dw      wrmem16
           lbr     idone               ; and return to caller

; *******************************
; *** D - Handle store opcode ***
; *******************************
op_store:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:STORE ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           lda     rd                  ; retrieve variable number
           plo     re                  ; set aside for now
           lda     rd                  ; get value
           phi     rf                  ; into rf
           ldn     rd
           plo     rf
           glo     re                  ; retrieve variable number
           sep     scall               ; store value in variable
           dw      setvar
           lbr     idone               ; then done

; ************************************
; *** E - Handle insert_obj opcode ***
; ************************************
ins_obj:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:INSERT_OBJ ',0
#endif
           sep     scall               ; remove object from wherever it is
           dw      remove
           ldi     low arg1            ; point to arg1
           plo     rd                  ; set data pointer
           inc     rd                  ; point to object number
           ldn     rd                  ; retrieve it
           stxd                        ; save it for now
           inc     rd                  ; point to arg2 lsb
           inc     rd
           ldn     rd                  ; retrieve object number
           phi     r8                  ; save parent object number
           sep     scall               ; get address for it
           dw      obj_addr
           inc     rf                  ; point to child field
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           inc     rf
           sep     scall               ; retrieve it
           dw      rdmem16
           plo     r8                  ; set it aside
           irx                         ; recover new object number
           ldx
           dec     r2                  ; keep on stack
           sep     scall               ; and write to child field
           dw      wrmem16
           irx                         ; recover new object number
           ldx
           sep     scall               ; get address for object
           dw      obj_addr
           inc     rf                  ; point to parent field
           inc     rf
           inc     rf
           inc     rf
           ghi     r8                  ; get parent object
           sep     scall               ; and write to object
           dw      wrmem16
           inc     rf                  ; point to sibling field
           glo     r8                  ; get sibling
           sep     scall               ; and write it
           dw      wrmem16
           lbr     idone               ; done

; *******************************
; *** F - Handle loadw opcode ***
; *******************************
op_loadw:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:LOADW ',0
#endif
           ldi     low arg2            ; point to arg2
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           ldn     rd                  ; get low byte
           shl                         ; multiply by 2
           plo     rf                  ; and put into rf
           dec     rd                  ; point to high byte
           ldn     rd                  ; retrieve it
           shlc                        ; continue multiply by 2
           phi     rf
           dec     rd                  ; now low byte of arg1
           sex     rd                  ; point x to argument
           glo     rf                  ; and add to index
           add
           plo     rf
           dec     rd
           ghi     rf
           adc
           phi     rf                  ; rf now has memory offset
           sex     r2                  ; ponit x back to stack
           sep     scall               ; read memory
           dw      rdmem16w
           ghi     r7                  ; transfer result
           phi     rf                  ; to rf
           glo     r7
           plo     rf
           lbr     res_save            ; save into result variable

; ********************************
; *** 10 - Handle loadb opcode ***
; ********************************
op_loadb:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:LOADB ',0
#endif
           ldi     low arg1            ; point to arg1 (array address)
           plo     rd                  ; set data pointer
           lda     rd                  ; read the address
           phi     rf                  ; into rf
           lda     rd                  ; get low byte 
           inc     rd                  ; point rd to low byte of index
           sex     rd                  ; set x to data
           add                         ; add index
           plo     rf
           ghi     rf                  ; now high bytes
           dec     rd
           adc
           phi     rf                  ; rf now has address
           sex     r2                  ; point x back to stack
           sep     scall               ; read memory
           dw      rdmem16
           plo     rf
           ldi     0
           phi     rf
           lbr     res_save            ; save into result variable
      
; ***********************************
; *** 11 - Handle get_prop opcode ***
; ***********************************
get_prop:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:GET_PROP ',0
#endif
           sep     scall               ; find property address
           dw      fnd_prop
           lbdf    use_dflt            ; jump if it was not found
           sep     scall               ; get size byte
           dw      rdmem16
           shr                         ; get only size
           shr
           shr
           shr
           shr
           lbz     prop_1              ; jump if 1 byte
           inc     rf                  ; move to actual bytes
           sep     scall               ; and read 2 of them
           dw      rdmem16w
           ghi     r7                  ; transfer to rf
           phi     rf
           glo     r7
           plo     rf
           lbr     res_save            ; and save result
prop_1:    inc     rf                  ; move to actual bytes
           sep     scall               ; read byte
           dw      rdmem16
           plo     rf                  ; place into rf
           ldi     0                   ; high byte is zero
           phi     rf
           lbr     res_save            ; save result
use_dflt:  ldi     low arg2            ; point to attribute argument
           plo     rd
           inc     rd
           ldn     rd                  ; retrieve it
           smi     1                   ; minus 1
           shl                         ; multiply by 2
           str     r2                  ; and prepare to add it
           ldi     low header          ; point to header memory
           adi     0ah                 ; point to msb of object table entry
           plo     r7                  ; place into r7
           ldi     high header         ; high byte of header address
           adci    0                   ; propagate carry
           phi     r7
           lda     r7                  ; read address
           phi     rf                  ; into rf
           ldn     r7
           add                         ; add in offset
           plo     rf                  ; and transfer to rf
           ghi     rf
           adci    0                   ; propagate carry
           phi     rf
           sep     scall               ; read value
           dw      rdmem16w
           ghi     r7                  ; transfer to rf
           phi     rf
           glo     r7
           plo     rf
           lbr     res_save            ; and save result
 
; ****************************************
; *** 12 - Handle get_prop_addr opcode ***
; ****************************************
get_paddr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:GET_PROP_ADDR ',0
#endif
           sep     scall               ; find property address
           dw      fnd_prop
           lbdf    no_prop             ; jump if property does not exist
           inc     rf                  ; move past size byte
           lbr     res_save            ; and save result
no_prop:   ldi     0                   ; signal no property
           phi     rf
           plo     rf
           lbr     res_save

; ****************************************
; *** 13 - Hanlde get_next_prop opcode ***
; ****************************************
get_nprop:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:GET_NEXT_PROP ',0
#endif
           ldi     low arg2            ; need property number
           plo     rd                  ; place into data pointer
           inc     rd                  ; need low byte
           ldn     rd                  ; get it
           lbz     nprop_z             ; jump if need first property
           sep     scall               ; find property address
           dw      fnd_prop
           lbdf    no_prop             ; jump if property does not exist
           sep     scall               ; get size byte
           dw      rdmem16
           shr                         ; want only size
           shr
           shr
           shr
           shr
           adi     1                   ; add bias
           str     r2                  ; save for add
           inc     rf                  ; move past size byte
           glo     rf                  ; add in size
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
nprop_go:  sep     scall               ; get next size/number byte
           dw      rdmem16
           ani     1fh                 ; want only property number
           plo     rf                  ; in RF
           ldi     0                   ; high byte is zero
           phi     rf
           lbr     res_save            ; save result
nprop_z:   ldi     arg1                ; need object number
           plo     rd                  ; place into data pointer
           inc     rd                  ; need lsb
           ldn     rd
           sep     scall               ; get first property address
           dw      frst_prop
           lbr     nprop_go            ; continue processing

; **********************************
; *** 14 - Handle the add opcode ***
; **********************************
op_add:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:ADD ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; place into data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd                  ; low byte, point to arg2
           plo     rf
           sex     rd                  ; use data pointer for X
           inc     rd                  ; point to lsb of arg2
           glo     rf                  ; add arg2 into rf
           add
           plo     rf
           dec     rd                  ; point to high byte
           ghi     rf
           adc
           phi     rf
           sex     r2                  ; point x back to stack
           lbr     res_save            ; and save result

; **********************************
; *** 15 - Handle the sub opcode ***
; **********************************
op_sub:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:SUB ',0
#endif
           sep     scall               ; subtract arguments
           dw      arg_sub
           lbr     res_save            ; and save result

; ******************************
; *** 16 - Handle mul opcode ***
; ******************************
op_mul:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:MUL ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; setup data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd
           plo     rf
           lda     rd                  ; 2nd argument to RD
           plo     re
           ldn     rd
           plo     rd
           glo     re
           phi     rd
           glo     rb                  ; save consumed registers
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           ghi     rc
           stxd
           sep     scall               ; multiply the numbers
           dw      f_mul16
           ghi     rb                  ; transfer answer to RF
           phi     rf
           glo     rb
           plo     rf
           irx                         ; recover consumed registers
           ldxa
           phi     rc
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldx
           plo     rb
           ldi     high data           ; reset rd
           phi     rd
           lbr     res_save            ; save result

; ******************************
; *** 17 - Handle div opcode ***
; ******************************
op_div:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:DIV ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; setup data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd
           plo     rf
           lda     rd                  ; 2nd argument to RD
           plo     re
           ldn     rd
           plo     rd
           glo     re
           phi     rd
           glo     rb                  ; save consumed registers
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           ghi     rc
           stxd
           sep     scall               ; divide the numbers
           dw      f_div16
           ghi     rb                  ; transfer answer to RF
           phi     rf
           glo     rb
           plo     rf
           irx                         ; recover consumed registers
           ldxa
           phi     rc
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldx
           plo     rb
           ldi     high data           ; reset rd
           phi     rd
           lbr     res_save            ; save result

; ******************************
; *** 18 - Handle mod opcode ***
; ******************************
op_mod:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':2OP:MOD ',0
#endif
           ldi     low arg1            ; point to first argument
           plo     rd                  ; setup data pointer
           lda     rd                  ; retrieve first argument
           phi     rf                  ; into rf
           lda     rd
           plo     rf
           sep     scall
           dw      mod_go
           lbr     res_save            ; save result

mod_go:    lda     rd                  ; 2nd argument to RD
           plo     re
           ldn     rd
           plo     rd
           glo     re
           phi     rd
           glo     rb                  ; save consumed registers
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           ghi     rc
           stxd
           sep     scall               ; divide the numbers
           dw      f_div16
           irx                         ; recover consumed registers
           ldxa
           phi     rc
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldx
           plo     rb
           ldi     high data           ; reset rd
           phi     rd
           sep     sret                ; return to caller


; ******************************************************************
; ***                   VAR instruction handlers                 ***
; ******************************************************************
; ******************************
; *** 0 - Handle call opcode ***
; ******************************
op_call:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:CALL ',0
#endif
           glo     rb                  ; save current game stack pointer
           stxd
           ghi     rb
           stxd
           glo     rc                  ; save current stack frame
           stxd
           ghi     rc
           stxd
           ghi     rb                  ; new stack frame is top of gstack
           phi     rc
           glo     rb
           plo     rc
           ldi     high hm_ptr
           phi     r7
           ldi     low hm_ptr          ; point to high memory pointer
           plo     r7
           inc     r7                  ; move to low byte
           inc     r7
           ldi     low arg1            ; need to get routine address
           plo     rd
           ldn     rd                  ; check for call to 0 or 1
           lbnz    op_call2
           inc     rd
           ldn     rd
           dec     rd
           ldi     0
           phi     rf
           plo     rf
           irx
           ldxa
           phi     rc
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldx
           plo     rb
           lbr     res_save
op_call2:
#ifdef DEBUG
           glo     rd
           plo     re
           ldi     low tab
           plo     rd
           ldn     rd
           adi     2
           str     rd
           glo     re
           plo     rd
#endif
           inc     rd                  ; point to lsb
           ldn     rd                  ; get address 
           shl                         ; multipy by 2
           str     r7                  ; store in lsb of hm_ptr
           dec     r7                  ; point to middle byte
           dec     rd                  ; msb of address
           ldn     rd                  ; retrieve it
           shlc                        ; continue multiply by 2
           str     r7                  ; store in mid of hm_ptr
           dec     r7                  ; point to high byte
           ldi     0                   ; determine msb
           shlc
           str     r7                  ; hm_ptr is now routine header
           sep     scall               ; get number of local vars
           dw      readhm
           stxd                        ; save on stack for now
lvar_lp:   irx                         ; get local var count
           ldx
           lbz     lvar_dn             ; jump if done with local args
           smi     1                   ; otherwise minus 1
           stxd                        ; and put back on stack
           sep     scall               ; read next default value
           dw      readhm
           phi     r7
           sep     scall
           dw      readhm
           sex     rb                  ; point x to game stack
           stxd                        ; store default value into local var
           ghi     r7
           stxd
           sex     r2                  ; point x back to main stack
           lbr     lvar_lp             ; loop for rest of local var values
lvar_dn:   sep     scall               ; need next byte - result var
           dw      readip
           stxd                        ; save onto stack
           ldi     low ip              ; point to ip
           plo     rd                  ; setup data pointer
           inc     rd                  ; point to lsb
           inc     rd
           ldn     rd                  ; place ip onto stack
           stxd
           dec     rd
           ldn     rd
           stxd
           dec     rd
           ldn     rd
           stxd
           ldi     low ip              ; point to ip
           plo     rd                  ; setup data pointer
           ldi     high hm_ptr
           phi     r7
           ldi     low hm_ptr          ; point to high memory pointer
           plo     r7
           lda     r7                  ; copy hm_ptr to ip
           str     rd
           inc     rd
           lda     r7                  ; copy 2nd byte
           str     rd
           inc     rd
           lda     r7                  ; copy 3rd byte
           str     rd
           inc     rd
           ghi     rc                  ; copy stack frame to r7
           phi     r7
           glo     rc
           plo     r7
           ldi     low num_args        ; need number of arguments
           plo     rd                  ; setup data pointer
           ldn     rd                  ; get number of args
           smi     1                   ; minus 1 for routine address
           plo     re                  ; save here
           ldi     low arg2            ; point to first sub arg
           plo     rd
arg_lp:    glo     re                  ; see if done
           lbz     idone               ; jump if so
           sex     r7                  ; use r7 for stack (stack frame)
           inc     rd                  ; point to lsb
           ldn     rd                  ; retrieve it
           stxd                        ; place into stack frame
           dec     rd                  ; point to high byte
           lda     rd                  ; retrieve it
           stxd                        ; write to stack frame
           sex     r2                  ; point x back to stack
           inc     rd                  ; point to next arg
           dec     re                  ; decrement arg count
           lbr     arg_lp              ; loop until all args copied

; ********************************
; *** 1 - Handle storew opcode ***
; ********************************
op_storew:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:STOREW ',0
#endif
           ldi     low arg2            ; point to arg1 (index)
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           ldn     rd                  ; get low byte
           shl                         ; multiply by 2
           plo     rf                  ; and put into rf
           dec     rd                  ; point to high byte
           ldn     rd                  ; retrieve it
           shlc                        ; continue multiply by 2
           phi     rf
           dec     rd                  ; now low byte of arg1
           sex     rd                  ; point x to argument
           glo     rf                  ; and add to index
           add
           plo     rf
           dec     rd
           ghi     rf
           adc
           phi     rf                  ; rf now has memory offset
           sex     r2                  ; ponit x back to stack
#ifdef DEBUG
           ldi     '['
           sep     scall
           dw      o_type
           sep     scall
           dw      hexword
           ldi     ']'
           sep     scall
           dw      o_type
           ldi     ' '
           sep     scall
           dw      o_type
#endif
           ldi     low arg3            ; need 3rd argument
           plo     rd
           lda     rd                  ; retrieve msb of value
           sep     scall               ; write to memory
           dw      wrmem16
           inc     rf                  ; next memory location
           lda     rd                  ; get msb of value
           sep     scall               ; and write it
           dw      wrmem16
           lbr     idone               ; done

; ********************************
; *** 2 - Handle storeb opcode ***
; ********************************
op_storeb:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:STOREB ',0
#endif
           ldi     low arg1            ; point to arg1 (array address)
           plo     rd                  ; set data pointer
           lda     rd                  ; read the address
           phi     rf                  ; into rf
           lda     rd                  ; get low byte 
           inc     rd                  ; point rd to low byte of index
           sex     rd                  ; set x to data
           add                         ; add index
           plo     rf
           ghi     rf                  ; now high bytes
           dec     rd
           adc
           phi     rf                  ; rf now has address
           sex     r2                  ; point x back to stack
           ldi     low arg3            ; need 3rd argument
           plo     rd
           inc     rd                  ; point to lsb
           ldn     rd                  ; retrieve it
           sep     scall               ; write to memory
           dw      wrmem16
           lbr     idone               ; done

; **********************************
; *** 3 - Hanlde put_prop opcode ***
; **********************************
put_prop:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:PUT_PROP ',0
#endif
           sep     scall               ; find property address
           dw      fnd_prop
           lbdf    idone               ; do nothing if not found
           sep     scall               ; get size byte
           dw      rdmem16
           inc     rf                  ; move memory pointer past size
           shr                         ; get only size
           shr
           shr
           shr
           shr
           lbz     putp1               ; jump if 1 byte
           smi     1                   ; see if 2 bytes
           lbz     putp2               ; jump if so
           lbr     idone               ; otherwise do nothing
putp1:     ldi     low arg3            ; need value of third argument
           plo     rd                  ; setup data pointer
put_lo:    inc     rd                  ; point to low byte
           ldn     rd                  ; read the byte
           sep     scall               ; write to memory
           dw      wrmem16
           lbr     idone               ; and finished
putp2:     ldi     low arg3            ; point to value argument
           plo     rd                  ; setup data pointer
           ldn     rd                  ; read high byte
           sep     scall               ; write to memory
           dw      wrmem16
           inc     rf                  ; point to next memory position
           lbr     put_lo              ; then process lo byte

; *******************************
; *** 4 - Handle sread opcode ***
; *******************************
op_sread:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:SREAD ',0
#endif
           ldi     low arg1            ; need buffer address
           plo     rd
           lda     rd                  ; get address
           phi     rf
           ldn     rd
           plo     rf
           sep     scall               ; add header offset
           dw      add_hdr
           glo     rc                  ; save consumed register
           stxd
           ghi     rc
           stxd
           sep     scall               ; get user input
           dw      o_input
           irx
           ldxa
           phi     rc
           ldx
           plo     rc
           ldi     10
           sep     scall
           dw      o_type
           ldi     13
           sep     scall
           dw      o_type
           ldi     low arg1            ; need buffer address
           plo     rd
           lda     rd                  ; get address
           phi     rf
           ldn     rd
           plo     rf
           sep     scall               ; add header offset
           dw      add_hdr
           sep     scall               ; convert input text to lowercase
           dw      tolc
           ldi     low arg1            ; need buffer address
           plo     rd
           lda     rd                  ; get address
           phi     rf
           ldn     rd
           plo     rf
           sep     scall               ; add header offset
           dw      add_hdr
           sep     scall               ; convert input text to lowercase
           dw      encode              ; encode the line
           ldi     low line_pos        ; sread forces new line
           plo     rd
           ldi     0
           str     rd
           lbr     idone               ; then done
           
; ************************************
; *** 5 - Handle print_char opcode ***
; ************************************
print_chr:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:PRINT_CHAR ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; set data pointer
           inc     rd                  ; point to low byte
           ldn     rd                  ; retrieve it
           sep     scall               ; and output it
           dw      z_type
           lbr     idone               ; done

; ***********************************
; *** 6 - Handle print_num opcode ***
; ***********************************
print_num:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:PRINT_NUM ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd                  ; set data pointer
           lda     rd                  ; retrieve value
           plo     re                  ; set here for a moment
           ldn     rd                  ; get low byte
           plo     rd                  ; need number in RD
           glo     re
           phi     rd
           ldi     high tbuffer        ; point to temp buffer
           phi     rf
           ldi     low tbuffer         ; low portion of address
           plo     rf
           sep     scall               ; call bios to convert number
           dw      f_intout
           ldi     0                   ; place a terminator
           str     rf
           ldi     high tbuffer        ; point to temp buffer
           phi     rf
           ldi     low tbuffer         ; low portion of address
           plo     rf
           ldn     rf                  ; get first byte
           lbnz    pnum_go             ; jump if not zero
           ldi     '0'                 ; put a real zero in
           str     rf
           inc     rf
           ldi     0                   ; then terminator
           str     rf
           dec     rf                  ; rf back to beginning
pnum_go:   ldn     rf                  ; see if need a space
           smi     '-'                 ; negative means no spave
           lbz     pnum_go2
           ldi     ' '                 ; output a space
           sep     scall
           dw      z_type
pnum_go2:  sep     scall               ; display the number
           dw      o_msg
           ldi     high data           ; fix rd
           phi     rd
           lbr     idone               ; done

; ********************************
; *** 7 - Handle random opcode ***
; ********************************
op_rnd:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:RANDOM ',0
#endif
           ldi     16                  ; need to get 16 bits
rnd_lp:    stxd                        ; save count
           sep     scall               ; get random bit
           dw      fn_lfsr
           glo     rf                  ; shift into result
           shlc
           plo     rf
           ghi     rf
           shlc
           phi     rf
           irx                         ; recover count
           ldx
           smi     1                   ; minus 1
           lbnz    rnd_lp              ; keep looping until all bits read
           ldi     low arg1            ; need to get range
           plo     rd                  ; setup data pointer
           sep     scall
           dw      mod_go              ; use mod to set range
           inc     rf                  ; shift up 1
           lbr     res_save            ; save result and return

; ******************************
; *** 8 - Handle push opcode ***
; ******************************
op_push:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:PUSH ',0
#endif
           ldi     low arg1            ; point to argument
           plo     rd
           inc     rd                  ; low byte first
           sex     rb                  ; set x to game stack
           ldn     rd                  ; get low of argument
           stxd                        ; store on game stack
           dec     rd                  ; point to high byte
           ldn     rd                  ; retrieve it
           stxd                        ; place onto game stack
           sex     r2                  ; point x back to main stack
           lbr     idone               ; and done

; ******************************
; *** 9 - Handle pull opcode ***
; ******************************
op_pull:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:PULL ',0
#endif
           sex     rb                  ; point x to game stack 
           irx
           ldxa                        ; retreive top value
           phi     rf                  ; into rf
           ldx
           plo     rf
           sex     r2                  ; point x back to stack
           ldi     low arg1            ; point to first argument
           plo     rd
           inc     rd                  ; need low byte
           ldn     rd                  ; get it
           sep     scall               ; save pulled value to variable
           dw      setvar
           lbr     idone               ; done

; **************************************
; *** A - Handle split_window opcode ***
; **************************************
splt_wnd:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:SPLIT_WINDOW ',0
#endif
           lbr     idone               ; ignore this one

; ************************************
; *** B - Handle set_window opcode ***
; ************************************
set_wnd:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:SET_WINDOW ',0
#endif
           lbr     idone               ; ignore this one

; ****************************************
; *** 13 - Handle output_stream opcode ***
; ****************************************
out_strm:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:OUTPUT_STREAM ',0
#endif
           lbr     idone               ; ignore this one

; ***************************************
; *** 14 - Handle input_stream opcode ***
; ***************************************
in_strm:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:INPUT_STREAM ',0
#endif
           lbr     idone               ; ignore this one

; ***************************************
; *** 15 - Handle sound_effect opcode ***
; ***************************************
sound:
#ifdef DEBUG
           sep     scall
           dw      f_inmsg
           db      ':VAR:SOUND ',0
#endif
           lbr     idone               ; ignore this one

#ifdef DEBUG
; ***********************************************************************
; ***     Debugging routines - will be removed in final version       ***
; ***********************************************************************
hexdigit:  plo     re
           smi     10
           lbnf    hexdig_go
           glo     re
           adi     7
           plo     re
hexdig_go: glo     re
           adi     30h
           sep     scall
           dw      o_type
           sep     sret

hexword:   ghi     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           ghi     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           glo     rf
           shr
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           glo     rf
           ani     0fh
           sep     scall
           dw      hexdigit
           sep     sret

hexout:    stxd                        ; save value
           shr                         ; get high nybble
           shr
           shr
           shr
           sep     scall
           dw      hexdigit
           irx
           ldx
           stxd
           ani     0fh
           sep     scall
           dw      hexdigit
           ldi     32
           sep     scall
           dw      o_type
           irx
           ldx
           sep     sret

; ***********************************************************************
#endif


alpha0:    db      'abcdefghijklmnopqrstuvwxyz'
alpha1:    db      'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
alpha2:    db      ' ',10,'0123456789.,!?_#',39,'"/\-:()'
err_1:     db      'Could not open story file'
crlf:      db      10,13,0
err_2:     db      'Wrong version, needs: ',0
endrom:    equ     $

tbuffer:   ds      32

dta:       ds      512

header:    ds      64
