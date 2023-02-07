.nolist
#include "ti83plus.inc"
.list
.org    $9D93
.db    t2ByteTok, tAsmCmp

;===============================================================================
;Intro, short description of what the user has to do
;===============================================================================
       b_call(_RunIndicOff)     ;We don't need the loading line
       b_call(_ClrLCDFull)      ;Clear the screen
       ld hl, 0                 ;Empty HL
       ld (CURROW), hl          ;Row zero
       ld hl, text              ;Load up text
       b_call(_PutS)            ;Write to screen
       b_call(_NewLine)         ;Write new line on screen
       ld hl, in_txt
       b_call(_PutS)            ;Write to screen
       b_call(_NewLine)         ;Write new line on screen

       /*
       Now we will provide the input for the conversion
       */
       jr hex_calc         ;Below was a test I left in, so skip it

;===============================================================================
;Testing how OP1 works... don't bother, it gets skipped
;===============================================================================
op1_test:
       b_call(_getKey)     ;Get input from user
       ld h,0              ;Zeroing HL
       ld l,0              ;
       ld (OP1),a          ;Loading our getKey data into OP1
       ld a, 0             ;HL is a 16-bit register, so we
       ld (OP1 + $01), a   ;have to zero the 8-bits next to OP1
       ld hl, (OP1)        ;Load value of OP1 into HL
       b_call(_DispHL)     ;Write to screen (ENTER should be 5)

;===============================================================================
;User input of 4(?) HEX digits, and Storing the HEX value in memory
;===============================================================================
       ;I would like to clean this up so that the user can input however many
       ;digits they want BEtWEEN 1 and 4, and execute the conversion using ENTER
hex_calc:  
       ld b,4                 ;Only want 4 digits for our hex number

digit_loop:                   ;Here is our loop for inputting our hex number
       push bc                ;Save our counter
            b_call(_getKey)        ;Get input from user, destroy counter
       pop bc                 ;Recover our counter

       cp $98                  ;If key code is $98 or $99 then loop back
       jr z, digit_loop        ;
       cp $99                  ;
       jr z, digit_loop        ;

       cp kCapF + 1           ;If the key code is bigger than F Key's location ($9F)
       jr nc, digit_loop      ;   OR smaller than 0 key ($8E), then loop back
       cp k0                  ;
       jr c, digit_loop       ;

       cp kCapA               ;If we have a letter, subtract the letter value
       jr nc, letter_sub      ;

       cp k9 + 1              ;If we have a number, subtract the number value
       jr c, number_sub       ;

letter_sub:
       ld c, $90              ;Loading $90 for proper letter to number value
       jr store               ;Subctracting $90 will give us the HEX value we want
number_sub:
       ld c, $8E              ;Loading $8E for proper number value
       jr store               ;Subctracting $8E will give us the HEX value we want
store:
       push af                ;We want A again in a moment
            sub c                 ;subtract to get the actual value for storage
            ld hl,hex_to_conv      ;HL is set to be hex_to_conv
            ld d,a                ;Putting A in D for safe keeping

            ld a, 2               ;If B is less than 2, use next byte in memory
            cp b                  ;
            jr nc, skip_shift     ;
            ld hl, hex_conv_two   ;

       skip_shift:
            ld a,(hl)    ;Putting the value we currently have for conversion to A
            rlca                  ;Shifting so we can have both nibbles in the same byte
            rlca
            rlca
            rlca
            or d                  ;Writing the most recent input to A without destroying the previous
            ld (hl), a   ;Writing A to our data storage
       pop af                 ;Here is where we want A again

       ;Displaying key inputs as numbers
       ld d,0                 ;Zeroing DE
       ld e,0                 ;
       ld e,a                 ;Loading our getKey data into de
       push bc                ;Saving B from the clutches of _KeyToString
            b_call(_KeyToString)   ;Turn our key input into string
            b_call(_PutPSB)        ;Display as string
       pop bc                 ;B has been resurrected
       djnz digit_loop        ;Decrease counter, jump back to top
       b_call(_NewLine)       ;Throw in a new line for some space

;===============================================================================
;Input to HEX value conversion
;===============================================================================

       ;This is where the math and storage of the HEX to DEC transformation will go
       ;Save number in a register? Might have to have a spot for it in mem
       ;The way that this conversion works is: 
       ;      We have a 4 digit HEX number: X1,X2,X3,X4. We need to do:
       ;      X1 * 16^3 + X2 * 16^2 + X3 * 16^1 + X4 * 16^0 = DEC

;The way this is written now, it's just taking our inputs and turning them into the actual
;      value that the user input as a 4 digit number. This probably could have been done
;      during the input phase, but we'll keep it for now since it works instead of 
;      re-writing the whole thing***

       jr conversion                 ;Skip over our multiplication "algorithm"

hex_mult:
       add hl, hl                    ;We are multiplying
       djnz hex_mult                 ;
       ret

conversion:
;Start with the most significant digit
       ld a, (hex_conv_two)        ;We are about to convert hex to dec, these are the most significant digits
       rlca                        ;Rotating so we ONLY get the first HEX digit for our multiplicatoin
       rlca                        ;
       rlca                        ;
       rlca                        ;
       ld c, hex_dec_shift         ;Remove the 2nd HEX digit      
       and c                       ;
       ld b, 12                    ;12 doublings is 16^3
       ld h, 0
       ld l, a
       call hex_mult               ;Go through our first round of multiplication (X1 * 16^3)

       ld de, (hex_whole_value)    ;We need to add these digits together
       add hl, de                  ;
       ld (hex_whole_value), hl    ;Put the multiplied number into hex_whole_value

;Now we need to add on the second digit
       ld a, (hex_conv_two)        ;We are about to convert hex to dec, these are the most significant digits
                                   ;No rotation this time, since we want the second digit
       ld c, hex_dec_shift         ;Remove the 1st HEX digit      
       and c                       ;
       ld b, 8                     ;8 doublings is 16^2
       ld h, 0
       ld l, a
       call hex_mult               ;Go through our second round of multiplication (X2 * 16^2)

       ld de, (hex_whole_value)    ;We need to add these digits together
       add hl, de                  ;
       ld (hex_whole_value), hl    ;Put the multiplied number into hex_whole_value

;Now we need to add the 3rd digit
       ld a, (hex_to_conv)         ;We are about to convert hex to dec, these are the most significant digits
       rlca                        ;Rotating so we ONLY get the first HEX digit for our multiplicatoin
       rlca                        ;
       rlca                        ;
       rlca                        ;
       ld c, hex_dec_shift         ;Remove the 4th HEX digit      
       and c                       ;
       ld b, 4                     ;4 doublings is 16
       ld h, 0
       ld l, a
       call hex_mult               ;Go through our first round of multiplication (X3 * 16)

       ld de, (hex_whole_value)    ;We need to add these digits together
       add hl, de                  ;
       ld (hex_whole_value), hl    ;Put the multiplied number into hex_whole_value

;Now we need to add the 4th digit
       ld a, (hex_to_conv)         ;We are about to convert hex to dec, these are the most significant digits
                                   ;Don't need our rotation
       ld c, hex_dec_shift         ;Remove the 3rd HEX digit      
       and c                       ;
                                   ;Don't need any doublings, so no setting up B
       ld h, 0
       ld l, a

       ld de, (hex_whole_value)    ;We need to add these digits together
       add hl, de                  ;
       ld (hex_whole_value), hl    ;Put the multiplied number into hex_whole_value

;***So now we have the HEX value stored at hex_whole_value

;===============================================================================
;HEX to DEC conversion
;===============================================================================

;All we need to do is add six to the 

       ;Testing what we have in there
       ld hl, text_conv
       b_call(_PutS)
       b_call(_NewLine)
       ld hl, (hex_whole_value)
       b_call(_DispHL)
       b_call(_NewLine)
       


;===============================================================================
;Count number of digits
;===============================================================================

       ;Count digits and write to EXPONENT part of FP

;===============================================================================
;Write digit pairs as HEX for the FP to display properly
;===============================================================================

       ;ie DEC 15 = $15, or else calc will display 0F

;===============================================================================
;Writing the DEC to the screen
;===============================================================================

        ;This is where the display feature will go (once it works) 
       ;      I think I need to make a floating point number first

       ;Here is the 2 byte conversion. It only converts 2 bytes, 1 at a time, individually
       ;b_call(_NewLine)
       ;ld hl, (hex_to_conv)
       ;b_call(_DispHL)
       ;ld hl, (hex_conv_two)
       ;b_call(_DispHL)
       ;b_call(_NewLine)

;===============================================================================
;Notes & tests for conversion, data manipulation, stringification etc
;===============================================================================

       ;The following is test code for getting a number in storage to display on screen
       ;      as a floating point number. We will need to do a HEX to DEC conversion,
       ;      find out how many DEC digits we have, write that in our FP, then 
       ;      write the DEC value to the FP as well

       ld hl, hex_whole_value   ;set HL to be the location of our raw HEX (Little endian)
       ld de, dec_fp   ;putting our FP pointer into DE  
       inc de                ;Not touching SIGN
       inc de                ;Not touching EXPONENT
       push de               ; Saving DE
              b_call(_Mov8B)       ;Sending 8-bytes from HL to DE
       pop de                ;Restoring DE
       inc de                ;Writing the next digit

       ld hl, hex_whole_value    ;Putting the next digit into HL to send to DE
       inc hl
       b_call(_Mov8B)        ;SEND IT

       ld hl, dec_fp  ;Set HL to point to the FP 
       b_call(_Mov9ToOp1)   ;Move the data to OP1, so that...
       ld a, 5              ;Max number of characters to display
       B_CALL(_FormReal)    ;...FormReal can convert the data to a string at...
       ld hl,OP3            ;...OP3, the location of our string
       b_call(_PutS)        ; display string



       ret                  ;Terminate program

;===============================================================================
;Data 
;===============================================================================

;Storage for the first two digits in HEX
hex_to_conv:         
       .db $00, $00

;Storage for the second two digits in HEX
hex_conv_two:
       .db $00, $00

;The number we use to isolate a single digit with AND
hex_dec_shift .equ  %00001111
      
;The location where our HEX input will be turned into an actual VALUE
hex_whole_value:
       .db $00, $00, $00

;Location where the DEC representation IN HEX will be for our FP to showcase
dec_conv:
       .db $00, $00, $00

;hex_to_conv will be used to make a floating point variable
dec_fp:
       .db $00       ;The first byte says the FP will be positive
       
       .db $83       ;The second byte says the FP will be ____*10^6

       ;The following seven bytes will be the digits of the FP
       .db $00, $00, $00, $00, $00, $00, $00, $00  

;The intro text for the program
text:
       .db "Hex to Dec!", 0
in_txt:
       .db "Input Hex value:", 0

;Text for when we have calculated a DEC value
text_conv:
       .db "Decimal value:", 0

;===============================================================================
;TODO
;===============================================================================

       ;Start program off with a selection of (HEX --> DEC) or (DEC --> HEX)
       ;DEC to HEX conversion
       ;Use FP to display number instead of DispHL
       ;Allow for ENTER to determine number of digits instead of requiring 4

.end
.end