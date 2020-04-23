;Insert main comment block here
;  Comment block below must be filled out completely for each assignment
;  ************************************************************* 
;  Student Name: Nathaniel Lee
;  COMSC-260 Spring 2020
;  Date: 03/01/2020
;  Assignment #4
;  Version of Visual Studio used (2019):  
;  Did program compile? Yes
;  Did program produce correct results? Yes (I did the binary addition myself)
;  Is code formatted correctly including indentation, spacing and vertical alignment? Yes
;  Is every line of code commented? Yes
;
;  Estimate of time in hours to complete assignment: 3 hours
;
;  In a few words describe the main challenge in writing this program: Simulating the Full Adder circuit
;  
;  Short description of what program does:
;  Simulating a Full Adder circuit by writing an Adder function using AND, OR, XOR instructions. 
;  While using various jup commands and the cmp instruction. 
;  *************************************************************
;  Reminder: each assignment should be the result of your
;  individual effort with no collaboration with other students.
;
;  Reminder: every line of code must be commented and formatted  
;  per the ProgramExpectations.pdf file on the class web site
; *************************************************************

.386      ;identifies minimum CPU for this program

.MODEL flat,stdcall    ;flat - protected mode program
                       ;stdcall - enables calling of MS_windows programs

;allocate memory for stack
;(default stack size for 32 bit implementation is 1MB without .STACK directive 
;  - default works for most situations)

.STACK 4096            ;allocate 4096 bytes (1000h) for stack


;*******************MACROS********************************

;mPrtStr
;usage: mPrtStr nameOfString
;ie to display a 0 terminated string named message say:
;mPrtStr message

;Macro definition of mPrtStr. Wherever mPrtStr appears in the code
;it will  be replaced with 

mPrtStr  MACRO  arg1    ;arg1 is replaced by the name of string to be displayed
         push edx
         mov edx, offset arg1    ;address of str to display should be in dx
         call WriteString        ;display 0 terminated string
         pop edx
ENDM



;*************************PROTOTYPES*****************************

ExitProcess PROTO,
    dwExitCode:DWORD    ;from Win32 api not Irvine to exit to dos with exit code


WriteDec PROTO          ;Irvine code to write number stored in eax
                        ;to console in decimal

ReadChar PROTO          ;Irvine code for getting a single char from keyboard
				        ;Character is stored in the al register.
			            ;Can be used to pause program execution until key is hit.

WriteChar PROTO         ;write the character in al to the console

WriteString PROTO		;Irvine code to write null-terminated string to output
                        ;EDX points to string

                     
;************************  Constants  ***************************

    LF         equ     0Ah                   ; ASCII Line Feed
    
;************************DATA SEGMENT***************************

.data
    inputAnum  byte 0,0,0,0,1,1,1,1       ; byte array that holds input values for adder circuit
    inputBnum  byte 0,0,1,1,0,0,1,1       ; byte array that holds input values for adder circuit 
    carryInNum byte 0,1,0,1,0,1,0,1       ; byte array that holds carry in value for adder circuit
    ARRAY_SIZE equ $-carryInNum           ; constant value that holds size of the array
    ;The '$' acts as a place maker where you are currently in memory
    ;which at the end of the carryInNum array.
    ;The ending address of the carryInNum array minus the beginning
    ;address equals the total bytes of the carryInNum array
    ;which is stored in the ARRAY_SIZE constant.
    ;NOTE: there can be no other variables between the 
    ;declation of the ARRAY_SIZE constant and the declaration
    ;of the array you are trying to calculate the size of.

    ;You can add LFs to the strings below for proper output line spacing
    ;but do not change anything between the quotes "do not change".

    ;I will be using a comparison program to compare your output to mine and
    ;the spacing must match exactly.


    ;Change my name to your name
    titleMsg            byte "Program 4 by Nathaniel Lee",LF,0  ; messsage to be shown at the beginning of the program

    testingAdderMsg     byte " Testing Adder",0                 ; message that allows adder being worked

    dashes              byte LF, " ------------",LF,0           ; dashes to organize output

    inputA              byte LF,"   Input A: ",0                ; inpput A label
    inputB              byte LF,"   Input B: ",0                ; input B label
    carryin             byte LF,"  Carry in: ",0                ; carry in label

    sum                 byte "       Sum: ",0                   ; sum label
    carryout            byte LF," Carry Out: ",0                ; carry out label


    endingMsg           byte LF,"Hit any key to exit!",0        ; end message

;************************CODE SEGMENT****************************

.code

main PROC

;write code for main function here. See the program specifications
;pdf on the class web site for more info.
    mPrtStr titleMsg               ; using MACRO to display title message string
    mPrtStr dashes                 ; using MACRO to display dashes string variable
    mPrtStr testingAdderMsg        ; using MACRO to display testing adder message string
    mPrtStr dashes                 ; using MACRO to display dashes string variable
    mov     esi, 0                 ; esi = 0

loopTop:                           ; beginning of while loop
   
    cmp     esi, ARRAY_SIZE        ; esi - ARRAY_SIZE (8)
    jae     done                   ; if esi is greater than or equal to 8 then jump to done label
   
    mPrtStr inputA                 ; using MACRO to dislay input A message
    movzx   eax, inputAnum[esi]    ; eax = element at esi in array inputAnum 
    call    WriteDec               ; call WriteDec to display decimal value in eax
    movzx   ebx, inputBnum[esi]    ; ebx = element at esi in array inputBnum
    movzx   ecx, carryInnum[esi]   ; ecx = element at esi in array carryInnum
    call    Adder                  ; call Adder function
    mov     ebp, eax               ; ebp = eax (eax = sum from adder function)
    
    mPrtStr inputB                 ; using MACRO to display input B message
    movzx   eax, inputBnum[esi]    ; eax = element at esi in array inputBnum
    call    WriteDec               ; call WriteDec to display decimal value
    mPrtStr carryin                ; using MACRO to display carryin message
    movzx   eax, carryInnum[esi]   ; eax = element at esi in array carryInnum
    call    WriteDec               ; call WriteDec to display decimal value

    
    mPrtStr dashes                 ; using MACRO to display dashed string variable
    mPrtStr sum                    ; using MACRO to display sum message

    mov     eax, ebp               ; eax = ebp (ebp = eax (sum from adder function))
    call    WriteDec               ; call WriteDec to display decimal value
    mPrtStr carryOut               ; using MACRO to display carryOut message string
    mov     eax, ecx               ; eax = ecx (carryOut from adder function)
    call    WriteDec               ; call WriteDec to dsiplay decimal value
    mov     al,  LF                ; al = line feed variable
    call    WriteChar              ; call WriteChar to print line feed
  
    inc     esi                    ; esi = esi + 1
    jmp     loopTop                ; unconditional jump to beginning of while loop

done:                              ; done label for after esi is greater or equal to ARRAY_SIZE

    mPrtStr endingMsg              ; using MACRO to display ending message string
    call    ReadChar               ; calling ReadChar to pause program after execution

main ENDP


;************** Adder – Simulate a full Adder circuit  
;  Adder will simulate a full Adder circuit that will add together 
;  3 input bits and output a sum bit and a carry bit
;
;    Each input and output represents one bit.
;
;  Note: do not access the arrays in main directly in the Adder function. 
;        The data must be passed into this function via the required registers below.
;
;       ENTRY - EAX = input bit A 
;               EBX = input bit B
;               ECX = Cin (carry in bit)
;       EXIT  - EAX = sum bit
;               ECX = carry out bit
;       REGS  -  (list registers you use)
;
;       For the inputs in the input columns you should get the 
;       outputs in the output columns below:
;
;        input                  output
;     eax  ebx   ecx   =      eax     ecx
;      A  + B +  Cin   =      Sum     Cout
;      0  + 0 +   0    =       0        0
;      0  + 0 +   1    =       1        0
;      0  + 1 +   0    =       1        0
;      0  + 1 +   1    =       0        1
;      1  + 0 +   0    =       1        0
;      1  + 0 +   1    =       0        1
;      1  + 1 +   0    =       0        1
;      1  + 1 +   1    =       1        1
;
;   Note: the Adder function does not do any output. 
;         All the output is done in the main function.
;
;Do not change the name of the Adder function.
;
;See additional specifications for the Adder function on the 
;class web site.
;
;You should use AND, OR and XOR to simulate the full adder circuit.
;
;You should save any registers whose values change in this function 
;using push and restore them with pop.
;
;The saving of the registers should
;be done at the top of the function and the restoring should be done at
;the bottom of the function.
;
;Note: do not save any registers that return a value (ecx and eax).
;
;Each line of the Adder function must be commented and you must use the 
;usual indentation and formating like in the main function. SP20
;
;Don't forget the "ret" instruction at the end of the function
;
;Do not delete this comment block. FA19 Every function should have 
;a comment block before it describing the function. 


Adder proc            ; beginning of Adder function

;Write code for the "Adder" procedure here. 

    push edi          ; save contents of edi onto stack
    push ebp          ; save contents of ebp onto stack

    mov edi, eax      ; edi = eax for first eax value
    xor eax, ebx      ; eax = xor eax, ebx (first column only gate)
    mov ebp, eax      ; ebp = edi first column only gate
    xor eax, ecx      ; eax = eax or ecx (sum)
    

    and ecx, ebp      ; ecx = second column middle and
    and edi, ebx      ; edi = second column last end
    or  ecx, edi      ; ecx = ecx or edi (carry out)

    pop ebp           ; move original ebp contents back into ebp
    pop edi           ; move original edi contents back into edi

    ret               ; ret pops return address off stack

Adder endp            ; end of Adder function

END main