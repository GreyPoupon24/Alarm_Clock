;Timer 1 ISR Generates a 2kHz square wave at pin P1.1 using


$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
SECONDS 	  EQU 0
BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P1.1
UPDOWN        equ P0.0
;use button p0.5 to incriment numbers when setting time
INCRIMENT_BUTTON equ P0.5
;use button at p0.1 to go to mode where time is set
SET_TIME_MODE equ P0.1
TIMER_TOGGLE equ P0.3 ;to toggle between setting timer fields and acivating timer
; Reset vector
org 0x0000
    ljmp main



; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	


; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
seconds_field:  ds 1 ; The seconds counter is incrememted in the ISR every second and it represents seconds


minutes_field: ds 1 ;for minutes
hours_field: ds 1 ;for hours

minutes_field_alarm: ds 1 ;keeping track of when to set off the alarm 
hours_field_alarm: ds 1

;timer fields to tick down and ring at 0
seconds_field_timer: ds 1
minutes_field_timer: ds 1
hours_field_timer: ds 1



; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
seconds_flag: dbit 1 ;set to one in the ISR every time 1000ms pass
AMPM_flag: dbit 1 ;set to 1 for PM and 0 for AM
AMPM_flag_alarm: dbit 1 ;choosing am/pm for the alarm
speaker_on_flag: dbit 1 ;flag to tell us whether or not to have the speaker, will be activated in alarm ringing loop
timer_on_flag: dbit 1;flag to tell the ISR to decrement seconds_field_timer every second


cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'TIME ', 0
set_message: db 'SET  ',0
alarm_string: db 'ALARM',0
ampm_string: db 'AM/PM',0
off: db 'off',0
on: db 'on ',0
clear: db '                ',0
wake_up: db 'WAKEUP!!WAKEUP!!',0
timer: db 'TIMER',0
timer_dome: db 'TIMER!!! DONE!!!'



;---------------------------------;
; Time display subroutines		  ;
;---------------------------------;

;updating minutes based on hours, hours on minutes, AM/PM on hours and resetting values that hit the upper limit

update_time:


;making seconds reset at 60 and incrimenting minutes
MOV R4,seconds_field
CJNE R4,#0x60,NOT_SIXTY_SECONDS
;if 60 seconds passed, reset seconds and incriment minutes

;don't forget to decimal adjust
MOV a,minutes_field
ADD a,#0x01
da a
MOV minutes_field,a
clr a
MOV seconds_field,#0x00
;skip incrimenting minutes and resetting seconds
NOT_SIXTY_SECONDS:



;making minutes reset at 60 and incrimenting hours
MOV R4,minutes_field
CJNE R4,#0x60,NOT_SIXTY_MINUTES
;if 60 seconds passed, reset minutes and incriment hours

;don't forget to decimal adjust
MOV a,hours_field
ADD a,#0x01
da a
MOV hours_field,a
clr a
MOV minutes_field,#0x00
;skip incrimenting hours and resetting minutes
NOT_SIXTY_MINUTES:

;making hours reset at 13 and swapping AMPM_flag
MOV R4,hours_field
CJNE R4,#0x13,NOT_THIRTEEN_HOURS
MOV R4,#0x01
MOV hours_field,R4
MOV R4,AMPM_flag
cjne R4,#0b0, SWITCH_TO_AM
;JB AMPM_flag,SWITCH_TO_AM
mov AMPM_flag,#0b1
;setb AMPM_flag
sjmp DONT_SWITCH_TO_AM 
SWITCH_TO_AM:
MOV AMPM_flag,#0b0
;clr AMPM_flag
DONT_SWITCH_TO_AM:
NOT_THIRTEEN_HOURS:

ret


display_time:
	;display seconds
	Set_Cursor(1, 12)     ; the place in the LCD where we want the seconds value
	Display_BCD(seconds_field) ; This macro is also in 'LCD_4bit.inc'
	;display minutes
	Set_Cursor(1,9)
	Display_BCD(minutes_field)
	;display hours 
	Set_Cursor(1,6)
	Display_BCD(hours_field)
	;picking between AM and PM
	;write the M
	Set_Cursor(1,16)
	Display_char(#'M')
	;write the A or P
	Set_Cursor(1,15)
	MOV R4,AMPM_flag
	cjne R4,#0b0,DISPLAY_PM
	;jb AMPM_flag,DISPLAY_PM
	;display AM
	Display_char(#'A')
	sjmp DONT_DISPLAY_PM
	DISPLAY_PM:	
	Display_char(#'P')
	DONT_DISPLAY_PM:
	
	



ret

;subroutine to display the time for the alarm
display_alarm_time:

	Set_Cursor(2,6)
	Display_BCD(hours_field_alarm)
	Set_Cursor(2,9)
	Display_BCD(minutes_field_alarm)
	;display colon between hours and minutes 
	Set_Cursor(2,8)
	Display_char(#':')
	;display am/pm for alarm
	;write the M for alarm
	Set_Cursor(2,16)
	Display_char(#'M')
	;write the A or P for alarm
	Set_Cursor(2,15)	
	MOV R4,AMPM_flag_alarm
	cjne R4,#0b0,DISPLAY_PM_ALARM
	;jb AMPM_flag,DISPLAY_PM
	;display AM
	Display_char(#'A')
	sjmp DONT_DISPLAY_PM_ALARM
	DISPLAY_PM_ALARM:	
	Display_char(#'P')
	DONT_DISPLAY_PM_ALARM:



ret






;---------------------------------;
; Timer display subroutines		  ;
;---------------------------------;

display_timer_time:
Set_cursor(2,8)
Display_BCD(hours_field_timer)
Set_cursor(2,10)
Display_char(#':')
Set_cursor(2,11)
Display_BCD(minutes_field_timer)
Set_cursor(2,13)
Display_char(#':')
Set_cursor(2,14)
Display_BCD(seconds_field_timer)




ret

update_timer:

;making seconds reset at -1 (99) and decrimenting minutes
MOV R4,seconds_field_timer
CJNE R4,#0x99,NOT_99

;if seconds go to zero, reset seconds and decriment minutes

;don't forget to decimal adjust
MOV a,minutes_field_timer
ADD a,#0x99  
da a
MOV minutes_field_timer,a
clr a
MOV seconds_field_timer,#0x59
;skip decrimenting minutes and resetting seconds
NOT_99:


;making minutes reset at -1 (99) and decrimenting hours
MOV R4,minutes_field_timer
CJNE R4,#0x99,NOT_99_BOOGALOO
;if minutes roll over to 99, reset minutes and decriment hours

;don't forget to decimal adjust
MOV a,hours_field_timer
ADD a,#0x99
da a
MOV hours_field_timer,a
clr a
MOV minutes_field_timer,#0x59
;skip incrimenting hours and resetting minutes
NOT_99_BOOGALOO:



ret




;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    
    setb TR0  ; Start timer 0
    clr TR0 ;timer 0 should be off by default
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P1.1!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2

	
	
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if a full second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know one full second had passed
	setb AMPM_flag ;to keep track of AM or PM time, 1 is PM 0 is AM
	
	
	;only want to enable timer 0 toggling if the alarm is supposed to be ringing 
	jnb speaker_on_flag,dont_toggle_speaker
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	dont_toggle_speaker:
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
		
	
	
	;Decrement timer counter if timer enable flag is on
	jnb timer_on_flag,dont_decrement_seconds
	mov a,seconds_field_timer
	add a,#0x99
	da a
	mov seconds_field_timer,a
	clr a
	dont_decrement_seconds:
	
	; Increment the seconds field counter
	mov a, seconds_field
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov seconds_field, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
















;---------------------------------;
; Routines for setting time       ;
;---------------------------------;

;---------------------------------;
; Set hours					      ;
;---------------------------------;

;setting hours on the time
SET_TIME_HOURS:
keep_setting_hours:

;Let the user know they are setting hours
Set_cursor(1,1)
Send_Constant_String(#set_message)
Set_cursor(1,4)
Display_char(#'h')
Set_cursor(1,5)
Display_char(#'r')



;make sure to display the time
lcall update_time
lcall display_time




;Look for button press to switch to setting minutes
jb SET_TIME_MODE,dont_switch_to_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,dont_switch_to_minutes
	;wait for button release
	adenosine: jnb SET_TIME_MODE, adenosine
	ljmp SET_TIME_MINUTES
	dont_switch_to_minutes:




;*****READ INPUTS TO INCRIMENT AND DECREMENT HOURS*****

jb INCRIMENT_BUTTON,do_not_incriment_hours
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_hours
	;wait for button release
	hyoscyamine: jnb INCRIMENT_BUTTON, hyoscyamine
	;button was pressed, time to incriment
	MOV a,hours_field
	ADD a,#0x01
	da a
	MOV hours_field,a
	clr a
	;reset to 1 if it hits 13, skip if hours_field is not 13
	MOV R4,hours_field
	CJNE R4,#0x13,no_thirteen_here
	MOV hours_field,#0x01
	no_thirteen_here:
	do_not_incriment_hours:





ljmp keep_setting_hours
ret


endless_loop:
endless_loop_part_two:

sjmp endless_loop_part_two

ret



;---------------------------------;
; Set minutes				      ;
;---------------------------------;


;setting minutes on the time
SET_TIME_MINUTES:
keep_setting_minutes:

;let the user know they are setting minutes
Set_cursor(1,4)
Display_char(#'m')
Set_cursor(1,5)
Display_char(#'i')

;display the time
lcall update_time
lcall display_time


;*****READ INPUTS TO INCRIMENT AND DECREMENT MINUTES*****



jb INCRIMENT_BUTTON,do_not_incriment_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_minutes
	;wait for button release
	diphenhydramine: jnb INCRIMENT_BUTTON, diphenhydramine
	;button was pressed, time to incriment
	MOV a,minutes_field
	ADD a,#0x01
	da a
	MOV minutes_field,a
	clr a
	;reset to 0 if it hits 60, skip if minutes_field is not 60
	MOV R4,minutes_field
	CJNE R4,#0x60,no_sixty_minutes_here
	MOV minutes_field,#0x00
	no_sixty_minutes_here:
	do_not_incriment_minutes:







;look for button press to transition to set am/pm mode
jb SET_TIME_MODE,keep_setting_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,keep_setting_minutes
	;wait for button release
	atropine: jnb SET_TIME_MODE, atropine
	ljmp SET_TIME_AMPM



sjmp keep_setting_minutes
ret



;DIDN'T USE THIS SUBROUTINE BECAUSE WHAT KIND OF CLOCK LETS YOU SET SECONDS LOL

;setting seconds on the time
SET_TIME_SECONDS:
keep_setting_seconds:

;let user know they are setting seconds

Set_cursor(1,4)
Display_char(#'s')
Set_cursor(1,5)
Display_char(#'e')

;*****READ INPUTS TO INCRIMENT AND DECREMENT SECONDS*****


;look for button press to set am/pm
jb SET_TIME_MODE,keep_setting_seconds
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,keep_setting_seconds
	;wait for button release
	scopolamine: jnb SET_TIME_MODE, scopolamine
	ljmp SET_TIME_AMPM

sjmp keep_setting_seconds
ret


;---------------------------------;
; AM/PM of time				      ;
;---------------------------------;


SET_TIME_AMPM:
keep_setting_ampm:

;let user know they are setting AM/PM
Set_Cursor(1,1)
Send_Constant_String(#ampm_string)

;show the time
lcall update_time
lcall display_time


;LOOK FOR BUTTON PRESS TO CHANGE AM TO PM AND PM TO AM
jb INCRIMENT_BUTTON,dont_change_ampm
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,dont_change_ampm
	;wait for button release
	dimenhydrinate: jnb INCRIMENT_BUTTON, dimenhydrinate
	;CHANGE AM/PM FLAG HERE
	MOV R4,AMPM_flag
	cjne R4,#0b0, SWITCH_TO_AM_BOOGALOO
	mov AMPM_flag,#0b1
	sjmp DONT_SWITCH_TO_AM_BOOGALOO 
	SWITCH_TO_AM_BOOGALOO:
	MOV AMPM_flag,#0b0
	DONT_SWITCH_TO_AM_BOOGALOO:
	dont_change_ampm:


;look for button press to set alarm time
jb SET_TIME_MODE,keep_setting_ampm
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,keep_setting_ampm
	;wait for button release
	muscarine: jnb SET_TIME_MODE, muscarine
	ljmp SET_ALARM_HOURS


sjmp keep_setting_ampm
ret



;---------------------------------;
; Routines for setting alarm time ;
;---------------------------------;



;---------------------------------;
; Set alarm hours				  ;
;---------------------------------;

;setting hours on the time
SET_ALARM_HOURS:
keep_setting_alarm_hours:

;Let the user know they are setting hours
Set_cursor(2,1)
Send_Constant_String(#set_message)
Set_cursor(2,4)
Display_char(#'h')
Set_cursor(2,5)
Display_char(#'r')
;let the user know they are not setting normal time so overwrite 'AM/PM'
Set_cursor(1,1)
Send_Constant_String(#Initial_message)


;make sure to display the time and alarm time
lcall update_time
lcall display_time
lcall display_alarm_time



;Look for button press to switch to setting alarm minutes
jb SET_TIME_MODE,dont_switch_to_alarm_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,dont_switch_to_alarm_minutes
	;wait for button release
	benztropine: jnb SET_TIME_MODE, benztropine
	ljmp SET_ALARM_MINUTES
	dont_switch_to_alarm_minutes:




;*****READ INPUTS TO INCRIMENT AND DECREMENT ALARM HOURS*****

jb INCRIMENT_BUTTON,do_not_incriment_alarm_hours
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_alarm_hours
	;wait for button release
    promethazine: jnb INCRIMENT_BUTTON, promethazine
	;button was pressed, time to incriment
	MOV a,hours_field_alarm
	ADD a,#0x01
	da a
	MOV hours_field_alarm,a
	clr a
	;reset to 1 if it hits 13, skip if hours_field is not 13
	MOV R4,hours_field_alarm
	CJNE R4,#0x13,no_thirteen_here_alarm
	MOV hours_field_alarm,#0x01
	no_thirteen_here_alarm:
	do_not_incriment_alarm_hours:





ljmp keep_setting_alarm_hours
ret

;---------------------------------;
; Set alarm minutes				  ;
;---------------------------------;

SET_ALARM_MINUTES:

keep_setting_alarm_minutes:

;let the user know they are setting minutes
Set_cursor(2,4)
Display_char(#'m')
Set_cursor(2,5)
Display_char(#'i')

;display the time
lcall update_time
lcall display_time
lcall display_alarm_time

;*****READ INPUTS TO INCRIMENT AND DECREMENT MINUTES*****



jb INCRIMENT_BUTTON,do_not_incriment_alarm_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_alarm_minutes
	;wait for button release
	biperiden: jnb INCRIMENT_BUTTON, biperiden
	;button was pressed, time to incriment
	MOV a,minutes_field_alarm
	ADD a,#0x01
	da a
	MOV minutes_field_alarm,a
	clr a
	;reset to 0 if it hits 60, skip if minutes_field is not 60
	MOV R4,minutes_field_alarm
	CJNE R4,#0x60,no_sixty_minutes_here_boogaloo
	MOV minutes_field_alarm,#0x00
	no_sixty_minutes_here_boogaloo:
	do_not_incriment_alarm_minutes:







;look for button press to transition to set am/pm mode
jb SET_TIME_MODE,keep_setting_alarm_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,keep_setting_alarm_minutes
	;wait for button release
	chlorpheniramine: jnb SET_TIME_MODE, chlorpheniramine
	ljmp SET_ALARM_TIME_AMPM



sjmp keep_setting_alarm_minutes
ret

ret


;---------------------------------;
; AM/PM of alarm			      ;
;---------------------------------;

SET_ALARM_TIME_AMPM:

keep_setting_alarm_ampm:

;let user know they are setting AM/PM
Set_Cursor(2,1)
Send_Constant_String(#ampm_string)

;show the time
lcall update_time
lcall display_time
lcall display_alarm_time

;LOOK FOR BUTTON PRESS TO CHANGE AM TO PM AND PM TO AM
jb INCRIMENT_BUTTON,dont_change_alarm_ampm
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,dont_change_alarm_ampm
	;wait for button release
	doxepin: jnb INCRIMENT_BUTTON, doxepin
	;CHANGE ALARM AM/PM FLAG HERE
	MOV R4,AMPM_flag_alarm
	cjne R4,#0b0, SWITCH_TO_AM_ALARM
	mov AMPM_flag_alarm,#0b1
	sjmp DONT_SWITCH_TO_AM_ALARM
	SWITCH_TO_AM_ALARM:
	MOV AMPM_flag_alarm,#0b0
	DONT_SWITCH_TO_AM_ALARM:
	dont_change_alarm_ampm:


;look for button press to return to main loop 
jb SET_TIME_MODE,dont_return_to_loop
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,dont_return_to_loop
	;wait for button release
	dicyclomine: jnb SET_TIME_MODE, dicyclomine
	ljmp loop

dont_return_to_loop:

sjmp keep_setting_alarm_ampm

ret



;---------------------------------;
; Alarm active loop				  ;
;---------------------------------;


alarm_active:
;gotta display the time while the alarm is active
lcall update_time
lcall display_time


Set_cursor(2,11)
Send_Constant_String(#on)


	;TOGGLE ALARM ON/OFF INPUT READING
	;alarm off sends us back to main loop
	jb INCRIMENT_BUTTON,DONT_TOGGLE_ALARM_BOOGALOO
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,DONT_TOGGLE_ALARM_BOOGALOO
	;wait for button release
	flavoxate: jnb INCRIMENT_BUTTON, flavoxate
	ljmp loop
	DONT_TOGGLE_ALARM_BOOGALOO:
	
	
	;compare hours minutes and AM/PM to see if alarm should ring
	clr c
	MOV a,minutes_field
	subb a,minutes_field_alarm
	jnz dont_ring
	clr c
	MOV a,hours_field
	subb a,hours_field_alarm
	jnz dont_ring
	clr c
	MOV a,AMPM_flag
	subb a,AMPM_flag_alarm
	jnz dont_ring
	;now make it ring
	ljmp alarm_ringing

	dont_ring:


ljmp alarm_active
ret


;---------------------------------;
; Alarm ringing loop			  ;
;---------------------------------;


alarm_ringing:
setb speaker_on_flag ;activate the speaker beeping
lcall update_time
lcall display_time
Set_cursor(2,1)
Send_constant_string(#wake_up)




;looking for alarm toggle button to turn ringing off
	jb INCRIMENT_BUTTON,DONT_TOGGLE_ALARM_BOOGALOO2
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,DONT_TOGGLE_ALARM_BOOGALOO2
	;wait for button release
	glycopyrrolate: jnb INCRIMENT_BUTTON, glycopyrrolate
	ljmp loop
	DONT_TOGGLE_ALARM_BOOGALOO2:



ljmp alarm_ringing
ret






;---------------------------------;
; Timer routines        		  ;
;---------------------------------;

SET_TIMER_HOURS:
;keep displaying normal time
lcall update_time
lcall display_time


;display the initial timer values
lcall display_timer_time
Set_cursor(2,1)
Send_constant_string(#TIMER)
;let user know they are setting hours
Set_cursor(2,6)
Display_char(#'h')
Set_cursor(2,7)
Display_char(#'r')
;clear the m from am/pm
Set_cursor(2,16)
Display_char(#' ')
;clear the first colon
Set_cursor(2,8)
Display_char(#' ')
;display first colon
Set_cursor(2,10)
Display_char(#':')
;display second colon
Set_cursor(2,13)
Display_char(#':')


;CHECKING FOR INCRIMENT BUTTON TO INCRIMENT TIMER HOURS
jb INCRIMENT_BUTTON,do_not_incriment_timer_hours
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_timer_hours
	;wait for button release
    orphenadrine: jnb INCRIMENT_BUTTON, orphenadrine
	;button was pressed, time to incriment
	MOV a,hours_field_timer
	ADD a,#0x01
	da a
	MOV hours_field_timer,a
	clr a
	do_not_incriment_timer_hours:

	;LOOKING FOR TIMER TOGGLE TO SWITCH TO SETTING MINUTES
	jb TIMER_TOGGLE,DONT_SET_TIMER_MINUTES
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb TIMER_TOGGLE,DONT_SET_TIMER_MINUTES
	;wait for button release
    oxitropium: jnb TIMER_TOGGLE, oxitropium
	ljmp SET_TIMER_MINUTES
	DONT_SET_TIMER_MINUTES:


ljmp SET_TIMER_HOURS
ret


;---------------------------------;
; Set timer minutes        		  ;
;---------------------------------;


SET_TIMER_MINUTES:
lcall update_time
lcall display_time
lcall display_timer_time
;let user know they are updating minutes
Set_cursor(2,6)
Display_char(#'m')
Set_cursor(2,7)
Display_char(#'i')

;CHECKING FOR INCRIMENT BUTTON TO INCRIMENT TIMER MINUTES
jb INCRIMENT_BUTTON,do_not_incriment_timer_minutes
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_timer_minutes
	;wait for button release
    oxybutynin: jnb INCRIMENT_BUTTON, oxybutynin
	;button was pressed, time to incriment
	MOV a,minutes_field_timer
	ADD a,#0x01
	da a
	MOV minutes_field_timer,a
	clr a
	do_not_incriment_timer_minutes:

	;LOOKING FOR TIMER TOGGLE TO SWITCH TO SETTING SECONDS
	jb TIMER_TOGGLE,DONT_SET_TIMER_SECONDS
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb TIMER_TOGGLE,DONT_SET_TIMER_SECONDS
	;wait for button release
    solifenacin: jnb TIMER_TOGGLE, solifenacin
	ljmp SET_TIMER_SECONDS
	DONT_SET_TIMER_SECONDS:

ljmp SET_TIMER_MINUTES
ret

;---------------------------------;
; Set timer seconds        		  ;
;---------------------------------;
SET_TIMER_SECONDS:
lcall update_time
lcall display_time
lcall display_timer_time
;let user know they are updating seconds
Set_cursor(2,6)
Display_char(#'s')
Set_cursor(2,7)
Display_char(#'e')




;CHECKING FOR INCRIMENT BUTTON TO INCRIMENT TIMER SECONDS
jb INCRIMENT_BUTTON,do_not_incriment_timer_seconds
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,do_not_incriment_timer_seconds
	;wait for button release
    tolterodine: jnb INCRIMENT_BUTTON, tolterodine
	;button was pressed, time to incriment
	MOV a,seconds_field_timer
	ADD a,#0x01
	da a
	MOV seconds_field_timer,a
	clr a
	do_not_incriment_timer_seconds:

	;LOOKING FOR TIMER TOGGLE TO SWITCH TO TIMER TICKING
	jb TIMER_TOGGLE,DONT_SET_TIMER_TICKING
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb TIMER_TOGGLE,DONT_SET_TIMER_TICKING
	;wait for button release
    tiotropium: jnb TIMER_TOGGLE, tiotropium
	ljmp TIMER_TICKING
	DONT_SET_TIMER_TICKING:


ljmp SET_TIMER_SECONDS
ret


;---------------------------------;
; Timer ticking         		  ;
;---------------------------------;



TIMER_TICKING:

lcall update_time
lcall display_time
lcall display_timer_time
;clear the hr mi or se from setting the timer
Set_cursor(2,6)
Display_char(#' ')
Set_cursor(2,7)
Display_char(#' ')

setb timer_on_flag ;make the Timer 2 isr decrement seconds on the timer
lcall update_timer
lcall display_timer_time


	;LOOKING FOR TIMER TOGGLE TO SWITCH TO MAIN LOOP
	jb TIMER_TOGGLE,DONT_LOOP_JUMP_BOOGALOO
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb TIMER_TOGGLE,DONT_LOOP_JUMP_BOOGALOO
	;wait for button release
    tropicamide: jnb TIMER_TOGGLE,tropicamide
	ljmp loop
	DONT_LOOP_JUMP_BOOGALOO:
	
;make timer ring and jump to timer done when it hits zero
MOV R4,seconds_field_timer
CJNE R4,#0x00,timer_not_done
MOV R4,minutes_field_timer
CJNE R4,#0x00,timer_not_done
MOV R4,hours_field_timer
CJNE R4,#0x00,timer_not_done
;timer is done if it got this far
ljmp TIMER_DONE
timer_not_done:


ljmp TIMER_TICKING
ret



;---------------------------------;
; Timer done            		  ;
;---------------------------------;


TIMER_DONE:
lcall update_time
lcall display_time
Set_cursor(2,1)
send_constant_string(#timer_dome)
setb speaker_on_flag ;speaker will sound when timer is done

	;LOOKING FOR TIMER TOGGLE TO SWITCH TO MAIN LOOP
	jb TIMER_TOGGLE,DONT_LOOP_JUMP
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb TIMER_TOGGLE,DONT_LOOP_JUMP
	;wait for button release
    trihexyphenidyl: jnb TIMER_TOGGLE,trihexyphenidyl
	ljmp loop
	DONT_LOOP_JUMP:

ljmp TIMER_DONE
ret


;ALARM IS OFF IN THE MAIN LOOP

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init

    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    setb SET_TIME_MODE ;P0.1 to read input to transition into time set mode, first reading hours
    setb INCRIMENT_BUTTON ;P0.5 to read input to incriment time unit when setting time
    setb TIMER_TOGGLE ;P0.3 to toggle between setting the fields of timer and enabling it
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    setb half_seconds_flag
	mov seconds_field, #0x00

	clr speaker_on_flag ;speaker is disabled to start
	clr a

	mov minutes_field,a
	mov minutes_field_alarm,a			;seconds hours and minutes must be initialized
	mov seconds_field_timer,#0x00
	mov hours_field_timer,#0x00
	mov minutes_field_timer,#0x00
	
	mov a,#0x01
	mov hours_field,a ;hours must start with 1
	mov hours_field_alarm,a
	clr a
	setb seconds_flag ;initialize full seconds flag
	mov AMPM_flag,#0b0
	mov AMPM_flag_alarm,#0b1

	
	
	
	; After initialization the program stays in this 'forever' loop
loop:
	clr speaker_on_flag ;to make sure the timer 2 isr is not toggling the speaker on and off when alarm should not be ringing
	clr TR0;alarm should be off if not in alarm ringing mode
	clr timer_on_flag ;timer should be off unless we are in the timer running loop
	
	;TOGGLE ALARM ON/OFF INPUT READING
	jb INCRIMENT_BUTTON,DONT_TOGGLE_ALARM
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb INCRIMENT_BUTTON,DONT_TOGGLE_ALARM
	;wait for button release
	doxylamine: jnb INCRIMENT_BUTTON, doxylamine
	ljmp alarm_active
	DONT_TOGGLE_ALARM:
	
	
	;TOGGLE TIMER INPUT READING
	jb TIMER_TOGGLE,DONT_TOGGLE_TIMER
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb TIMER_TOGGLE,DONT_TOGGLE_TIMER
	;wait for button release
	ipratropium: jnb TIMER_TOGGLE, ipratropium
	ljmp SET_TIMER_HOURS
	DONT_TOGGLE_TIMER:
	
	
	
	
	;SET TIME MODE INPUT READING
	jb SET_TIME_MODE,DONT_SET_TIME_MODE
	;debounce delay
	Wait_Milli_Seconds(#50)
	jb SET_TIME_MODE,DONT_SET_TIME_MODE
	;wait for button release
	acetocholine: jnb SET_TIME_MODE, acetocholine
	ljmp SET_TIME_HOURS
	DONT_SET_TIME_MODE:



	;BOOT INPUT READING
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the seconds_field.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the seconds field
	mov seconds_field, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
loop_a:
	jnb seconds_flag, loop
	
	
	
loop_b:

;to reset initial message after setting time
set_cursor(1,1)
Send_Constant_String(#Initial_Message)
set_cursor(1,8)
Display_char(#':')
set_cursor(1,11)
display_char(#':')

;update hours minutes and seconds if they overflow above 12 60 and 60 respectivley and update AM/PM
lcall update_time


clr seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2

		
;TIME DISPLAYS SUBROUTINE
lcall display_time

	
	
	
	
	
	
	;ALARM DISPLAYS
	;display alarm text
	Set_Cursor(2,1)
	Send_Constant_String(#alarm_string)
	
	;say that alarm is off
	Set_Cursor(2,11)
	Send_Constant_String(#off)
	
	;empty extra space in column 14
	Set_Cursor(2,14)
	display_char(#' ')
	


	;DISPLAY ALARM TIMES
	lcall display_alarm_time
	

	
	
	
    ljmp loop




END
