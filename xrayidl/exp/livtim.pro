
;
;
;	PROGRAM : LIVTIM
;	AUTHOR  : G.R. HASSINGER
;	DATE    : APRIL, 1992
;
;	** THIS PROGRAM IS AN IDL TRANSLATION OF THE ORIGINAL VAX FORTRAN
;	PROGRAM CREATED BY G.R. HASSINGER OF MPE. **
;	
;		- jeffrey a. mendenhall
;
;	Calculates PSPC livetime factor from Count Rates and Deadtime Param.
;
;
; 	The PSPC livetime factor, a value between 0 and 1 which has to be
; 	multiplied to the exposure time to obtain the effective live 
; 	exposure time, is calculated from a product of two values:
;
; 	FLIVE1 using the input A1-lower-level-discriminator count rate 
; 	(A1LL) [cts/s] and the deadtime factor (DEADTP) [musec] according 
; 	to the recipe in the TN-ROS-ME-ZA00-025. The deadtime parameter
; 	DEADTP, which is actually a function of mean energy and PSPC, 
; 	should be specified from outside as a parameter.
;
; 	FLIVE2 from the ratio between the accepted and evaluated X-ray 
; 	event rate (AEXE) and the accepted X-ray event rate (AXE). A 
; 	difference between those two indicates loss of events in the
; 	telemetry stream because of a busy readout.
;
; 	A1LL                  EE-A1LL count rate from HK-data [cts/s]
; 	AXE                   EE-AXE  count rate from HK-data [cts/s]
; 	AEXE                  EE-AEXE  count rate from HK-data [cts/s]
; 	DEADTP                Deadtime Parameter (ca. 190-260 [musec])
; 	FLIVE1                PSPC Livetime Factor (between 0 and 1)
; 	FLIVE2                Livetime Factor (between 0 and 1)
; 	FLIVE                 Ttotal Livetime Factor (between 0 and 1)
; 	DONE                  O = 0 no error 
;                            	= 1 negative square root ARG (FLIVE1=1)
;                               = 1 denominator = 0 (FLIVE2=1)
;                               = 3 


PRO    	LIVTIM,A1LL,DEADTP,AXE,AEXE,FLIVE1,FLIVE2,FLIVE

; 	Initializations

        DONE = 0

;	Calculate PSPC livetime FLIVE1

        ARG = 2.0E-6 * A1LL * DEADTP            

; 	Check for error condition

        IF((ARG LT 0) OR (ARG GT 1)) THEN BEGIN
            FLIVE1 = 1.
        end else FLIVE1=SQRT(1.0-ARG) 

;	Calculate livetime FLIVE2

        IF(AXE EQ 0) THEN begin
            FLIVE2 = 1.
        end else FLIVE2=AEXE/AXE

;	Multiply the two values
 
        FLIVE = FLIVE1 * FLIVE2

        RETURN
        END     
