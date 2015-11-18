;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       tsi_bitcodes
;
;*PURPOSE:
; A procedure to return the TSI (temporal status interval) bit codes
; given in the TSI table of the .fits file
;
;*CALLING SEQUENCE:
;	tsi_bitcodes,instr,fail_codes,fail_vals,stat_codes,stat_badvals$
;                   ,telq_codes,telq_badvals
;
;*PARAMETERS:
; INPUTS:
;        INSTR         Instrument ('RP' for Rosat PSPC = default,
;                                  'RH' for Rosat HRI)
; OUTPUTS:
;        FAIL_CODES    String array giving ASCII translation of bit-encoded
;                      failed flags.
;        FAIL_VALS     String array giving on values of failed flags
;        STAT_CODES    String array giving ASCII translation of bit-encoded 
;                      status settings. 
;        STAT_BADVALS  Values of status flag which correspond to "bad" data
;                      (i.e., data which would not be included for standard
;                      processing)
;        TELQ_CODES    String array giving ASCII translation of bit-encoded
;                      telemetry quality status flags
;        TELQ_BADVALS  Values of telemetry quality status flag which 
;                      correspond to "bad" data
;
;*EXAMPLES: 
;       tsi_bitcodes,'',fail_codes,fail_vals,stat_codes,stat_badvals
;  (assumes instr = default = 'RP')
;       tsi_bitcodes,'h',fail_codes,fail_vals,stat_codes,stat_badvals
;  (this should work)
;       tsi_bitcodes,'RH',fail_codes
;  (so should this)
;
;*RESTRICTIONS:
;       Only works for Rosat PSPC or HRI data (so far)
;
;*SUBROUTINES CALLED: none
;
;*MODIFICATION HISTORY:
;    written 25 Nov 1991 by GAR
;    modified 27 Nov 1991 (GAR) to return values for failed flags
;-
;-------------------------------------------------------------------------------
pro tsi_bitcodes,instr,fail_codes,fail_vals,stat_codes,stat_badvals,$
telq_codes,telq_badvals
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' tsi_bitcodes, instr (def = "RP"), FAIL_CODES, FAIL_VALS, '$
       +'STAT_CODES, STAT_BADVALS, TELQ_CODES, TELQ_BADVALS'
  retall
endif
;
fail_codes = ''
fail_vals = ''
stat_codes = ''
stat_badvals = ''
telq_codes = ''
telq_badvals = ''
;
instr = strupcase(instr)
if (instr eq '') then instr='RP'
if (strlen(instr) eq 1) then instr = 'R'+instr
if ((instr ne 'RP') and (instr ne 'RH')) then begin
  print,'Only Rosat PSPC ("RP") or Rosat HRI ("RH") are supported. Returning.'
  retall
endif
;
if (instr eq 'RP') then begin             ;set codes for bit flags
  fail_codes = ['High Voltage','Carousel','Gas Systems','PSPC Temperature'$
               ,'Low Voltage','Current','Aspect Quality','Calibration Quality'$
               ,'PSPC Temperature','Pressure','High Voltage','Filter Wheel'$
               ,'Missing HK data','Detector','Running Mean Bkg (RMB)'$
               ,'Diff: IRM - BKG. (DFB)'$
               ,'Inter-OBI Gap','EOF']
  stat_codes = ['High Voltage','Carousel','Gas Systems','PSPC Temperature'$
               ,'Low Voltage','Current','Aspect Quality','Calibration'$
               ,'PSPC Temperature','Pressure','High Voltage'$
               ,'Filter Wheel Position Problem'$
               ,'Missing HK data','Status of Detector']
  stat_badvals = ['1','1','1','1','1','1','1','0','1','1','1','1','1','1']
  telq_codes = replicate('',n_elements(stat_codes))
endif 
if (instr eq 'RH') then begin
  fail_codes = ['End of data','Interval between OBIs','High Background'$
               ,'High Voltage Level','Viewing Geometry','Aspect Status'$
               ,'Aspect Error','Bad Telemetry','High Voltage Off'$
               ,'Data Dropout','Bad HRI Telemetry','Bad Telemetry Time'$
               ,'SAA Detector Index Bad','SAA Detector A','SAA Detector B'$
               ,'Temperature Sensor 1','Temperature Sensor 2'$
               ,'Temperature Sensor 3','UV Calibration Lamp On'$
               ,'HV Reduced for SAA','User_excluded']
  stat_codes = ['End of Data','Interval between OBIs','High Voltage'$
               ,'Data Dropout','Bad HRI Telemetry','Bad Telemetry Time'$
               ,'UV Calibration Lamp On','High Voltage Reduced in SAA']
  stat_badvals = ['1','1','0','1','1','1','1','1']
  telq_codes = [' ',' ',' '$
               ,'Following Frame shows Decrement in Spacecraft Clock'$
               ,' ','Non-incrementing Spacecraft Clock'$
               ,'Decrementing Spacecraft Clock','Bad Sync Word'$
               ,'Bad Validity Bit in Primary Science Packet'$
               ,'Bad Bits in Secondary Science Spare Bits']
endif
fail_vals = replicate('1',n_elements(fail_codes))     ;all flags set by '1'
telq_badvals = replicate('1',n_elements(telq_codes))
;
return
end        ;pro tsi_bitcodes
