;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;           rspsfprof
;*PURPOSE:
; A procedure to create a 1d radial profile of the PSF for specific angular
; bins (for the appropriate spectrum, if PSPC)
;
;*CALLING SEQUENCE:
;   rspsfprof,instr,offcen,radp,psf,rate=rate,group=group,ecen=ecen
;
;*PARAMETERS:
; INPUTS:
;   INSTR    Instrument (default is P for PSPC)
;   OFFCEN   Off axis angle for center of source (arcmin)
;   RADP     The central radii of the angular bins given for the profile
;            (in arcsec)
;
; OPTIONAL INPUTS:
;   RATE     Spectrum to be used for PSF calculation. If not defined, will
;            be extracted over region defined by SREGION
;   GROUP    2d array giving beginning and ending pi channel boundaries
;            (default = boundaries given by !group)
;   ECEN     Central energies (in keV) for each element in RATE
;            Note: both GROUP and ECEN may not be entered
;
; OUTPUTS:
;   PSF      The radially averaged point spread function
;   OPARMS   String array which is the parsed form of inputs (same as
;            LIST in example 3 below). Allows program to be rerun using 
;            same values w/o resetting.
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  CHAN2ENERGY
;  RSPSFP
;  RSPSFH
;
;*MODIFICATION HISTORY:
;    written  18 Nov 1993 (GAR)
;    modified 03 Mar 1994 (GAR) to be compatible with new version of RSPSFH
;      which includes off-axis parameterization.
;-
;-------------------------------------------------------------------------------
pro rspsfprof,instr,offcen,radp,psf,rate=rate,group=group,ecen=ecen
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSPSFPROF,instr,offcen,radp,psf,rate=rate,group=group,ecen=ecen'
  print,'     '
  print,'    Keywords RATE and GROUP *or* ECEN must be defined'
  retall
endif
;
instruc = strupcase(instr)
if ((instruc ne 'H') and (instruc ne 'P')) then begin
  print,' Instrument ',instr,' not allowed. Must be H or P.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
if (instruc eq 'P') then begin
  if (n_elements(rate) eq 0) then begin       
    print,' The input spectrum is not defined. '
    print,' Please enter an input spectral file, or use the keywords' $
         ,' GROUP or ECEN, and RATE'
    print,' Returning.'
    retall
  endif
;
  if ( (n_elements(ecen) ne 0) and (n_elements(group) ne 0) ) then begin
    print,' You may not use both ECEN *and* GROUP. Please use one or' $
         ,' the other.'
    print,' Returning.'
  endif
;
  if (n_elements(ecen) eq 0) then begin   ;calculate channel central energies 
    if (n_elements(group) eq 0) then group = !group    ;use !group as default
    chan2energy,ecen,edel,group=group   
  endif 
  nchan = n_elements(ecen)
endif
;
; Compute expected PSF. First, com
;
psf = radp*0.
case instruc of
   'P': begin
        for ii=0,nchan-1 do begin
          rspsfp,offcen,ecen(ii),radp,psfi,ierr=ierr
          psf = psf + rate(ii)*psfi
        endfor
        psf = psf/total(rate)
        end
   'H': rspsfh,offcen,radp,psf,ierr=ierr
endcase
;  
return
end            ;rspsfprof
