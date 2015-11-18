;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   limit_group
;
;*PURPOSE:
; A procedure to limit the groups in a channel grouping vector to those
; groups entirely contained within specified pi channel (or energy) limits
;
;*CALLING SEQUENCE:
;   limit_group,group,ngroup,pilim=pilim,elim=elim
;
;*PARAMETERS:
; INPUTS:
;   GROUP    2d array giving beginning and ending pi channel boundaries
;
; OPTIONAL INPUTS:
;   PILIM    2 element vector giving [lower, upper] values of pi channel
;            bounds
;   ELIM     2 element vector giving [lower, upper] values of energy limits
;            (in kev)
;            Note: either PILIM or ELIM must be defined (but not both)
;
; OUTPUTS:
;   NGROUP   2d array giving beginning and ending pi channel boundaries
;            of groups which fall entirely within the specified range
;
;*EXAMPLES: 
;        IDL> limit_group,!group,ngroup,pilim=[55,198]
;
;*RESTRICTIONS:
;  Either PILIM or ELIM must be defined, but NOT both
;
;*NOTES:
; This procedure will truncate the groups to channels which entirely
; fit within the given range. To make the channel boundaries match the
; specified range exactly, it may be necessary to redefine the input groups
;
;*SUBROUTINES CALLED:
; CHAN2ENERGY
;
;*MODIFICATION HISTORY:
;    written 18 Nov 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro limit_group,group,ngroup,pilim=pilim,elim=elim
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' LIMIT_GROUP,group,ngroup,pilim=pilim,elim=elim'
  retall
endif
;
if ( (n_elements(pilim) eq 0) and (n_elements(elim) eq 0) ) then begin
  print,' You must enter either a PI channel range (PILIM) or '
  print,'                       an energy range (ELIM) in kev '
  print,' (but not both)'
  print,' Please check your inputs. Returning.'
  retall
endif
if ( (n_elements(pilim) ne 0) and (n_elements(elim) ne 0) ) then begin
  print,' Either PI limits or energy limits may be set, not both.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
if (n_elements(pilim) ne 0) then begin
  pimin = pilim(0)
  pimax = pilim(1)
  igood = where((group(*,0) ge pimin) and (group(*,1) le pimax),ngood)
  if (ngood le 0) then begin
    print,' No channel groups within the specified limits. Returning.'
    retall
  endif
  pibot = min(group(*,0)) > pimin
  pitop = max(group(*,1)) < pimax
  ibot = where(group(*,0) ge pibot,nbot)
  ibot = ibot(0)
  itop = where(group(*,1) le pitop,ntop)
  itop = max(itop)
  nnew = itop - ibot + 1 
  ngroup = intarr(nnew,2)
  ngroup(0,0) = group(ibot:itop,0)
  ngroup(0,1) = group(ibot:itop,1)
endif
;
if (n_elements(elim) ne 0) then begin
  emin = elim(0)
  emax = elim(1)
  chan2energy,ecen,edel,group=group
  ebot = ecen - edel/2.
  etop = ecen + edel/2.
  igood = where( (ebot ge 0.07) and (etop le 2.0),ngood)
  if (ngood le 0) then begin
    print,' No energies between 0.07 and 2.0 keV specified. Returning.'
    retall
  endif
;
  ibot = where(ebot ge emin,nbot)
  ibot = ibot(0)
  itop = where(etop le emax,ntop)
  itop = max(itop)
  nnew = itop - ibot + 1
  ngroup = intarr(nnew,2)
  ngroup(0,0) = group(ibot:itop,0)
  ngroup(0,1) = group(ibot:itop,1)
endif
;
return
end          ;pro limit_group
