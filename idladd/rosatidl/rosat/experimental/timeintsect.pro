;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   timeintsect
;*PURPOSE:
; A procedure to find the time intervals which are the intersections
; between one interval (tmin,tmax) and intervals defined by the vectors
; (actbeg,actend)
;
;*CALLING SEQUENCE:
;   timeintsect,tmin,tmax,actbeg,actend,newtbeg,newtend,nact
;
;*PARAMETERS:
; INPUTS:
;    tmin    - Scalar giving the lower bound of the single interval
;    tmax    - Scalar giving the upper bound of the single interval
;    actbeg  - Vector of lower bounds of the 2nd interval (or set of
;              intervals)
;    actend  - Vector of upper bounds of the 2nd interval (or set of
;              intervals)
;
; OUTPUTS:
;    newtbeg - Lower bounds of the intersections of the intervals
;    newtend - Upper bounds of the intersections of the intervals
;    nact  - Number of interval intersections
;
;*RESTRICTIONS:
;    TMIN and TMAX must be single valued. ACTBEG and ACTEND may be
;    scalars or vectors (but must have the same number of elements).
;
;*NOTES:
;    If no intersections are found, then zeroes are returned.
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    written 12 Aug 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro timeintsect,tmin,tmax,actbeg,actend,newtbeg,newtend,nact
;
if (n_params(0) eq 0) then begin
  print,' timeintsect, tmin, tmax, actbeg, actend, NEWTBEG, NEWTEND, NACT'
  retall
endif
;
nbtimes = n_elements(actbeg)
netimes = n_elements(actend)
if (nbtimes ne netimes) then begin
  print,' Time intervals ACTBEG & ACTEND are of different sizes.'
  print,' Check inputs. Returning.'
  return
endif
if (nbtimes eq 0) then begin
  print,' Time intervals ACTBEG & ACTEND are not defined.'
  print,' Check inputs. Returning.'
  return
endif
;
check = 1
if ((tmin eq 0) and (tmax eq 0)) then begin     ;defaults for both min & max
  newtbeg = actbeg
  newtend = actend
  nact = nbtimes
endif else begin
  if (tmin eq 0) then tmin = min(actbeg)
  if (tmax eq 0) then tmax = max(actend)
;
  itry = where( (actbeg ge tmin) and (actbeg le tmax),nbeg) 
  if (nbeg gt 0) then newtbeg = actbeg(itry) 
  itry = where( (actend ge tmin) and (actend le tmax),nend)
  if (nend gt 0) then newtend = actend(itry) 
;
  case 1 of
    (nbeg gt nend): $
      if (nend gt 0) then newtend = [newtend,tmax] else newtend = tmax
    (nend gt nbeg): $
      if (nbeg gt 0) then newtbeg = [tmin,newtbeg] else newtbeg = tmin
    ((nbeg eq nend) and (nbeg ne 0)): begin
      if (newtbeg(0) gt newtend(0)) then begin
        newtbeg = [tmin,newtbeg]
        newtend = [newtend,tmax]
      endif
    end
    ((nbeg eq 0) and (nend eq 0)): begin
      jtry = where( (tmin ge actbeg) and (tmin le actend),nbeg2)
      jtry = where( (tmax ge actbeg) and (tmax le actend),nend2)    
      if ( (nbeg2 eq 0) and (nend2 eq 0) ) then begin     ;no intersection
        newtbeg = 0
        newtend = 0
        check = 0
      endif else begin
        newtbeg = tmin
        newtend = tmax
      endelse
    end
  endcase
;
  nact = check*n_elements(newtbeg)
endelse
;
return
end            ;pro timeintsect
