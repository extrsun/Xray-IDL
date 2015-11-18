;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   tintsect2
;*PURPOSE:
; A procedure to find the time intervals which are the intersections
; between two sets of time intervals, defined by the vectors
; (tbeg,tend) and (actbeg,actend)
;
;*CALLING SEQUENCE:
;   tintsect2,tbeg,tend,actbeg,actend,newtbeg,newtend,nact
;
;*PARAMETERS:
; INPUTS:
;    tbeg  -   Vector of lower bounds of the 1st interval (or set of
;              intervals)
;    tend  -   Vector of upper bounds of the 1st interval (or set of
;              intervals)
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
;
;*NOTES:
;    If no intersections are found, then zeroes are returned.
;
;*SUBROUTINES CALLED:
;    TIMEINTSECT
;
;*MODIFICATION HISTORY:
;    written 12 Aug 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro tintsect2,tbeg,tend,actbeg,actend,newtbeg,newtend,nact
;
if (n_params(0) eq 0) then begin
  print,' tintsect2, tbeg, tend, actbeg, actend, NEWTBEG, NEWTEND, NACT'
  retall
endif
;
nbtim = n_elements(tbeg)
netim = n_elements(tend)
if (nbtim ne netim) then begin
  print,' Time intervals TBEG & TEND are of different sizes.'
  print,' Check inputs. Returning.'
  return
endif
if (nbtim eq 0) then begin
  print,' Time intervals TBEG & TEND are not defined.'
  print,' Check inputs. Returning.'
  return
endif
;
nbact = n_elements(actbeg)
neact = n_elements(actend)
if (nbact ne neact) then begin
  print,' Time intervals ACTBEG & ACTEND are of different sizes.'
  print,' Check inputs. Returning.'
  return
endif
if (nbact eq 0) then begin
  print,' Time intervals ACTBEG & ACTEND are not defined.'
  print,' Check inputs. Returning.'
  return
endif
;
newtbeg = [-999.D0] 
newtend = newtbeg
nact = 0
for i=0,nbtim-1 do begin
  tmin = tbeg(i) 
  tmax = tend(i)
  timeintsect,tmin,tmax,actbeg,actend,ntbeg,ntend,nt
  if (nt gt 0) then begin
    newtbeg = [newtbeg,ntbeg] 
    newtend = [newtend,ntend]
    nact = nact + nt
  endif
endfor
;
if (nact gt 0) then begin
  newtbeg = newtbeg(1:*)
  newtend = newtend(1:*)
endif else begin
  newtbeg = 0
  newtend = 0
endelse
;
return
end            ;pro tintsect2
