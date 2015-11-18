;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;				rsplotevr
;
;*PURPOSE:
; A procedure to plot time histories of event rates
; User may expand the horizontal scale and make hard copies
;
;*CALLING SEQUENCE:
;       rsplotevr, sct, rate1, rate2, rate3, rate4, rate5, rtits, pltitle
;
;*PARAMETERS:
; INPUTS:
;	sct - Spacecraft time (sec since launch?)
;       rate1,..5 - Event rates 1,..5
;       xtit - title for time axis
;       rtits - titles for rates
;       pltitle - plot title
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;
;*EXAMPLES:
;
;*RESTRICTIONS:
;	Routine attempts to display graphics on terminal.
;*NOTES:
;  PRINTPS should be set to the print command:
;    under VMS, type, e.g. PRINTPS :== print/que=ps/notify/delete
;          (PRINTPS is a system symbol.)
;    under Unix, add this command (or a similar one) to your .cshrc file
;          setenv PRINTPS "lpr -r -s"
;          (PRINTPS is an environmental variable.)
;
;*MODIFICATION HISTORY:
;    written 16 May 1991 by GAR
;    modified to use STACKPLOT 16 July 1991 by GAR
;    modified 18 Nov 1991 to use PSPRINT (GAR)
;    modified 04 Feb 1992 to change PSPRINT to PRINTPS (GAR)
;-
;-------------------------------------------------------------------------------
pro rsplotevr,sct,rate1,rate2,rate3,rate4,rate5,xtit,rtits,pltitle
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'RSPLOTEVR, sct, rate1, rate2, rate3, rate4, rate5'
  print,'          [, xtit, ytits, pltitle]'
  retall
endif
if (npar lt 8) then pltitle = ''
if (npar lt 7) then ytits = ['Rate 1','Rate 2','Rate 3','Rate 4','Rate 5']
if (npar lt 6) then xtit = 'Spacecraft time (sec)'
;
ptitle = !p.title
xtitle = !x.title
oxmin = !xmin
oxmax = !xmax
devname = !d.name
;
indpl = where( (rate1 gt 0) and (rate2 gt 0) and (rate3 gt 0) $
  and (rate4 gt 0)  and (rate5 gt 0) )
set_xy,sct(min(indpl)),sct(max(indpl))
!x.title = xtit
!p.title = pltitle
stackplot,sct,rate1,rate2,rate3,rate4,rate5,yname=rtits
;
; allow user to expand the time scale, if desired
; also allow user to make more than one plot for one obi
;
pltanother = 1
;
while (pltanother eq 1) do begin       
  ans = ''
  read,' Change the horizontal scale? (Y or N) ',ans
  yesno,ans
  while (ans eq 1) do begin
    print,' Use cursor to mark left and right endpoints of region to plot'
    cursor,xpos,ypos,1
    nxmin = xpos
    cursor,xpos,ypos,1
    nxmax = xpos
    if (nxmin gt nxmax) then begin
      nxmax = nxmin
      nxmin = xpos
    endif
    set_xy,nxmin,nxmax
    stackplot,sct,rate1,rate2,rate3,rate4,rate5,yname=rtits
    ans = 0
  endwhile
;
; now allow user to make a hardcopy, if desired
;
  ans = ''
  read,' Make a hardcopy of this plot (Y or N)?  ',ans
  yesno,ans
  if (ans eq 1) then begin
    ans = ''
    read,' Portrait (P) or Landscape (L) mode? (default = P) ',ans
    ans = strupcase(ans)
    set_plot,'ps'
    if (ans eq 'L') then device,/landscape else $
      device,/portrait,/inches,xoffset=1.0,xsize=7.0,yoffset=0.5,ysize=9.5
    stackplot,sct,rate1,rate2,rate3,rate4,rate5,yname=rtits
    device,/close
    set_plot,devname
    spawn,'PRINTPS idl.ps'
  endif
;
; allow user to make another plot of a section from this obi, if desired
;
  pltanother = ''
  read,' Plot another part (or all) of this obi? (Y or N) ',pltanother
  yesno,pltanother
  if (pltanother eq 1) then begin
    ans = ''
    read,' Replot the entire obi? (Y or N) ',ans
    yesno,ans
    if (ans eq 1) then begin
      set_xy,sct(min(indpl)),sct(max(indpl))
      stackplot,sct,rate1,rate2,rate3,rate4,rate5,yname=rtits
    endif
  endif
;
endwhile
;
set_xy,oxmin,oxmax
!p.title = ptitle
!x.title = xtitle
;
return      ;pro rsplotevr
end
