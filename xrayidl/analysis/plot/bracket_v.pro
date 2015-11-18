pro bracket_v,xdis,nx1,i_values=i_values,m_values=m_values
;**
;** Subroutine written originally for imlabelmmm to find where in xdis the
;** valuenx1 is located or bracketed.  The values in the vector xdis smoothly
;** change and xdis does not have to be monotonic.  For instance if nx1 = .5
;** and xdis = [0.,.3,.7,1.3,.9,.5,.2], then bracket_v will return i_values
;** equal to [1,2] (since xdis(1) and xdis(2) bracket nx1) and m_values
;** equal to 5 (since xdis(5) = nx1).
;**
;** If no matches are found, m_values = -1 is returned; if no bracketing
;** values are found, i_values = -1 is returned.
;**
;** INPUTS:
;**		xdis	 - vector containing values to search in;
;**		nx1	 - value to match;
;**
;** OUTPUTS:
;**		i_values - array containing xdis bin numbers that contain
;**			   values that bracket nx1;
;**		m_values - array containing xdis bin numbers that contain
;**			   values that match nx1.
;**
;** Written by kachun 12 July, 1994.

;sel0 = where(xdis ge nx1)
;sel1 = where(xdis le nx1)
;----------------
;modified by wqd (7/28/98) to solve problems 
	;arised from small numerical error
nx1n=nx1 
cnt=0L
redo: sel0 = where(xdis ge nx1n,nsel0)
	sel1 = where(xdis le nx1n,nsel1)
if nsel0 eq 0 then begin
	nx1n=nx1n-1.e-4
	cnt=cnt+1
;	print,cnt
	goto,redo
endif
if nsel1 eq 0 then begin
	nx1n=nx1n+1.e-4
	goto,redo
endif
;-------------------
;** Check to see if nx1n is a value in xdis:

match,sel0,sel1,match0,match1

if (total(match0) ne -1) and (total(match1) ne -1) then $
  m_values = sel0(match0) else m_values = -1

;** Find bracketing values of nx1n:

;** Do special case where xdis has only two values (the match subroutine
;** won't work properly with vectors less than 3 bins in size):
if n_elements(xdis) eq 2 then begin
  if min(xdis) lt nx1n and max(xdis) gt nx1n then begin
    i_values = intarr(1,2)
    i_values(0,0) = where(xdis eq min(xdis))
    i_values(0,1) = where(xdis eq max(xdis))
  endif else i_values = -1
  return
endif

match,sel0+1,sel1,match0,match1 ;** Matches are match0 and match1
match,sel0-1,sel1,match2,match3 ;** Matches are match2 and match3

;** Create vector that will contain bracketing values:

m_size = 0
if total(match0) ne -1 then nmatch0 = n_elements(match0) else nmatch0 = 0
if total(match2) ne -1 then nmatch2 = n_elements(match2) else nmatch2 = 0
m_size = nmatch0 + nmatch2
i_values = intarr(m_size,2)

if nmatch0 ne 0 then begin
  i_values(0:nmatch0-1,0) = sel0(match0)
  i_values(0:nmatch0-1,1) = sel1(match1)
endif

if nmatch2 ne 0 then begin
  i_values(nmatch0:*,0) = sel0(match2)
  i_values(nmatch0:*,1) = sel1(match3)
endif

;** Remove any bins in i_values that contain matches in m_values:

for j=0,n_elements(m_values)-1 do begin
  selm = where(i_values(*,0) eq m_values(j))
  if total(selm) ne -1 then i_values(selm,*) = -10
  seln = where(i_values(*,1) eq m_values(j))
  if total(seln) ne -1 then i_values(seln,*) = -10
endfor

sel = where(i_values(*,0) ne -10,nsel)

if nsel ne 0 then begin

  temp = intarr(nsel,2)

  for j=0,nsel-1 do temp(j,*) = i_values(sel(j),*)

  ;** Reorder any bins so that temp(i,0) < temp(i,1) for any value of i:
  for j=0,nsel-1 do $
    if temp(j,0) gt temp(j,1) then begin
      temp0 = temp(j,0)
      temp1 = temp(j,1)
      temp(j,0) = temp1
      temp(j,1) = temp0
    endif

endif else temp = -1

i_values = temp

return
end

