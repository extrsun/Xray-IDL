pro sou_match,sras,sdecs,slist,slistrm,noremove=noremove,selrm=selrm
;+
; Match source positions with a source structure
; and then remove the sources and put them into a new structure
; sras, sdecs - RA and Dec of sources to be matched
; slist - input source structure
; slistrm - source structure contains sources removed
; noremove - if set, no removal in slist
; selrm - index of the removed sources in the original source structure
; written by wqd, 1/16/2002
;-
nparams=n_params()
 if nparams eq 0 then begin
print,'CALL SEQUENCE - sou_match,sras,sdecs,slist,slistrm'
print,',selrm=selrm,noremove=noremove'
return
endif

ns=n_elements(sras) 
selrm=[-999]
if ns eq 0 then begin
	print,'no source given'
	return
endif else begin
	for k=0,ns-1 do begin
		trans_dist,sras(k),sdecs(k),slist.ra,slist.dec,xp,yp,/das,/deg
		print,min((xp^2+yp^2),sel)
		selrm=[selrm,sel]
	endfor
endelse
selrm=selrm(1:*)
if nparams gt 3 then slistrm=slist(selrm)
if not keyword_set(noremove) then remove,selrm,slist
return
end