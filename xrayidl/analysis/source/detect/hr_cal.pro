pro hr_cal,slist,nslist,hr,hre,hr2,hr2e,hr1,hr1e,soufile=soufile,hrslow=hrslow,probth=probth,nband=nband,dhrch=dhrch
;+
; calculate hardness ratios
; 
; slist - source structure list
; nslist - new source structure list with hardness ratios included
; soufile - source fits file. If provided, slist will be read out from the file
; hr, hr2, hr1 - HR, HR2, and HR1 (if nband=4)
; hre, hr2e, hr1e - errors of HR, HR2, and HR1
; hrslow - lower limit of the S/N for calculating the hardness ratios
;		(def=4)
; probth - probability threshold for selecting the output sources
; nbands - number of bands used in calculating the hardness ratios
; dhrch - the choice for the HR definition (the same for bands=3): 
;         for dhrch=0 (def), HR=(c(2:3)-c(1))/(c(2:3)+c(1)); for dhrch=1, 
;         HR=(c(2:3)-c(0:1))/(c(2:3)+c(0:1))
;; for dhrch=2, HR=(c(2)-c(0:1))/(c(2)+c(0:1))
;
; written by wqd, 6/17/2002
; change the default HR definition to dhrch=0
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - hr_cal,slist,nslist,hr,hre,hr2,hr2e,hr1,hr1e'
print,',soufile=soufile,hrslow=hrslow,probth=probth,nband=nband,dhrch=dhrch'
return
endif
if n_elements(dhrch) eq 0 then dhrch=0
if n_elements(hrslow) eq 0 then hrslow=4.

if n_elements(soufile) ne 0 then begin
 	sou_fits_info,soufile,slist,flow=flow,probth=probth,/all,nsel=ns 
endif else begin
	sel=where(slist.prob lt probth,ns)
	if ns ne 0 then slist=slist(sel) else begin 
		print,'No source above the probth threshold'
		return
	endelse 
endelse
if n_elements(nband) eq 0 then nband=4
cntrth=1.e-10
ca=fltarr(nband,ns)
cea=ca
ca(0,*)=slist.cntrb1 
ca(1,*)=slist.cntrb2 
ca(2,*)=slist.cntrb3 
cea(0,*)=slist.cntrbe1
cea(1,*)=slist.cntrbe2
cea(2,*)=slist.cntrbe3
if nband eq 4 then begin
	ca(3,*)=slist.cntrb4 
	cea(3,*)=slist.cntrbe4
	hr1=imdiv(ca(1,*)-ca(0,*),(ca(1,*)+ca(0,*)) > cntrth)
	hr1e=2/((ca(0,*)+ca(1,*)) > cntrth)^2 $
	*sqrt((cea(0,*)*ca(1,*))^2+(cea(1,*)*ca(0,*))^2)
	hr1 = (hr1 > (-1.)) < 1.
	sel=where((ca(0,*)+ca(1,*)) le hrslow*sqrt(cea(0,*)^2+cea(1,*)^2),nsel)
	if nsel ne 0 then hr1e(sel)=999 ;the error estimate is not good for
		; for poor  counting statstics.
	struct_col_add,slist,reform([reform(hr1,ns),reform(hr1e,ns)],ns,2) $
		,['HR1','HR1E'],0.0*[1,1],nslist
endif else nslist=slist
case dhrch of
1: begin
    hr=imdiv(total(ca(nband-2:nband-1,*),1)- $
             total(ca(0:nband-3,*),1),total(ca,1) > cntrth)
    hre=2/(total(ca,1) > cntrth)^2*sqrt((total(cea(0:nband-3,*),1) $
        *total(ca(nband-2:nband-1,*),1))^2+(total(cea(nband-2:nband-1,*),1) $
                                            *total(ca(0:nband-3,*),1))^2)
   end
0: begin
    hr=imdiv(total(ca(nband-2:nband-1,*),1)- $
             ca(nband-3,*),total(ca(nband-3:nband-1,*),1) > cntrth)
    hre=2/(total(ca(nband-3:nband-1,*),1) > cntrth)^2*sqrt((cea(nband-3,*) $
        *total(ca(nband-2:nband-1,*),1))^2+(total(cea(nband-2:nband-1,*),1) $
                                            *ca(nband-3,*))^2)
  end
2: begin
    hr=imdiv(ca(2,*)-total(ca(0:1,*),1),total(ca(0:2,*),1) > cntrth)
    hre=2/(total(ca(0:2,*),1) > cntrth)^2*sqrt((total(cea(0:1,*),1)*ca(2,*))^2$
            +(cea(2,*)*total(ca(0:1,*),1))^2)
end
3: print,'dhrch not defined!!!'
endcase
hr2=imdiv(ca(nband-1,*)-ca(nband-2,*),total(ca(nband-2:nband-1,*),1) > cntrth)
hr2e=2/(total(ca(nband-2:nband-1,*),1) > cntrth)^2*sqrt((cea(nband-2,*)*ca(nband-1,*))^2+(cea(nband-1,*)*ca(nband-2,*))^2)
hr = (hr > (-1.)) < 1.
hr2 = (hr2 > (-1.)) < 1.

sel=where(total(ca,1) le hrslow*sqrt(total(cea^2,1)),nsel)
if nsel ne 0 then hr2e(sel)=999 
sel=where(total(ca(nband-2:nband-1,*),1) le hrslow*sqrt(total(cea(nband-2:nband-1,*),1)^2),nsel)
if nsel ne 0 then hr2e(sel)=999 
struct_col_add,nslist,reform([reform(hr,ns),reform(hre,ns),reform(hr2,ns),reform(hr2e,ns)],ns,4),['HR','HRE','HR2','HR2E'],0.0*[1,1,1,1],temp_slist
nslist=temp_slist
if !debug eq 3 then stop
return
end
