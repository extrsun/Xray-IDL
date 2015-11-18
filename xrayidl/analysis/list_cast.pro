pro list_cast,mra,mdec,hdr,oldlist,listnew,mxv,myv,pixsize=pixsize,mrefp=mrefp,mtype=mtype,naxis=naxis,msel=msel,roll=roll,xyreal=xyreal,xykeep=xykeep,detagname=detagname
;+
; cast a n event list from an off-set observation to a norminal pointing 
; direction
;
; mra, mdec - RA and Dec (or Galactic Longitude and latitude, if mtype=1)
;	of the merge image
; hdr - fits header of the event file to be merged
; oldlist - list of events  to be merged
; listnew - merged list of events in the merged coordinates
; pixsize - pixel size in units (arcseconds; def = !size_pixel)
; mtype - if = 1, RA and Dec of the events are converted into 
;		Galactic coordinates
;            =2: 	Galactic to  RA and Dec
;            else:  RA and Dec to  RA and Dec
; mrefp - merged reference pixel values (x and y)
; naxis - the x and y axis of the merged image (events outside the images
; 		are not included in the merged event list); naxis= mrefp*2-1
; msel - number of events included 
; mxv, myv - the casted coordinates of the included events 
; xyreal - if set, mxv and myv will not be converted into integers
; xykeep - if set, x and y coordinates will be not changed; e.g., the
;          new coordinates mxv, myv should be used 
; detagname - calternative (e.g., detector) coordinates 
;	with the tagname = detagname+'x' and detagname+'y'
;
; written by wqd Jun/18/98
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - list_cast,mra,mdec,hdr,oldlist,listnew,mxv,myv,pixsize=pixsize,mrefp=mrefp,mtype=mtype,naxis=naxis,msel=msel,roll=roll,xyreal=xyreal,xykeep=xykeep,detagname=detagname'
return
endif
if n_elements (detagname) eq 0 then detagname=''
if N_elements(pixsize) eq 0 then pixsize=!size_pixel
pixsize=[1,1]*pixsize

refp=sxpar(hdr,'crpix*')
crval=sxpar(hdr,'crval*')
lpixsize=[abs(sxpar(hdr,'cdelt1')),sxpar(hdr,'cdelt2')]*3600.0d
tagin=tag_names(oldlist)
	match,tagin,strupcase(detagname+'x'),xtagn
	match,tagin,strupcase(detagname+'y'),ytagn
	x=oldlist.(xtagn(0)) & y=oldlist.(ytagn(0)) 
trans_loct,(oldlist.(xtagn(0))-refp(0))*lpixsize(0),(oldlist.(ytagn(0))-refp(1))*lpixsize(1),crval(0),crval(1),crap,cdecp,/deg,/das ;changed from pixsize=pixsize on 5/5/04
if n_elements(mtype) eq 0 then mtype=0
case mtype of
    1: begin
	equi=sxpar(hdr,'EQUINOX')
	if equi eq 0 then begin
		print,'equinox is not in the header and assumed to be J2000!'
                equi=2000
            endif 
	glactc,crap,cdecp,equi,crap,cdecp,1,/deg
	glactc,crval(0),crval(1),equi,cra,cdec,1,/deg
    end 
    2: begin
	equi=sxpar(hdr,'EQUINOX')
        if equi eq 0 then begin
		print,'equinox is not in the header and assumed to be J2000!'
                equi=2000
            endif 
	glactc,crap,cdecp,equi,crap,cdecp,2,/deg
	glactc,cra,cdec,equi,2,crval(0),crval(1),/deg
    end 
    else: begin
	cra=crval(0)
	cdec=crval(1)
    end 
endcase
;trans_dist,mra,mdec,crap,cdecp,xp,yp,/deg,pixsize=pixsize
;if n_elements(roll) ne 0 then rot_xy,xp,yp,roll,block=1,xpref=0,ypref=0,/xyreal
;trans_dist,cra,cdec,mra,mdec,refpx,refpy,/deg,pixsize=pixsize
;if N_elements(mrefp) eq 0 then mrefp=nint([refpx,refpy]+refp)
;if N_elements(naxis) eq 0 then naxis=mrefp*2-1
;xp=nint(xp+mrefp(0))
;yp=nint(yp+mrefp(1))
;;changed to the following, which seems to make sense. wqd 5/3/04
trans_dist,mra,mdec,crap,cdecp,xp,yp,/deg,/das
if n_elements(roll) ne 0 then rot_xy,xp,yp,roll,block=1,xpref=0,ypref=0,/xyreal
xp=xp/pixsize(0) & yp=yp/pixsize(1)

if N_elements(mrefp) eq 0 then begin
    trans_dist,mra,mdec,cra,cdec,refpx,refpy,/deg,/das
    mrefp=nint([refpx,refpy]/pixsize+refp)
endif
if n_elements(naxis) eq 0 then naxis=mrefp*2-1
if keyword_set(xyreal) then begin
    xp=xp+mrefp(0)
    yp=yp+mrefp(1)
endif else begin
    xp=nint(xp+mrefp(0))
    yp=nint(yp+mrefp(1))
endelse
;sel=where(xp ge 1 and xp le naxis(0) and yp ge 1 and yp le
;naxis(1),msel)
sel=where(xp ge 0.5 and xp le (naxis(0)+0.5) and yp ge 0.5 and yp le (naxis(1)+0.5),msel)
	;using in the fortran format
if msel ne 0 then begin
	listnew=oldlist(sel)
	mxv=xp(sel)
	myv=yp(sel)
        if keyword_set(xykeep) eq 0 then begin
            listnew.(xtagn(0))=mxv
            listnew.(ytagn(0))=myv
        endif 
endif 
if !debug eq 1 then stop
return
end

