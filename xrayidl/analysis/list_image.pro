pro list_image,listo,xmin,ymin,image,dimx,dimy,block=block,filter=filter,detagname=detagname,sel=sel,loc=loc,xp=xp,yp=yp,emin=emin,emax=emax,rsel=rsel,fid=fid,nsel=nsel
;+
; cast a list of counts into an image 
;
; list - table list of counts, whose content does not need to match 
; the image boundaries
; xmin, ymin - DADA pixel coordinates of the lower left corner of the image
; 		(starting at (1,1) for the fortran format)
; emin,emax - lower and upper energy (or channel) limits of counts to be chosen
; image - the output image
; dim - the dimension of the image
; filter - an image of the size of the image for excluding counts in regions
;	 with filter values <= 0
; detagname - cast the image in an alternative (e.g., detector) coordinates 
;	with the tagname = detagname+'x' and detagname+'y'
; sel - the output address of the selected counts in the list
; loc - the output location of the selected counts in the image
; rsel - if set, reverse the selection criterion (i.e., counts in regions
;	with filter value <=0)
; xp,yp - data pixel coordinates of individual counts, if supplied, listo
;	will not be used
; fid - if given, only those events with their file ID equal to fid will be
;	included
; nsel - number of counts in the image 
;
; writen by wqd, 1994
; modified by wqd to use the keyword detagname for alternative coordinates
; 	(12/28/2001)
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_image,listo,xmin,ymin,image,dimx,dimy'
print,',block=block,filter=filter,detagname=detagname,sel=sel,loc=loc'
print,',xp=xp,yp=yp,emin=emin,emax=emax,rsel=rsel,fid=fid,nsel=nsel'
return
endif
if n_elements(dimy) eq 0 then dimy=dimx
if n_elements(block) eq 0 then block=!block
if n_elements(xp) eq 0  then begin
 if n_elements(emin) eq 0 then begin
	list=listo 
	nss=0
 endif else begin
;		ss=where(listo.pi ge emin and listo.pi lt emax, nss)
		ss=where(listo.energy ge emin and listo.energy lt emax, nss)
		list=listo(ss)
 endelse
 nfid=n_elements(fid)
 if nfid ne 0 then begin
	st=[-999] & nc=0
	for kk=0,nfid-1 do begin
		ss=where(list.fid eq fid(kk),nss)
		if nss ne 0 then st=[st,ss]
		nc=nc+nss
	endfor
	if nc eq 0 then begin
		print,'no match is found for fid = ',fid
		return
	endif else list=list(st(1:*))
 endif
;
 if n_elements (detagname) eq 0 then begin
	xp=list.x & yp=list.y
 endif else begin
	;block=30./1.8684151 ;use this fixed block to get pixel size=15"
	tagin=tag_names(list)
	match,tagin,strupcase(detagname+'x'),xtagn
	match,tagin,strupcase(detagname+'y'),ytagn
	xp=list.(xtagn(0)) & yp=list.(ytagn(0)) ;xragn has to be scalar
;	print,'Assuming the PSPC image pixel as ', 0.93420756*block,' arcsec'
 endelse
endif else nss=0
; hdimx=dimx*block/2. ; now in units of pixel
; hdimy=dimy*block/2. ; now in units of pixel
; xmin=nint(xp-hdimx)+1 ;because list.x >=1
; ymin=nint(yp-hdimy)+1 
;xmax=xmin+long(dimx)*block-1
;ymax=ymin+long(dimy)*block-1
xmax=xmin+long(dimx)*block
ymax=ymin+long(dimy)*block
sel=where(xp ge xmin and xp lt xmax and yp ge ymin and yp lt ymax,nsel)
if nsel eq 0 then begin
 print,'no counts in the list is in the image!!!'
 return
endif
loc=long(dimx)*(long(yp(sel)-ymin)/long(block))+ $
		long(xp(sel)-xmin)/long(block)
if n_elements(filter) ne 0 then begin
	c=where(filter(loc) le 0.,nc)
	if nc ne 0 then begin
		if keyword_set(rsel) then begin
			sel=c 
			loc=loc(sel)
		endif else remove,c,loc,sel
	endif
endif
if nss ne 0 then sel=ss(sel)
print,'Creating image'
image=intarr(dimx,dimy)
sz=size(loc)  ; check to see if x is a scalar
case 1 of
  (sz(0) ne 0): begin        ; x is a vector
     h=histogram(loc)
     bin=lindgen(n_elements(h))+min(loc)
     image(bin)=h
     end
  else:    image = 0
endcase
if !debug eq 2 then stop
return
end 
