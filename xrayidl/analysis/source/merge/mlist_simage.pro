pro mlist_simage,hdr,listm,scrval,mdim,block,images,shdr $
,detagname=detagname,sel=sel,emin=emin,emax=emax,fid=fid,pixsize=pixsize $
,refp=refp,crval=crval,xmins=xmins,ymins=ymins
;+
; Extract a subimage from a list of counts
;
; hdr - fits header of the event file
; refp - (x, y) reference pixel position. If given, overriding hdr.
; crval - two element vector of RA and Dec (deg). If given, overriding hdr
; listm - list of events
; scrval - two element vector of RA and Dec (deg). 
; mdim - dimesion of the subimage
; block - block of the subimage in units of the event pixel
; fid - observation file name (e.g., 2269). If given, select only the events 
;	from the file from a merged event list
; pixsize - pixel size in units (arcseconds; def = !size_pixel)
; xmins,ymins - the lower left corner pixel position in the count list
;
;*OUTPUTS:
; images - subimage
; shdr - fits header of a subset image
; detagname - prefix of the coordinates (def =''; e.g., 'm' for merged list)
; sel - index for the selected events
;
; written by wqd 5/3/02
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - mlist_simage,hdr,listm,scrval,mdim,block'
print,',images,shdr,detagname=detagname,sel=sel,emin=emin,emax=emax'
print,',fid=fid,pixsize=pixsize,refp=refp,crval=crval'
return
endif
if N_elements(pixsize) eq 0 then pixsize=!size_pixel
if n_elements(refp) eq 0 then refp=sxpar(hdr,'crpix*')
;refp=refp(n_elements(refp)-2:*)
if n_elements(crval) eq 0 then crval=sxpar(hdr,'crval*')
;crval=crval(n_elements(crval)-2:*)
if n_elements(type) eq 0 then type=2
trans_dist,scrval(0),scrval(1),crval(0),crval(1),xp,yp,/deg,pixsize=pixsize
;refps=(mdim-1.)*0.5*float(block)+[xp,yp] ;the lower left cornor is (1,1)
;xmins=nint(refp(0)-refps(0))
;ymins=nint(refp(1)-refps(1))
refps=mdim*0.5*float(block)+[xp,yp] 
xmins=nint(refp(0)-0.5-refps(0))+1 ;the lower left cornor is (1,1)
ymins=nint(refp(1)-0.5-refps(1))+1
refps=refps/float(block)+0.5
get_fitshead,0,shdr,hdr,del=block*pixsize/3600.0d,dim=mdim,crval=crval $
	,cpx=refps(0),cpy=refps(1),type=type
list_image,listm,xmins,ymins,images,mdim(0),mdim(1),block=block $
	,detagname=detagname,sel=sel,emin=emin,emax=emax,fid=fid
;if !debug eq 2 then stop
return
end
