pro filter_sp,xs_pix,ys_pix,filter,filter_s,block=block,rs=rs,rb1=rb1,rb2=rb2 $
	,souexc=souexc,pixcenter=pixcenter
;-
; xs_pix,ys_pix - the pixel postion (in units of 0.5") of the source
; in the filter. If set to be -1, the center of the filter will be used.
; souexc - if set, zero pixels in the core region will remain to be zero
;
; get a filter for a specific source to extract the source's spectral data
; region within a source radius is set to be -1 in the filter
; regions outside the source radius and the background annulus are set to be 0
; writen by wqd, May 5, 1994
;+
if n_params() eq 0 then begin
	print,'CALL SEQUENC - filter_sp,xs_pix,ys_pix,filter,filter_s'
	print,',block=block,rs=rs,rb1=rb1,rb2=rb2,souexc=souexc,pixcenter=pixcenter'
	return
endif

if n_elements(block) eq 0 then block=!block
sz=size(filter)
if keyword_set(pixcenter) ne 0 then begin
	xs=xs_pix/float(block)+(sz(1)-1.)*0.5
	ys=ys_pix/float(block)+(sz(2)-1.)*0.5
endif else begin
	if xs_pix lt 0 then begin
		xs=(sz(1)-1)*0.5
		ys=(sz(2)-1)*0.5
	endif else begin
		xs=xs_pix/float(block)
		ys=ys_pix/float(block)
	endelse
endelse
dist_circle,dis,sz(1),xs,ys
;
tran=60./(!size_pixel*block)
rrs=rs*tran ; photon list coordinates
rrb1=rb1*tran ; radii are in units of arcminutes
rrb2=rb2*tran
filter_s=filter
sel=where(dis gt rrb2 or (dis ge rrs and dis lt rrb1),nsel)
if nsel ne 0 then filter_s(sel)=0. ;regions not to be used
sel=where(dis lt rrs,nsel)
if nsel ne 0 then filter_s(sel)=-1. 
	;filter_s(sel)  ;to be used as source region
if keyword_set(souexc) ne 0 then begin
	sel=where(filter_s lt 0. and filter eq 0,nsel)
	if nsel ne 0 then filter_s(sel)=0.
endif
end