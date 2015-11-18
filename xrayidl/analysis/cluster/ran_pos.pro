pro ran_pos,sel,bimage,xmin,ymin,xp,yp,seed=seed,block=block
;+
; Name:
; ran_pos
; PURPOSE:
; produce randum counts in selected bins, accounding to a count image
;
;*Inputs:
; sel - selected location in the image
; bimage - expected count image
; block - block size of the image
; xmin, ymin - the min and max of the lower left corner of the image
;
;*Outputs:
; xp, yp - the output count x and y location in the rosat coordinates
;
; written by wqd, Aug. 23, 2003
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ran_pos,sel,bimage,xmin,ymin,xp,yp'
print,',seed=seed,block=block'
return
endif
if n_elements(block) eq 0 then block=30
;if n_elements(seed) eq 0 then seed=384884

sz=size(bimage)
xdim=sz(1)
;poisson,bimage(sel),bc,seed
bc=poidev(bimage(sel),seed=seed)
nsel=n_elements(sel)
s=where(bc gt 0,ns)
if ns ne 0 then begin
 xx=(sel(s) mod xdim)*block+xmin ;the bin x and y location in the rosat coord
 yy=(sel(s)/xdim)*block+ymin
 xp=[-999]
 yp=[-999]

 for k=0L,ns-1 do begin
	kk=s(k)
	xp=[xp,xx(k)+randomu(seed,bc(kk))*block]
	yp=[yp,yy(k)+randomu(seed,bc(kk))*block]
 endfor
 xp=fix(xp(1:*))
 yp=fix(yp(1:*))
endif else print,'there is no simulated counts in the image' 
if !debug eq 1 then stop
return
end
