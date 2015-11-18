pro preduce,im,h0,hx,hy,hc,exptime,exptime0,threshold
;
; perform one level of reduction of image im
; works on odd-sized images
;
	temp = size(im)
	nx = temp(1)
	ny = temp(2)
;
; round nx/2,ny/2 up if nx,ny odd
;
	nx2 = (nx+1)/2
	ny2 = (ny+1)/2
;
; add row or column pad to make images even in size
;
	h = fltarr(2*nx2,2*ny2)
	h(0:nx-1,0:ny-1) = im
	h = reform(h,2,nx2,2,ny2)
;
	xe = fltarr(2*nx2,2*ny2)
	xe(0:nx-1,0:ny-1) = exptime
	xe = reform(xe,2,nx2,2,ny2)
;
; compute exptime0 appropriate for h0
;
	exptime0 = reform(xe(1,*,1,*)+xe(1,*,0,*)+xe(0,*,1,*)+xe(0,*,0,*), nx2, ny2)
	w = where(exptime0 GT 0)
	h0 = reform(h(1,*,1,*)+h(1,*,0,*)+h(0,*,1,*)+h(0,*,0,*), nx2, ny2)
;
	if nx GT 1 then begin
		xe1 = reform(xe(1,*,1,*)+xe(1,*,0,*),nx2,ny2)
		xe2 = reform(xe(0,*,1,*)+xe(0,*,0,*),nx2,ny2)
		hx = fltarr(nx2,ny2)
		hx(w) = 0.5/exptime0(w)*( $
				xe2(w)*(h(1,*,1,*)+h(1,*,0,*))(w) - $
				xe1(w)*(h(0,*,1,*)+h(0,*,0,*))(w)   $
			)
		hx(w) = hx(w) * (abs(hx(w)) GT threshold*0.5/exptime0(w)*sqrt( $
				xe2(w)^2*(h(1,*,1,*)+h(1,*,0,*))(w) + $
				xe1(w)^2*(h(0,*,1,*)+h(0,*,0,*))(w)   $
			))
	endif
;
	if ny GT 1 then begin
		xe1 = reform(xe(1,*,1,*)+xe(0,*,1,*),nx2,ny2)
		xe2 = reform(xe(1,*,0,*)+xe(0,*,0,*),nx2,ny2)
		hy = fltarr(nx2,ny2)
		hy(w) = 0.5/exptime0(w)*( $
				xe2(w)*(h(1,*,1,*)+h(0,*,1,*))(w) - $
				xe1(w)*(h(1,*,0,*)+h(0,*,0,*))(w)   $
			)
		hy(w) = hy(w) * (abs(hy(w)) GT threshold*0.5/exptime0(w)*sqrt( $
				xe2(w)^2*(h(1,*,1,*)+h(0,*,1,*))(w) + $
				xe1(w)^2*(h(1,*,0,*)+h(0,*,0,*))(w)   $
			))
	endif
;
	if nx GT 1 AND ny GT 1 then begin
		xe1 = reform(xe(1,*,1,*)+xe(0,*,0,*),nx2,ny2)
		xe2 = reform(xe(1,*,0,*)+xe(0,*,1,*),nx2,ny2)
		hc = fltarr(nx2,ny2)
		hc(w) = 0.5/exptime0(w)*( $
				xe2(w)*(h(1,*,1,*)+h(0,*,0,*))(w) - $
				xe1(w)*(h(1,*,0,*)+h(0,*,1,*))(w)   $
			)
		hc(w) = hc(w) * (abs(hc(w)) GT threshold*0.5/exptime0(w)*sqrt( $
				xe2(w)^2*(h(1,*,1,*)+h(0,*,0,*))(w) + $
				xe1(w)^2*(h(1,*,0,*)+h(0,*,1,*))(w)   $
			))
	endif
;
; convert back to odd sizes if necessary
; don't bother if size=1 (no difference coefficients required then)
;
	testx = 2*nx2 ne nx
	testy = 2*ny2 ne ny
	nnx=nx2-testx
	nny=ny2-testy
	if testx and (nnx GT 0) then begin
		hx = reform( hx(0:nnx-1,*), nnx, ny2)
	endif
	if testy and (nny GT 0) then begin
		hy = reform( hy(*,0:nny-1), nx2, nny)
	endif
	if (testx or testy) and (nnx GT 0) and (nny GT 0) then begin
		hc = reform( hc(0:nnx-1,0:nny-1), nnx, nny)
	endif
	return
end

function pexpand,h0,hx,hy,hc,exptime,exptime0
;
; perform one level of expansion of h0,hx,hy,hc; returns image im
; this version works on odd-sized images
;
	temp = size(h0)
	nx = temp(1)
	ny = temp(2)
;
; nx2 = nx*2 or nx*2-1 depending on whether original image was even or odd,
; and similarly for ny2
;
; hx or hy may be undefined if nx=nx2=1
;
	if (n_elements(hx) gt 0) then begin
		temp = size(hx)
		nx2 = nx + temp(1)
	endif else begin
		nx2 = nx
	endelse
	if (n_elements(hy) gt 0) then begin
		temp = size(hy)
		ny2 = ny + temp(2)
	endif else begin
		ny2 = ny
	endelse
;
	im = fltarr(2,nx,2,ny)
;
; pad with zeros if necessary for odd images
;
; The reform is necessary because if ny=1 fltarr(nx,ny) turns into a 1-D array.
; (I consider this an IDL bug, but I suppose it could be called a feature.)
;
	hhx = reform(fltarr(nx,ny),nx,ny)
	if (n_elements(hx) gt 0) then hhx(0:nx2-nx-1,0:ny-1) = hx
	hhy = reform(fltarr(nx,ny),nx,ny)
	if (n_elements(hy) gt 0) then hhy(0:nx-1,0:ny2-ny-1) = hy
	hhc = reform(fltarr(nx,ny),nx,ny)
	if (n_elements(hc) gt 0) then hhc(0:nx2-nx-1,0:ny2-ny-1) = hc
;
; fill in edge values of hc from hx,hy for odd length rows, columns
;
	if nx2 MOD 2 NE 0 then hhc(nx-1,*) = -hhy(nx-1,*)
	if ny2 MOD 2 NE 0 then hhc(*,ny-1) = -hhx(*,ny-1)
;
; avoid divide by zero problems by simply changing value of exposure time
; when an entire 2x2 block has zero exposure (this does not affect results
; since exptime=0 if exptime0=0.)
;
	mm = min(exptime0(where(exptime0 GT 0)))
	be = fltarr(2*nx,2*ny)
	be(0:nx2-1,0:ny2-1) = exptime
	be = reform(be/rebin(exptime0>mm,2*nx,2*ny,/sample),2,nx,2,ny)
;
	im(1,*,1,*) = reform(be(1,*,1,*)*h0+hhx+hhy+hhc,nx,ny)
	im(1,*,0,*) = reform(be(1,*,0,*)*h0+hhx-hhy-hhc,nx,ny)
	im(0,*,1,*) = reform(be(0,*,1,*)*h0-hhx+hhy-hhc,nx,ny)
	im(0,*,0,*) = reform(be(0,*,0,*)*h0-hhx-hhy+hhc,nx,ny)
	im = reform((be GT 0)*im,2*nx,2*ny)
;
; return odd image without padding
;
	return,reform(im(0:nx2-1,0:ny2-1),nx2,ny2)
end

function pfilter,im,threshold,nlevels,exptime=exptime,nottoplevel=nottoplevel
;
; Compute H-transform filter of image IM with Poisson statistics
; using THRESHOLD sigma cutoff by shifting the H-transform grid to
; smooth the result.  This version calls itself recursively to do the
; shifting with minimum computation.  NLEVELS determines the number of
; H-transform reductions to do.  If specified, EXPTIME is the effective
; exposure time for each pixel; in that case the returned image is
; the count rate for each pixel.
;
; The NOTTOPLEVEL keyword is used in the recursive calls to determine when
; the routine is being called at a lower level.  It should not be used
; in user calls.
;
; 24 November 1992, R. White
;
	if n_elements(nlevels) LE 0 then nlevels = 5
	if n_elements(im) EQ 1 OR nlevels LE 0 then begin
;
; at bottom level just return count rate
;
		h = float(im)
		if (NOT keyword_set(NOTTOPLEVEL)) AND $
		 (n_elements(exptime) GT 0) then begin
			message,'Normalizing by exposure time',/inform
			w = where(exptime GT 0)
			if w(0) GE 0 then h(w)=h(w)/exptime(w)
		endif
;		message,'Entering & Returning',/inform
		return,h
	endif
;
	temp = size(im)
	nx = temp(1)
	ny = temp(2)
;
; print a message to let user know something is happening
;
	if nx GT 63 AND ny GT 63 then $
		message,'nx'+string(nx,format='(i4)')+' ny'+string(ny,format='(i4)'), $
			/inform
	if n_elements(exptime) le 0 then exptime = replicate(1.,nx,ny)
;
	preduce,im,p0,px,py,pc,exptime,exptime0,threshold
	p0 = pfilter(p0,threshold,nlevels-1,exptime=exptime0,/nottoplevel)
	rim = pexpand(p0,px,py,pc,exptime,exptime0)
	nrim = 1
;
	if nx GT 2 then begin
		im2 = fltarr(nx+1,ny)
		im2(1:nx,*) = im
		bexptime = fltarr(nx+1,ny)
		bexptime(1:nx,*) = exptime
		preduce,im2,p0,px,py,pc,bexptime,exptime0,threshold
		p0 = pfilter(p0,threshold,nlevels-1,exptime=exptime0,/nottoplevel)
		rim = rim + (pexpand(p0,px,py,pc,bexptime,exptime0))(1:nx,*)
		nrim = nrim + 1
	endif
;
	if ny GT 2 then begin
		im2 = fltarr(nx,ny+1)
		im2(*,1:ny) = im
		bexptime = fltarr(nx,ny+1)
		bexptime(*,1:ny) = exptime
		preduce,im2,p0,px,py,pc,bexptime,exptime0,threshold
		p0 = pfilter(p0,threshold,nlevels-1,exptime=exptime0,/nottoplevel)
		rim = rim + (pexpand(p0,px,py,pc,bexptime,exptime0))(*,1:ny)
		nrim = nrim + 1
	endif
;
	if nx GT 2 AND ny GT 2 then begin
		im2 = fltarr(nx+1,ny+1)
		im2(1:nx,1:ny) = im
		bexptime = fltarr(nx+1,ny+1)
		bexptime(1:nx,1:ny) = exptime
		preduce,im2,p0,px,py,pc,bexptime,exptime0,threshold
		p0 = pfilter(p0,threshold,nlevels-1,exptime=exptime0,/nottoplevel)
		rim = rim + (pexpand(p0,px,py,pc,bexptime,exptime0))(1:nx,1:ny)
		nrim = nrim + 1
	endif
;
	if keyword_set(NOTTOPLEVEL) then begin
;		message,'Returning nx ny'+string(nx)+string(ny),/inform
		return,reform(rim/nrim,nx,ny)
	endif else begin
;
; at top level return, divide by exposure time to get count rate
;
		message,'Normalizing by exposure time',/inform
		rim = reform(rim/nrim,nx,ny)
		w = where(exptime GT 0)
		rim(w) = rim(w)/exptime(w)
;		message,'Returning nx ny'+string(nx)+string(ny),/inform
		return,rim
	endelse
end

