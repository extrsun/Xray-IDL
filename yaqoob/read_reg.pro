pro read_reg,fname,x,y,r, bx, by, bwx, bwy, bin=bin
; Added box regions, Mar. 1997

	if n_params(0) eq 0 then begin
	  print,'read_reg,fname,x,y,r, bx, by, bwx, bwy'
	  print,'If bin, then remap from (x,y) to ((x-0.5)*bin+0.5, (y-0.5)*bin+0.5)'
	  return
	endif

	x = fltarr(100) & y = x & r = x & bx = x & by = x & bwx = x & bwy = x
	i = 0 & j = 0

	openr,1,fname
	a = ''
	excl = 0
	while not eof(1) do begin
	  readf,1,a
	  if strpos(a,'CIR') gt 0 then begin
	    excl = strmid(a,0,1) eq '-'
	    j1 = strpos(a,'(')
	    j2 = strpos(a,',')
	    x(i) = float(strmid(a,j1+1,j2-j1-1))
	    j1 = j2
	    j2 = strpos(a,',',j1+1)
	    y(i) = float(strmid(a,j1+1,j2-j1-1))
            j1 = j2
            j2 = strpos(a,')',j1+1)
            r(i) = float(strmid(a,j1+1,j2-j1-1))
	    if excl then r(i) = -r(i)
	    i = i+1
	  endif
          if strpos(a, 'BOX') gt 0 then begin
            excl = strmid(a, 0, 1) eq '-'
            j1 = strpos(a,'(')
	    j2 = strpos(a,',')
	    bx(j) = float(strmid(a,j1+1,j2-j1-1))
	    j1 = j2
	    j2 = strpos(a,',',j1+1)
	    by(j) = float(strmid(a,j1+1,j2-j1-1))
            j1 = j2
            j2 = strpos(a,',',j1+1)
            bwx(j) = float(strmid(a,j1+1,j2-j1-1))
            j1 = j2
            j2 = strpos(a,')',j1+1)
            bwy(j) = float(strmid(a,j1+1,j2-j1-1))
	    if excl then bwx(j) = -bwx(j)
	    j = j+1
          endif
	endwhile
	close,1
        if i eq 0 then stop
	x = x(0:i-1)
	y = y(0:i-1)
	r = r(0:i-1)
        if j gt 0 then begin
          bx = bx(0:j-1)
          by = by(0:j-1)
          bwx = bwx(0:j-1)
          bwy = bwy(0:j-1)
        endif else begin
          bx = 0 & by = 0 & bwx = 0 & bwy = 0
        endelse
	if n_elements(bin) gt 0 then begin
	  x = (x-0.5)*bin+0.5
	  y = (y-0.5)*bin+0.5
	  r = r*bin
	  bx = (bx-0.5)*bin+0.5
	  by = (by-0.5)*bin+0.5
	  bwx = bwx*bin
	  bwy = bwy*bin
	endif
	return
	end
