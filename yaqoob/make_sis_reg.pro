pro make_sis_reg,plist,cx,cy,rootname,bin,srcrad,bkgrad,sis=sis

	if n_params(0) eq 0 then begin
	  print,'make_sis_reg,plist,cx,cy,rootname,bin,srcrad,bkgrad,sis=sis'
	  print,'Create proper xselect regions for a src and bkg centered
	  print,'at cx,cy with srcrad and bkgrad in arcmins.'
	  print,'sis = 0 or 1'
	  print,'bin = bin factor of output, default = 4'
 	  print,'srcrad = radius of src in arcmins'
	  print,'if only bkgrad is a scalar, bkgrad = inner radius of bkg in arcmins, 
	  print,'  bkg = bkgrad - edge of ccds'
	  print,'if bkg is a vector, bkgrad = [inner,outer] rad. of an annulus'
	  print,'If root name = ngcXXX, srcrad = 4 and bkgrad = 6,'
	  print,'output filenames will be:'
	  print,'ngcXXX_src_4min.reg, ngcXXX_bkg_5-10min.reg'
	  print,"Note that it is ~ 8' from the center to the corner of a ccd"
	  retall
	endif

scale = 37.7082

; This array remaps ccdid to system where 1st bit = column, 2nd bit = row
	remap = [1b,0b,2b,3b]

	xsize = 425
	ysize = 450

	if n_elements(bin) eq 0 then bin = 1
	print,'Bin = ',strn(bin)

	if n_elements(sis) eq 0 then begin
	  sis = 0
	  read,'sis 0 or 1? ',sis
	endif
	bx = (cx-1)/bin+0.5
	by = (cy-1)/bin+0.5

; Find ccd with the most counts and exclude any other chip not present
; based on minmax of main ccd
	cr = minmax(plist.ccd)
	if cr(1)-cr(0) eq 0 then ccd = cr(0) else begin
	  hccd = histogram(plist.ccd)
	  ccd = (where(hccd eq max(hccd)))(0)+cr(0)
	endelse

	print,'Chip ',strn(ccd),' has the most counts'
	xr = minmax(plist(where(plist.ccd eq ccd)).detx)
	yr = minmax(plist(where(plist.ccd eq ccd)).dety)
	if sis then begin
	  ccd = (ccd+2) mod 4
	  print,'  SIS0 equivalent: ',strn(ccd,format='(4i1)')
	endif
	mask = ''
	a = remap(ccd)
	for i=0,3 do begin
	  w=where(plist.ccd eq i,n)
	  x = avg(xr) & y = avg(yr)
	  if n eq 0 then begin
	    if sis then b = remap((i+2) mod 4) else b = remap(i)
	    flag = not(a or b) or (a and b)
; Is chip i in the same row as chip ccd?
	    if (flag and 1) eq 1 then begin
	      print,strn(i)+' and '+strn(ccd),' are in the same row' 
	      if a gt b then x = xr(0)-xsize/2-bin else x = xr(1)+xsize/2+bin
	    endif
; Is chip i in the same column as chip ccd?
	    if (flag and 2) eq 2 then begin
	      print,strn(i)+' and '+strn(ccd),' are in the same column'
	      if a gt b then y = yr(0)-ysize/2-bin else y = yr(1)+ysize/2+bin
	    endif
	    if (flag and 3) eq 0 then begin
	      print,strn(i)+' and '+strn(ccd),' are on opposite corners'
	      case a of
		1: begin
		  x = xr(1)+xsize/2+bin
		  y = yr(0)-ysize/2-bin
		  end
		0: begin
		  x = xr(1)+xsize/2+bin
		  y = yr(1)+ysize/2+bin
		  end
		2: begin
		  x = xr(0)-xsize/2-bin
		  y = yr(1)+ysize/2+bin
	          end
		3: begin
		  x = xr(0)-xsize/2-bin
		  y = yr(0)-ysize/2-bin
		  end
	      endcase
	    endif
	    print,'Excluding chip ',strn(i),': ',strn(x),' , ',strn(y)
	    mask = [mask,'-BOX('+strn((x-1)/bin+1)+','+strn((y-1)/bin+1)+$
	      ','+strn(float(xsize)/bin)+','+strn(float(ysize)/bin)+')']
	  endif
	endfor

	if n_elements(srcrad) eq 0 then srcrad = 4
	if n_elements(bkgrad) eq 0 then bkgrad = 5
	openw,1,rootname+'_src_'+strn(srcrad,format='(f6.2)')+'min.reg'
	printf,1,' CIRCLE('+strn(bx)+','+strn(by)+','+$
	  strn(srcrad*scale/bin)+')'	
	for i=1,n_elements(mask)-1 do printf,1,mask(i)
	close,1

	if n_elements(bkgrad) eq 1 then begin
	  openw,1,rootname+'_bkg_'+strn(bkgrad,format='(f6.2)')+'min.reg'
; Now find out which ccds are present and only include those in bkg reg
	  for ccd=0,3 do begin
	    w = where(plist.ccd eq ccd,n)
	    if n gt 0 then begin
	      xr = (minmax(plist(w).detx)-1)/bin+1
	      yr = (minmax(plist(w).dety)-1)/bin+1
              printf,1,' BOX('+strn(avg(xr))+','+strn(avg(yr))+','+$
                strn(xr(1)-xr(0)+1)+','+strn(yr(1)-yr(0)+1)+')'
	    endif
	  endfor
	  printf,1,'-CIRCLE(',strn(bx)+','+strn(by)+','+strn(bkgrad*scale/$
	    bin)+')'
	  for i=1,n_elements(mask)-1 do printf,1,mask(i)
	  close,1
	endif else begin
	  openw,1,rootname+'_bkg_'+strn(bkgrad(0),format='(f6.2)')+'-'+$
	    strn(bkgrad(1),format='(f6.2)')+'min.reg'
	  printf,1,' CIRCLE(',strn(bx)+','+strn(by)+','+strn(bkgrad(1)*scale/$
            bin)+')'
	  printf,1,'-CIRCLE(',strn(bx)+','+strn(by)+','+strn(bkgrad(0)*scale/$
            bin)+')'
	  for i=1,n_elements(mask)-1 do printf,1,mask(i)
; Fix for xselect quirk of allowing regions to extend beyond detector bounds
	  printf,1,'-BOX(25,160,50,320)'
	  printf,1,'-BOX(295,160,50,320)'
	  printf,1,'-BOX(160,25,320,50)'
	  printf,1,'-BOX(160,295,320,50)'
	  close,1 
	endelse
	return
	end



