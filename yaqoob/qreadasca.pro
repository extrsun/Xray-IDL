pro qreadasca,fname,plist,h,gti,tab

	if n_params(0) eq 0 then begin
	  print,'qreadasca,fname,plist,h,gti,tab'
	  return
	endif
	tab=readfits(fname,h,ext=2,/silent)
	gstart = tbget(h,tab,'start')
	gstop = tbget(h,tab,'stop')
        print, 'Net exposure time = ' + strn(total(gstop-gstart)) + ' seconds'
	gti = dblarr(n_elements(gstart),2)
	gti(*,0) = gstart
	gti(*,1) = gstop
	tab=readfits(fname,h,ext=1,/silent)
	mode = SXPAR(h,'DATAMODE')
	dmode = strmid(mode,0,3)
	if (dmode eq 'BRI') then brplist,1,h,tab,plist
	if (dmode eq 'FAI') then ftplist,h,tab,plist
	if (dmode eq 'PH ') then gislist,1,h,tab,plist
	print,dmode
	return
	end	
