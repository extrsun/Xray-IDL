pro read_arf,fname,e_lo,e_hi,specresp,expos,effarea,h1=h1,h0=h0

	if n_params(0) eq 0 then begin
	  print,'read_arf,fname,e_lo,e_hi,specresp,effarea,expos,h1=h1,h0=h0
	  retall
	endif

	openr,unit,fname,/block,/get_lun
	FXHREAD,unit,h0
	free_lun,unit
	fxbopen,unit,fname,1,h1
	n = sxpar(h1,'naxis2')
	if n eq 0 then return
	fxbread,unit,e_lo,1
	fxbread,unit,e_hi,2
	fxbread,unit,specresp,'specresp'
;	fxbread,unit,effarea,'eff_area'
;	fxbread,unit,expos,'exposure'
	fxbclose,unit
	return
	end

