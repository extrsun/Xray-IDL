pro get_actime,start,ending,fname=fname,nrow=nrow
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_actime,start,ending,fname=fname,nrow=nrow'
print,',nfrlowr=nfrlowr'
return
endif
if n_elements(fname) eq 0 then fname=!seq_no
if !proc eq 'MPE' then $ ;read German PSPC format
	mpe_gtimes,fname,start,ending,nrow,dir=!data_dir $ 
else begin
	case !proc of
		'RDF': tab=readfits(fname+'_bas.fits',h,ext=1)
		'ASCA': tab=readfits(fname,h,ext=2)
		else: tab=readfits(fname+'.fits',h,ext=1)
	endcase
	start=tbget(h,tab,1) ; start of SGTI
	ending=tbget(h,tab,2)   ; end of SGTI
	nrow=n_elements(start)
endelse 
	print,'number of time intervals is ',nrow
return
end