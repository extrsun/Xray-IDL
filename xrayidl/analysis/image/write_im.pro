pro write_im,blow,bhigh,refhead,image,outfile=outfile,dim=dim,block=block $
		,exptail=exptail,det=det
;-
; refhead - the fits file head 
;
; produce a count image in a PSPC  band in either sky or detector coordinates
; writen by  WQD, July 26, 1993
; Change the band definition from !bandchb to !bandch. WQD, Sept 17 1993
; change blow and bhigh into vectors so that images in several bands can be 
; created in a singal run. WQD, Oct 13, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUNECE - write_im,blow,bhigh,refhead,image,outfile=outfile'
print,',dim=dim,block=block,exptail=exptail,det=det'
return
endif
if n_elements(outfile) eq 0 then outfile=!data_dir+!seq_no+'_im' 
if n_elements(exptail) eq 0 then exptail=''
if n_elements(tfile) eq 0 then begin
	tfile=!seq_no+'_gti'+strtrim(exptail,2)+'.dat'
	print,'use the default tfile; ',tfile 
endif
print,'producing the count image'
chlow=!bandch(*,0)
chhigh=!bandch(*,1)
;
if n_elements(dim) eq 0 then dim=512
nbands=n_elements(blow)
print,nbands,' images will be produced in bands: '
forprint,blow,bhigh
;
for k=0,nbands-1 do begin
 emin=chlow(blow(k)-1)
 emax=chhigh(bhigh(k)-1)
 print,'The energy region selected is emin = ',emin,'; emax = ',emax
 make_image,image,dim=dim,emin=emin,emax=emax,block=block $
		,tfile=tfile,det=det

 	file=outfile+strtrim(blow(k),2)+strtrim(bhigh(k),2)+ $
		strtrim(exptail,2)+'.fits'
 print,'Output the image into the file: ',file
 hist='Count image created in the '+strtrim(blow(k),2)+ $
	'-'+strtrim(bhigh(k),2)+' band and with gti file = '+ tfile
 get_fitshead,image,head,refhead,del=block*!size_pixel/3600.,hist=hist
	writefits,file,image,head
endfor
end