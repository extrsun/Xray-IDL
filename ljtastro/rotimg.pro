pro rotimg, filelis=filelis, prefix=prefix, filepath=filepath, refhead=refhead, angpara=angpara

if n_elements(angpara) eq 0 then angpara='CROTA2'

;readfile name
filename=""
openr,lun,filelis,/get_lun
for n=1, 20 do begin 
readf,lun,filename 
img=readfits(filepath+filename,head) 
if strlen(strtrim(filename,2)) eq 0 then begin 
goto, NEXT
endif

;rotate image and write fits file
rollang=sxpar(head,angpara) 
cast,filepath+filename,refhead,outa=outimg,inroll=rollang 
;HROT, img, head, outimg, newhead, rollang, -1, -1, 1, MIS = 0 
writefits,prefix+strtrim(string(n),2)+'.fits',outimg,head

;update fits head
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', angpara, 0 
NAXIS1=sxpar(refhead,'NAXIS1') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'NAXIS1', NAXIS1
NAXIS2=sxpar(refhead,'NAXIS2') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'NAXIS2', NAXIS2
CRVAL1=sxpar(refhead,'CRVAL1') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'CRVAL1', CRVAL1
CRVAL2=sxpar(refhead,'CRVAL2') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'CRVAL2', CRVAL2
CRPIX1=sxpar(refhead,'CRPIX1') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'CRPIX1', CRPIX1
CRPIX2=sxpar(refhead,'CRPIX2') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'CRPIX2', CRPIX2
CDELT1=sxpar(refhead,'CDELT1') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'CDELT1', CDELT1
CDELT2=sxpar(refhead,'CDELT2') 
FXHMODIFY, prefix+strtrim(string(n),2)+'.fits', 'CDELT2', CDELT2
NEXT:
endfor

end
