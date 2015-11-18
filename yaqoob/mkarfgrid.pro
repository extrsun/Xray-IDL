pro mkarfgrid,specname,rmfname,arfnames,x1,x2,nx,y1,y2,ny,xl,xh,yl,yh,arfscript=arfscript,fudge=fudge,arffil=arffil,xrtrsp=xrtrsp,xrtpsf=xrtpsf
if n_params(0) eq 0 then begin
print,'mkarfgrid,specname,rmfname,arfnames,x1,x2,nx,y1,y2,ny,xl,xh,yl,yh,arfscript=arfscript,/fudge,/arffil,xrtrsp=xrtrsp,xrtpsf=xrtpsf'
 print,'Make a grid of arf files by systematically stepping '
 print,'through a grid of optical axes.'
 print,' ** INPUTS ** '
 print,'SPECNAME 	- Name of the input spectral file '
 print,'RMFNAME		- Name of the input RMF file '
 print,'X1 X2 NX- Bounds of X bins of Optical axes and # of X vals'
 print,'Y1 Y2 NY- Bounds of Y bins of Optical axes and # of Y vals'
 print,'ARFSCRIPT 	- name of script to generate arfs'
 print,'FUDGE		- Gaussian fudge applied if set '
 print,'ARFFIL		- filter fudge applied if set '
 print,'XRTRSP		- xrt response file '
 print,'XRTPSF		- xrt psf file'
 print,' ** OUTPUTS ** '
 print,' '
 print,'XL XH YL YH - Bin boundaries of optical axes grid '
 print,'ARFNAMES    - output grid of names for created ARF files'
 retall
end
dot=strpos(specname,'.')
rootname=strmid(specname,0,dot)
;copy the spectral file to a temporary one
cpstr='cp '+specname+' temp.pha'
spawn,cpstr
;set up defaults for ascaarf 
if n_elements(arfscript) eq 0 then begin
 xx='_x'+strtrim(string(nx),2)
 yy='_y'+strtrim(string(ny),2)
 arfscript=rootname+xx+yy+'.arfscript'
endif
;open the script file
openw,1,arfscript
printf,1,cpstr
caldir='/FTP/caldb/data/asca/'
if n_elements(fudge) eq 0 then cfudge='no' else cfudge='yes'
if n_elements(arffil) eq 0 then carffil='no' else carffil='yes'
if n_elements(xrtrsp) eq 0 then $
xrtrsp=caldir+'xrt/bcf/xrt_ea_v2_0.fits'
if n_elements(xrtpsf) eq 0 then $
xrtpsf=caldir+'xrt/bcf/xrt_psf_v2_0.fits'
dx=(x2-x1)/float(nx) & dy=(y2-y1)/float(ny)
xl=x1+dx*findgen(nx) & xh=xl+dx
yl=y1+dy*findgen(ny) & yh=yl+dy
xcen=0.5*(xl+xh) & ycen=0.5*(yl+yh)
;set up the output filenames
arfnames=strarr(nx,ny)
for i=0l,nx-1l do begin
  si=strtrim(string(i),2) 
  xopt=xcen(i)
 for j=0l,ny-1l do begin
  yopt=ycen(j)
  print,' I J',i,j
  print,'OPTICAL AXES ',xopt,yopt
  sj=strtrim(string(j),2)
   arfnames(i,j)=rootname+'_x'+si+'_y'+sj+'.arf' 
  print,'file ',arfnames(i,j) 
;now fudge optical axes
 fkeystr='fparkey '+strtrim(xopt,2)+' "temp.pha[0]" OPTIC1'
 print,fkeystr
 printf,1,fkeystr
; spawn,fkeystr
 fkeystr='fparkey '+strtrim(yopt,2)+' "temp.pha[0]" OPTIC2'
 print,fkeystr
 printf,1,fkeystr
; spawn,fkeystr
exstr1='ascaarf phafile=temp.pha rmffile='+strtrim(rmfname)
exstr2=' outfile='+strtrim(arfnames(i,j))+' point=yes simple=yes '
exstr3='fudge='+strtrim(cfudge)+' arffil='+strtrim(carffil)
exstr4=' clobber=yes xrtrsp='+strtrim(xrtrsp)+' xrtpsf='+strtrim(xrtpsf)
 exstr=exstr1+exstr2+exstr3+exstr4
 printf,1,exstr
 printf,1,' '
; spawn,exstr
 endfor
endfor
close,1
return
end
