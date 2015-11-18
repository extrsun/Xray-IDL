PRO get_dssinfo,hdr,xsz,ysz,xpixelsz,ypixelsz,a,b,objra,objdec,$
	objctx,objcty,cnpix,pltscale,pltra,pltdec,ppo
;+
;
NAXIS = sxpar(hdr,'naxis*')
xsz=naxis(0)
ysz=naxis(1)
ppo = sxpar(hdr,'ppo*')
xpixelsz= sxpar(hdr,'xpixelsz')
ypixelsz= sxpar(hdr,'ypixelsz')
a=sxpar(hdr,'amdx*')
b=sxpar(hdr,'amdy*')
pltra=(sxpar(hdr,'pltrah')+sxpar(hdr,'pltram')/60d + $
	sxpar(hdr,'pltras')/3600d)*(180d/12d)
pltdec=double(strcompress(sxpar(hdr,'pltdecsn'),/remov)+'1')*( sxpar(hdr,'pltdecd') $
	+sxpar(hdr,'pltdecm')/60d +sxpar(hdr,'pltdecs')/3600d)
stringad,sxpar(hdr,'objctra')+sxpar(hdr,'objctdec'),objra,objdec
objctx=sxpar(hdr,'objctx')
objcty=sxpar(hdr,'objcty')
cnpix=sxpar(hdr,'cnpix*')
pltscale=sxpar(hdr,'pltscale')
return
end
                       

