function evmtrx,mxy
; Use to convert an xy coord event matrix 'mxy' to a 512x512 array
; Array cell size is 27 microns, i.e. the Astro-D CCD.
sz=(size(mxy))(2) & a=intarr(512,512)
for i=0,sz-1 do begin
cell=fix(256+mxy(*,i)/.0027) & cell=0>cell<511
a(cell(1),cell(0))=a(cell(1),cell(0))+1 & endfor
return,a
end
