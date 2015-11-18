pro box_radec,refhead,vra,vdec
dimx=sxpar(refhead,'naxis1')
dimy=sxpar(refhead,'naxis2')
cra=sxpar(refhead,'crval1')
cdec=sxpar(refhead,'crval2')
delx=sxpar(refhead,'cdelt1')
dely=sxpar(refhead,'cdelt2')
cpx=sxpar(refhead,'crpix1')
cpy=sxpar(refhead,'crpix2')
vx=[-(cpx-0.5),-(cpx-0.5),dimx+0.5-cpx,dimx+0.5-cpx,-(cpx-0.5)]*abs(delx)*3600.
vy=[-(cpy-0.5),dimy+0.5-cpy,dimy+0.5-cpy,-(cpy-0.5),-(cpy-0.5)]*dely*3600.
trans_loct,vx,vy,cra,cdec,vra,vdec,/das,/deg
return
end
