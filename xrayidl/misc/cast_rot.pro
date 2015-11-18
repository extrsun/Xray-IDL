pro cast_rot,angle,xc,yc,del,righthand=righthand
;try to deal with an HST image of n3556, but cast does not deal with
;CD astrometry parameters yet.
extast,hdr,astr,nopar
if nopar eq -1 then begin
    print,'The header does not contain the necessary astrometry info!!!'
    return
endif
if keyword_set(righthand) then begin
    angle=-90+angle
    for k=1,5 do astr.(k)=reverse(astr.(k))
endif
crpix=astr.crpix
crval=astr.crval
rangle=angle*!pi/180.
xr=xc*cos(rangle)-yc*sin(rangle)
yr=xc*sin(rangle)+yc*cos(rangle)
trans_loct,xr,yr,crval(0),crval(1),rar,decr,/deg,pixsize=del
print,crval,rar,decr
stop
get_fitshead,o,ohm,crval=[rar,decr],del=del(0)

 get_fitshead,o,oho,crval=crval,del=del(0),cpx=crpix(0),cpy=crpix(1)


crval=sxpar(hdr,'crval*')

