pro fits_xy,fname,extn,fitsx,fitsy,x,y
tab=readfits(fname,h,ext=extn)
x=fits_get(h,tab,fitsx)
y=fits_get(h,tab,fitsy)
return
end
