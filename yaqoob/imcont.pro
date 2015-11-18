pro imcont,im,h,nl
imlo=min(im) & imhi=max(im)
imlo=max([imlo,1.]) & imhi=max([imhi,1.])
if imhi eq imlo then return
del=(alog(imhi)-alog(imlo))/float(nl)    
lev=exp(alog(imlo)+del*findgen(nl))
imcontour,smooth(im,2.),h,type=0,nlevels=nl,levels=lev
set_plot,'ps'
device,/landscape
imcontour,smooth(im,2.),h,type=0,nlevels=nl,levels=lev
device,/close
set_plot,'x'
return
end
