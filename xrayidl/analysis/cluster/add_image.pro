obsno=['800344n00','800253','800048n00','800480','800248','800047','800048n00','800097','800479','800511n00']
mpe=[1,0,0,0,0,0,0,1,0,0]
nob=n_elements(obsno)
idlsave=strarr(nob)+'idlsave.dat'
idlsave(2)='idlsave_a222.dat'
idlsave(6)='idlsave_a223.dat'
fac=4./3. ;2 Mpc
dimsso=300
add=fltarr(dimsso,dimsso,nob)
rsa=fltarr(nob)
for kk=0,nob-1 do begin
	envset,obsno(kk),mpe=mpe(kk) ;,tail=tail(kk)
	restore,idlsave(kk)
	rblock=rs*fac*240./dimsso
	print,'rblock',rblock
	get_fitshead,fltarr(dimsso,dimsso),hs,h,crval=[ra,dec],del=rblock/7200.
;	cast,'f47_sremoved.fits',hs,outa=s
	cast,'',hs,outa=s,ina=ff,inh=headfits('f47_sremoved.fits')
	add(*,*,kk)=s
	rsa(kk)=rs
endfor
end

