pro combasca,sissrc,sisbgd,sisgti,gissrc,gisbgd,gisgti,totsrc,totbgd,totgti,sisbscl,gisbscl,totbscl,plo=plo,phi=phi,evtfiles=evtfiles,specfiles=specfiles,sext=sext
if n_params(0) eq 0 then begin
 print,'combasca,sissrc,sisbgd,sisgti,gissrc,gisbgd,gisgti,totsrc,totbgd,totgti,sisbscl,gisbscl,totbscl,plo=plo,phi=phi,evtfiles=evtfiles,specfiles=specfiles,sext=sext'
 print,'Take four instrument events files and combine S0,S1 and G2,G3'
 print,'photon lists and also 4 instrument photon lists'
 print,'EVTFILES(8): events for src s0,s1,s2,s3 and bgd s0..s3 in that order'
 print,'SPECFILES(8): spectral files in same order as above'
 print,'If evtfiles and specfiles are not specified they will be '
 print,'searched for in the current directory'
 print,'Spectral files are used to get correct normalization for '
 print,'source and background'
 print,'** INPUTS ** '
 print,'PLO & PHI are LONARR(4) specifying the upper and lower '
 print,'PI bounds respectively to filter on before combining events'
 print,'The instrument PI values are specified in the order s0,s1,s2,s3' 
 print,'NOTE the PI values are inclusive '
 print,'Defaults for SIS 0.5-10 keV and GIS 0.7-10 keV are'
 print,'PLO=[140,140,61,61]
 print,'PHI=[1725,1725,848,848]
 print,'SEXT = pha or pi (spectral file extension) default is pha'
 print,' ** WARNING ***'
 print,' MAKE SURE SISSRC, SISBGD, GISSRC, GISBGD, TOTSRC, TOTBGD'
 print,' DO NOT ALREADY EXIST '
 retall
end 
if n_elements(sext) eq 0 then sext='pha'
if n_elements(plo) eq 0 then plo=[19,18,61,61]
if n_elements(phi) eq 0 then phi=[347,341,848,848]
if n_elements(evtfiles) eq 0 then begin
 evtfiles=strarr(8)
 srcevt=findfile('*src.evt',count=nsrcevt)
 if nsrcevt ne 4 then print,'Need 4 src evt files'
 evtfiles(0:3)=srcevt
 bgdevt=findfile('*bgd.evt',count=nbgdevt)
 if nbgdevt ne 4 then print,'Need 4 bgd evt files'
 evtfiles(4:7)=bgdevt
endif
for k=0,7 do print,evtfiles(k)
if n_elements(specfiles) eq 0 then begin
 specfiles=strarr(8)
 srcspec=findfile('*src.'+sext,count=nsrcspec)
 if nsrcspec ne 4 then print,'Need 4 src spectral files'
 specfiles(0:3)=srcspec
 bgdspec=findfile('*bgd.'+sext,count=nbgdspec)
 if nbgdspec ne 4 then print,'Need 4 bgd spectral files'
 specfiles(4:7)=bgdspec
endif
for k=0,7 do print,specfiles(k)
;now read the files
qreadasca,evtfiles(2),s0src,h,s0sgti,tab
qreadasca,evtfiles(3),s1src,h,s1sgti,tab
qreadasca,evtfiles(0),g2src,h,g2sgti,tab
qreadasca,evtfiles(1),g3src,h,g3sgti,tab
qreadasca,evtfiles(6),s0bgd,h,s0bgti,tab
qreadasca,evtfiles(7),s1bgd,h,s1bgti,tab
qreadasca,evtfiles(4),g2bgd,h,g2bgti,tab
qreadasca,evtfiles(5),g3bgd,h,g3bgti,tab
;now filter the events on PI
s0src=s0src(where(s0src.pi ge plo(0) and s0src.pi le phi(0)))
s0bgd=s0bgd(where(s0bgd.pi ge plo(0) and s0bgd.pi le phi(0)))
s1src=s1src(where(s1src.pi ge plo(1) and s1src.pi le phi(1)))
s1bgd=s1bgd(where(s1bgd.pi ge plo(1) and s1bgd.pi le phi(1)))
g2src=g2src(where(g2src.pi ge plo(2) and g2src.pi le phi(2)))
g2bgd=g2bgd(where(g2bgd.pi ge plo(2) and g2bgd.pi le phi(2)))
g3src=g3src(where(g3src.pi ge plo(3) and g3src.pi le phi(3)))
g3bgd=g3bgd(where(g3bgd.pi ge plo(3) and g3bgd.pi le phi(3)))
;and the spectral files
h0=headfits(specfiles(2))
h1=headfits(specfiles(3))
h2=headfits(specfiles(0))
h3=headfits(specfiles(1))
h4=headfits(specfiles(6))
h5=headfits(specfiles(7))
h6=headfits(specfiles(4))
h7=headfits(specfiles(5))
kw='BACKSCAL'
sb=sxpar(h4,kw)+sxpar(h5,kw)
gb=sxpar(h6,kw)+sxpar(h7,kw)
ss=sxpar(h0,kw)+sxpar(h1,kw)
gs=sxpar(h2,kw)+sxpar(h3,kw)
sisbscl=sb/ss
gisbscl=gb/gs
totbscl=(sb*25.+gb)/(ss*25.+gs)
;now combine the photon lists
pi=0
clo=0
chi=4096
applist,s0src,pi,clo,chi,sissrc
applist,s1src,pi,clo,chi,sissrc
applist,s0bgd,pi,clo,chi,sisbgd
applist,s1bgd,pi,clo,chi,sisbgd
applist,g2src,pi,clo,chi,gissrc
applist,g3src,pi,clo,chi,gissrc
applist,g2bgd,pi,clo,chi,gisbgd
applist,g3bgd,pi,clo,chi,gisbgd
applist,sissrc,pi,clo,chi,totsrc
applist,gissrc,pi,clo,chi,totsrc
applist,sisbgd,pi,clo,chi,totbgd
applist,gisbgd,pi,clo,chi,totbgd
;now combine the GTIs
combtint,s0sgti,s1sgti,sisgti
combtint,g2sgti,g3sgti,gisgti
combtint,sisgti,gisgti,totgti
;now filter the photon lists on these net GTIs
gtiflt,sissrc,sisgti
gtiflt,sisbgd,sisgti
gtiflt,gissrc,gisgti
gtiflt,gisbgd,gisgti
gtiflt,totsrc,totgti
gtiflt,totbgd,totgti
return
end
