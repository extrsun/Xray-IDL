pro asca_spec,spec,spece,see,arfe,middl,tonaxis,geff $
,phafil=phafil,arffil=arffil,ebadlo=ebadlo,ebadhi=ebadhi
;================================================
!proc='ASCA'
if n_elements(arffil) eq 0 then arffil='source.arf'
if n_elements(phafil) eq 0 then phafil='source.pha'

; get telescope on-axis effective area
fildir='/data5/asca/a0401/'
fname_tonaxis='tel_onaxis.dat'
openr,un,fildir+fname_tonaxis,/get
readf,un,nenerg
tonaxis=fltarr(2,nenerg)
readf,un,tonaxis
tonaxis=tonaxis(1,*)
free_lun,un

; get detector effective area
fname_geff='gis_eff.dat'
openr,un,fildir+fname_geff,/get
readf,un,nenerg
geff=fltarr(2,nenerg)
readf,un,geff
free_lun,un
ee=fltarr(nenerg)
ee(*)=geff(0,*)
geff=geff(1,*)

; get arf
nregmod=1
nregimg=3
fname_geff='g2.junk.arf'
openr,un,fildir+'max/'+fname_geff,/get
arf0=fltarr(1+nregmod*nregimg,nenerg)
readf,un,arf0
free_lun,un
arf=arf0(1:*,*)
;for k=0,nenerg-1 do arf(*,k)=arf(*,k)/tonaxis(k)
; plot,ee,arf(0,*)/tonaxis ;shows the dependence with neither
; mirror or detector energy effects

; interplate the arf into the energy bins of the rmf file

rspfil='/caldb/caldbtop/data/asca/gis/cpf/95mar06/gis2v4_0.rmf'
aa_gtenrg,rspfil,nech,lower,hiher,middl,status
arfe=fltarr(nregmod*nregimg,nech)
arfv=fltarr(nenerg)
for k=0,nregimg-1 do begin
	arfv(*)=arf(k,*)*geff ;now include the position independent effect
	linterp,ee,arfv,middl,arfvm
	arfe(k,*)=arfvm
endfor
; Write the ARF file
wt_arf,lower, hiher, middl, arfe(0,*),rspfil=rspfil,phafil=phafil,arffil=arffil

; stop,'after arf'
; calculate the deconvolved spectra

gt_ebound,rspfil,nech,see
fname_geff='g2.junk.sp'
openr,un,fildir+'max/'+fname_geff,/get
sp0=fltarr(1+2*nregimg,nech)
readf,un,sp0
free_lun,un
sp0=sp0(1:*,*)
arfs=fltarr(nregmod*nregimg,nech)
for k=0,nregimg-1 do begin
	arfv(*)=arf(k,*)/tonaxis
	linterp,ee,arfv,see,arfvm
	arfs(k,*)=arfvm
endfor
; linterp,ee,tonaxis,see,stonaxis

arfs = arfs > 0.01
aa=fltarr(3)
aa(0)=4.^2
aa(1)=8.^2-4.^2
aa(2)=12.^2-8.^2

dnorm=(1.-(aa(0)/aa(2))*(arfs(2,*)/arfs(0,*)))
spec=(sp0(0,*)-sp0(2,*)*(aa(0)/aa(2)))/dnorm
spece=sqrt(sp0(0,*)+sp0(2,*)*(aa(0)/aa(2))^2)/dnorm

; prepare for outputing the spectrum
if n_elements(ebadlo) eq 0 then ebadlo=0.5
if n_elements(ebadhi) eq 0 then ebadhi=12
s=where(see lt ebadlo or see gt ebadhi)
qual=intarr(nech)
qual(s)=5 ;bad channels

evtfile='/data5/ASCA/A0401/evt/'+'ad82010000g200170m.evt'
get_actime,a,b,fname=evtfile,nr=nr
gti=dblarr(nr,2) ; format is the same as used in wrtfitspec.pro
gti(*,0)=a
gti(*,1)=b

mk_fitspec,spec,spece,gti,evtfile,sname=phafil,bck=bck,rsp=rspfil,arf=arffil,qual=qual

stop,'at the end'
return
end

