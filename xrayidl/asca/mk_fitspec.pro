pro mk_fitspec,spec,spece,gti,evtfile,sname=sname,bck=bck,rsp=rsp,arf=arf,qual=qual
if n_params(0) eq 0 then begin
 print,'Make OGIP standard FITS spectral File '
 print,'First run GETAREA to generate X0,Y0, and WMAP'
 print,'** INPUTS ** '
 print,'PLIST	- Input photon list '
 print,'GEO	- The geometrical information array'
 print,'GTI	- IDL GTI array [from MK1GTI]'
 print,'TEXP	- Exposure time [from MK1GTI]'
 print,'X0,Y0	- Bottom Left hand coords of WMAP [from GETAREA]'
 print,'WBIN	- Rebin factor for WMAP [You chose this]'
 print,'WMAP	- The WMAP [use GETAREA]'
 print,'NPHA	- Number of Pulse height channels '
 print,'	- (must be power of 2 and <= 1024)'
 print,'	- NPHA< 0 for PHA spectrum, >0 for PI spectrum '
 print,'EVTFILE - Full name of one of the original raw event files '
 print,'	- which is used to generate header info for the spectrum'
 print,'SNAME 	- Name of output spectral file '
 print,'STYP	- Determines the type of binning for spectrum '
 print,'	- 0: GIS spectrum '
 print,'	- 1: SIS BRIGHT or converted FAINT '
 print,'	- 2: SIS FAINT mode '
 print,'	- 3: PSPC spectrum '
 print,'BCK	- Background file name '
 print,'RSP	- Response Matrix file name '
 print,'ARF	- ARF file name '
 print,'IBGD	- =0 if this is to be a source file, =1 for a BGD file '
 retall
end

mission='ASCA'
instr='GIS2'
if n_elements(sname) eq 0 then begin
 sname=' '
 read,'Enter name of output spectral file ',sname
endif
if n_elements(bck) eq 0 then bck='none'
if n_elements(rsp) eq 0 then rsp='none'
if n_elements(arf) eq 0 then arf='none'
;now generate the preliminary header information for the primary,
;1st and 2nd extensions for the spectral fits file
print,'Making FITS file headers ..'
texp=total(gti(*,1)-gti(*,0))
totc=total(spec)*texp
x0=1
y0=1
wbin=1
wmap=fltarr(1,1)
ibgd=0
npha=1024
mkspechdr,totc,gti,texp,mission,instr,ibgd,npha,x0,y0,wbin,wmap,$
evtfile,h0,h1,h2,rsp=rsp,bck=bck,anc=arf
;fill in the WMAP
;print,'Filling in the WMAP ..'
;fillwmap,plist,x0,y0,wbin,wmap
;Now write out the spectral file
print,'Now writing the spectral file '
;wrtfitspec,h0,h1,h2,spec,gti,wmap,specname=sname
;specout=fltarr(npha)
;speceout=fltarr(npha)
;specout(*)=spec
;speceout(*)=spece
wrt_fitspec,h0,h1,h2,spec,spece,gti,wmap,specname=sname,qual=qual
;that's it
return
end
