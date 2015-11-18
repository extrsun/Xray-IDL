pro multi_spec,list,outroot,evtfname=evtfname,wmapfile=wmapfile,pifile=pifile,exposure=exposure,backscal=backscal,wemin=wemin,wemax=wemax,rebin=rebin,rmfdir=rmfdir,tstart=tstart,bspecfname=bspecfname,norsp=norsp,outdir=outdir
;+
; create a single spectrum from a merged events file
;
;*NOTES:
;
; 1) The program does not correct for the arf (and rmf) variation with
; time, which affects mostly at low energies (< 2 keV). 
;
; 2) before running the routine, such setup is needed:
; instr='acisi_low' ;for the GC project
; cenvset,instr 
;
; and ciao should be running in the same window
;
;*INPUTS:
; list - merged event list, each event include the "ccd_id, detx, dety,
;        energy, pi" information.
;
;*OUTPUTS:
; outroot - the root name for the output spectrum and rsp file (def='spec')
;
;*OPTIONAL Inputs:
; exposure - exposure time (s) of the feature of the event file. If
;            not provided, exposure is extracted from the evtfname
;            header (works typically only for a single observation!!)
; tstart - the start time of the observation (or representative of the
;          merged observations), used for calculating the arf; If
;            not provided, is extracted from the evtfname header
; evtfname - the merged event file, used to obtain the exposure and
;            tstart keywords
; backscal - the backscal of the output spectrum, default=1. Important
;            for the background subtraction
; wemin,wemax - lower and upper limits of event energy (eV) used to
;               create the WMAP image (creating arf and rmf file) 
;               (def: wemin=300 and wemax=8000)
; rebin  - the pixel block for the WMAP image
;          (def: rebin=8)
; rmfdir - the directory saving the rmf files for individual positions
;          in CCD
;          (def: '$PUBDIR/caldb_local//rmfmy/') 
; wmapfile - the ext=0 header will be used for creating the wmap file header
; pifile   - the ext=0,1 header will be used as the reference of the
;            final spctrum header
; bspecfname - file name of the background spectrum to be included in
;              the source spectrum header.
; outdir  - the directory which is used to create the output spectrum,
;           the default is './'
; writen by Hui Dong, June 18th, 2007
; revised by wqd, mainly to remove the filter part and to simply the
; inputs, July 5, 2007
;-

if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  multi_spec,list,wmapfile,outroot,evtfname=evtfname,pifile=pifile'
print,',exposure=exposure,backscal=backscal,wemin=wemin,wemax=wemax,rebin=rebin'
print,',rmfdir=rmfdir,tstart=tstart,bspecfname=bspecfname,norsp=norsp,outdir=outdir'
return 
endif

if n_elements(outdir) eq 0 then outdir='./'
if n_elements(outroot) eq 0 then outroot='spec'

if n_elements(evtfname) ne 0 then evthead=headfits(evtfname,ext=1)
if n_elements(exposure) eq 0 then exposure=sxpar(evthead,'exposure')

if n_elements(backscal) eq 0 then begin
   backscal=1
   print,'backscale= 1 is assumed!!!'
endif

;=================================================
;Begin: Creating the spectrum file
;=================================================
if n_elements(pifile) eq 0 then pifile='$PUBDIR/caldb_local/rmfmy/spec_header.pi'
if keyword_set(norsp) then outrsp='NONE' else outrsp=outroot+'.rsp'
outpi=outroot+'.pi'

ncha=1024
spec=histogram(list.pi,min=1,max=1024)
rf=headfits(pifile)
rf1=headfits(pifile,ext=1)

row={channel:0L,pi:0L,counts:0L}
slist=replicate(row,ncha)
channel=lindgen(ncha)+1
pi=lindgen(ncha)+1
slist(*).channel=channel(*)
slist(*).pi=pi(*)
slist(*).counts=spec(0:ncha-1)
;remove count rate entry from the original header, using
;sxdelpar,rf1,'TTYPE4' & sxdelpar,rf1,'TUNIT4' & sxdelpar,rf1,'TLMIN4'& sxdelpar,rf1,'TFORM4'
FXWRITE,outdir+outpi,rf
sxaddpar,rf1,'exposure',exposure
sxaddpar,rf1,'backscal',backscal
sxaddpar,rf1,'ANCRFILE','NONE'
sxaddpar,rf1,'RESPFILE',outrsp
if n_elements(bspecfname) ne 0 then sxaddpar,rf1,'BACKFILE',bspecfname
mwrfits,slist,outdir+outpi,rf1
if keyword_set(norsp) then return
;=================================================
;End: Creating the spectrum file
;=================================================

;=================================================
;Begin: Creating the WMAP
;=================================================
if n_elements(wmapfile) eq 0 then wmapfile='$PUBDIR/caldb_local/rmfmy/wmap_header.pi'
wmapheado=headfits(wmapfile)
if n_elements(rmfdir) eq 0 then rmfdir='$PUBDIR/caldb_local/rmfmy/'
if n_elements(tstart) eq 0 then tstart=sxpar(evthead,'tstart')
wmaphead=wmapheado
sxaddpar,wmaphead,'TSART',tstart

outwmap=outroot+'.wmap'

;this may be done outside the program to save some time:
if n_elements(wemin) eq 0 then wemin=300
if n_elements(wemax) eq 0 then wemax=8000        
wsel=where((list.energy ge wemin) and (list.energy lt wemax),wnct)

if wnct eq 0 then begin
   print,'There is no events within the energy range'
   print,wemin,wemax
   return
endif

wlist=list(wsel)

if n_elements(rebin) eq 0 then rebin=8
wdim=8192/rebin ;specific for ACIS

list_image,wlist,0.5,0.5,wmap,wdim,block=rebin,det='det'
writefits,outwmap,wmap,wmaphead
;=================================================
;End: Creating the WMAP
;=================================================

;=================================================
;Begin: Creating the RSP
;=================================================
outarf=outroot+'.warf'
outfef=outroot+'.wfef'

spawn,'punlearn ardlib'
spawn,'pset ardlib AXAF_ACIS0_BADPIX_FILE=NONE'
spawn,'pset ardlib AXAF_ACIS1_BADPIX_FILE=NONE'
spawn,'pset ardlib AXAF_ACIS2_BADPIX_FILE=NONE'
spawn,'pset ardlib AXAF_ACIS3_BADPIX_FILE=NONE'
spawn,'punlearn mkwarf'
spawn,'pset mkwarf infile='+outwmap
spawn,'pset mkwarf outfile='+outarf
spawn,'pset mkwarf weightfile='+outfef
spawn,'pset mkwarf feffile=CALDB'
spawn,'pset mkwarf egridspec=0.3:11.0:0.01'
spawn,'pset mkwarf threshold=0'
spawn,'pset mkwarf mirror=HRMA'
spawn,'pset mkwarf verbose=1'
spawn,'pset mkwarf mode=h'
spawn,'mkwarf clobber=yes'


create_weight_rmf,rmfdir,outroot,outdir=outdir
;=================================================
;End: Creating the RSP
;=================================================

spawn,'rm '+outarf+' '+outfef+' '+outwmap

end
