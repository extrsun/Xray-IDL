;+
;
;NAME:
;read_spec
;
;PURPOSE:
; Read spectral files of various types
;
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;read_spec,spfil,sptyp,ecen,edel,rate,sigrate,time,numpix
;
;INPUTS:
; spfil     name of input spectral file
; sptyp     type (origin) of spectrum to be read
;           Allowed values are 'QDP', 'PROS', 'PHA', 'ASPC'
;           If sptyp = 'QDP', then an ASCII file of the type created by
;              typing plot ecd in XSPEC, then writing a QDP .plot file,
;              is expected
;           If sptyp = 'PROS', then an SDAS binary table of the type created
;              by the xspectral package in PROS is expected
;           If sptyp = 'PHA', then an XSPEC .pha format file is expected
;           If sptyp = 'ASPC', then an ASCII file of the type written by
;              by write_aspc is expected
;
;OUTPUTS:
; ECEN      floating point vector containing central energy of each channel
; EDEL      floating point vector containing channel energy widths
; RATE      floating point vector containing extracted spectrum (counts/s)
;           from ROSAT pspc file (as given by make_spec, for example)
; SIGRATE   associated error vector
; TIME      integration time (seconds)
; NUMPIX    area of region (in pixels) from which rate & sigrate 
;           were extracted
; GROUP     channel grouping (set to !group if not specified)
;
;OPTIONAL INPUT PARAMETERS:
; CHATTER   controls program feedback to user ("chattiness")
;
;OPTIONAL OUTPUT PARAMETERS:
;none
;
;COMMON BLOCKS:
;none
;SIDE EFFECTS:
;none
;RESTRICTIONS:
;none
;
;PROCEDURE:
;
;MODIFICATION HISTORY:
;written 13 Jul 1992 by GAR
;
;-
pro read_spec,spfil,sptyp,ecen,edel,rate,sigrate,time,numpix,chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' READ_SPEC, spfil, sptyp, ECEN, EDEL, RATE, SIGRATE, TIME, NUMPIX,'
  print,'            chatter=chatter'
  retall
endif
if (n_elements(chatter) eq 0) then chatter = 1
;
fdecomp,spfil,disk,dir,name,ext,ver
ftype = strupcase(sptyp)
if (ext eq '') then begin
  if (ftype eq 'QDP') then ext = '.plot'
  if (ftype eq 'PHA') then ext = '.pha'
  if (ftype eq 'PROS') then ext = '.tab'
  if (ftype eq 'ASPC') then begin
    pos = strpos(name,'_asc')
    pos = pos(0)
    if (pos lt 0) then name = name + '_asc'
    if (ext eq '') then ext = '.spc' else ext = '.'+ext
  endif
endif else ext = '.'+ext
if (ver ne '') then ver = ';'+ver
fname = disk+dir+name+ext+ver
;
case (ftype) of
  'PROS': begin          ;use TAB_READ and TAB_VAL to read a binary SDAS table
        if (chatter eq 1) then $
           print,' Reading SDAS binary table file ',fname
        tab_read,fname,tcb,table,header
        elo = tab_val(tcb,table,'lo_energy')
        ehi = tab_val(tcb,table,'hi_energy')
        ecen = (ehi + elo)/2.
        edel = ehi - elo
        rate = tab_val(tcb,table,'net')
        sigrate = tab_val(tcb,table,'neterr')
        time = sxpar(header,'LIVETIME')*sxpar(header,'LIVECORR')
        numpix = sxpar(header,'SAREA')
        end
;
  'QDP': begin           ;read an ASCII file (of QDP .plot format)
         if (chatter eq 1) then $
            print,' Reading ASCII (QDP .plot format) file ',fname
         get_lun,un
         openr,un,fname
         dum = '' 
         readf,un,dum         ;first two lines contain QDP plot commands
         readf,un,dum
         readf,un,dum
;
         nchan = 0
         ecen = fltarr(500)
         edel = ecen
         rate = ecen
         sigrate = ecen
         while not eof(un) do begin
           readf,un,e,de,r,sigr
           ecen(nchan) = e
           edel(nchan) = 2.*de
           rate(nchan) = r
           sigrate(nchan) = sigr
           nchan = nchan + 1
         endwhile
         free_lun,un
         ecen = ecen(0:nchan-1)
         edel = edel(0:nchan-1)
         rate = rate(0:nchan-1)
         sigrate = sigrate(0:nchan-1)
         numpix = 0.
         time = 0.
         end
;
  'PHA': begin          ;read an XSPEC format .pha file
         if (chatter eq 1) then $
            print,' Reading XSPEC .pha format file ',fname
         end
;
  'ASPC': begin
          read_aspc,fname,group,rate,sigrate,time,numpix
          chan2energy,ecen,edel,group=group      
          end
;
  else: begin
        print,sptyp,' is not a valid option for SPTYP.'
        print,' Allowed options are: PROS, QDP, PHA, ASPC.  Returning.'     
        end
endcase
;
return
end             ;pro read_spec
