;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;            defimap
;
;*PURPOSE:
; A procedure to generate a vector of the names of the PSPC instrument 
; map FITS files available for generating exposure maps 
;
;*CALLING SEQUENCE:
;	defimap,instmap,nimap,chans,sufx,chatter=chatter,dir=dir
;
;*PARAMETERS:
; OPTIONAL INPUTS:
;        chatter - keyword which specifies whether or not to list the
;                  names of the files
;        dir     - directory of files (if not default directory)
;
; OUTPUTS:
;        instmap - ASCII string vector of the names of the PSPC instrument
;                  map files
;        nimap   - number of channel bands
;        chans   - string vector giving the various channel bands
;                  e.g. '_8_19'
;        sufx    - string vector giving the channels and detector for
;                  each map file
;
;*NOTES:
;	Uses system variable !version.os to select appropriate list of
;       files.
;
;       Uses system variable !imapdir as default directory for detector
;       maps. Upon entering IDL, !imapdir is set to getenv('ZDEF') by
;       rosatlib.pro.
;
;       At the Rosat GOF, ZIMAP is set equal to /caldb/data/rosat/pspc/bcf
;       on the Suns (by $ZCOM/.idlrc), and to CALDB:[DATA.ROSAT.PSPC.BCF] 
;       on the Vaxen (by ZCOM:idldef.com)
;
;*MODIFICATION HISTORY:
;       written  06 Aug 1993 (GAR)
;       modified 23 Sep 1993 (GAR) to add keyword DIR, and to remove
;         scale factor from output list -- scale factors are now included
;         as a keyword in the FITS header of the detector map
;       modified 01 Oct 1993 to use system variable !imapdir = default 
;          directory for detector map files, so user can change this within IDL
;          !imapdir is initially set to getenv('ZIMAP') by ROSATLIB upon
;          starting IDL (GAR)
;
; MODIFICATIONS ARE MADE BY INCLUDING THE KEY WORD: IGAIN 
; WHEN IGAIN = 1 (OBNSERVATION TAKEN AFTER 911011), CHANNEL 8_19
; IS USED TO REPLACE 11_19. WQD, oact 12. 1993
;-
;-------------------------------------------------------------------------------
;pro defimap,instmap,nimap,chans,sufx,chatter=chatter,dir=dir
PRO DEFIMAP_M,INSTMAP,NIMAP,CHANS,SUFX,CHATTER=CHATTER,DIR=DIR,IGAIN ;!
;
;  default directory for detector maps !imapdir is set to getenv('ZIMAP') 
;  upon entering IDL
;  At the Rosat GOF, ZIMAP is set equal to /caldb/data/rosat/pspc/bcf
;  on the Suns, and to CALDB:[DATA.ROSAT.PSPC.BCF] on the Vaxen
;  (within the files $ZCOM/.idlrc or ZCOM:idldef.com)
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' DEFIMAP, INSTMAP, NIMAP, CHANS, SUFX, chatter=chatter, dir=dir'
  retall
endif
if (n_elements(chatter) eq 0) then chatter = 1
if (n_elements(dir) eq 0) then dir = ''
if (!debug gt 1) then print,dir
;
defdir = !imapdir
if (!version.os ne 'vms') then defdir = defdir + '/'
if (dir eq '') then dir = defdir
;
;	Instrument map information
;
; Change as of Aug 6, 1993 -- in the new maps, Jeff says the instrument maps
;   are labelled C for before Jan 25, 1991 and B for afterwards
; (this was opposite to the convention from before)
;
;-----------------------------------------------
;chans = ['_8_19','_20_41','_42_51','_52_69','_70_90','_91_131','_132_201']
;chans = [chans,'_11_19','_8_41','_52_90','_91_201','_42_131','_42_201']
CHANS = ['_11_19','_20_41','_42_51','_52_69','_70_90','_91_131','_132_201'] ;!
CHANS = [CHANS,'_8_19','_8_41','_52_90','_91_201','_42_131','_42_201']
IF IGAIN EQ 1 THEN BEGIN
	CHANS(0)=CHANS(7)
	PRINT,'8_19 CHANNEL DETECTOR MAP IS USED TO REPLACE 11_19 MAP'
	PRINT,' TO ACCOUNT FOR THE LOW GAIN AFTER 911011.'
ENDIF ;!
;---------------------------------------------
nimap = n_elements(chans)
sufx = [chans+'_c',chans+'_b']
instmap = dir+'det'+sufx+'.fits'
;
if (chatter gt 0) then begin          ;print out the choices
  print,'   '
  print,'   For PSPC, choices for BAND are channels'
  for ii=0,nimap-1,7 do begin
    iend = (nimap-1) < (ii+6)
    print,'      '
    print,f='$(i7,6i10)',ii+indgen(iend-ii+1)+1
    print,f='$(7a10)',chans(ii:iend)
  endfor
endif
;
return
end            ;pro defimap
