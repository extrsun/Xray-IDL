;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;				rsobitimes
;
;*PURPOSE:
; A procedure to read OBI start and end times, in spacecraft time and in UT
; from ROSAT FITS file headers.
;
;*CALLING SEQUENCE:
;	rsobitimes, hdr, SCTBEG, SCTEND, UTBEG, UTEND, proc=proc
;
;*PARAMETERS:
; INPUTS:
;	hdr - FITS header
;
; OPTIONAL INPUTS:
;       proc - format of processed files. Choices are US, MPE, RDF.
;              if not specified, US is assumed.
;
; OUTPUTS:
;       SCTBEG - spacecraft start time of OBI (in seconds).
;                A single value for US & MPE format files; a vector for RDF
;                format files, containing as many elements as there are OBIs
;       SCTEND - spacecraft end time of OBI (in seconds).
;                A single value for US & MPE format files; a vector for RDF
;                format files, containing as many elements as there are OBIs
;       UTBEG  - start time(s) and date(s) in UT.  
;                For US & MPE format files, a 3 element vector containing 
;                the year, day number of year, and seconds of day.
;                For RDF format files, a 3 by NOBI element array.
;       UTEND  - end time(s) and date(s) of OBI. Like UTBEG.
;
;*MODIFICATION HISTORY:
;    written 20 Sept 1991 by GAR
;    modified 14 Feb 1992 to use GETHISTVAL (GAR)
;    modified 01 Mar 1992 to use chatter=0 in call to GETHISTVAL (GAR)
;    modified 25 Aug 1992 (GAR) to read MPE format files (keyword PROC added)
;    modified 16 Dec 1993 (GAR) to read info from RDF format headers
;-
;-------------------------------------------------------------------------------
pro rsobitimes,hdr,sctbeg,sctend,utbeg,utend,proc=proc
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSOBITIMES, hdr, SCTBEG, SCTEND, UTBEG, UTEND, proc=proc (US)'
  return
endif
if (n_elements(proc) eq 0) then proc = 'US'
months = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT',$
          'NOV','DEC']
;
procuc = strupcase(proc)
;
utbeg = fltarr(3)                 ;year, day number of year, seconds of day
utend = utbeg
;
if ((procuc eq 'US') or (procuc eq 'MPE')) then begin
  check = 'OBI'
  if (procuc eq 'MPE') then check = 'OBS_'
  gethistval,hdr,check,value,lnum,ch=0
  pos = max(lnum)
  if (pos lt 0) then begin
    print,' This header does not contain OBI info. Returning.'
    retall
  endif
endif
;
case procuc of
  'US':  begin
         token = '='
         gethistval,hdr,'OBI start time in',sctbeg,lnum,tok=token,ch=0
         if ( (n_elements(lnum) gt 1) or (lnum(0) lt 0) ) then begin
           print,' This is not an OBI header. Use a different procedure ', $ 
                 '(rs_seqinfo?)'
         endif else begin
           gethistval,hdr,'OBI end time in',sctend,tok=token,ch=0 
;
           gethistval,hdr,'OBI start time UTC',utb,lnum,tok=token,ch=0
           yy = fix(utb/1.e4)  
           mm = fix( (utb-yy*1.e4)/1.e2)
           dd = fix(utb-yy*1.e4-mm*1.e2)
           yy = yy + 1900
           dayn = ymd2dn(yy,mm,dd)
           utbeg(0) = yy
           utbeg(1) = dayn
;
           gethistval,hdr,'OBI end time UTC',utb,lnum,tok=token,ch=0
           yy = fix(utb/1.e4)  
           mm = fix( (utb-yy*1.e4)/1.e2)
           dd = fix(utb-yy*1.e4-mm*1.e2)
           yy = yy + 1900
           dayn = ymd2dn(yy,mm,dd)
           utend(0) = yy
           utend(1) = dayn
;
           gethistval,hdr,'OBI start within',utsec,lnum,tok=token,ch=0
           utsec = utsec/1000.
           utbeg(2) = utsec
           gethistval,hdr,'OBI end within',utsec,lnum,tok=token,ch=0
           utsec = utsec/1000.
           utend(2) = utsec
         endelse
         end
;
  'MPE': begin
         nhdr = n_elements(hdr)
         token = '='
         gethistval,hdr,'OBS_CLOCK',value,lnum,tok=token,ch=0
         if ( (n_elements(lnum) gt 1) or (lnum(0) lt 0) ) then begin
           print,' This is not an OBI header. Use a different procedure ', $ 
                 '(rs_seqinfo?)'
         endif else begin
           htry = strtrim( hdr(lnum(0)+1),2 )
           try = gettok(htry,' ')
           htry = strtrim(htry,2)
           try = gettok(htry,' ')
           value = double(try)
           sctbeg = value
           sctend = double(htry)
;
           gethistval,hdr,'OBS_DATE',utb,lnum,tok=token,ch=0
           htry = strtrim( hdr(lnum(0)+1),2 )
           try = gettok(htry,' ')
           htry = strtrim(htry,2)
           try = gettok(htry,' ')
           yy = fix(strmid(try,7,4))
           mm = strmid(try,3,3)
           im = where(months eq mm)
           mm = im + 1
           dd = fix(strmid(try,0,2))
           dayn = ymd2dn(yy,mm,dd)
           utbeg(0) = yy
           utbeg(1) = dayn
;
           yy = fix(strmid(htry,7,4))
           mm = strmid(htry,3,3)
           im = where(months eq mm)
           mm = im + 1
           dd = fix(strmid(htry,0,2))
           dayn = ymd2dn(yy,mm,dd)
           utend(0) = yy
           utend(1) = dayn
;
           gethistval,hdr,'OBS_UT',utsec,lnum,tok=token,ch=0
           htry = strtrim( hdr(lnum(0)+1),2 )
           try = gettok(htry,' ')
           htry = strtrim(htry,2)
           try = gettok(htry,'  ')
           hrut = fix(strmid(try,0,2))
           minut = fix(strmid(try,3,2))
           secut = float( strmid(try,6,strlen(try)-6) )
           utsec = hrut*3600. + minut*60. + secut
           utbeg(2) = utsec
;
           htry = strtrim(htry,2)
           hrut = fix(strmid(htry,0,2))
           minut = fix(strmid(htry,3,2))
           secut = float( strmid(htry,6,strlen(htry)-6) )
           utsec = hrut*3600. + minut*60. + secut
           utend(2) = utsec
         endelse
         end
;
  'RDF': begin
         nobi = fxpar(hdr,'NUM_OBIS')       ;number of OBIs
         sctbeg = dblarr(nobi)
         sctend = sctbeg
         utbeg = dblarr(3,nobi)
         utend = utbeg
;
         for jj=0,nobi-1 do begin
           ftype = 'OBIB'+strtrim(string(jj+1),2)
           value = fxpar(hdr,ftype)
           sctbeg(jj) = double(value)
;
           ftype = 'OBIE'+strtrim(string(jj+1),2)
           value = fxpar(hdr,ftype)
           sctend(jj) = double(value)
;
           ftype = 'BTIM'+strtrim(string(jj+1),2)
           value = fxpar(hdr,ftype)
           hh = gettok(value,':')
           mm = gettok(value,':')
           utsec = hh*3600. + mm*60. + value
;
           ftype = 'BDAY'+strtrim(string(jj+1),2)
           value = fxpar(hdr,ftype)
           dd = gettok(value,'/')
           mm = gettok(value,'/')
           yy = 1900 + value
           dayn = ymd2dn(yy,mm,dd)
;
           utbeg(0,jj) = yy
           utbeg(1,jj) = dayn
           utbeg(2,jj) = utsec
;
           ftype = 'ETIM'+strtrim(string(jj+1),2)
           value = fxpar(hdr,ftype)
           hh = gettok(value,':')
           mm = gettok(value,':')
           utsec = hh*3600. + mm*60. + value
;
           ftype = 'EDAY'+strtrim(string(jj+1),2)
           value = fxpar(hdr,ftype)
           dd = gettok(value,'/')
           mm = gettok(value,'/')
           yy = 1900 + value
           dayn = ymd2dn(yy,mm,dd)
;
           utend(0,jj) = yy
           utend(1,jj) = dayn
           utend(2,jj) = utsec
         endfor          ;of getting OBI start and stop times for each OBI
         end
;
   else: begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         end
endcase
;
return
end         ;pro rsobitimes
