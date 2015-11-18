;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;        tsilist
;
;*PURPOSE:
; A procedure to list the Temporal Status Intervals flags for a Rosat
; events list (FITS) file
;
;*CALLING SEQUENCE:
; tsilist,name,tsibeg,tsiend,failed,status,telqual,textout=textout
;
;*PARAMETERS:
; INPUTS:
;   name        File name of input TSI file
;   tsibeg      Start time of interval
;   tsiend      End time of interval
;   failed      String array giving ASCII translation of bit-encoded
;               failed flags for each interval. If no flags were set,
;               then FAILED(ii) = 'Good Time Interval'
;   status      String array giving ASCII translation of status
;               settings. According to the Data Products Guide, these
;               are the settings for determining whether flags are set.
;   telqual     String array giving ASCII translation of telemetry
;               quality status settings.
;
; OUTPUT PARAMETERS:
;    The following dev/file is opened for output.
;
; 		textout=1	SYS$OUTPUT TERMINAL
;		textout=2	SYS$PRINT PRINTER
;		textout=3	<PROGRAM>.PRT
;		textout=4	LASER.TMP FILE 
;		textout=5	USER MUST OPEN FILE
;		textout= filename.tmp
;
;*MODIFICATION HISTORY:
;  written 27 Nov 1991 by GAR
;  modified 05 Jun 1992 to work with new version of GETTOK (GAR)
;-
;-------------------------------------------------------------------------------
pro tsilist,name,tsibeg,tsiend,failed,status,telqual,textout=textout
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' TSILIST, name, tsibeg, tsiend, failed, status, telqual, '+$
        'textout=textout'
  retall
endif
if (n_elements(textout) eq 0) then textout=!TEXTOUT   ;use default output
;
textopen,textout=textout,'tsilist'
printf,!TEXTUNIT,'  '
printf,!TEXTUNIT,form='(a)',$
       'Listing of TSI Failed, Status, and Telemetry Flags for'
printf,!TEXTUNIT,form='(a,/)',name
;
ntsi = n_elements(tsibeg)
;
for jj=0,ntsi-1 do begin
  failhdr = '     Data fail flags: '
  stathdr = '     Status    flags: '
  telhdr =  '     Telemetry flags: '
;
  failflag = strtrim(failed(jj),2)
  statflag = strtrim(status(jj),2)
  telflag =  strtrim(telqual(jj),2)
;
  printf,!TEXTUNIT,form='$(1x,a9,i4,2x,a7,f14.2,2x,a6,f14.2,2x,a6,f10.1)',$
         'Interval ',jj+1,'Start =',tsibeg(jj),'Stop =',tsiend(jj),$
         'Time =',tsiend(jj)-tsibeg(jj)
;
  totlen = strlen(failflag)
  nlines = fix(totlen/54) + 1
  while (nlines gt 1) do begin      ;more than one line left to print
    save = failflag
    prlen = 0
    flstr = ''
    trylen = 1
    while (prlen le 54) do begin
      try = ' '+gettok(save,',')
      trylen = strlen(try)
      prlen = trylen + prlen
      if (prlen le 54)then begin     ;keep going to next flag
        flstr = flstr + try + ','    
        failflag = save
      endif
    endwhile
    printf,!TEXTUNIT,form='(a)',failhdr+strtrim(flstr,2)
    failhdr = '                      '
    nlines = nlines-1
  endwhile
  flstr = gettok(failflag,'//')
  printf,!TEXTUNIT,form='(a)',failhdr+strtrim(flstr,2)
;
  totlen = strlen(statflag)
  nlines = fix(totlen/54) + 1
  while (nlines gt 1) do begin      ;more than one line left to print
    save = statflag
    prlen = 0
    flstr = ''
    trylen = 1
    while (prlen le 54) do begin
      try = ' '+gettok(save,',')
      trylen = strlen(try)
      prlen = trylen + prlen
      if (prlen le 54)then begin     ;keep going to next flag
        flstr = flstr + try + ','    
        statflag = save
      endif
    endwhile
    printf,!TEXTUNIT,form='(a)',stathdr+strtrim(flstr,2)
    stathdr = '                      '
    nlines = nlines-1
  endwhile
  flstr = gettok(statflag,'//')
  printf,!TEXTUNIT,form='(a)',stathdr+strtrim(flstr,2)
;
  totlen = strlen(telflag)
  nlines = fix(totlen/54) + 1
  while (nlines gt 1) do begin      ;more than one line left to print
    save = telflag
    prlen = 0
    flstr = ''
    trylen = 1
    while (prlen le 54) do begin
      try = ' '+gettok(save,',')
      trylen = strlen(try)
      prlen = trylen + prlen
      if (prlen le 54)then begin     ;keep going to next flag
        flstr = flstr + try + ','    
        telflag = save
      endif
    endwhile
    printf,!TEXTUNIT,form='(a)',telhdr+strtrim(flstr,2)
    telhdr = '                      '
    nlines = nlines-1
  endwhile
  flstr = gettok(telflag,'//')
  printf,!TEXTUNIT,form='(a)',telhdr+strtrim(flstr,2)
;
  printf,!TEXTUNIT,'  '
endfor
;
textclose
return
end           ;pro tsilist          
