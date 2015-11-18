;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME: RSGETPAR
;
;*PURPOSE: Parameter processing routine.
;
;*CALLING SEQUENCE:
;       RSGETPAR,parms,dfile,PAR,nmax
;
;*PARAMETERS:
; INPUT:
;       parms - user parameter
;               = 0             use all defaults
;               = 1             interative
;               = array         user supplied values
;               = 'name=val,...'  user supplied non-defaults
;               = '$filename'     non-default parameter file
;       dfile - file name containing defaults
;               must be in directory specified by ZDEF:
;       nmax  - maximum parameter number of parameters to be returned
;
; OUTPUT:
;       par - parameter array (string)
;
;*MODIFICATION HISTORY:
;      taken from GHRS routine GETPAR written by D. Lindler, JFK, ACC
;      05 Oct 1991
;      modified 13 Apr 1992 to return up to parameter nmax for mode=4
;      (interactive mode)  (GAR)
;-
;-------------------------------------------------------------------------------
pro rsgetpar,parms,dfile,par,nmax
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'RSGETPAR, parms, dfile, PAR, nmax'
  retall
endif
;
; determine type of parameter processing
;
sz=size(parms)
if (sz(0) eq 0) then begin                      ;scalar?
  if (sz(1) eq 7) then begin                    ;string?
    if (strpos(parms,'=') lt 0) then $          ;file name?
       mode=1 else $                            ;mode 1 - filename
       mode=2                                   ;mode 2 - parameter string
  endif else begin                              ;non string scalar
    if (parms eq 0) then mode=3 else $          ;mode 3 - use all defaults
       mode=4                                   ;mode 4 - interative
  endelse
endif else begin
  mode=5                                        ;parameter array
endelse
;
case mode of
; 
  1: begin                              ;get from file
     rsgetdefpar,dfile,names,par        ;get defaults - will want all param
     get_lun,unit
     openr,unit,parms
     st=''
     while (not eof(unit)) do begin
       readf,unit,st
       if (strmid(st,0,1) ne ';') then $         ;comment?
          setparval,st,names,par                 ;set new value
;
       if (!err lt 0) then begin                 ;invalid parameter
         print,'File with invalid parameter=',parms
         close,unit
         free_lun,unit
         retall
       endif
     endwhile
     close,unit
     free_lun,unit
  end; mode 1
; 
  2: begin                              ;parameter string supplied
     rsgetdefpar,dfile,names,par        ;get defaults - will want all param
     pstring=parms
     while (strlen(pstring) ne 0) do begin
       st=gettok(pstring,',')           ;get next parameter
       rssetparval,st,names,par         ;set par value
       if (!err lt 0) then begin        ;invalid parameter
         print,'Bad parameter string:'
         print,'   ',parms
         retall
       endif
    endwhile
  end; mode 2
; 
  3: rsgetdefpar,dfile,names,par        ;use all defaults
; 
  4: begin                                   ;interactive
     rsgetdefpar,dfile,names,par,descr,nmax  ;read defaults
     num=n_elements(names)                   ;number of parameters
     command=''
     while (command ne 'END') do begin
       if (command eq '') then begin    ;print default values
         x=strarr(num+1)
         ngood=0
         for i=0,num-1 do begin
           if (names(i) ne '') then begin
;            pp = '                    '
             pp = string(bytarr(20)+32b)       ; avoid IDL 2.1.1 problem
             strput,pp,par(i),0
             if (strtrim(names(i)) ne '') then pp='='+pp else pp=' '+pp
             x(ngood)=' '+string(names(i),format='(A10)')+pp
             ngood=ngood+1
           endif
         endfor
;
         ngood2 = (ngood+1)/2
         for ii=0,ngood2-1 do print,x(ii)+x(ii+ngood2)
;        close,unit
;        free_lun,unit
       endif         ; command=''
;
       if (command eq 'HELP') then begin          ;print descriptions
;        get_lun,unit
;        openw,unit,'sys$output',/stream,/more
         for i=0,num-1 do begin
           if (names(i) ne '') then begin
;            pp = '                    '
             pp = string(bytarr(20)+32b)          ; avoid IDL 2.1.1 problem
             if (strtrim(names(i)) ne '') then strput,pp,'=',0
             strput,pp,par(i),1
             st = ' '+string(names(i),format='(a10)')+pp+ ' '+ descr(i)
;            printf,unit,st
             print,st
           endif
         endfor ;i
;        close,unit
;        free_lun,unit
       endif
;
       if (command eq 'HELP') or (command eq '') then begin  ; get next command
         print,' '
         print,'Enter parameters:  name=value'
         print,'Enter HELP for description of parameters, END when done'
         print,'Hit <CR> to review values, or type:   name=?'
       endif
       read,'? ',command
;
       npos = strpos(command,'=')
       if (npos ne -1) then begin                    ;is it a new parameter?
         if (strpos(command,'?') ne -1) then begin   ;no, print value
           ipos = -1
           try = strupcase(strmid(command,0,npos))
           for i=0,num-1 do if (try eq strtrim(names(i),2)) then ipos = i
           if (ipos ge 0) then print,names(ipos),'=',par(ipos)           
         endif else rssetparval,command,names,par    ;yes, it is new
       endif else command = strupcase(command)         
     endwhile
  end; mode=4
;
  5: begin                                       ;user supplied values
     sz=size(parms) & num=sz(1)
     par=strarr(num)
     for i=0,num-1 do par(i)=parms(i)
  end; mode 5
endcase
;
return
end              ;pro rsgetpar
