;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   getshc
;
;*PURPOSE:
;   A procedure reads spherical harmonic coefficients from the specified     
;   file into an array.                                          
;
;*CALLING SEQUENCE:
;   getshc,fspec,NMAX,ERAD,SH,IER, dir=dir
;
;*PARAMETERS:
; INPUTS:
;      fspec   - File specification                               
;
; OPTIONAL INPUTS:
;      dir     - directory containing the magnetic field data files
;
; OUTPUTS:
;      nmax    - Maximum degree and order of model                
;      erad    - Earth's radius associated with the spherical harmonic 
;                coefficients, in the same units as elevation
;      gh      - Schmidt quasi-normal internal spherical harmonic coefficients
;      ier     - Error number: =  0, no error
;                              = -2, records out of order
;                              = FORTRAN run-time error number    
;
;*RESTRICTIONS:
;
;*NOTES:
;
;    Uses system variable !igrfdir as default directory for detector
;    maps. Upon entering IDL, !igrfdir is set to getenv('ZAUX') by
;    rosatlib.pro.
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted  22 Jan 1994 (GAR) from Fortran code FELDCOF.FOR from NSSDC
;    written by A. Zunde
;    USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225    
;    modified 02 Feb 1994 (GAR) to use system variable !igrfdir = default 
;    directory for IGRF data files, so user can change this within IDL
;    !igrfdir is initially set to getenv('ZAUX') by ROSATLIB upon
;    starting IDL 
;-
;-------------------------------------------------------------------------------
pro getshc,fspec,nmax,erad,gh,ier,dir=dir
;
;  default directory for IGRF data files !igrfdir is set to getenv('ZAUX') 
;  upon entering IDL
;  (within the files $ZCOM/.idlrc or ZCOM:idldef.com)
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' GETSHC, fspec, NMAX, ERAD, GH, IER, dir=dir'
  retall
endif
if (n_elements(dir) eq 0) then dir = ''
if (!debug gt 1) then print,dir
;
defdir = !igrfdir
if (!version.os ne 'vms') then defdir = defdir + '/'
if (dir eq '') then dir = defdir
;
;  Open coefficient file. Read past first header record.        
;  Read degree and order of model and Earth's radius.           
;
openr,iu,dir+fspec,/get_lun
dumstr = ''
readf,iu,dumstr
readf,iu,nmax,erad
;
;  Read the coefficient file, arranged as follows:              
;
;					N     M     G     H          
;					----------------------       
;				    /   1     0    GH(1)  -          
;				   /	1     1    GH(2) GH(3)       
;				  /	2     0    GH(4)  -          
;				 /	2     1    GH(5) GH(6)       
;	    NMAX*(NMAX+3)/2 	/	2     2    GH(7) GH(8)       
;	       records		\	3     0    GH(9)  -          
;				 \      .     .     .     .          
;				  \	.     .     .     .          
;	    NMAX*(NMAX+2)	   \	.     .     .     .          
;	    elements in GH	    \  NMAX  NMAX   .     .          
;                                                                               
;	N and M are, respectively, the degree and order of the       
;	coefficient.                                                 
;
i = -1
ier = 0
gh = fltarr(nmax*(nmax+2))                   ;number of elements in gh
for nn=1,nmax do begin                                              
  for mm=0,nn do begin                                            
    readf,iu,n,m,g,h         
    if ((nn ne n) or (mm ne m)) then begin                   
      ier = -2                                         
      close,iu
      free_lun,iu
      return
    endif
;
    i = i + 1                                            
    gh(i) = g                                            
    if (m ne 0) then begin
      i = i + 1                                        
      gh(i) = h
    endif                                                
  endfor
endfor
;
close,iu
free_lun,iu
return                                                       
end        ;pro getshc                                                          
