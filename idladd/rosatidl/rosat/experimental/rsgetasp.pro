;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsgetasp
;
;*PURPOSE:
;   A procedure to read aspect data (roll angle, changes in X and Y pointing)
;   from US or MPE format Rosat aspect FITS files, or RDF format ancillary
;   files
;
;*CALLING SEQUENCE:
;       RSGETASP, inputs, obinum, SCTIME, ROLL, DELX, DELY, NOMASP, 
;                 ASPERRS, SCASP, FLAGS, OBINFO=OBINFO, OPARMS=OPARMS
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (elements must be in correct order)
;                string of form '$filename' where filename is the
;                   name of the file containing one parameter
;                   per line in the form: parname=value
;       Any parameter not specified is set to its default.
;       Defaults for the parameters can be found by using interactive
;       selection params=1 or examinining the default (text) file,
;       ZDEF:RSGET.DEF.
;
;       The following parameters are availble.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        EXTYP       Extension of input file (e.g., CAS, ASP)
;                    (default is CAS)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is 'US')
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = 1)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;        READORB     Controls whether to read orbit data or not
;                    (default = 'N')
;
; OPTIONAL INPUTS:
;       obinum - vector containing numbers of OBIs to read (0 for all)
;
; OUTPUTS:
;       sctime - spacecraft times for which event rates were determined
;       roll - roll angle of detector (in degrees)
;       delx - x translation in NS system (in units of 0.5 arcsec)
;              = change in pointing RA relative to nominal (in pixels)
;       dely - y translation in NS system (in units of 0.5 arcsec)
;              = change in pointing Dec relative to nominal (in pixels)
;       nomasp - 2D array containing nominal RA, Dec and Roll for each
;                obi segment
;       asperrs - data structure containing RA, Dec, and Roll angle 
;                 uncertainties for each time of aspect measurement
;                 (in degrees for RA, Dec, Roll angle)
;                 has the structure of replicate(row,ndat) where
;                 row={aspecterrors,ra:0.0D0,dec:0.0D0,roll:0.0D0}
;                 and ndat is the total number of data points
;                 0 is returned for MPE format, US format CAS & SAS files
;
;       scasp   - data structure containing RA, Dec and Roll angles "of the
;                 spacecraft pointing" (uncorrected?) for each time of
;                 aspect measurement
;                 (in degrees for RA, Dec, Roll angle)
;                 has the structure of replicate(row,ndat) where
;                 row={scaspects,ra:0.0D0,dec:0.0D0,roll:0.0D0}
;                 and ndat is the total number of data points
;                 0 is returned for MPE, US format files
;
;       flags   - data structure containing aspect quality and star tracker 
;                 flags, and aspect status
;                 has the structure of replicate(row,ndat) where
;                 row={scaspflags,qual:0.0D0,stt:0.0D0,status:0}
;                 and ndat is the total number of data points
;
;       obinfo -  data structure containing dates, times, & indices of
;                 beginnings & ends of obi segments
;                 has the structure of replicate(row,num), where
;                 row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                      sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                      ibeg:0L,iend:0L}
;                 and num is the total number of intervals
;         ibeg  - indices of values in SCTIME equal to values in SCTBEG
;         iend  - indices of values in SCTIME equal to values in SCTEND
;
;       oparms  - string array which is the parsed form of inputs (same as
;                 LIST in example 3 below). Allows program to be rerun using 
;                 same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsgetasp,1,obinum,sct,roll,delx,dely, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?extyp=ao
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,extyp=ao,chatter=0'
;        IDL>rsgetasp,list,obinum,sctime,roll,delx,dely, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='ao'
;        IDL>list(9)='0'
;        IDL>rsgetasp,list,obinum,sctime,roll,delx,dely, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsgetasp,'myinput.dat',obinum,sctime,roll,delx,dely, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Reads FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameters INSTR and TRIM are ignored (and need not be specified).
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  OLDGETASP
;  RDFGETASP
;  MATCH_FILES
;  READFITS
;  FITS_INFO
;  FITS_GET
;  FTGET
;  TBGET
;  RS_NOMASP
;  RSOBITIMES
;  TABINV
;  NINT
;
;*MODIFICATION HISTORY:
;    written 17 July 1991 by GAR
;    modified 31 July 1991 by GAR to read HRI aspect data
;    modified 28 Aug 1991 by GAR to return IBEG
;    modified 30 Aug 1991 by GAR to allow selection of OBI numbers
;    modified 18 Sep 1991 by GAR to return sctbeg as keyword, to use
;                                subroutine RSOBITIMES, to return utbeg &
;                                utend as keywords
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified to combine sctbeg & obibeg, sctend & obiend, and to make into
;        keywords  08 Oct 1991  GAR
;    modified 5 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new version
;        of MATCH_FILES, and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 25 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET)
;    24 Oct 1992 (GAR) BUG IN DETERMINING ASPECT TIMES FIXED (i.e., where
;      there appeared to be series of duplicate times)
;    modified 10 Aug 1993 (GAR) to use extension for archival MPE format 
;      data
;    modified 13 Aug 1993 (GAR) to replace call to GETNOMASP with call to
;      RS_NOMASP
;    modified 16 Dec 1993 (GAR) to read aspect data from RDF format 
;      ancillary files: old RSGETASP was renamed to OLDGETASP; new code now
;      calls OLDGETASP and the corresponding RDFGETASP for reading RDF 
;      format files; also to return data structures ASPERRS & SCASP
;    modified 22 Dec 1993 (GAR) to return aspect quality & star tracker flags,
;      aspect status in data structure FLAGS
;-
;-------------------------------------------------------------------------------
pro rsgetasp,inputs,obinum,sctime,roll,delx,dely,nomasp,asperrs,scasp,$
             flags,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETASP, inputs, obinum (0 for all), SCTIME, ROLL, DELX, DELY,'
  print,'           NOMASP, ASPERRS, SCASP, FLAGS, OBINFO=OBINFO,', $ 
        ' OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER ' $
       +'from RSGET.DEF'
  return
endif
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF

proc = strupcase(proc)
if proc eq 'MPEUS' then proc='MPE' ;added by wqd, April, 1996
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
case 1 of
  (proc eq 'US') or (proc eq 'MPE'):  begin
     scasp = 0
     flags = 0
     case npar of
;
;  OLDGETASP does not return asqpect quality and star tracker flags. 
;  Read these using OLDGETASPQU. Use FILLASPQU to define these over the
;  times of aspect measurement.
;
      10: begin
          oldgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,asperrs,$
                    obinfo=obinfo
          if ( (instr eq 'P') and (proc eq 'US') ) then begin
            oparms2 = oparms
            oparms2(2) = ''
            oldgetaspqu,oparms2,obinum,sctqual,qualflag,starflag
            fillaspqu,sctime,sctqual,qualflag,starflag,qual,stt
            ndat = n_elements(sctime)
            row = {scaspflags,qual:0.0D0,stt:0.0D0,status:0}
            flags = replicate(row,ndat)
            if (ndat gt 1) then begin
              flags.qual = qual
              flags.stt = stt
            endif else begin
              flags.qual = qual(0)
              flags.stt = stt(0)
            endelse
          endif
          end
       9: oldgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,asperrs,$
                    obinfo=obinfo
       8: oldgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,asperrs,$
                    obinfo=obinfo
       7: oldgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,obinfo=obinfo
       6: oldgetasp,oparms,obinum,sctime,roll,delx,dely,obinfo=obinfo
       5: oldgetasp,oparms,obinum,sctime,roll,delx,obinfo=obinfo
       4: oldgetasp,oparms,obinum,sctime,roll,obinfo=obinfo
       3: oldgetasp,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETASP needs at least 3 parameters to run. Returning.'
             retall
             end
      endcase
      end         
;
  (proc eq 'RDF'): begin
     case npar of
       10: rdfgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,asperrs,$
                    scasp,flags,obinfo=obinfo
       9: rdfgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,asperrs,$
                    scasp,obinfo=obinfo
       8: rdfgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,asperrs,$
                    obinfo=obinfo
       7: rdfgetasp,oparms,obinum,sctime,roll,delx,dely,nomasp,obinfo=obinfo
       6: rdfgetasp,oparms,obinum,sctime,roll,delx,dely,obinfo=obinfo
       5: rdfgetasp,oparms,obinum,sctime,roll,delx,obinfo=obinfo
       4: rdfgetasp,oparms,obinum,sctime,roll,obinfo=obinfo
       3: rdfgetasp,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETASP needs at least 3 parameters to run. Returning.'
             retall
             end
     endcase
;
;  RDFGETASP returns all RA.s & Dec.s in degrees. Change this to 0.5 arcsec
;  pixels to be compatible with old RSGETASP (now OLDGETASP)
;
     rfact = 2.*3600.
     roll = float(roll)
     if (npar ge 4) then delx = long(rfact*delx)
     if (npar ge 5) then dely = long(rfact*dely)
     if (npar ge 8) then begin
       asperrs.ra = rfact*asperrs.ra
       asperrs.dec = rfact*asperrs.dec
     endif
     end
;
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase
;
return
end         ;pro rsgetasp
