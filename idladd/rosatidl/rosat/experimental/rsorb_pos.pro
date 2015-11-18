;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsorb_pos
;
;*PURPOSE:
;   A procedure to read satellite orbital positions and other data 
;   from US or MPE format Rosat orbit (.SO) FITS files, or RDF format 
;   ancillary files
;
;*CALLING SEQUENCE:
;       rsorb_pos, inputs, obinum, SCTIME, SATPOS, ORBVECS, RELPOS,
;                  OBINFO=OBINFO, OPARMS=OPARMS
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
;                    (default is SO)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is 'US')
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = 1)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;
; OPTIONAL INPUTS:
;       obinum - vector containing numbers of OBIs to be read (0 for all)
;
; OUTPUTS:
;       sctime   - spacecraft times for which orbital positions were determined
;       satpos   - data structure containing the following:
;         lon    - satellite orbit longitudes (decimal degrees)
;         lat    - satellite orbit latitudes (decimal degrees)
;         height - height of satellite above Earth (in kilometers)
;       has the structure of replicate(row,npos), where
;       row={sporblonlat,lon:0.0,lat:0.0,height:0.0}
;       and npos is the total number of measurements
;
;       orbvecs - data structure containing the unit vectors pointing
;                 from the center of the Earth to the Sun, Moon, and 
;                 satellite. The structure is as follows:
;         sunx  - x component of unit vector
;         suny  - y component   "        "
;         sunz  - z component   "        "
;         moonx - x component of unit vector
;         moony - y component   "        "
;         moonz - z component   "        "
;         satx  - x component of unit vector
;         saty  - y component   "        "
;         satz  - z component   "        "
;       has the structure of replicate(row,npos), where
;       row={sporbvecs,sunx:0.0,suny:0.0,sunz:0.0,moonx:0.0,moony:0.0,$
;            moonz:0.0,satx:0.0,saty:0.0,satz:0.0}
;       and npos is the total number of measurements
;
;       relpos - data structure containing various satellite positions
;                relative to the Weilheim station, etc.:
;                (It's still not clear to me what az & el are)
;         az   - azimuth of satellite (decimal degrees)
;         el   - elevation of satellite (decimal degrees)
;         gha  - Greenwich Hour Angle of satellite (decimal degrees)
;         dist - Distance of satellite to Weilheim station (kilometers)
;       has the structure of replicate(row,npos), where
;       row={sporbazel,az:0.0,el:0.0,gha:0.0,dist:0.0}
;       and npos is the total number of measurements
;
;       obinfo - data structure containing dates, times, & indices of
;                beginnings & ends of obi segments
;                has the structure of replicate(row,num), where
;                row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                     sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                     ibeg:0L,iend:0L}
;                and num is the total number of intervals
;         ibeg - indices of values in SCTIME equal to values in SCTBEG
;         iend - indices of values in SCTIME equal to values in SCTEND
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsorb_pos,1,obinum,sctime,satpos, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=P
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=P,chatter=0'
;        IDL>rsorb_pos,list,obinum,sctime,satpos, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(3)='P'
;        IDL>list(9)='0'
;        IDL>rsorb_pos,list,obinum,sctime,satpos, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsorb_pos,'myinput.dat',obinum,sctime,satpos, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Will read FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameter TRIM is ignored (and need not be specified).
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  FITS_INFO
;  READFITS
;  TBGET
;  RSOBITIMES
;  YMD2DN
;  UT2SCT
;
;*MODIFICATION HISTORY:
;    written 30 Apr 1991 by GAR
;    modified to use match_files 17 May 1991 by GAR
;    modified 28 Aug 1991 by GAR to handle observations spanning > 1 day
;                                and to return IBEG 
;    modified 30 Aug 1991 by GAR to allow selection of OBIs to be read
;    modified 11 Sep 1991 by GAR to work under XWindows emulator
;    modified 17 Sep 1991 by GAR to put calculation of sctbeg into
;                                subroutine RSOBITIMES
;    modified 30 Sep 1991 by GAR because HRI field names are not same as
;                                field names for PSPC
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified to combine sctbeg & obibeg, sctend & obiend, and to make into
;        keywords  08 Oct 1991  GAR
;    modified 08 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 01 Mar 1992 to return various outputs in data structures (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new version
;        of MATCH_FILES, and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 26 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET)
;    modified 10 Aug 1993 (GAR) to use extension for archival MPE format 
;      data
;    modified 21 Dec 1993 (GAR) to read orbital data from RDF format 
;      ancillary files: old RSORB_POS was renamed to OLDORB_POS; new code now
;      calls OLDORB_POS and the corresponding RDFORB_POS for reading RDF 
;      format files; also to return spacecraft times (instead of UT times)
;-
;-------------------------------------------------------------------------------
pro rsorb_pos,inputs,obinum,sctime,satpos,orbvecs,relpos,obinfo=obinfo,$
              oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSORB_POS, inputs, obinum (0 for all), SCTIME, SATPOS, ORBVECS, '
  print,'            RELPOS, OBINFO=OBINFO, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER ' $
       +'from RSGET.DEF'
  retall
endif
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
instr = strtrim(oparms(3),2)
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
instr = strtrim(strupcase(instr),2)
if ( (instr ne 'P') and (instr ne 'H')) then begin                  
  instr = ''
  read,' Please specify instrument - P = PSPC, H = HRI  ',instr
  instr = strupcase(instr)
  oparms(3) = instr
endif
;
case 1 of
  (proc eq 'US') or (proc eq 'MPE'):  begin
     case npar of
       6: oldorb_pos,oparms,obinum,time,satpos,orbvecs,relpos,obinfo=obinfo
       5: oldorb_pos,oparms,obinum,time,satpos,orbvecs,obinfo=obinfo
       4: oldorb_pos,oparms,obinum,time,satpos,obinfo=obinfo
       3: oldorb_pos,oparms,obinum,time,obinfo=obinfo
       else: begin
             print,' RSORB_POS needs at least 3 parameters to run. Returning.'
             retall
             end
      endcase
;
;  OLDORB_POS returns UT times TIME, not spacecraft times SCTIME
;  Use info in OBINFO & UT2SCT to convert
;
      sctime = ut2sct(time,obinfo)
      end         
;
  (proc eq 'RDF'): begin
     case npar of
       6: rdforb_pos,oparms,obinum,sctime,satpos,orbvecs,relpos,obinfo=obinfo
       5: rdforb_pos,oparms,obinum,sctime,satpos,orbvecs,obinfo=obinfo
       4: rdforb_pos,oparms,obinum,sctime,satpos,obinfo=obinfo
       3: rdforb_pos,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSORB_POS needs at least 3 parameters to run. Returning.'
             retall
             end
     endcase
;
;  RDFORB_POS returns satellite height and distance in meters, not kilometers.
;  Also, the Earth-satellite vector has not been divided by its length
;
     if (npar ge 4) then satpos.height = satpos.height/1000. 
     if (npar ge 5) then begin
       satx = orbvecs.satx/1.D8
       saty = orbvecs.saty/1.D8
       satz = orbvecs.satz/1.D8
       length = sqrt(satx*satx+saty*saty+satz*satz)  ;length of vector
       orbvecs.satx = satx/length
       orbvecs.saty = saty/length
       orbvecs.satz = satz/length
     endif       
     if (npar ge 6) then relpos.dist = relpos.dist/1000.
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
end         ;pro rsorb_pos
