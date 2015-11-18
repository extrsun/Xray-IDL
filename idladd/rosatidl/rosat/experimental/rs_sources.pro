;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rs_sources
;
;*PURPOSE:
; 	Read SASS output from sources file into IDL Version 2 Structure
;
;*CALLING SEQUENCE:
;	RS_SOURCES, inputs, HDR, STRUCT, OPARMS=OPARMS
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
;       ZDEF:RS_SOURCES.DEF.
;
;       The following parameters are availble.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        DATYP       Type of data to be read from the file
;                    SRC (1st ext, = def), SKY (2nd ext), FIT (3rd ext), 
;                    VARY (4th ext), BKSP (5th ext), BKRT (6th ext)
;                    Note: Only 'src' and 'sky' are allowed for US format files
;                          &/or HRI files
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE, RDF)
;                    (default is 'US')
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;
; OUTPUTS:
;       hdr    -  FITS table header (string array)
;	struct -  IDL structure with info from file 
;       oparms  - string array which is the parsed form of inputs (same as
;                 LIST in example 3 below). Allows program to be rerun using 
;                 same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rs_sources,1,hsrc,srcdat
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?datyp=sky
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,datyp=sky,chatter=0'
;        IDL>rs_sources,list,hsrc,srcdat
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='sky'
;        IDL>list(9)='0'
;        IDL>rs_sources,list,hsrc,srcdat
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rs_sources,'myinput.dat',hsrc,srcdat
;
;*RESTRICTIONS:
;  Does not work yet for MPE format data.
;  Do not specify keyword EXTNUM for US format data.
;
;*NOTES:
;  Writing privileges are not required.
;
;  The previous problem with fields 27 & 28 in the SASS source list file
;     appears to be due to a disagreement in what the value of tnull should
;     be. FTGET sets tnull to '************** ' (14 *s followed by a blank), 
;     but the null values in fields 27 & 28 are '***************' (15 *s).
;     This has temporarily been fixed by a new version of ftget
;     in the experimental library.
;
;  Now reads FITS file directly. Do NOT convert to pseudo SDAS format!
;  Input parameters INSTR and TRIM are ignored (and need not be specified).
;
;*SUBROUTINES CALLED:
;  OLD_SOURCES
;  RDF_SOURCES
;  READFITS  (IDL Astronomical Users' Library)
;  ZPARCHECK       "                "
;  SXOPEN          "                "
;  SXHREAD         "                "
;  FXPAR           "                "
;  SXREAD          "                "
;  REMCHAR         "                "
;  GETTOK          "                "
;  CREATE_STRUCT
;  FITS_GET
;  FTGET    (IDL Astronomical Users' Library)
;  FTSIZE          "                "
;  FTINFO          "                "
;  TBGET
;  TBSIZE
;  TBINFO
;  ADSTRING        "                "
;  RADEC           "                "
;  NINT            "                "
;
;*MODIFICATION HISTORY:
;  Version 1.0 RAS January 1992
;  Modified 26 Feb 1992 for Rosat IDL Library (GAR). Combines rs_simbad
;    and previous rs_sources.
;  Modified 06 Jul 1992 to use READFITS and new version of MATCH_FILES;
;    and to read type of structure from header (GAR)
;  Modified 21 Apr 1993 to search for files temp_*.pro before trying to 
;    delete them
;  Modified 10 Sep 1993 (GAR) to work with new version of CREATE_STRUCT
;  modified 24 Dec 1993 (GAR) to read data from RDF format source analysis
;    files: old RS_SOURCES was renamed to OLD_SOURCES; new code now
;    calls OLD_SOURCES and the corresponding RDF_SOURCES for reading RDF 
;    format files. Also now uses the parameter interface (INPUTS).
;  modified 27 Dec 1993 (GAR): OLD_SOURCES no longer calls create_struct;
;    instead structures sasslist & skylist are explicitly defined.
;    The temporary .pro file is no longer created, so writing privileges
;    are no longer required.
;-
;-------------------------------------------------------------------------------
pro rs_sources,inputs,hdr,struct,oparms=oparms
;
; procedure to read source information and return IDL structure
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RS_SOURCES, inputs, HDR, STRUCT, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, DATYP, INSTR, PROC, and CHATTER ' $
       +'from RS_SOURCES.DEF'
  print,'  '
  print,'   Choices for DATYP are '
  print,'           sky & src only for HRI and/or US format' 
  print,'           sky, src, fit, vary, bksp, bkrt for PSPC.'
  retall
endif
;
dfile = 'rs_sources'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
datyp = strtrim(oparms(2),2)
datyp = strlowcase(datyp)
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
case 1 of
  proc eq 'US':  begin
;
     case datyp of
       'src':  filename = dir+obseq+'_'+datyp+'.fits'
       'sky':  filename = dir+obseq+'_'+datyp+'.fits'
       else:   begin
               print,' Sorry, data type ',datyp,' not supported for US format.'
               print,' Please check your inputs. Returning.'
               end
     endcase
;
     case npar of
       3: old_sources,filename,hdr,struct
       2: old_sources,filename,hdr
       else: begin
             print,' RS_SOURCES needs at least 2 parameters to run. Returning.'
             retall
             end
      endcase
      end         
;
  proc eq 'RDF': begin
;
     filename = dir+obseq+'_src.fits'                ;for all data types
     case npar of
       3: rdf_sources,filename,hdr,struct,instr=instr,datyp=datyp
       2: rdf_sources,filename,hdr,instr=instr,datyp=datyp
       else: begin
             print,' RS_SOURCES needs at least 2 parameters to run. Returning.'
             retall
             end
     endcase
     end
;
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, RDF. Returning.'
         retall
         end
endcase
;
return
end         ;pro rs_sources
