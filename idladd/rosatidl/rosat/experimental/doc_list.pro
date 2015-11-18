;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;		doc_list
;
;*PURPOSE:
;   A procedure to provide a list of useful procedures and 
;   summaries of what they do.
;
;*CALLING SEQUENCE:
;       doc_list, qual, project=project, docdir=docdir
;
;*PARAMETERS:
; INPUTS:
;	qual    - string variable giving general topic (e.g., 'fits')
;       project - string keyword giving project for which routines are
;                 desired (default = 'ROSAT')
;       docdir  - directory containing the .LST files  (default is 
;                 getenv('ZDOC'))
;
;*EXAMPLES:
;       doc_list,'all'     gives a one line summary for all topics for the
;                          default project (Rosat)
;
;       doc_list,'fits_3D'    gives a more complete description for Rosat FITS
;                             related procedures
;
;       doc_list,project='ROSAT'   gives a list of topic options for project
;                                  ROSAT
;       doc_list              gives a list of currently supported projects
;
;*RESTRICTIONS:
;       So far, only project='ROSAT' or 'IDLAUL' are allowed.
;
;*NOTES:
;
;*PROCEDURE:
;
;*MODIFICATION HISTORY:
;    written 06 May 1991 by GAR
;    revised 17 June 1991 by GAR to include IDL AUL procedures
;    modified 05 Nov 1991 for compatibility with Sun Unix (GAR)
;    revised 03 Oct 1993 (GAR) to use system variable !docdir (set to
;      getenv('ZDEF') upon entering IDL)
;    last updated 22 Nov 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro doc_list,qual,project=project,docdir=docdir
;
projopts = ['ROSAT','IDLAUL']
if (n_elements(docdir) eq 0) then docdir = ''
if (docdir eq '') then zdoc = !docdir else zdoc = docdir
if (!version.os ne 'vms') then zdoc = zdoc+'/'
;
setproj = n_elements(project)
npar = n_params(0)
if (npar eq 0) and (setproj eq 0) then begin
  print,' doc_list, [qual, project=project (def=ROSAT)]
  print,'  '
  print,' Use this procedure to get a list of useful IDL procedures',$
      ' for various projects'
  print,' Currently, the following projects are supported:
  print,format='(9(A6,2x))',projopts
  print,'  '
  return
endif
;
if (npar ne 0) and (setproj eq 0) then project = 'ROSAT'  ; default project
project = strupcase(project)
;
case (project) of
'ROSAT': begin
   qopts = ['fits_3d','fits_aul','misc','cal','emap','orb','data','spat',$
            'spec','tim']
   filopts = ['rosat_fits_3d.lst','rosat_fits_aul.lst','rosat_misc.lst'$
             ,'rosat_calib.lst','rosat_emap.lst','rosat_orbit.lst'$
             ,'rosat_data.lst','rosat_spatial.lst','rosat_spectral.lst'$
             ,'rosat_timing.lst']
  end
'IDLAUL': begin
   qopts = ['util','astr','daoph','datab','dtio','fitsio','fitsa'$
           ,'fitsb','grdisp','img','imdisp','math','misc','plot'$
           ,'sdasa','sdasb','spec','struc','time','tv','jhuapl']
   filopts = ['aul_astro.lst','aul_astrom.lst','aul_daophot.lst'$
             ,'aul_database.lst','aul_disktapeio.lst','aul_fitsio.lst'$
             ,'aul_fits_asc.lst','aul_fits_bin.lst','aul_graphdev.lst'$
             ,'aul_image.lst','aul_imgdisplay.lst','aul_math.lst'$
             ,'aul_misc.lst','aul_plot.lst','aul_sdas_asc.lst'$
             ,'aul_sdas_bin.lst','aul_spectra.lst','aul_structure.lst'$
             ,'aul_time.lst','aul_tv.lst','aul_jhuapl.lst']
   end
else: print,project,' is not currently supported'
endcase
nopts = n_elements(qopts)
;
if (npar eq 0) and (setproj ne 0) then begin    ;print list of options
  qual_options,project,docdir=zdoc
  return
endif
;
for jj=0,nopts-1 do begin
  file = filopts(jj)
  if ( (strupcase(qual) eq 'ALL') or (qual eq qopts(jj)) ) then $
  type,zdoc+file
endfor
;
return
end        ;pro doc_list
