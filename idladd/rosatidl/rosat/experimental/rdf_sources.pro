;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdf_sources
;
;*PURPOSE:
; 	Read SASS output from sources file into IDL Version 2 Structure
;
;*CALLING SEQUENCE:
;	RDF_SOURCES, filename, HDR, STRUCT, instr=instr, datyp=datyp
;
;*PARAMETERS:
; INPUTS:
;       filename -  Name of file (including directory). Assumes extension is
;                   '.fits' if not supplied.
;
; OPTIONAL INPUTS:
;       instr    -  Insrument. 'H' for HRI, 'P' for 'PSPC'. If not specified,
;                   assumes PSPC
;       datyp    -  Keyword giving type of data to be read.
;                   Valid options are 'src' for the source table (1st ext)
;                                     'sky' for the sky table (2nd ext)
;                                     'fit' for the fit results (3rd ext)
;                                     'vary' for the variab results (4th ext)
;                                     'bksp' for the bkg spectrum (5th ext)
;                                     'bkrt' for the bkg rate (6th ext)
;                   If not specified, then a default of 'src' is assumed.
;                   Note: Only 'src' and 'sky' are allowed for US format files
;                         &/or HRI files
;
; OUTPUTS:
;       hdr    -  FITS table header (string array)
;	struct -  IDL structure with info from file 
;
;*RESTRICTIONS:
;  Do not specify keyword EXTNUM for US format data.
;
;*NOTES:
;  Does not call CREATE_STRUCT
;
;*SUBROUTINES CALLED:
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
;  Written  27 Dec 1993 by GAR (Adapted from OLD_SOURCES)
;-
;-------------------------------------------------------------------------------
pro rdf_sources,filename,hdr,struct,instr=instr,datyp=datyp
;
; procedure to read source information and return IDL structure
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDF_SOURCES, filename, HDR, STRUCT, instr=instr, datyp=datyp'
  print,'  '
  print,'   Choices for DATYP are '
  print,'           "sky" & "src" only for HRI'
  print,'           "sky", "src", "fit", "vary", "bksp", "bkrt" for PSPC.'
  retall
endif
if (n_elements(instr) eq 0) then instr = ''
instr = strupcase(instr)
if (instr eq '') then instr = 'P'           ;default is PSPC
if (n_elements(datyp) eq 0) then datyp = ''
datyp = strlowcase(datyp)                   ;convert to lower case letters
if (datyp eq '') then datyp = 'src'         ;default data type = 'src'
;
if (instr eq 'H') then begin
  if ( (datyp ne 'src') and (datyp ne 'sky') ) then begin
    print,' Sorry, data type ',datyp,' not supported for HRI.'
    print,' Please check your inputs. Returning.'
    retall
  endif
endif
;
case datyp of
  'src':  extnum = 1
  'sky':  extnum = 2
  'fit':  extnum = 3
  'vary': extnum = 4
  'bksp': extnum = 5
  'bkrt': extnum = 6
  else:   begin
          print,' Sorry, data type ',datyp,' not supported for RDF format.'
          print,' Please check your inputs. Returning.'
          retall
          end
endcase
;
fdecomp,filename,disk,dir,file,qual,ver
if (qual eq '') then qual = '.fits' else qual = '.' + qual
match_files,disk+dir,file,qual,name,nlist    ;look for the right files
name = name(0)                               ;use latest version
tab = readfits(name,hdr,ext=extnum)
ndat = fxpar(hdr,'NAXIS2')                   ;number of rows (data points)
;
case datyp of
  'src':  begin
          case instr of
            'H': begin
                 row = {hri_srclist,sds_num:0,ra:0.D0,dec:0.D0,x:0.0,y:0.0, $
                        xy_err:0.0,im_x:0.0,im_y:0.0,lev1_x:0.0,lev1_y:0.0, $
                        offax:0.0,cir_rad:0.0,gross_cts:0.0,gross_err:0.0, $
                        cir_bkg:0.0,net_rt:0.0,net_rt_err:0.0,onekev_qe:0.0, $
                        onekev_vig:0.0,onekev_csc:0.0,x_size:0.0,y_size:0.0, $
                        box_cts:0.0,threshold:0.0,snr:0.0,ept:0.D0, $
                        src_prob:0.0,epi:0.D0,x_ec:0.0,y_ec:0.0,rad_ec:0.0, $
                        ec_csc:0.0,size_cor:0.0,sc_err:0.0,vary:'',radec:''}
                 struct = replicate(row,ndat)
                 struct.sds_num = fits_get(hdr,tab,'SDS_NUM')
                 struct.ra = fits_get(hdr,tab,'RA')
                 struct.dec = fits_get(hdr,tab,'DEC')
                 struct.x = fits_get(hdr,tab,'X')
                 struct.y = fits_get(hdr,tab,'Y')
                 struct.xy_err = fits_get(hdr,tab,'XY_ERR')
                 struct.im_x = fits_get(hdr,tab,'IM_X')
                 struct.im_y = fits_get(hdr,tab,'IM_Y')
                 struct.lev1_x = fits_get(hdr,tab,'LEV1_X')
                 struct.lev1_y = fits_get(hdr,tab,'LEV1_Y')
                 struct.offax = fits_get(hdr,tab,'OFFAX')
                 struct.cir_rad = fits_get(hdr,tab,'CIR_RAD')
                 struct.gross_cts = fits_get(hdr,tab,'GROSS_CTS')
                 struct.gross_err = fits_get(hdr,tab,'GROSS_ERR')
                 struct.cir_bkg = fits_get(hdr,tab,'CIR_BKG')
                 struct.net_rt = fits_get(hdr,tab,'NET_RT')
                 struct.net_rt_err = fits_get(hdr,tab,'NET_RT_ERR')
                 struct.onekev_qe = fits_get(hdr,tab,'1KEV_QE')
                 struct.onekev_vig = fits_get(hdr,tab,'1KEV_VIG')
                 struct.onekev_csc = fits_get(hdr,tab,'1KEV_CSC')
                 struct.x_size = fits_get(hdr,tab,'X_SIZE')
                 struct.y_size = fits_get(hdr,tab,'Y_SIZE')
                 struct.box_cts = fits_get(hdr,tab,'BOX_CTS')
                 struct.threshold = fits_get(hdr,tab,'THRESHOLD')
                 struct.snr = fits_get(hdr,tab,'SNR')
                 struct.ept = fits_get(hdr,tab,'EPT')
                 struct.src_prob = fits_get(hdr,tab,'SRC_PROB')
                 struct.epi = fits_get(hdr,tab,'EPI')
                 struct.x_ec = fits_get(hdr,tab,'X_EC')
                 struct.y_ec = fits_get(hdr,tab,'Y_EC')
                 struct.rad_ec = fits_get(hdr,tab,'RAD_EC')
                 struct.ec_csc = fits_get(hdr,tab,'EC_CSC')
                 struct.size_cor = fits_get(hdr,tab,'SIZE_COR')
                 struct.sc_err = fits_get(hdr,tab,'SC_ERR')
                 struct.vary = fits_get(hdr,tab,'VARY')
                 end
;
            'P': begin
                 row = {pspc_srclist,mplsx_id:0L,solst_id:0L,ra:0.D0, $ 
                        dec:0.D0,x:0.0,y:0.0,im_x:0.0,im_y:0.0,im_x_err:0.0, $
                        im_y_err:0.0,lev1_x:0.0,lev1_y:0.0,offax:0.0, $
                        net_rt:0.0,net_rt_err:0.0,bkg_rt:0.0,xy_size:0.0, $
                        ept:0.0,max_like:0.0,dist_src:0.0,dist_rib:0.0, $    
                        ext_size:0.0,ext_like:0.0,vary:'',hr_1:0.0,he_1:0.0, $
                        hr_2:0.0,he_2:0.0,priority:'',chipl:0.0,chirs:0.0, $
                        mdet:'',vignet:0.0,radec:''}
                 struct = replicate(row,ndat)
                 struct.mplsx_id = fits_get(hdr,tab,'MPLSX_ID')
                 struct.solst_id = fits_get(hdr,tab,'SOLST_ID')
                 struct.ra = fits_get(hdr,tab,'RA')
                 struct.dec = fits_get(hdr,tab,'DEC')
                 struct.x = fits_get(hdr,tab,'X')
                 struct.y = fits_get(hdr,tab,'Y')
                 struct.im_x = fits_get(hdr,tab,'IM_X')
                 struct.im_y = fits_get(hdr,tab,'IM_Y')
                 struct.im_x_err = fits_get(hdr,tab,'IM_X_ERR')
                 struct.im_y_err = fits_get(hdr,tab,'IM_Y_ERR')
                 struct.lev1_x = fits_get(hdr,tab,'LEV1_X')
                 struct.lev1_y = fits_get(hdr,tab,'LEV1_Y')
                 struct.offax = fits_get(hdr,tab,'OFFAX')
                 struct.net_rt = fits_get(hdr,tab,'NET_RT')
                 struct.net_rt_err = fits_get(hdr,tab,'NET_RT_ERR')
                 struct.bkg_rt = fits_get(hdr,tab,'BKG_RT')
                 struct.xy_size = fits_get(hdr,tab,'XY_SIZE')
                 struct.ept = fits_get(hdr,tab,'EPT')
                 struct.max_like = fits_get(hdr,tab,'MAX_LIKE')
                 struct.dist_src = fits_get(hdr,tab,'DIST_SRC')
                 struct.dist_rib = fits_get(hdr,tab,'DIST_RIB')
                 struct.ext_size = fits_get(hdr,tab,'EXT_SIZE')
                 struct.ext_like = fits_get(hdr,tab,'EXT_LIKE')
                 struct.vary = fits_get(hdr,tab,'VARY')
                 struct.hr_1 = fits_get(hdr,tab,'HR_1')
                 struct.he_1 = fits_get(hdr,tab,'HE_1')
                 struct.hr_2 = fits_get(hdr,tab,'HR_2')
                 struct.he_2 = fits_get(hdr,tab,'HE_2')
                 struct.priority = fits_get(hdr,tab,'PRIORITY')
                 struct.chipl = fits_get(hdr,tab,'CHIPL')
                 struct.chirs = fits_get(hdr,tab,'CHIRS')
                 struct.mdet = fits_get(hdr,tab,'MDET')
                 struct.vignet = fits_get(hdr,tab,'VIGNET')
                 end
          endcase
;
; Now change ra and dec in degrees to ra in hr, m, s & dec in deg, m, s
;
          ra = struct.ra
          dec = struct.dec
          radec = strarr(ndat)
          for ii=0,ndat-1 do radec(ii) = adstring(ra(ii),dec(ii))
          struct.radec = radec
          end
;
  'sky':  begin
          row = {skylist,src:0,ra:0.0,dec:0.0,id:0,obj:'',acc:'',vmag:'', $
                 bmag:'',spmor:'',ruxvn:'',ref:0,dist:'',radec:''}
          struct = replicate(row,ndat)
          struct.src = fits_get(hdr,tab,'SRC')
          struct.ra = fits_get(hdr,tab,'RA')
          struct.dec = fits_get(hdr,tab,'DEC')
          struct.id = fits_get(hdr,tab,'ID')
          ftype = 'OBJ'
          if (instr eq 'H') then ftype = 'OBJECT'
          struct.obj = fits_get(hdr,tab,ftype)
          ftype = 'ACC'
          if (instr eq 'H') then ftype = 'ACC_FLAG'
          struct.acc = fits_get(hdr,tab,ftype)
          struct.vmag = fits_get(hdr,tab,'VMAG')
          struct.bmag = fits_get(hdr,tab,'BMAG')
          ftype = 'SPMOR'
          if (instr eq 'H') then ftype = 'SP_MORPH'
          struct.spmor = fits_get(hdr,tab,ftype)
          struct.ruxvn = fits_get(hdr,tab,'RUXVN')
          ftype = 'REF'
          if (instr eq 'H') then ftype = 'NUM_REFS'
          struct.ref = fits_get(hdr,tab,ftype)
          struct.dist = fits_get(hdr,tab,'DIST')
;
; Now change ra and dec in degrees to ra in hr, m, s & dec in deg, m, s
;
          ra = struct.ra
          dec = struct.dec
          radec = strarr(ndat)
          for ii=0,ndat-1 do radec(ii) = adstring(ra(ii),dec(ii))
          struct.radec = radec
          end
;
  'fit':  begin
          row = {pspc_fitss,mplsx_id:0L,snr:0.0,im_x:0.0,im_y:0.0, $
                 percent:0.0,rad_opt:0,id:0,npt:0,offax:0.0,gross_cts:0.0, $
                 intens:0.0,chi_sq:0.0,log_nh:0.0,errnh:0.0,param:0.0, $ 
                 errpar:0.0}
          struct = replicate(row,ndat)
          struct.mplsx_id = fits_get(hdr,tab,'MPLSX_ID')
          struct.snr = fits_get(hdr,tab,'SNR')
          struct.im_x = fits_get(hdr,tab,'IM_X')
          struct.im_y = fits_get(hdr,tab,'IM_Y')
          struct.percent = fits_get(hdr,tab,'PERCENT')
          struct.rad_opt = fits_get(hdr,tab,'RAD_OPT')
          struct.id = fits_get(hdr,tab,'ID')
          struct.npt = fits_get(hdr,tab,'NPT')
          struct.offax = fits_get(hdr,tab,'OFFAX')
          struct.gross_cts = fits_get(hdr,tab,'GROSS_CTS')
          struct.intens = fits_get(hdr,tab,'INTENS')
          struct.chi_sq = fits_get(hdr,tab,'CHI_SQ')
          struct.log_nh = fits_get(hdr,tab,'LOG_NH')
          struct.errnh = fits_get(hdr,tab,'ERRNH')
          struct.param = fits_get(hdr,tab,'PARAM')
          struct.errpar = fits_get(hdr,tab,'ERRPAR')
          end
;
  'vary': begin
          row = {pspc_variab,mplsx_id:0L,im_x:0.0,im_y:0.0,offax:0.0, $
                 ext_size:0.0,gross_cts:0.0,mean_rt:0.0,obs_time:0.0, $ 
                 max_like:0.0,csqbin:0.0,csqred:0.0,csqnbins:0,ksflag:'', $ 
                 ksmaxdif:0.0,ks90:0.0,ks95:0.0,ks99:0.0,ustat:0.0,ualfa:0.0, $ 
                 ubeta:0.0,umaxlike:0.0,usig:0.0,usigbeta:0.0,fftnbins:0.0, $
                 fftsampt:0.0,fftlothr:0,fftsigthr:0.0,fftcomp:0,fftper:0.0, $
                 fftpow:0.0,fftsigpow:0.0,fftflag:''}
          struct = replicate(row,ndat)
          struct.mplsx_id = fits_get(hdr,tab,'MPLSX_ID')
          struct.im_x = fits_get(hdr,tab,'IM_X')
          struct.im_y = fits_get(hdr,tab,'IM_Y')
          struct.offax = fits_get(hdr,tab,'OFFAX')
          struct.ext_size = fits_get(hdr,tab,'EXT_SIZE')
          struct.gross_cts = fits_get(hdr,tab,'GROSS_CTS')
          struct.mean_rt = fits_get(hdr,tab,'MEAN_RT')
          struct.obs_time = fits_get(hdr,tab,'OBS_TIME')
          struct.max_like = fits_get(hdr,tab,'MAX_LIKE')
          struct.csqbin = fits_get(hdr,tab,'CSQBIN')
          struct.csqred = fits_get(hdr,tab,'CSQRED')
          struct.csqnbins = fits_get(hdr,tab,'CSQNBINS')
          struct.ksflag = fits_get(hdr,tab,'KSFLAG') 
          struct.ksmaxdif = fits_get(hdr,tab,'KSMAXDIF')
          struct.ks90 = fits_get(hdr,tab,'KS90')
          struct.ks95 = fits_get(hdr,tab,'KS95')
          struct.ks99 = fits_get(hdr,tab,'KS99')
          struct.ustat = fits_get(hdr,tab,'USTAT')
          struct.ualfa = fits_get(hdr,tab,'UALFA')
          struct.ubeta = fits_get(hdr,tab,'UBETA')
          struct.umaxlike = fits_get(hdr,tab,'UMAXLIKE')
          struct.usig = fits_get(hdr,tab,'USIG')
          struct.usigbeta = fits_get(hdr,tab,'USIGBETA')
          struct.fftnbins = fits_get(hdr,tab,'FFTNBINS')
          struct.fftsampt = fits_get(hdr,tab,'FFTSAMPT')
          struct.fftlothr = fits_get(hdr,tab,'FFTLOTHR')
          struct.fftsigthr = fits_get(hdr,tab,'FFTSIGTHR')
          struct.fftcomp = fits_get(hdr,tab,'FFTCOMP')
          struct.fftper = fits_get(hdr,tab,'FFTPER')
          struct.fftpow = fits_get(hdr,tab,'FFTPOW')
          struct.fftsigpow = fits_get(hdr,tab,'FFTSIGPOW')
          struct.fftflag = fits_get(hdr,tab,'FFTFLAG')
          end
;
  'bksp': begin
          row = {pspc_bkgspc,channel:0,counts:0.0}
          struct = replicate(row,ndat)
          struct.channel = fits_get(hdr,tab,'CHANNEL')
          struct.counts = fits_get(hdr,tab,'COUNTS')
          end
;
  'bkrt': begin
          strname = 'pspc_bkrt'
          row = {pspc_bkgrte,time:0.D0,timedel:0.0,t_beg:0.D0,t_end:0.D0, $
                             rate:0.0,error:0.0}
          struct = replicate(row,ndat)
          struct.time = fits_get(hdr,tab,'TIME')
          struct.timedel = fits_get(hdr,tab,'TIMEDEL')
          struct.t_beg = fits_get(hdr,tab,'T_BEG')
          struct.t_end = fits_get(hdr,tab,'T_END')
          struct.rate = fits_get(hdr,tab,'RATE')
          struct.error = fits_get(hdr,tab,'ERROR')
          end
endcase
;
return
end         ;pro rdf_sources
