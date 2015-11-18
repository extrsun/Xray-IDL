;+
; NAME:
;   rosat_blank
; PURPOSE:
;   return blank RASS FSC structure
; CALLING SEQUENCE:
;   rosat0= rosat_blank()
; REVISION HISTORY:
;   23-Apr-2010  Written by Blanton, NYU
;-
;------------------------------------------------------------------------------
function rosat_blank

rosat0= { sourcename:' ', $
          ra:0.D, $  ;; J2000 deg
          dec:0.D, $  ;; J2000 deg
          radecerr:0., $ ;; arcsec
          flags:' ', $
          flags2:' ', $
          cps:0., $
          cps_err:0., $
          bgcps:0., $
          exptime:0., $
          hr1:0., $
          hr1_err:0., $
          hr2:0., $
          hr2_err:0., $
          ext:0., $
          extl:0., $
          srcl:0., $
          extr:0., $
          priority:' ', $
          erange:' ', $
          vigf:0., $
          orgdat:' ', $
          moddat:' ', $
          id:0L, $
          fieldid:0L, $
          srcnum:0L, $
          rct1:-1, $
          rct2:-1, $
          rct3:-1, $
          itb:' ', $
          ite:' ', $
          rl:-1, $
          cat:' '}
          

return, rosat0

end
