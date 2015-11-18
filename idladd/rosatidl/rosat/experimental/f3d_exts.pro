function f3d_exts,instr
;
; returns valid 3D binary FITS extensions for instrument given by instr
; RH = Rosat HRI, RP = Rosat PSPC, RB = Rosat HRI and PSPC
;
;
in = strupcase(instr)
case in of
'RH': begin
      fexts = ['.abk','.ah','.ao','.bcr','.beb','.bgi','.bir']
      fexts = [fexts,'.bki','.bsc','.bts','.cpb','.cps','.dbm','.dms']
      fexts = [fexts,'.fits','.gte','.htl','.hts','.ltf','.mo','.mob']
      fexts = [fexts,'.oan','.obi','.obp','.obs','.obt','.ots','.pms','.poe']
      fexts = [fexts,'.qeg','.s1d','.saa','.sgi','.sps','.ssc']
      fexts = [fexts,'.sxb','.tsh','.ugi','.upm','.ute','.vts','.vtu','.xrb']
      fexts = [fexts,'_bk.fits','_img.fits','_mex.fits']
      fexts = [fexts,'_sky.fits','_src.fits']
      end
;
'RP': begin
      fexts = ['.asc','.asp','.cas','.dmp','.evr','.fits','.hkb','.mds']
      fexts = [fexts,'.moi','.oar','.par','.rcr','.sa','.sas','.so','.sta']
      fexts = [fexts,'.tap']
      fexts = [fexts,'_bk1.fits','_bk2.fits','_bk3.fits']
      fexts = [fexts,'_im1.fits','_im2.fits','_im3.fits','_mex.fits']
      fexts = [fexts,'_sky.fits','_sp.fits','_src.fits']
      end
;
'RB': begin
      fexts = ['.abk','.ah','.ao','.asc','.asp','.bcr','.beb','.bgi']
      fexts = [fexts,'.bir','.bki','.bsc','.bts','.cas','.cpb','.cps']
      fexts = [fexts,'.dbm','.dmp','.dms','.evr','.fits','.gte','.hkb']
      fexts = [fexts,'.htl','.hts','.ltf','.mds','.mo','.mob','.moi']
      fexts = [fexts,'.oan','.oar','.obi','.obp','.obs','.obt','.ots']
      fexts = [fexts,'.par','.pms','.poe','.qeg','.rcr','.s1d','.sa','.saa']
      fexts = [fexts,'.sas','.sgi','.so','.sps','.ssc','.sta','.sxb']
      fexts = [fexts,'.tap','.tsh','.ugi','.upm','.ute','.vts','.vtu','.xrb']
      fexts = [fexts,'_bk.fits','_bk1.fits','_bk2.fits','_bk3.fits']
      fexts = [fexts,'_im1.fits','_im2.fits','_im3.fits','_img.fits']
      fexts = [fexts,'_mex.fits','_sky.fits','_sp.fits','_src.fits']
      end
else: fexts=''
endcase
;
return,fexts
end           ;function f3d_exts
