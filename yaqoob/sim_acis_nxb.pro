function sim_acis_nxb, plist, texp=texp, verb=verb
; A. Ptak, Jan. 1998

if n_params(0) eq 0 then begin
    print, 'bgd = sim_acis_nxb(plist, texp=texp)'
    print, 'The instrumental background is simulated for each chip present'
    print, 'in plist, returned in seperate plist'
    print, 'If texp is not given, max(plist.time)-min(plist.time)'
    print, '/verb makes it verbose'
    retall
end

if n_elements(texp) ne 1 then texp = max(plist.time) - min(plist.time)
ccds = histogram(plist.ccdid, min=0)
maxccd = n_elements(ccds) - 1
if maxccd ne max(plist.ccdid) then begin
  print, 'Something is wrong... maxccd = ' + strn(maxccd) + $
    ', but max(plist(ccdid)) = ' + strn(max(plist.ccdid))
  stop
endif
ccdarea = (1024*0.495/60)^2
fi = 2.32e-3*ccdarea ; rates from pg. 60 Proposer's Guide
bi = 6.81e-3*ccdarea ; 
if keyword_set(verb) then print, 'FI bgd. rate = ' + strn(fi) + $
  ' counts/s/ccd, BI rate = ' + strn(bi) + ' counts/s/ccd'
ficts = fi*texp
bicts = bi*texp
if keyword_set(verb) then print, strn(ficts) + ' FI counts/ccd, ' + $
  strn(bicts) + ' BI counts/ccd'

ccdids = 0
phas = 0
tdetxs = 0
tdetys = 0
chipxs = 0
chipys = 0

for i=0, maxccd do begin
    if ccds(i) gt 0 then begin
        if i eq 5 or i eq 7 then n = poidev(bicts, seed=seed) else $
          n = poidev(ficts, seed=seed)
        n = n(0)
        if keyword_set(verb) then print, 'Adding ' + strn(n) + $
          ' counts for ccd ' + strn(i)
        ccdids = [ccdids, replicate(i, n)]
; Get uniformly distributed random values of chipx, chipy, pha
        phas = [phas, randomu(seed, n)*2048]
        chipx = randomu(seed, n)*1024
        chipxs = [chipxs, chipx]
        chipy = randomu(seed, n)*1024
        chipys = [chipys, chipy]
; Now convert to tdetx, tdety using offsets and flips determined from
; a XRB sim.
        case i of
            0: begin
                tdetx = 3061 + chipy
                tdety = 5131 - chipx
               end
            1: begin
                tdetx = 5131 - chipy
                tdety = 4107 + chipx
               end
            2: begin
                tdetx = 3061 + chipy
                tdety = 4085 - chipx
               end
            3: begin
                tdetx = 5131 - chipy
                tdety = 3061 + chipx
               end
            4: begin
                tdetx = 791 + chipx
                tdety = chipy
               end
            5: begin
                tdetx = 1833 + chipx
                tdety = chipy
               end
            6: begin
                tdetx = 2875 + chipx
                tdety = chipy
               end
            7: begin
                tdetx = 3917 + chipx
                tdety = chipy
               end
            8: begin
                tdetx = 4959 + chipx
                tdety = chipy
               end
            9: begin
                tdetx = 6001 + chipx
                tdety = chipy
               end
       endcase
           
       tdetxs = [tdetxs, tdetx]
       tdetys = [tdetys, tdety]
    endif
endfor

n = n_elements(phas) - 1
blist = replicate(plist(0), n)
blist.ccdid = ccdids(1:*)
blist.pha = phas(1:*)
blist.chipx = chipxs(1:*)
blist.chipy = chipys(1:*)
blist.tdetx = tdetxs(1:*)
blist.tdety = tdetys(1:*)
; This trans. determined from an ACIS-S sim. of the XRB
; for this sim. it recovers ypos and zpos to within ~ 1 pixel
; using this for the ACIS-I ccds it is off by ~ 5 pixels, but at this
; point ypos and zpos aren't nec. in ACIS-I obs.
blist.ypos = (68.1778+80.8334)/(7009-800)*(blist.tdetx-800)-80.8334
blist.zpos = (12.2884+12.2873)/1024*blist.tdety-12.2873

blist.time = randomu(seed, n)*texp

return, blist
end

