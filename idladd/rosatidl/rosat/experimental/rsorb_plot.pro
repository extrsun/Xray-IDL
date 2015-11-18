;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       RSORB_PLOT
;
;*PURPOSE:
; A procedure to overplot Rosat latitude and longitude during specified
; start and end times.
;
;*CALLING SEQUENCE:
;	rsorb_plot, rs_lon, rs_lat, obinfo, obinum, thick=thick, $
;                   orb_color=orb_color
;
; INPUTS:
;	rs_lon     - satellite orbit longitudes (read using RSORB_POS)
;       rs_lat     - satellite orbit latitudes (read using RSORB_POS)
;       obinfo     - data structure containing dates, times, & indices of
;                    beginnings & ends of obi segments
;                    has the structure of replicate(row,num), where
;                    row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                         sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                         ibeg:0L,iend:0L}
;                    and num is the total number of intervals
;             ibeg - indices of values in SCTIME equal to values in SCTBEG
;             iend - indices of values in SCTIME equal to values in SCTEND
;
; OPTIONAL INPUTS:
;	obinum     - number(s) of obi segments to plot - e.g., [1,3,4]
;                    default is to plot all obi segments (obinum=0)
;       thick      - line thickness for drawing data (default = 1)
;       orb_color  - color of rosat orbit (0-255)
;
; OUTPUTS:
;	results are overplotted on existing plot                        
;
;*PARAMETERS:
;
;*NOTES:
;	This routine overplots on an existing latitude/longitude plot.
;	This plot can be made using MAP_SET with MAP_GRID &/or MAP_CONTINENTS
;
;*PROCEDURE:
;
; HISTORY:
;	 based on HST_PLOT kindly donated by GHRS 23 Apr 1991
;	 modified for Rosat 29 Apr 1991 by GAR
;        modified 30 Aug 1991 by GAR to make OBI numbers begin with 1 (not 0)
;        modified 18 Feb 1992 to be compatible with RSORBIT, RSORB_POS (GAR)
;        modified 29 Dec 1993 (GAR) to be compatible with new RSORBIT,which
;          calls the intrinsic IDL routines MAP_SET, MAP_GRID, and
;          MAP_CONTINENTS; also called using obinfo instead of UT times
;-
;-------------------------------------------------------------------------------
pro rsorb_plot,rs_lon,rs_lat,obinfo,obinum,thick=thick,orb_color=orb_color
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSORB_PLOT, rs_lon, rs_lat, obinfo, obinum, thick=thick,', $
        ' orb_color=orb_color'
  retall
end
if (n_elements(orb_color) eq 0) then orb_color = 220  ; default color
if (n_elements(thick) eq 0) then thick = 1            ; default line thickness
;
ibeg = obinfo.ibeg
iend = obinfo.iend
nobi = n_elements(ibeg)
;
if (npar lt 4) then obinum = 0
obiplot = obinum
if (obiplot(0) eq 0) then obiplot = indgen(nobi) + 1     ;plot all OBIs
;
for jj=0,nobi-1 do begin
  !c=0
  lon = rs_lon(ibeg(jj):iend(jj))
  lat = rs_lat(ibeg(jj):iend(jj))
  isel = where(lon gt 180.,nsel) 
  if (nsel gt 0) then lon(isel) = lon(isel) - 360.      ;long def from 0 to 360
  oplot,lon,lat,color=orb_color,thick=thick,noclip=0,psym=0
  if (!debug gt 5) then stop,'Stopping in segment',jj
endfor    ;plotting all requested segments
;
return
end       ;pro rsorb_plot
