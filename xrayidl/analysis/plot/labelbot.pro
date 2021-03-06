    ;** Find normalized coordinate values of the tick mark:

    ;** Plot RA labels along bottom axis if parameter is set:
    ;     if botlabel eq 1 then begin
    if min(ydis) lt ny1+3.e-2 then begin

      ;** Find bins closest to bottom edge:
      bracket_v, ydis, ny1, i_values=i_values, m_values=m_values
      if total(i_values) ne -1 then niv = n_elements(i_values(*,0)) else $
        niv = 0
      if total(m_values) ne -1 then nmv = n_elements(m_values) else nmv = 0

      xt1_vec = fltarr(niv+nmv)

      ;** Interpolate between the bins closest to edge to find out
      ;** where to place the tick mark:
      if niv ne 0 then $
      for l=0,niv-1 do begin
        linterp, ydis(i_values(l,0):i_values(l,1)), $
          xdis(i_values(l,0):i_values(l,1)), ny1, xt1
        xt1_vec(l) = xt1
      endfor
      if nmv ne 0 then $
      for l=0,nmv-1 do begin
        linterp, ydis(m_values(l,0):m_values(l,1)), $
          xdis(m_values(l,0):m_values(l,1)), ny1, xt1
        xt1_vec(niv+l) = xt1
      endfor

      xsel = where(xt1_vec ge nx1 and xt1_vec le nx2,nxsel)

      if nxsel ne 0 then begin

        xt1_vec = xt1_vec(xsel)
        xt1 = xt1_vec

        ;** Label tick marks (every other tick):
