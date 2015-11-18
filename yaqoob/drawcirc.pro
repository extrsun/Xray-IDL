pro drawcirc,sr,xc,yc
if n_params(0) eq 0 then begin
 print,'drawcirc,sr,xc,yc'
 retall
end
ang=(1.+findgen(360))*!pi/180. & cosa=cos(ang) & sina=sin(ang)
a=xc+sr*cosa & b=yc+sr*sina
c=[transpose(a),transpose(b)]
oplot,a,b,psym=-1,symsize=0.1
return
end

