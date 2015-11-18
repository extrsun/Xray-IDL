pro test,xc,yc,sr
ang=(1.+findgen(360))*!pi/180. & cosa=cos(ang) & sina=sin(ang)
a=xc+sr*cosa & b=yc+sr*sina
c=[transpose(a),transpose(b)]
oplot,a,b
return
end

