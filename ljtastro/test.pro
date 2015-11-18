pro test
S = STRLOWCASE('Abdsga')
S = STRUPCASE('Abdsga')


;Create a string variable to hold the response.
answer = ''
 ;Ask the question.
READ, 'Answer yes or no:  ', answer
IF (STRUPCASE(answer) EQ 'YES') THEN $
   ;Compare the response to the expected answer.
   PRINT,'YES' ELSE PRINT, 'NO'


Result = STRCMP( 'String1', 'string2' ,3, /FOLD_CASE)
print,Result


a=1234
stringarr1=strtrim(string(lindgen(2000)+100000),2)
stringarr2='src'+stringarr1+'.reg'
print,'src'+'1234'
print,string(a)
print,stringarr1[500]
print,stringarr2[500]


name=string((findgen(100000)+100000)*10)
print,name[2000:2010]
strput,name,'src',0
strput,name,'.reg',9
print,name[2000:2010]


print,strjoin('src',string(a),/SINGLE)


print,strpos(name[2005],'0')
print,strpos(name[2005],'0',4)
print,strpos(name[2005],'e')
print,strpos(name[2005],'e',/REVERSE_SEARCH)
print,strpos(name[2005],'e',9,/REVERSE_SEARCH)


A = ' This string has leading and trailing white space  '
PRINT, '>', STRTRIM(A), '<'
PRINT, '>', STRTRIM(A,1), '<'
PRINT, '>', STRTRIM(A,2), '<'

end