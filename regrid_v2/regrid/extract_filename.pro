; $Id: extract_filename.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
;-------------------------------------------------------------
;+
; NAME:
;        EXTRACT_FILENAME
;
; PURPOSE:
;        extract the file filename from a full qualified filename
;
; CATEGORY:
;        File I/O
;
; CALLING SEQUENCE:
;        filename=EXTRACT_FILENAME(FULLANME [,keywords])
;
; INPUTS:
;        FULLNAME --> a fully qualified filename containing path information.
;
; KEYWORD PARAMETERS:
;        FILEPATH --> a named variable that returns the path of the
;           file. This can be used if both, the filename and the name
;           of the file will be used. Otherwise it is recommended to
;           use EXTRACT_PATH instead.
;
; OUTPUTS:
;        A string containing the filename to be analyzed.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        See also EXTRACT_PATH
;
; EXAMPLE:
;        print,extract_filename('~mgs/IDL/tools/extract_filename.pro')
;
;             will print  'extract_filename.pro'
;
;        print,extract_filename('example.dat',filepath=filepath)
;
;             will print  'example.dat', and filepath will contain ''
;
;
; MODIFICATION HISTORY:
;        mgs, 18 Nov 1997: VERSION 1.00
;        mgs, 21 Jan 1999: - added extra check for use of '/' path specifiers
;              in Windows OS;
;-
; Copyright (C) 1997, Martin Schultz, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine extract_filename"
;-------------------------------------------------------------


;-------------------------------------------------------------
; This routine is an IDL library routine which is copyrighted by
; RSI. However, since they labeled it obsolete, I don't think they
; have too many objections if you use and/or redistribute this routine. 

FUNCTION RSTRPOS, Expr, SubStr, Pos     
  ON_ERROR, 2
  N = N_PARAMS()
  if (n lt 2) then message, 'Incorrect number of arguments.'
 
  ; Is expr an array or a scalar? In either case, make a result
  ; that matches.
  if (size(expr, /n_dimensions) eq 0) then result = 0 $
  else result = make_array(dimension=size(expr,/dimensions), /INT)
 
  RSubStr = STRING(REVERSE(BYTE(SubStr)))       ; Reverse the substring
 
  for i = 0L, n_elements(expr) - 1 do begin
    Len = STRLEN(Expr[i])
    IF (N_ELEMENTS(Pos) EQ 0) THEN Start=0 ELSE Start = Len - Pos
 
    RString = STRING(REVERSE(BYTE(Expr[i])))    ; Reverse the string
 
    SubPos = STRPOS(RString, RSubStr, Start)
    IF SubPos NE -1 THEN SubPos = Len - SubPos - STRLEN(SubStr)
    result[i] = SubPos
  endfor
 
  RETURN, result
END    



function extract_filename,fullname,filepath=thisfilepath

; determine path delimiter
    if (!version.os_family eq 'Windows') then sdel = '\' else sdel = '/'

    filename = ''
    thisfilepath = ''

retry:
; look for last occurence of sdel and split string fullname
    p = rstrpos(fullname,sdel)

; extra Windows test: if p=-1 but fullname contains '/', retry
    if (p lt 0 AND strpos(fullname,'/') ge 0) then begin
       sdel = '/'
       goto,retry
    endif


    if (p ge 0) then begin
       thisfilepath = strmid(fullname,0,p+1)
       filename = strmid(fullname,p+1,strlen(fullname)-1)
    endif else $
       filename = fullname


return,filename

end

