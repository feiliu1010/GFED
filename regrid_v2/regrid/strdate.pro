;+
; NAME:
;        STRDATE
;
; PURPOSE:
;        This functions returns a date string in a standard
;        format. The default format is US: 'mm/dd/yyyy hh:MM',
;        optionally you can select the german format 'dd.mm.yyyy
;        hh:MM', or a short form without the time, or a "sortable"
;        form that begins with the year.
;
; CATEGORY:
;        date and time
;
; CALLING SEQUENCE:
;        result=STRDATE([DATE][,keywords])
;
; INPUTS:
;        DATE --> (optional) Either a up to 6 element array
;            containing year, month, day, hour, minute, and secs
;            (i.e. the format returned from BIN_DATE) or
;            a structure containing at least year, month, and day,
;            optionally also hour, and minute. Finally, date can be a
;            date string in "standard" format as returned by
;            SYSTIME(0). If DATE is omitted, STRDATE will
;            automatically return the current system time. 
;
; KEYWORD PARAMETERS:
;        SHORT --> omit the time value, return only date
;
;        SORTABLE --> will return 'YYYY/MM/DD HH:MM' 
;
;        GERMAN --> will return 'DD.MM.YYYY HH:MM'
;
;        IS_STRING --> indicates that DATE is a date string rather
;            than an integer array. This keyword is now obsolete but kept
;            for compatibility.
;
; OUTPUTS:
;        A date string formatted as 'MM/DD/YYYY HH:MM'.
;        If SHORT flag is set, the format will be 'MM/DD/YYYY'
;
; SUBROUTINES:
;
; REQUIREMENTS:
;        Uses chkstru function to test for structure and tags.
;
; NOTES:
;        /GERMAN and /SORTABLE will have effect of SORTABLE but
;        with dots as date seperators.
;        No error checking is performed on input date (e.g. 31.02.2000)
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        mgs, 11 Nov 1997: VERSION 1.00
;        mgs, 26 Mar 1998: VERSION 1.10 
;            - examines type of DATE parameter and accepts structure
;              input.
;        mgs, 26 Aug 2000: - changed copyright to open source
;            - now uses chkstru to test for structure type and tags
;
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000 Martin Schultz
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


function strdate,date,is_string=is_string, $
            short=short,sortable=sortable,german=german
 

   forward_function chkstru
 
   on_error,2
 
 
   if n_elements(date) gt 0 then begin

      ;; analyze format of DATE
      dtype = size(date, /TYPE)
      if dtype eq 7 then is_string = 1 else is_string=0
      is_stru = ChkStru(date,['year','month','day'])

      ;; report error if date is a structure but does not contain the
      ;; required tags
      IF Chkstru(date) AND not is_stru THEN BEGIN
         Message, 'Date must be a structure with year, month, day [hour, minute]!'
      ENDIF
      if is_string then bdate=bin_date(date) $ 
      else if is_stru then begin
         bdate = intarr(6)
         bdate[0] = date.year
         bdate[1] = date.month
         bdate[2] = date.day
         if ChkStru(date,['hour']) then bdate[3] = date.hour
         if ChkStru(date,['min']) OR ChkStru(date,['minute']) then $
            bdate[4] = date.minute
       
      endif else bdate = date

   endif else bdate = bin_date() ; insert system time

 
   ;; in case of not enough elements pad with zero's
;   tmp = intarr(6)
;   bdate = [bdate,tmp]
 
   ;; convert to formatted string items
   bdate = strtrim(string(bdate,format='(i4.2)'),2)
 
   ;; and compose result string
   ;; determine date seperator
   if keyword_set(german) then sep='.' else sep='/'

   ;; default : US format
   result = bdate[1]+sep+bdate[2]+sep+bdate[0]
   ;; german format, day first
   if keyword_set(german) then $
      result = bdate[2]+sep+bdate[1]+sep+bdate[0]
   ;; sortable: year month day
   if(keyword_set(sortable)) then $
      result = bdate[0]+sep+bdate[1]+sep+bdate[2]
   
   if not keyword_set(SHORT) then  $
      result = result+' '+bdate[3]+':'+bdate[4]
   
   return,result
 
end
 
