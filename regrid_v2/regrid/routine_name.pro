;+
; NAME:
;        ROUTINE_NAME  (function)
;
; PURPOSE:
;        This function returns the name of the routine which calls
;        this function, or the name of its caller.
;
; CATEGORY
;        General Programming
;
; CALLING SEQUENCE:
;        rname = ROUTINE_NAME()
;
; INPUTS:
;        none
;
; KEYWORD PARAMETERS:
;        FILENAME -> returns the file in which the routine can be found
;
;        /CALLER -> returns information about the caller of the routine
;           instead of the routine itself
;
; OUTPUTS:
;        The name of the caller routine is returned in lowercase
;        characters (can be used to construct a filename by adding
;        ".pro")
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;        From the command line:
;             print,routine_name()
;        results in   $main$
;
;        Very useful in conjunction with USAGE.PRO:
;             usage,routine_name()
;        displays help information on the current routine.
;
; MODIFICATION HISTORY:
;        mgs, 27 Mar 1998: VERSION 1.00
;        mgs, 22 Apr 1998: - added FILENAME and CALLER keywords
;        mgs, 14 Jan 1998: - needed fix for filename when working on PC:
;                $MAIN$ allows no str_sep
;        mgs, 26 Aug 2000: - changed copyright to open source
;                          - [] array notation
;        mgs, 27 Mar 2001: - changed str_sep to strsplit
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


function routine_name,filename=filename,caller=caller

   ; extract the name of the current routine from the caller stack
   ; the first element will always be ROUTINE_NAME ;-)
   help,call=c

   caller = keyword_set(caller)
   i = 1 + caller
   if (i ge n_elements(c)) then return,'No caller for $MAIN$'

   thisroutine = strsplit(strcompress(c[i])," ", /EXTRACT)

   if (n_elements(thisroutine) gt 1) then filename = thisroutine[1] $
   else filename = ''

   ; cut < and ( brackets from filename info
   len = strlen(filename)
   filename = strmid(filename,1,len-2)

return,strlowcase(thisroutine[0])
end

