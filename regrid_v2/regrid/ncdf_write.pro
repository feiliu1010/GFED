;; *** Error: if one of the dims is 1 it crashes because dim is set to
;; zero somewhere !!! (see ch4_surface.nc)

;-------------------------------------------------------------
;+
; NAME:
;        NCDF_Write
;
; PURPOSE:
;        Write data in netcdf format. Data must be a structure as
;        returned from ncdf_read, and the attributes must also be
;        passed.
;
; CATEGORY:
;        NCDF Tools
;
; CALLING SEQUENCE:
;        NCDF_Write,filename,data,dimnames,attributes=attributes
;
; INPUTS:
;        FILENAME -> A filename or template to select a file or
;            preselect the choice in the PICKFILE dialog (see OPEN_FILE)
;
;        DATA -> The data array to write (structure)
;
;        DIMNAMES -> The names of the dimensions. These must
;            correspond to the names of dimension variables.
;
; KEYWORD PARAMETERS:
;        ATTRIBUTES -> A structure containing the global and variable
;            attributes (see ncdf_read)
;
;        HISTORY_ENTRY -> A string that will be appended or used for
;            the global history attribute. Default is 'created [date]'
;            if no history attribute was given in the attributes
;            structure, and '; last written [date]' if the attributes
;            structure contains a global.history attribute.
;
;        SELECTION -> A string list of variable names OR an index list
;            for variables to be written to the file. This name or
;            index list must include the dimension variables. If no
;            selection is given, all variables in the data structure
;            are written.
;
;        UNLIMITED -> A string, specifying one dimension to be
;            UNLIMITED. The NetCDF standard claims that this variable
;            has to be the last in all fields using it. ncdf_write
;            currently does not give a warning if this is not the
;            case. 
;
;        VARDIM -> A two dimensional array of strings specifying the
;            dimensions for a variable. The entries are formed like
;            ['varname','dim1','dim2',...]. This is necessary, when two
;            dimensions have the same size, because ncdf_write
;            usually associates dimensions by size and will use the
;            first dimension of adequate size. Only the variables,
;            which would not be treated corroctly by ncdf_write
;            automatically have to be specified. (See also example below.)
;
;        DEGENERATE -> A list of variable names, which will be tested
;            for occurence of the record dimension (unlimited). If
;            this dimension is not present (due to IDL's behaviour of
;            cutting dimensions for example), it will be added.
;
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;  (1) Variable dimensions are associated with netcdf dimension
;      indices based on their size, if vardim is not specified. This
;      will lead to errors if two dimensions of the same size are
;      defined. At present, not even a warning is given in this case.
;
; EXAMPLE:
;        NCDF_Write,'~/data/*.nc',data,attr
;
;        ;; example using the vardim feature, if e.g. x and y have the
;        ;; same size
;        ncdf_write, '~/data/*.nc', data, attr, $
;                    vardim=[['y','y','',''],['flowers','x','y','t']]
;
; MODIFICATION HISTORY:
;        mgs, 21 Aug 2001: VERSION 1.00 (derived from gte_writencdf)
;        mgs, 23 Aug 2001: 
;             - bug fix: GE instead of GT in numsel loop
;        mgs, 25 Nov 2001:
;             - allow for character variables (as byte arrays)
;        mgs, 01 Dec 2001:
;             - handles pointer data and attributes
;        mgs, 06 Feb 2002:
;             - bug fix: crashed for scalar variables
;             - added option for variable name case
;        mgs, 08 Mar 2002:
;             - bug fix for upcase name (crashed when not given)
;        gjb, 06 Jul 2002:
;             - bug fix: scalars have 0 dimensions, not 1
;             - added vardim argument
;        gjb, 07 Jul 2002:
;             - added unlimited argument
;        mgs, 18 Jul 2002:
;             - check for unlimited keyword fixed
;             - force upper initial 'Conventions' attributes
;        mgs, 15 Aug 2003:
;             - dimension errors captured
;        mgs, 17 Feb 2004:
;             - degenerate keyword added
;        mgs, 24 Feb 2004:
;             - degenerate bug fixed (occured if keyword was not present)
;        mgs, 01 Mar 2004:
;             - another degenerate related bug fix: unlimii undefined
;               when degenerate not used
;        mgs, 07 Sep 2004:
;             - added attribute type short
;        mgs, 29 Nov 2004:
;             - eliminated problem with undefined "history_entry"
;        mgs, 14 Apr 2005:
;             - fixed yet another problem with undefined global attributes
;-


FUNCTION nw_GetAttributeType, attribute, pchar=pchar, $
                              plong=plong, pfloat=pfloat, pdouble=pdouble, pshort=pshort


   pchar = 0
   pshort = 0
   plong = 0
   pfloat = 0
   pdouble = 0

   tname = Size( attribute, /TName )
   ispointer = ( tname EQ 'POINTER' )
   IF ispointer THEN tname = Size( *attribute, /TName )

   IF tname EQ 'STRING' OR tname EQ 'BYTE' THEN pchar = 1
   IF tname EQ 'INT' THEN pshort = 1
   IF tname EQ 'LONG' THEN plong = 1
   IF tname EQ 'FLOAT' THEN pfloat = 1
   IF tname EQ 'DOUBLE' THEN pdouble = 1

   ;; Return result in appropriate type (use string as default)
   IF ispointer THEN BEGIN 
      thisattr = *attribute
   ENDIF ELSE BEGIN
      thisattr = attribute
   ENDELSE 

   result = StrTrim(thisattr, 2)
   IF pshort THEN result = Fix(thisattr)
   IF plong THEN result = Long(thisattr)
   IF pfloat THEN result = Float(thisattr)
   IF pdouble THEN result = Double(thisattr)

   RETURN, result

END


;-------------------------------------------------------------


PRO NCDF_Write,filename,data,dimnames,attributes=attributes, $
               selection=selection, history_entry=history_entry, $
               Upcase_name=Upcase_name, vardim = vardim, unlimited = unlimited, $
               degenerate_list=degenerate

;;   ON_ERROR, 2

   ;; extract variable names from data structure
   IF (NOT chkstru(data)) THEN BEGIN
      message,'Invalid data; must be a structure!',/Continue
      RETURN
   ENDIF
   names = Tag_Names(data)

   ;; Test other needed parameters
   IF N_Elements(dimnames) EQ 0 THEN BEGIN
      message,'Must supply names of dimensions!',/Continue
      RETURN
   ENDIF 

   ;; Test if dimnames are contained in names
   dimi = Make_Selection(names, StrUpCase(dimnames))
   IF Min(dimi) LT 0 THEN stop, 'Dimname not found!'

   ;; Test for UNLIMITED
   if n_elements(unlimited) GT 0 then begin
      unlimi = Make_Selection(names, StrUpCase(unlimited))
      if (unlimi lt 0) then begin
         print, 'UNLIMITED dimension not found.'
         print,  'All dimensions set according to their length'
         unlimited = 0
      endif
   endif

   ;; Test if for variable dimension definitions
   if (keyword_set(vardim)) then begin
      if n_elements(size(vardim, /dimensions) eq 1) then begin
         vardim = reform(vardim, n_elements(vardim), 1)
      endif
      nVardim = n_elements(vardim[0, *])
      vardimNames = strarr(nVardim)
      vardimNum   = intarr(nVardim)
      for jn = 0L, nVardim-1 do begin
         vardimNames[jn] = vardim[0, jn]
         vardimNum[jn]   = make_selection(names, strupcase(vardimNames[jn]))
      endfor
   endif else begin
      vardimNum = [-1]
   endelse

   ;; Check if attributes are passed
   ;; (if not, ncdf file will be very rudimentary)
   IF N_Elements(attributes) EQ 0 THEN BEGIN
      Message,'No attributes defined! Lazy bone or what?', /Continue
      hasattr = 0
   ENDIF ELSE BEGIN
      IF (NOT chkstru(attributes)) THEN BEGIN
         message,'Invalid attributes; must be a structure!',/Continue
         RETURN
      ENDIF
      hasattr = 1
      attrnames = Tag_Names(attributes)
   ENDELSE 
   
   ;; Test if selection is of type string. If so, convert to index
   ;; array
   IF N_Elements(selection) EQ 0 THEN selection = lindgen(N_Elements(names))
   IF ( size(selection,/type) EQ 0 ) THEN $
       numsel = make_selection(names,selection,/ONLY_VALID) $
   ELSE $
       numsel = long(selection)


   ;; Write netcdf file

   ;; First test if filename is valid and/or let user select one per
   ;; dialog
   open_file, filename, dummy, /write, filename=truename, title=title
   IF dummy LE 0 THEN BEGIN
      Message, 'Cannot open file '+filename+' for writing!',/Continue
      RETURN
   ENDIF 
   free_lun, dummy

   ;; create netcdf file
   ncfile = truename

   id = NCDF_CREATE(ncfile,/CLOBBER)

   ;; define dimensions
   dims = LonArr(N_Elements(dimi))
   dimdef = LonArr(N_Elements(dimi))
   FOR i=0L, N_Elements(dimi)-1 DO BEGIN
      ispointer = ( Size(data.(dimi[i]), /TName) EQ 'POINTER' )
      IF ispointer THEN BEGIN 
         dims[i] = ( Size(*(data.(dimi[i])), /Dimensions) )[0] > 1
      ENDIF ELSE BEGIN 
         dims[i] = ( Size(data.(dimi[i]), /Dimensions) )[0] > 1
      ENDELSE  
      if keyword_set(unlimited) then begin
         if unlimi eq dimi[i] then begin
            dimdef[i] = NCDF_DIMDEF(id, StrLowCase(names[dimi[i]]), /unlimited)
            print, i, ' defined as unlimited'
            unlimii = i   ;; index of unlimited dimension in dims field
         endif else begin
            dimdef[i] = NCDF_DIMDEF(id, StrLowCase(names[dimi[i]]), dims[i])
         endelse
      endif else begin
         dimdef[i] = NCDF_DIMDEF(id, StrLowCase(names[dimi[i]]), dims[i])
      endelse
   ENDFOR 

   ;; define variables
   vdef = LonArr(N_Elements(numsel))
   FOR i=0L, N_Elements(numsel)-1 DO BEGIN
      IF numsel[i] GE 0 THEN BEGIN
         ii = numsel[i]
         ;; Test if degenerate check must be carried out for this
         ;; variable
         IF n_elements(degenerate) gt 0 THEN BEGIN
            dgtest = ( where( StrUpCase(degenerate) EQ StrUpCase(names[ii])) GE 0 )
         ENDIF ELSE BEGIN
            dgtest = 0
            unlimii = -1L
         ENDELSE

         ;; Find out dimension IDs for variable
         ispointer = ( Size(data.(ii), /TName) EQ 'POINTER' )
         IF ispointer THEN BEGIN 
            s = Size(*(data.(ii)), /Dimensions)
            thistype = Size(*(data.(ii)), /TName)
         ENDIF ELSE BEGIN 
            s = Size(data.(ii), /Dimensions)
            thistype = Size(data.(ii), /TName)
         ENDELSE 
         
         ;; Variable dimensions explicitly listed via VARDIM keyword
         vardimi = where(ii eq vardimNum)
         if (vardimi gt -1) then begin
            thisVardim = vardim[*, vardimi]
            case n_elements(s) of
               1: begin
                  if (s[0] eq 0) then begin
                     wd = -1L
                     dvec = -1L
                  endif else begin
                     wd = make_selection(strupcase(dimnames), $
                                              strupcase(thisVardim[1]))
                     dvec = [dimdef[wd]]
                  endelse
               end
               2: begin
                  wd1 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[1]))
                  wd2 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[2]))
                  dvec = [dimdef[wd1], dimdef[wd2]]
               end
               3: begin
                  wd1 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[1]))
                  wd2 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[2]))
                  wd3 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[3]))
                  dvec = [dimdef[wd1], dimdef[wd2], dimdef[wd3]]
               end
               4: begin
                  wd1 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[1]))
                  wd2 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[2]))
                  wd3 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[3]))
                  wd4 = make_selection(strupcase(dimnames), $
                                            strupcase(thisVardim[4]))
                  dvec = [dimdef[wd1], dimdef[wd2], dimdef[wd3], dimdef[wd4]]
               end
               ELSE : Message, 'Program only written for up to 4 dimensions'
            endcase
            ;; variable dimensions determined automatically through
            ;; comparison of dimension sizes
         endif else begin
            CASE N_Elements(s) OF
               1 : BEGIN
                  IF s[0] EQ 0 THEN BEGIN  ;; scalar variable
                     wd = -1L
                     dvec = -1L
                     IF dgtest AND wd NE unlimii THEN BEGIN 
                        dvec  = [ dimdef[unlimii] ]
                     ENDIF 
                  ENDIF ELSE begin
                     wd = Where(dims EQ s[0])
                     dvec = [ dimdef[wd[0]] ]
                     IF dgtest AND wd[0] NE unlimii THEN BEGIN 
                        dvec  = [ dvec, dimdef[unlimii] ]
                     ENDIF 
                  ENDELSE 
               END 
               2 : BEGIN
                  wd1 = Where(dims EQ s[0])
                  wd2 = Where(dims EQ s[1])
                  IF (wd1[0]<wd2[0]) LT 0 THEN message,'dimension error in '+names[ii]+':'+string(wd1,wd2,format='(2i4)')
                  dvec = [ dimdef[wd1[0]], dimdef[wd2[0]] ]
                  IF dgtest AND wd1[0] NE unlimii AND wd2[0] NE unlimii THEN BEGIN 
                     dvec  = [ dvec, dimdef[unlimii] ]
                  ENDIF 
               END 
               3 : BEGIN
                  wd1 = Where(dims EQ s[0])
                  wd2 = Where(dims EQ s[1])
                  wd3 = Where(dims EQ s[2])
                  IF (wd1[0]<wd2[0]<wd3[0]) LT 0 THEN message,'dimension error in '+names[ii]+':'+string(wd1,wd2,wd3,format='(3i4)')
                  dvec = [ dimdef[wd1[0]], dimdef[wd2[0]], $
                           dimdef[wd3[0]] ]
                  IF dgtest AND wd1[0] NE unlimii AND wd2[0] NE unlimii $
                     AND wd3[0] NE unlimii THEN BEGIN 
                     dvec  = [ dvec, dimdef[unlimii] ]
                  ENDIF 
               END 
               4 : BEGIN
                  wd1 = Where(dims EQ s[0])
                  wd2 = Where(dims EQ s[1])
                  wd3 = Where(dims EQ s[2])
                  wd4 = Where(dims EQ s[3])
                  IF (wd1[0]<wd2[0]<wd3[0]<wd4[0]) LT 0 THEN message,'dimension error in '+names[ii]+':'+string(wd1,wd2,wd3,wd4,format='(4i4)')
                  dvec = [ dimdef[wd1[0]], dimdef[wd2[0]], $
                           dimdef[wd3[0]], dimdef[wd4[0]] ]
                  IF dgtest AND wd1[0] NE unlimii AND wd2[0] NE unlimii $
                     AND wd3[0] NE unlimii AND wd4[0] NE unlimii THEN BEGIN 
                     dvec  = [ dvec, dimdef[unlimii] ]
                  ENDIF 
               END 
               ELSE : Message, 'Program only written for up to 4 dimensions'
            ENDCASE 
         endelse

         ;; Create NCDF variable definition
         thename = names[ii]
         IF N_Elements(Upcase_name) GE i+1 THEN BEGIN
            IF Upcase_name[i] GT 0 THEN BEGIN
               thename = StrUpcase(thename)
            ENDIF ELSE BEGIN
               thename = STrLowCase(thename)
            ENDELSE 
         ENDIF ELSE BEGIN
            thename = StrLowcase(thename)
         ENDELSE 

         IF max(dvec) LT 0 THEN BEGIN
            IF thistype EQ 'BYTE' THEN BEGIN
               vdef[i] = NCDF_VARDEF(id, thename, /BYTE)
            ENDIF ELSE IF thistype EQ 'LONG' THEN BEGIN
               vdef[i] = NCDF_VARDEF(id, thename, /LONG)
            ENDIF ELSE BEGIN
               vdef[i] = NCDF_VARDEF(id, thename, /FLOAT)
            ENDELSE 
         ENDIF ELSE BEGIN 
            IF thistype EQ 'BYTE' THEN BEGIN
               vdef[i] = NCDF_VARDEF(id, thename, dvec, /BYTE)
            ENDIF ELSE IF thistype EQ 'LONG' THEN BEGIN
               vdef[i] = NCDF_VARDEF(id, thename, dvec, /LONG)
            ENDIF ELSE BEGIN
               vdef[i] = NCDF_VARDEF(id, thename, dvec, /FLOAT)
            ENDELSE 
         ENDELSE 
         IF vdef[i] LT 0 THEN BEGIN 
            print,'Error defining variable '+thename
            stop
         ENDIF 

         ;; Check which attributes are available
         IF hasattr THEN BEGIN
            w = Where(attrnames EQ names[ii], cnt)
            IF cnt GT 0 THEN BEGIN
               ispointer = ( Size(attributes.(w[0]),/TName) EQ 'POINTER' )
               IF ispointer THEN BEGIN
                  thisattr = *(attributes.(w[0]))
               ENDIF ELSE BEGIN 
                  thisattr = attributes.(w[0])
               ENDELSE 
               ;; Check validity of attribute
               IF (size(thisattr, /TName) EQ 'STRUCT') THEN BEGIN 
                  properties = Tag_Names(thisattr)
                  FOR k=0L, N_Elements(properties)-1 DO BEGIN
                     ;; Check property data type 
                     theattribute = nw_GetAttributeType( thisattr.(k), $
                                                         pchar=pchar, pshort=pshort, $
                                                         plong=plong, pfloat=pfloat, $
                                                         pdouble=pdouble)
                     ;; Define NCDF variable attributes
                     NCDF_ATTPUT, id, vdef[i], StrLowCase(properties[k]), $
                       theattribute, char=pchar, short=pshort, long=plong, $
                       float=pfloat, double=pdouble
                  ENDFOR 
               ENDIF ;; thisattr = STRUCT
            ENDIF 
         ENDIF
      ENDIF
   ENDFOR 


   ;; Define global attributes
   hcnt = 0L
   history_default = 'created '+systime(0)+' by IDL ncdf_write'
   IF hasattr THEN BEGIN
      IF ChkStru(attributes,'GLOBAL') THEN BEGIN
         ispointer = ( Size(attributes.global, /TName) EQ 'POINTER' )
         IF ispointer THEN gattr = *(attributes.global) ELSE gattr = attributes.global
         ;; make sure gattr is a structure, else create one
         if ChkStru(gattr) eq 0 then begin
            gattr = { title:'netcdf data file created by ncdf_write.pro' }
         endif 
         gtags = StrLowCase(Tag_Names(gattr))

         ;; change first letter of 'conventions' to uppercase
         wc = where(gtags EQ 'conventions', cnt)
         IF cnt GT 0 THEN gtags[wc[0]] = 'Conventions'

         ;; Default history entry
         IF N_Elements(history_entry) EQ 0 THEN BEGIN
            wh = Where(gtags EQ 'history', hcnt)
            IF hcnt GT 0 THEN BEGIN
               history_entry = systime(0) + 'IDL ncdf_write'
            ENDIF ELSE BEGIN
               history_entry = history_default
            ENDELSE
         ENDIF

         lf = String(10B)
         FOR k = 0L, N_Tags(gattr)-1 DO BEGIN
            ;; Check property data type 
            theattribute = nw_GetAttributeType(gattr.(k), pchar=pchar, pshort=pshort, $
                                               plong=plong, pfloat=pfloat, pdouble=pdouble)
            ;; If this is the history attribute, remember it
            IF gtags[k] EQ 'history' THEN BEGIN
               history_entry = String(history_entry) + lf + String(gattr.(k))
            ENDIF ELSE BEGIN
               NCDF_ATTPUT, id, /GLOBAL, gtags[k], theattribute, $
                  char=pchar, short=pshort, long=plong, float=pfloat, double=pdouble
            ENDELSE 
         ENDFOR 
      ENDIF 
   ENDIF ELSE BEGIN
      ;; Default history entry
      IF N_Elements(history_entry) EQ 0 THEN history_entry = history_default
   ENDELSE 
   IF N_Elements(history_entry) EQ 0 THEN BEGIN
      history_entry = history_default
   ENDIF

   ;; Append history entry at the end
   NCDF_ATTPUT, id, /GLOBAL, 'history', String(history_entry), /CHAR

   ;; change to DATA mode and put data into file
   NCDF_CONTROL,id,/ENDEF


   FOR i=0L, N_Elements(numsel)-1 DO BEGIN
      IF numsel[i] GE 0 THEN BEGIN
         ii = numsel[i]
         ispointer = ( Size(data.(ii), /TName) EQ 'POINTER' )
         IF ispointer THEN BEGIN 
            NCDF_VARPUT, id, vdef[i], *(data.(ii))
         ENDIF ELSE BEGIN 
            NCDF_VARPUT, id, vdef[i], data.(ii)
         ENDELSE 
      ENDIF 
   ENDFOR 

   ;; close file
   NCDF_CLOSE,id


END



