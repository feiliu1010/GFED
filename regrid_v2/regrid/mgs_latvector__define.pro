;+
; NAME:
;   MGS_LatVector (object)
;
; PURPOSE:
;     This object defines the latitude component of a rectangular
;   geophysical grid. Practically all of it's functionality is
;   inherited from the generic MGS_GridVector object; this object
;   simply constrains a few parameters (e.g. automatic setting of the
;   limit vector), and handles the computation of grid box weights
;   which are defined as gaussian weights for latitudes.
;     The gridvector object (and, hence, the latvector object) is
;   particular useful if one is dealing with different grids which may
;   have different resolutions or a different offset. A gridvector
;   allows you to:
;    (1) easily create a grid vector by supplying a list of values or
;        a few key parameters, 
;    (2) to retrieve grid vector indices for a given coordinate range,
;    (3) to retrieve coordinate values for an index array,
;    (4) to determine the fractional coverage of grid boxes for
;        a given coordinate range.
;     The grid vector defines a list of "center" coordinates and a
;   list of "edge" coordinates. The program can compute one from the
;   other assuming some regularity in the grid. The grid need not be
;   global in scope, nor need the grid box center coordinates be
;   equidistant. 
;     MGS_LatVector can handle grid vectors in reverse direction
;   (i.e. north->south latitudes), but these may not be cyclical. 
;
; AUTHOR:
;
;   Dr. Martin Schultz
;   Max-Planck-Institut fuer Meteorologie
;   Bundesstr. 55, D-20146 Hamburg
;   email: martin.schultz@dkrz.de
;
; CATEGORY:
;   General object programming
;
; CALLING SEQUENCE:
;
; NOTES:
;
; EXAMPLE:
;    See example procedure at the end of this file
;
; MODIFICATION HISTORY:
;   mgs, 17 Aug 2001:
;          - derived from generic GridVector object
;   mgs, 04 Sep 2001:
;          - added isglobal property to force latitude range -90 to 90
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
; Copyright © 2001 Martin Schultz
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


; =====================================================================================
; Grid vector information routines:
; GetIndex: Return the index vector for a given coordinate range
; GetValue: Return the coordinate values for a given index
; (unchanged from MGS_GridVector)

; =====================================================================================
; Grid vector creation and manipulation routines:
; PrintStatus : report some information about current settings
; Shift       : translate grid vector and impose or adjust limit
; Reverse     : reverse the order of the grid vector 
; Create      : create a new grid vector



; -------------------------------------------------------------------------------------
; ComputeWeights: (private)
;   This method computes the gaussian weights of the latitude
; elements. The gaussian weight indicates the fraction of the latitude
; half-circle covered by an individual latitude element. It is defined
; so that the integral from -90..90 dgerees is 2. This must be taken
; into account when using the gaussian weights to compute grid box
; areas.

FUNCTION MGS_LatVector::ComputeWeights

   IF self.nvalues EQ 0 THEN RETURN, 0.

   ;; Compute gaussian weights
   tmp = sin(*self.edges * !DPI/180.d0)
   result = (tmp - shift(tmp,1))[1:*]

   ;; Safety check: edge coordinates should be monotonously
   ;; increasing. In case they are not, warn and stop
   IF Max(result) LT 0. THEN stop, self.name+': Edges not monotonous!'

   RETURN, result
END


; -------------------------------------------------------------------------------------
; gauaw:  (private)
; This routine computes the gaussian latitude abscissas and
; weights. Note that the integral of the gaussian weights
; from -90 to 90 degrees lat. yields 2 per definition. 
;
; Translated to IDL from ECHAM4 code written by 
; M. Jarraud, ECMWF, Dec 1982, 
; L. Kornblueh, U. Schulzweida, and A. Rhodin, MPIfM, 1999
; Martin Schultz, 19 JUL 2000 and bug fix 24 Aug 2000

; INPUT: nlat = The number of latitude coordinates that shall be
; returned
; OUTPUT: lats = vector of latitudes (~ grid box centers)
;         weights = vector of gaussian weights

; Comments:
; Gaussian latitudes are defined as latitudes where the Legendre
; functions are zero for a given truncation. They do not
; correspond directly to the center coordinates of a grid box. To
; compute the grid box edges, use the gaussian weights:
;    sinlat = fltarr(nlat+1)
;    sinlat[0] = -1.      ; South Pole
;    for i=1L,nlats do sinlat[i] = sinlat[i-1] + weights[i-1]
;    latedges = asin(sinlat)*180./!PI

PRO MGS_LatVector::gauaw, nlat, lats=lats, weights=weights


   ;; Perform calculation in double precision
   eps = 1.D-30     ; machine precision
   ITEMAX = 20      ; maximum number of iterations

   ;; result arrays need one extra element because FORTRAN starts at
   ;; index 1, and code wasn't "rewritten".

   lats = dblarr(nlat+1)
   weights = dblarr(nlat+1)

   ins2 = fix( nlat/2. + nlat MOD 2. )

   ;; Find first approximation of the roots of the
   ;; Legendre polynomial of degree nlat
    
   FOR jgl = 1L, ins2 DO BEGIN
      z = double(4*jgl-1)*!dpi/double(4.*long(nlat)+2)
      lats(jgl) = COS(z+1./(TAN(z)*double(8.*long(nlat)^2)))
   ENDFOR

   ;; Computes roots and weights
   ;; Perform the Newton loop
   ;; Find 0 of Legendre polynomial with Newton loop
   FOR jgl = 1L, ins2 DO BEGIN

       za = lats[jgl]
    
       FOR iter = 1L, itemax+1 DO BEGIN
          zk = 0.0D

          ;; Newton iteration step
    
          zkm2 = 1.0D
          zkm1 = za
          zx = za
          FOR jn = 2L, nlat DO BEGIN
             zk = (double(2*jn-1)*zx*zkm1-double(jn-1)*zkm2)/double(jn)
             zkm2 = zkm1
             zkm1 = zk
          ENDFOR
          zkm1 = zkm2
          zldn = (double(nlat)*(zkm1-zx*zk))/(1.-zx*zx)
          zmod = -zk/zldn
          zxn = zx+zmod
          zan = zxn
          ;; computes weight
    
          zkm2 = 1.0
          zkm1 = zxn
          zx = zxn
          FOR jn = 2L,nlat DO BEGIN
             zk = (double(2*jn-1)*zx*zkm1-double(jn-1)*zkm2)/double(jn)
             zkm2 = zkm1
             zkm1 = zk
          ENDFOR
          zkm1 = zkm2
          zw = (1.0-zx*zx)/(double(long(nlat)*long(nlat))*zkm1*zkm1)
          za = zan
          IF (ABS(zmod) LE eps) THEN GOTO,finished
          ;; Check for math error 
          IF Check_Math(/PRINT) NE 0 THEN BEGIN
             help,iter,zmod,zldn
             stop
          ENDIF
       ENDFOR

finished:
       lats[jgl] = zan
       weights[jgl] = zw * 2.
    
    ENDFOR

    FOR jgl = 1L, fix(nlat/2.) DO BEGIN
       isym = nlat-jgl+1
       lats(isym) = -lats(jgl)
       weights(isym) =  weights(jgl)
    ENDFOR


    ;; since IDL starts at index 0 but code was written in FORTRAN
    ;; with start index 1, omit the first value.
    lats = lats[1:*]
    weights = weights[1:*]

END


; -------------------------------------------------------------------------------------
; CreateGaussianLatitudes: (private)
;   This method creates a gaussian latitude vector and sets the
; respective object properties. Latitudes are always computed
; south->north in this method.

PRO MGS_LatVector::CreateGaussianLatitudes, nvalues=nvalues, ok=ok

   ok = 0

   IF N_Elements(nvalues) EQ 0 THEN BEGIN
      self->ErrorMessage, $
         'Must specify nvalues for a gaussian grid vector'
      RETURN
   ENDIF 

   nv = nvalues
   self->gauaw, nv, lats=lats, weights=gw
   ;; Compute center and edge coordinates in degrees
   cvec = asin(Reverse(lats))*180.d0/!DPI
   sinlat = dblarr(nv+1)
   sinlat[0] = -1.d0            ; South Pole
   for i=1L,nv do sinlat[i] = sinlat[i-1] + gw[i-1]
   sinlat = sinlat < 1.d0
   wzero = Where(Abs(sinlat) LT 1.d-9, count)
   IF count GT 0 THEN sinlat[wzero] = 0.d0
   evec = asin(sinlat)*180.d0/!DPI
   
   ;; Store information in object fields
   *self.centers = cvec
   *self.edges   = evec
   *self.weights = gw
   self.nvalues  = nv
   
   ok = 1

END

; -------------------------------------------------------------------------------------
; Create:
;   This method overwrites the Create method of the generic gridvector
; object. It checks the type keyword for the value 'gaussian' and
; creates a gaussian latitude vector if this value is
; found. Otherwise, the inherited Create method is called with the
; default limit set to -90 .. 90.

FUNCTION MGS_LatVector::Create, values, $
                                type=type, $
                                nvalues=nvalues, $
                                start=start, $
                                limit=limit, $
                                _Extra=extra
                                ; inherited keywords from MGS_GridVector:
                                ;   edges (boolean)
                                ;   increment (float)
                                ;   adjust_values (boolean)
                                ;   allow_wrap (boolean)


   self->ClearGridDefinition
   retval = 0
   isdone = 0

   ;; Set default limit
   IF N_Elements(limit) EQ 0 THEN limit = [ -90., 90. ]

   ;; Check argument and keywords
   IF N_Elements(type) GT 0 THEN BEGIN
      IF StrLowCase(type) EQ 'gaussian' THEN BEGIN
         ;; Note: gaussian latitudes are always computed for the
         ;; complete latitude cycle. Use the limit keyword (or the
         ;; Truncate method) in order to cut out a region if desired.
         self->CreateGaussianLatitudes, nvalues=nvalues, ok=retval
         self.type = 'gaussian'

         ;; Analyse start keyword to find out direction
         reversed = 0
         IF N_Elements(start) GT 0 THEN BEGIN
            IF start GT 0. THEN reversed = 1
         ENDIF 
         IF self.debug GT 1 THEN print,'grid reversal : ',reversed
         self.reversed = reversed

         extent = limit[1] - limit[0]
         IF extent LT 179.99 THEN BEGIN
            retval = self->Truncate(limit)
         ENDIF 
         self.limit = limit

         isdone = 1
      ENDIF 
   ENDIF 

   IF NOT isdone THEN BEGIN
      retval = self->MGS_GridVector::Create(values, $
                                            type=type, $
                                            nvalues=nvalues, $
                                            start=start, $
                                            limit=limit, $
                                            _Extra=extra)
   ENDIF 

   RETURN, retval

END


; =====================================================================================
; Standard object methods:
;     GetProperty : retrieve object values
;     SetProperty : set object values
;     Cleanup     : free object pointers and destroy
; (unchanged from MGS_GridVector)
;     Init        : initialize object

; -------------------------------------------------------------------------------------
; Init:
; This method initializes the object values for the longitude
; vector. If no limit range is provided, the limit is set to [-90.,
; 90.]. The object's name is defaulted to 'Latitude'. 
; In all other aspects, initialisation is performed by
; GridVector::Init. 

FUNCTION MGS_LatVector::Init,  $
                  values,                 $ ; The grid vector values (optional)
                  limit=limit,            $ ; the range limit of the grid vector
                  isglobal=isglobal,      $ ; force latitude range from -90 to 90
                  name=name,              $ ; the object's name (default 'Longitude')
                  _Extra=extra  ; For future additions
                                ; Inherited keywords:
                                ; from MGS_GridVector:
                                ; type      : The grid vector type ('regular', 
                                ;             'irregular', 'gaussian')
                                ; edges     : Values denote edge coordinates 
                                ;             rather than centers (boolean)
                                ; nvalues   : Number of center coordinates
                                ; start     : First center coordinate value
                                ; increment : Increment between consecutive 
                                ;             coordinates (regular vector)
                                ; adjust_values : cut vectors if out of range
                                ;  
                                ; from MGS_BaseObject:
                                ; uvalue    : a user-defined value
                                ; no_dialog : Don't display message dialogs
                                ; debug     : Put object in debugging state


   ;; Set default name
   IF N_Elements(name) EQ 0 THEN name = 'Latitude'

   ;; Set default limit range to cover the globe
   IF N_Elements(limit) EQ 0 THEN limit = [-90., 90.]

   retval = self->MGS_GridVector::Init(values, name=name, _Extra=extra)

   ;; Force latitude extent
   IF retval AND Keyword_Set(isglobal) THEN BEGIN
      IF (*self.edges)[0] GT 0. THEN BEGIN
         (*self.edges)[0] = 90.
      ENDIF ELSE BEGIN
         (*self.edges)[0] = -90.
      ENDELSE 
      nedges = N_Elements(*self.edges)
      IF (*self.edges)[nedges-1] GT 0. THEN BEGIN
         (*self.edges)[nedges-1] = 90.
      ENDIF ELSE BEGIN
         (*self.edges)[nedges-1] = -90.
      ENDELSE 
   ENDIF 

   RETURN, retval

END


; -------------------------------------------------------------------------------------
; This is the object definition for the gridvector object.
; NOTE:
; (1) If allow_wrap is set to 1, limit is only used to specify the
; allowed extent of the grid (the wrapping period).

PRO MGS_LatVector__Define

   objectClass = { MGS_LatVector,  $    ; The object class

                   INHERITS MGS_GridVector  $
                 }
                   
END


