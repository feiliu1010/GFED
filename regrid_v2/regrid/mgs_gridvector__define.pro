;+
; NAME:
;   MGS_GridVector (object)
;
; PURPOSE:
;     This object defines a component of a rectangular geophysical
;   grid. For practical applications, it is recommended to use the
;   LonVector and LatVector objects instead.
;     The gridvector object is particular useful if one is dealing
;   with different grids which may have different resolutions or a
;   different offset. A gridvector allows you to:
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
;     MGS_GridVector allows for cyclical coordinates (i.e. longitudes
;   can "wrap" on a global grid) and it deals with such coordinates
;   transparently. The program can also handle grid vectors in reverse
;   direction (i.e. north->south latitudes), but these may not be cyclical.
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
;    *** THIS OBJECT STILL NEEDS TO UNDERGO RIGOROUS TESTING !! ***
;
; EXAMPLE:
;    See example procedure at the end of this file
;
;
; MODIFICATION HISTORY:
;   Martin G. Schultz, 16 May 2001: VERSION 0.10
;   mgs, 01 Jun 2001: 
;          - added debug output for grid reversal
;          - GetProperty now returns values reversed if reversed flag
;            is set
;          - bug fix in GetValue: values were incorrect for reversed
;            grid
;   mgs, 25 Jun 2001:
;          - SetProperty method activated for allow_wrap
;   mgs, 17 Aug 2001:
;          - weights property now computed in seperate routine which
;            is overwritten by the LonVector and LatVector objects.
;   mgs, 23 Aug 2001:
;          - bug fix in GetFraction: values need to be sorted if only
;            one index of a reversed latitude is passed.
;   mgs, 17 Apr 2002:
;          - GetIndex now detects missing value
;   mgs, 24 Jan 2003:
;          - fixed roundoff error in edge or center computation (near-zero)
;   mgs, 14 Apr 2005:
;          - fixed error for creating regular grids with N->S
;            latitudes
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
; Copyright © 2000-2001 Martin Schultz
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



; -------------------------------------------------------------------------------------
; GetFraction:  (private)
;   This method computes the fractions of a number of neighbouring grid
; boxes specified by an index vector that are contained in the given range vector.
; NOTE:
; (1) No error checking is done on the range vector. It is recommended
; not to use GetFraction directly but retrieve the fractions through
; the GetIndex function instead.
; (2) For simplicity, GetFraction always assumes index and range are
; in ascending order. Rearranging and reversal is done in GetIndex
; (another reason not to use GetFraction directly).


FUNCTION MGS_GridVector::GetFraction, index, range

   count = N_Elements(index)
   IF count EQ 0 THEN RETURN, 0.d0
   IF Min(index) LT 0 THEN RETURN, 0.0d0

   ;; Preset all fractions with 1.
   fraction = DblArr(count) + 1.d0

   ;; Adjust first and last fraction value
   values = self->GetValue(index, /edges, /plus_one, $
                           order=self.allow_wrap)

   IF N_Elements(values) LT 2 THEN RETURN, 0.0d0

   IF N_Elements(index) EQ 1 THEN values = values[Sort(values)]

IF self.debug GE 3 THEN BEGIN
print,'****1** values = ',values
print,'****1** range  = ',range
ENDIF 

   ;; ... first fraction 
   IF self.allow_wrap THEN BEGIN
      wrap_period = self.limit[1] - self.limit[0]
      f0 = ((range[0]-values[0]+10.d0*wrap_period) mod wrap_period)/(values[1]-values[0])
   ENDIF ELSE BEGIN
      f0 = (range[0]-values[0])/(values[1]-values[0])
      f0 = f0 > 0.d0
   ENDELSE 

   ;; *** DEBUG ***
   if f0 lt 0. or f0 gt 1. then stop
   fraction[0] = 1.d0 - f0
   IF Abs(fraction[0]) LT 1.d-9 THEN fraction[0] = 0.d0


   ;; ... last fraction
   IF N_Elements(range) EQ 1 THEN range=[range, range]
   lasti = N_Elements(index)-1
   IF self.allow_wrap THEN BEGIN
      fi = ((values[lasti+1]-range[1]+wrap_period) mod wrap_period)/ $
         (values[lasti+1]-values[lasti])
   ENDIF ELSE BEGIN
      fi = (values[lasti+1]-range[1])/(values[lasti+1]-values[lasti])
      fi = fi > 0.d0
   ENDELSE 

   ;; *** DEBUG ***
   if fi lt 0. or fi gt 1. then stop
   fraction[lasti] = fraction[lasti] - fi
   IF Abs(fraction[lasti]) LT 1.d-9 THEN fraction[lasti] = 0.d0

   ;; *** DEBUGGING ONLY ***
   IF Min(fraction, max=maxfrac) LT 0. OR maxfrac GT 1. THEN BEGIN
      print,'****** values=',values
      print,'****** range=',range
      print,'f0,fi,lasti = ',f0,fi,lasti
      self->ErrorMessage,['Invalid fraction:',String(fraction)]
stop
   ENDIF 

   RETURN, fraction

END


; -------------------------------------------------------------------------------------
; GetIndex:
;   This function returns the vector indices and fractional coverage
; for a given coordinate range. A value of -1L is returned if no index
; is found in the given range. 
;
; Arguments and Keywords:
; range: The longitude or latitude range that shall be extracted
; exclusive: Set this keyword if grid boxes shall be excluded if the 
;    range value is on the grid box edge. Default is to include the
;    neighbouring grid boxes.
; no_adjust: (private keyword) For the default "inclusive" index
;    search, the range vector is adjusted in order to prevent possible
;    mismatching due to roundoff errors. However, if wrapping occurs,
;    GetIndex is called recursively, and then it must not use adjusted
;    range values.
; fractions: A named variable provided with this keyword will contain
;    the fractional coverage for each grid box index.
; count: The number of indices found
; 
; NOTES:
; (1) This function constitues one of the foremost reasons why I
; decided to write this object: Getting an index vector for a specific
; longitude/latitude range can be hellish. There are so many pitfalls
; along the way: wrapping of longitude coordinates, order of latitude
; coordinates, what to do at grid box interfaces, roundoff errors,
; ... I can only hope that this code is structured well enough so that
; I will be able to understand it if someone finds a bug ;-(

FUNCTION MGS_GridVector::GetIndex, Range, $
                       exclusive=exclusive, $
                       no_adjust=no_adjust, $
                       fractions=fractions, $
                       Count=count

   retval = -1L
   count = 0L

   ;; No range: error
   IF N_Elements(range) EQ 0 OR N_Elements(Range) GT 2 THEN BEGIN
      self->ErrorMessage, 'Range must have 1 or 2 elements'
      RETURN, retval
   ENDIF 

   ;; Detect missing value
   IF max(ABS(range)) GT 1.e3 THEN BEGIN
      return, retval
   ENDIF 

   thisrange=Double(range)

   ;; Individual point
   IF N_Elements(thisrange) EQ 1 THEN BEGIN
      thisrange = [ thisrange, thisrange ]
   ENDIF
   
   ;; Order of range reversed?
   reversed_range = (thisrange[0] GT thisrange[1]) AND NOT self.allow_wrap
   IF reversed_range THEN thisrange = Reverse(thisrange)

   ;; Determine a few key parameters
   extent = self.limit[1] - self.limit[0]
   IF self.allow_wrap THEN wrap_period = extent

   ;; Check range: must be within wrap_period or extent (default=360.) degrees
   distance = thisrange[1]-thisrange[0]
   IF distance GT extent THEN BEGIN
      self->ErrorMessage, 'Range exceeds allowed extent of '+ $
         StrTrim(String(extent,format='(f10.3)'),2)
stop  ; ###
      RETURN, retval
   ENDIF 

   ;; If not exclusive, adjust range to make sure all grid boxes are
   ;; found (beware of roundoff errors)
   nonadjrange = thisrange
   IF Keyword_Set(exclusive) EQ 0 AND Keyword_Set(no_adjust) EQ 0 THEN $
      thisrange = thisrange + [ -1.d-3, 1.d-3 ]
      
   ;; Adjust range coordinates so that they fit into vector extent
   minedges = Min(*self.edges, max=maxedges)
   IF self.allow_wrap THEN BEGIN
      IF self.debug GT 1 THEN $
         print,'### wrap_period, limits = ',wrap_period,minedges,maxedges
      WHILE thisrange[0] LT minedges DO $
         thisrange[0] = thisrange[0] + wrap_period
      WHILE thisrange[0] GT maxedges DO $
         thisrange[0] = thisrange[0] - wrap_period
      WHILE thisrange[1] LT minedges DO $
         thisrange[1] = thisrange[1] + wrap_period
      WHILE thisrange[1] GT maxedges DO $
         thisrange[1] = thisrange[1] - wrap_period
   ENDIF 

   IF self.debug GT 1 THEN BEGIN
      print,'### Status: new range, allow_wrap: ', $
         String(thisrange,format='(2f14.10)'), self.allow_wrap
      print,'###         reversed grid, reversed range: ', $
         self.reversed, reversed_range
   ENDIF 

   ;; Set up range vector or pair of range vectors:
   must_wrap = (thisrange[0] GT thisrange[1]) AND self.allow_wrap
   range1 = thisrange

   ;; Split into two range vectors if wrapping occurs
   IF must_wrap THEN BEGIN
      range1 = [ thisrange[0], maxedges ]
      range2 = [ minedges, thisrange[1] ]
   ENDIF
   
   ;; Fix special case: distance is a multiple of wrap period
   ;; This would have been transformed to a single point
   IF self.allow_wrap AND distance GT 0. AND $
      Abs(thisrange[1]-thisrange[0]) LT 1.e-4 THEN BEGIN
      range1 = [ thisrange[0], maxedges ]
      range2 = [ minedges, thisrange[1] ]
   ENDIF 

   ;; Get indices: in case of wrapping, call recursively for
   ;; individual parts
   IF N_Elements(range2) GT 0 THEN BEGIN
      index1 = self->GetIndex(range1, $
                                    exclusive=exclusive, $
                                    no_adjust=1)
      index2 = self->GetIndex(range2, $
                                    exclusive=exclusive, $
                                    no_adjust=1)

      IF self.debug EQ 3 THEN BEGIN
         print,'### index1 = ',index1
         print,'### index2 = ',index2
      ENDIF 
      index = [ index1, index2 ]
      ok = Where(index GE 0, count)
      IF count GT 0 THEN index = index[ok]

   ENDIF ELSE BEGIN

      ;; No wrapping: retrieve indices directly
      ;; Exclusive selection
      IF Keyword_Set(exclusive) THEN BEGIN
         minindex = Min(Where(*self.edges GT range1[0]))
         maxindex = Max(Where(*self.edges LT range1[1]))
         IF self.debug GT 1 THEN $
            print,'### exclusive index = ',minindex,'...',maxindex
      ENDIF ELSE BEGIN
         ;; inclusive search (default)
         minindex = Min(Where(*self.edges GE range1[0]))
         maxindex = Max(Where(*self.edges LE range1[1]))
         IF self.debug GT 1 THEN $
            print,'### inclusive index = ',minindex,'...',maxindex
      ENDELSE      

      success = (minindex GE 0 AND maxindex GE 0)

      ;; Transform edge indices to center indices:
      ;; First index is a right edge, i.e. center index = edge index-1
      ;; Last index is a left edge, i.e. center index = edge index
      ;; Index number must stay below n
      IF success THEN BEGIN
         minindex = (minindex - 1) > 0
         maxindex = maxindex < (self.nvalues-1)
         count = maxindex-minindex+1
         IF self.debug GT 1 THEN $
            print,'### minindex, maxindex, count : ',minindex,maxindex,count
         IF count GT 0 THEN index = LIndgen(count)+minindex
      ENDIF 
   ENDELSE 

   ;; Revert index if necessary
   IF self.reversed AND count GT 0 THEN $
      index = self.nvalues - 1 - index

IF self.debug GE 3 THEN BEGIN
   print,'*****999*** self.reversed, reversed_range, index before fractions :', $
      self.reversed, reversed_range, index
ENDIF 
   ;; Compute fractional coverage if requested
   ;; *** NOTE: Might have to be done before index is reversed ! ***
   IF Arg_Present(fractions) THEN BEGIN
      fractions = self->GetFraction(index, nonadjrange)
   ENDIF 

   IF reversed_range AND count GT 0 THEN BEGIN
      index = reverse(index)
      IF N_Elements(fractions) GT 1 THEN fractions = Reverse(fractions)
   ENDIF 

   IF self.debug GT 1 AND count GT 0 THEN BEGIN
      print,'### final index = ',index
   ENDIF 

   IF count GT 0 THEN retval = index

   RETURN, retval

END


; -------------------------------------------------------------------------------------
; GetValue:
;    This method returns coordinate values for a given index
; vector. NaN is returned if the index is invalid.
; Arguments and Keywords:
; Index: The index value or index vector
; Edge: Return the coordinate values of the "left" edges instead of
;    the centers
; Plus_One: Returns one extra edge value (the last right edge). Only
;    effective if edge is also set.
; Order: Return the values in monotonously increasing order. This is
;    useful for longitude vectors that may have wrapped (only active
;    for LonIndex=1). Example: If a grid is defined from 0..360 degree
;    longitude with a 10 degree spacing, GetIndex(LonRange=[-10.,10.])
;    will return indices for 350., 0., and 10. degrees. Without the
;    order keyword, these values will be returned by
;    GetValue(index). If you set the order keyword, GetValue will
;    instead return -10., 0., and 10.

FUNCTION MGS_GridVector::GetValue, index, Edges=edges, $
                       Plus_One=plus_one, Order=order


   retval = !Values.F_NaN
   
   IF N_Elements(index) EQ 0 THEN RETURN, retval

   ;; Test index range
   minindex = Min(index, max=maxindex)
   IF minindex LT 0 THEN RETURN, retval 
   strict = Keyword_Set(plus_one) OR Keyword_Set(edges) EQ 0
   IF strict THEN BEGIN
      IF maxindex GT self.nvalues-1 THEN RETURN, retval
   ENDIF ELSE BEGIN
      IF maxindex GT self.nvalues THEN RETURN, retval
   ENDELSE 

   lastindex = index[N_Elements(index)-1]

   ;; Need to retrieve center or edge coordinates via GetProperty to
   ;; take into account possible grid reversal
   IF Keyword_Set(edges) THEN BEGIN
      self->GetProperty, edges=edges
      result = (edges)[index]
      IF Keyword_Set(plus_one) THEN BEGIN
         ;; Add an extra edge value. If the index vector is in
         ;; ascending order, the "right" edge must be appended at the
         ;; end, for a reversed index, the extra edge must be
         ;; prepended
         index_reversed = 0
         IF N_Elements(index) GT 1 THEN BEGIN
            IF index[N_Elements(index)-2]-lastindex EQ 1 THEN index_reversed = 1
         ENDIF 
         IF index_reversed THEN BEGIN
            result = [ (edges)[index[0]+1], result ]
         ENDIF ELSE BEGIN
            result = [ result, (edges)[lastindex+1] ]
         ENDELSE 
      ENDIF 
   ENDIF ELSE BEGIN
      self->GetProperty, centers=centers
      result = (centers)[index]
   ENDELSE 

   ;; Add or subtract wrap period if values shall be monotonous
   IF self.allow_wrap AND Keyword_Set(order) AND N_Elements(result) GT 1 THEN BEGIN
      wrap_period = self.limit[1] - self.limit[0]
      minval = Min(result)
      lasti = N_Elements(result)-1
      ;; left edge
      IF result[0] GT minval THEN BEGIN
         i=0L
         WHILE result[i] GT minval AND i LT lasti DO BEGIN
            result[i] = result[i]-wrap_period
            i = i+1
         ENDWHILE 
      ENDIF 
      ;; right edge
      maxval = Max(result)
      IF result[lasti] LT maxval THEN BEGIN
         i=lasti
         WHILE result[i] LT maxval AND i GT 0L DO BEGIN
            result[i] = result[i]+wrap_period
            i = i-1
         ENDWHILE 
      ENDIF 
   ENDIF 

   RETURN, result
END


; =====================================================================================
; Grid vector creation and manipulation routines:
; PrintStatus : report some information about current settings
; Shift       : translate grid vector and impose or adjust limit
; Reverse     : reverse the order of the grid vector 
; Create      : create a new grid vector


; -------------------------------------------------------------------------------------
; PrintStatus:
;   This method provides some status information about the grid

PRO MGS_GridVector::PrintStatus

   ;; Print status information
   print, 'The grid vector '+self.name+' contains:'
   IF self.nvalues EQ 0 THEN BEGIN
      print, 'no information.'
      RETURN
   ENDIF 

   print,self.nvalues,' center coordinates from ',(*self.centers)[0],' to ', $
      (*self.centers)[self.nvalues-1]
   print,N_Elements(*self.edges),' edge coordinates from ',(*self.edges)[0],' to ', $
      (*self.edges)[self.nvalues]
   print,' reversed = ',self.reversed,'  allow_wrap = ',self.allow_wrap
   print,' limit = ',self.limit

END


; -------------------------------------------------------------------------------------
; ComputeCenterCoordinates: (private)
;   This method uses the stored edge coordinates of either lon or lat
; to compute the center coordinates. Longitude and latitude are boolean
; flags to indicate which coordinates are to be computed.

FUNCTION MGS_GridVector::ComputeCenterCoordinates, edges

   ;; longitudes and latitudes are always assumed to be monotonously
   ;; increasing or decreasing and regularily spaced
   centers = 0.5*( edges + Shift(edges,1) )[1:*]

   ;; check for a value that is too close to zero to be non-zero
   w = where(abs(centers) LT 1.d-12, cnt)
   IF cnt GT 0 THEN centers[w] = 0.d0

   RETURN, centers

END


; -------------------------------------------------------------------------------------
; ComputeEdgeCoordinates: (private)
;   This method uses center coordinates to compute the edge
; coordinates assuming a regular spacing

FUNCTION MGS_GridVector::ComputeEdgeCoordinates, centers

   nvalues = N_Elements(centers)

   ;; Compute delta between "central" coordinates
   delta = ( centers - Shift(centers,1) )[1:*]

   ;; Will add 1/2 delta to each center coordinate: need to
   ;; repeat the last one and invert the first delta
   delta = [ -0.5*delta[0], 0.5*delta, 0.5*delta[nvalues-2] ]

   ;; Need to duplicate first center coordinate and apply deltas
   edges = [ centers[0], centers ] + delta

   ;; check for a value that is too close to zero to be non-zero
   w = where(abs(edges) LT 1.d-12, cnt)
   IF cnt GT 0 THEN edges[w] = 0.d0

   RETURN, edges

END


; -------------------------------------------------------------------------------------
; ComputeWeights: (private)
;   This method is a placeholder for the respective method from the
; LonVector and LatVector object. Weights are computed as normalized,
; relative size of each gridvector element.

FUNCTION MGS_GridVector::ComputeWeights

   IF self.nvalues EQ 0 THEN RETURN, 0.

   result = ( *self.edges - shift(*self.edges,1) )
   result = result[1:*]

   ;; Safety check: edge coordinates should be monotonously
   ;; increasing. In case they are not, warn and stop
   IF Max(result) LT 0. THEN stop, self.name+': Edges not monotonous!'

   ;; Normalize weights
   totres = Total(result)

   RETURN, result/totres
END


; -------------------------------------------------------------------------------------
; Truncate: (private)
;    Shorten the center and edge vectors according to the
; limit. The return value is 1 if truncation was successful, and 0
; otherwise. 
;    If the only_edges keyword is set, Truncate will abort with an
; error message if not all center coordinates lie within the given
; limit. 
; NOTE:
; (1) limit must be in ascending order

FUNCTION MGS_GridVector::Truncate, limit, only_edges=only_edges 

   retval = 0

   IF N_Elements(limit) NE 2 THEN BEGIN
      self->ErrorMessage, 'Limit must have two elements!'
      RETURN, retval
   ENDIF 

   nval = self.nvalues
   IF nval EQ 0 THEN RETURN, retval

   centers = *self.centers
   edges = *self.edges

   wcok = Where(centers GE limit[0] AND centers LE limit[1], count)

   retval = Keyword_Set(only_edges) EQ 0 OR count EQ nval

   IF count EQ 0 THEN BEGIN
      self->ErrorMessage, 'No coordinate values in given range'
      RETURN, 0
   ENDIF 

   IF retval THEN BEGIN
      centers = centers[wcok] 
      weok = [ wcok, wcok[count-1]+1 ]
      edges = edges[weok]
      edges[0] = edges[0] > limit[0]
      edges[count] = edges[count] < limit[1]
      nval = count
   ENDIF ELSE BEGIN
      self->ErrorMessage, 'Coordinate values exceed range'
   ENDELSE 

   IF retval THEN BEGIN
      ;; Store updated information in object properties
      self.nvalues = nval
      *self.centers = centers
      *self.edges = edges
      self.limit = limit

      ;; Compute new weighting factors
      weights = self->ComputeWeights()
      *self.weights = weights

   ENDIF 

   RETURN, retval

END
      

; -------------------------------------------------------------------------------------
; Shift:
;    This method shifts the current grid vector by the specified
; amount. Unless the adjust_limit keyword is set, grid boxes outside
; the current limit settings will be deleted. If you set adjust_limit,
; the limit vector will be shifted by the same amount as the center
; and edge coordinates, but only up to the boundaries given in the
; hard_limit keyword if any.

PRO MGS_GridVector::Shift, value, adjust_limit=adjust_limit, $
                  hard_limit=hard_limit

   IF N_Elements(value) EQ 0 THEN RETURN

   shiftval = Double(value[0])

   ;; Create a working copy of the grid vector values
   centers = *self.centers
   edges = *self.edges
   limit = self.limit

   ;; ... and a backup copy
   oldcenters = *self.centers
   oldedges = *self.edges
   oldlimit = self.limit

   ;; Change coordinates
   centers = centers + shiftval
   edges = edges + shiftval

   ;; Adjust limits
   IF Keyword_Set(adjust_limit) THEN BEGIN
      limit = limit + shiftval
      ;; Is a hard limit given?
      IF N_Elements(hard_limit) EQ 2 THEN BEGIN
         limit = (limit > hard_limit[0]) < hard_limit[1]
         IF limit[0] EQ limit[1] THEN BEGIN
            self->ErrorMessage, 'New limit outside hard limit range'
            RETURN
         ENDIF 
      ENDIF
   ENDIF

   IF self.debug GT 1 THEN BEGIN
      print,'### before Truncate:'
      print,'### centers = ',centers
      print,'### edges = ',edges
      print,'### limit = ',limit
   ENDIF 

   *self.centers = centers
   *self.edges   = edges
   self.limit    = limit

   ;; Truncate centers and edges to new limit value
   ok = self->Truncate(limit)

   IF self.debug GT 1 THEN BEGIN
      print,'### result of Truncate:',ok
   ENDIF 

   ;; Restore old values if truncation failed
   IF NOT ok THEN BEGIN
      *self.centers = oldcenters
      *self.edges   = oldedges
      self.limit    = oldlimit
   ENDIF 

END 


; -------------------------------------------------------------------------------------
; Reverse:
;    This method changes the order of the grid vector.

PRO MGS_GridVector::Reverse

   self.reversed = 1 - self.reversed

END 


; -------------------------------------------------------------------------------------
; ClearGridDefinition:
;   This method resets all object properties to an undefined grid.

PRO MGS_GridVector::ClearGridDefinition

   *self.centers = 0.d0
   *self.edges   = 0.d0
   *self.weights = 0.d0
   self.nvalues  = 0L
   self.reversed = 0
   self.limit = [ 0., 0. ]
   self.allow_wrap = 0
   self.type = 'unknown'

END


; -------------------------------------------------------------------------------------
; CreateFromValues: (private)
;   This method creates a grid definition vector from a vector of
; coordinate values. By default, the values are interpreted as
; coordinate center values; set the edges keyword to denote that
; values define the grid box edges instead. If centers are given,
; edges are computed automatically and vice versa.
;   This is a private method, the user interface is provided with the
; Create method.

PRO MGS_GridVector::CreateFromValues, values, edges=edges, ok=ok

   ok=ok

   IF self.debug GT 1 THEN print,'Creating grid vector from '+ $
      StrTrim(N_Elements(values),2)+' values...'

   ;; Need at least 2 values
   IF N_Elements(values) LT 2 THEN BEGIN
      self->ErrorMessage, 'Gridvector must have at least 2 values'
      RETURN
   ENDIF 

   ;; Check increment (vector reversed?)
   increment = values[1] - values[0]
   reversed = (increment LT 0.)
   
   ;; Make working copy of values, possibly reversed
   IF reversed THEN thevalues = Reverse(values) ELSE thevalues = values

   ;; Find out if values are center or edge values
   IF Keyword_Set(edges) THEN BEGIN
      evec = thevalues
      cvec = self->ComputeCenterCoordinates(evec)
   ENDIF ELSE BEGIN
      cvec = thevalues
      evec = self->ComputeEdgeCoordinates(cvec)
   ENDELSE 
   nv = N_Elements(cvec)

   ;; Store information in object fields
   *self.centers = cvec
   *self.edges   = evec
   self.nvalues  = nv

   ;; Compute the weight factors
   weights = self->ComputeWeights()
   *self.weights = weights

   ok = 1

END


; -------------------------------------------------------------------------------------
; CreateRegularGridVector: (private)
;   This method computes the center and dge coordinates and the
; weights for a regular grid, given the number of values, the first
; (center) value, and the increment.

PRO MGS_GridVector::CreateRegularGridVector, nvalues=nvalues, $
                                             start=start, $
                                             increment=increment, $
                                             ok=ok

   ok = 0

   ;; NOTE: Error condition is rather strict here; it would be
   ;; possible to compute either start, increment, or offset from
   ;; the others if the limit vector is also provided
   IF N_Elements(nvalues) EQ 0 OR N_Elements(start) EQ 0 $
      OR N_Elements(increment) EQ 0 THEN BEGIN
      self->ErrorMessage, $
         'Must specify nvalues, start, and increment if no values are given'
      RETURN
   ENDIF 
   IF self.debug GT 1 THEN print,'Creating grid vector with start = '+ $
      StrTrim(start,2)+', increment = '+StrTrim(increment,2)+', nvalues = '+ $
      StrTrim(nvalues,2)+' ...'

   ;; Make sure increment is positive and set reversed flag
   nv = nvalues
   reversed = (increment LT 0.)
   theincrement = Abs(increment)
   IF reversed THEN thestart = -Double(start) ELSE thestart = Double(start)
   cvec = LIndgen(nv)*Double(theincrement) + thestart
   evec = self->ComputeEdgeCoordinates(cvec)

   ;; Store information in object fields
   *self.centers = cvec
   *self.edges   = evec
   self.nvalues  = nv
   self.reversed = reversed

   ;; Compute the weight factors
   weights = self->ComputeWeights()
   *self.weights = weights

   ok = 1

END


; -------------------------------------------------------------------------------------
; Create:
;   This method creates the grid vector center and edge coordinates in
; the following way:
; (1) If values is a multi-element vector, the values are used as
; center coordinates or edge coordinates depending on the edges
; keyword (centers is default, edges is a boolean flag). The missing
; coordinate vector is computed automatically (see
; ComputeEdgeCoordinates and ComputeCenterCoordinates methods). Values
; must be monotonously increasing or decreasing, but they need not be
; regularily spaced.
; (2) If type is set to 'gaussian' and nvalues is given, a gaussian
; latitude grid vector is set up by computing the required Bessel
; functions. The routine gauaw is reproduced from the ECHAM climate
; model. 
; (3) If type is set to 'regular' and nvalues, start, and increment
; are given, the grid vector is computed accordingly.
; (4) After the center and edge coordinates have been computed, the
; limit vector is evaluated. If any center coordinate lies outside the
; limit, an error message is produced unless the allow_wrap keyword is
; set, or the adjust_values keyword. If allow_wrap is set, it is only
; tested if the grid vector values do not exceed the total limit
; extent (limit[1]-limit[0]), if adjust_values is set, the grid vector
; is truncated to fit into the limit range. If no limit vector is
; provided, a limit is computed automatically.
;
; The function returns 1 if the grid vector was succesfully created
; and 0 otherwise.
; NOTES:
; (1) Internally, the grid vector is always stored in ascending
; order. If the given values are descending, or the increment is
; negative, the values are reversed and the reversed flag is set. For
; a gaussian grid, use the start keyword to define if it shall be
; south->north (default, start < 0) or north->south (start > 0).

FUNCTION MGS_GridVector::Create, values, $
                                 edges=edges, $
                                 type=type, $
                                 nvalues=nvalues, $
                                 start=start, $
                                 increment=increment, $
                                 limit=limit, $
                                 adjust_values=adjust, $
                                 allow_wrap=allow_wrap


   self->ClearGridDefinition

   retval = 0

   ;; Check argument and keywords
   IF N_Elements(type) EQ 0 THEN type = 'unknown'

   ;; Create the grid center and edge values depending on the given
   ;; information and the grid vector type

   IF N_Elements(values) GT 0 THEN BEGIN
      ;; Create grid vector from a vector of center or edge
      ;; coordinates 
      self->CreateFromValues, values, edges=edges, ok=retval
      ;; Copy type value verbatim
      IF N_Elements(type) GT 0 THEN self.type = StrLowCase(type)

   ENDIF ELSE BEGIN

      ;; Grid vector determined through nvalues, start, increment
      IF StrLowCase(type) EQ 'regular' THEN BEGIN
         ;; Grid vector specified as start, increment, and nvalues
         self->CreateRegularGridVector, nvalues=nvalues, $
            start=start, $
            increment=increment, $
            ok=retval
         self.type = 'regular'
      ENDIF ELSE BEGIN
         ;; Grid vector cannot be created
         ;; Note: The LatVector object defines a new type 'gaussian'
         IF N_Elements(type) EQ 0 THEN type = 'unknown'
         self->ErrorMessage, 'Cannot create grid vector for type '+type
         RETURN, retval
      ENDELSE 

   ENDELSE 


   IF self.debug GT 1 THEN BEGIN
      print, Obj_Class(self),': ### Create, before limit analysis : '
      self->PrintStatus
   ENDIF 

   ;; Store wrapping property
   self.allow_wrap = Keyword_Set(allow_wrap)

   ;; Analyse the limit information or compute the limit vector
   IF N_Elements(limit) GT 0 THEN BEGIN
      IF N_Elements(limit) NE 2 THEN BEGIN
         self->ErrorMessage, 'Limit must have 2 elements'
         self->ClearGridDefinition
         RETURN, retval
      ENDIF 
      
      ;; Make sure limit is sorted and check extent
      thelimit = limit(sort(limit))
      IF thelimit[1]-thelimit[0] GT 360. THEN BEGIN
         self->ErrorMessage, 'Limit exceeds 360 degrees'
         self->ClearGridDefinition
         RETURN, retval
      ENDIF 

      ;; Apply limit only if coordinate wrapping is not allowed
      IF Keyword_Set(allow_wrap) EQ 0 THEN BEGIN

         retval = self->Truncate(thelimit, $
                                 only_edges=Keyword_Set(adjust) EQ 0)

      ENDIF ELSE BEGIN
         ;; If wrapping is allowed, we are done
         retval = 1
      ENDELSE 

      IF retval THEN self.limit = limit

   ENDIF ELSE BEGIN
      ;; No limit given - use minimum and maximum of edge coordinates
      ;; If coordinate wrapping is allowed, limit is always set to 0 .. 360
      thelimit = [ Min(*self.edges), Max(*self.edges) ]
      IF thelimit[1]-thelimit[0] GT 360. THEN BEGIN
         self->ErrorMessage, 'Limit exceeds 360 degrees'
         self->ClearGridDefinition
         RETURN, retval
      ENDIF 
      retval = 1
      self.limit = thelimit
   ENDELSE 

   IF self.debug GT 1 THEN BEGIN
      print, Obj_Class(self),': ### Create, final grid information : '
      self->PrintStatus
   ENDIF 

   RETURN, retval

END


; =====================================================================================
; Standard object methods:
;     GetProperty : retrieve object values
;     SetProperty : set object values
;     Cleanup     : free object pointers and destroy
;     Init        : initialize object

; -------------------------------------------------------------------------------------
; GetProperty:
; This method extracts specific object values and returns them to the
; user.

PRO MGS_GridVector::GetProperty,  $
             type=type,              $ ; The grid vector type ('regular', 
                                       ;   'irregular', 'gaussian')
             centers=centers,        $ ; Center coordinates
             edges=edges,            $ ; Edge coordinates 
             weights=weights,        $ ; The area(!) weights (global sum = 1)
             nvalues=nvalues,        $ ; Number of center coordinates
             start=start,            $ ; First center coordinate value
             increment=increment,    $ ; Increment between consecutive 
                                       ;   coordinates (regular vector)
             limit=limit,            $ ; the range limit of the grid vector
             allow_wrap=allow_wrap,  $ ; coordinate may cycle
             reversed=reversed,      $ ; grid is in descending order
             _Ref_Extra=extra
                                ; Inherited keywords:
                                ; name      : The variable name
                                ; uvalue    : a user-defined value
                                ; no_copy   : Copy pointer values?
                                ; no_dialog : Object uses dialogs for messages?
                                ; debug     : Object is in debug mode?
                                ; status    : Object valid? (string)


   IF self.debug GT 1 THEN print,'## '+Routine_Name()+' in '+self.name

   ;; Call GetProperty method from parent object
   ;; Note that execution continues even if retrieval of a baseobject
   ;; property fails.
   self->MGS_BaseObject::GetProperty, _Extra=extra

;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error retrieving grid vector properties!'
;      RETURN
;   ENDIF

   type = self.type

   nvalues = self.nvalues
   IF N_Elements(*self.centers) GT 0 THEN centers = *self.centers
   IF N_Elements(*self.edges) GT 0 THEN edges   = *self.edges
   IF N_Elements(*self.weights) GT 0 THEN weights = *self.weights

 
   IF N_Elements(centers) GT 0 THEN start = centers[0]
   IF N_Elements(centers) GT 1 THEN increment = centers[1] - centers[0]

   limit = self.limit
   allow_wrap = self.allow_wrap
   reversed = self.reversed

   ;; Reverse results for centers, edges, and weights, if reversed
   ;; flag is set
   IF reversed THEN BEGIN
      IF N_Elements(centers) GT 0 THEN centers = Reverse(centers)
      IF N_Elements(edges) GT 0 THEN edges = Reverse(edges)
      IF N_Elements(weights) GT 0 THEN weights = Reverse(weights)
   ENDIF 

END


; -------------------------------------------------------------------------------------
; SetProperty:
;    This method sets specific object values. 

PRO MGS_GridVector::SetProperty,  $
                  allow_wrap=allow_wrap, $
                  _Extra=extra
;                                ; Inherited keywords:
;                                ; name      : The variable name
;                                ; uvalue    : a user-defined value
;                                ; no_copy   : don't copy data when
;                                ;             creating pointers
;                                ; no_dialog : Don't display
;                                ;             interactive dialogs
;                                ; debug     : put object in debug mode

    ;; Call SetProperty method of BaseObject
    self->MGS_BaseObject::SetProperty, _Extra=extra

;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error setting grid properties!'
;      result = 'CANCELLED'
;      RETURN
;   ENDIF

   IF N_Elements(allow_wrap) GT 0 THEN self.allow_wrap = Keyword_Set(allow_wrap)

END

; -------------------------------------------------------------------------------------
; Cleanup:
; This method frees all data stored in the object. If dimvars are
; objects pointing to other variables, these will be left intact.

PRO MGS_GridVector::Cleanup

   Ptr_Free, self.centers, self.edges, self.weights

   ;; Call parent's cleanup method
   self->MGS_BaseObject::Cleanup

END

; -------------------------------------------------------------------------------------
; Init:
; This method initializes the object values. The grid vector
; coordinates can be defined either by providing a vector of center
; coordinates or edge coordinates (set the edges keyword in this
; case), or by specifying the number of values, the start coordinate,
; and the increment. The coordinate value vector must be
; monotonous. If the increment is negative or the value vector is
; descending, it will be reversed, and the reversed flag will be set.
; For global longitude coordinates, set the allow_wrap flag - the
; limit vector will then be regarded as a measure of the extent (360
; deg) only.
; NOTE:
; (1) It is recommended to set the object name to either 'Longitude'
; or 'Latitude'.
; (2) The default unit for the grid vector is degrees_north or
; degrees_east. For a non-gaussian grid it might be possible to define
; the grid vector in different units as well, but you may come across
; a hardwired 360. degree limit here or there.

FUNCTION MGS_GridVector::Init,  $
                  values,                 $ ; The grid vector values (optional)
                  type=type,              $ ; The grid vector type ('regular', 
                                            ;   'irregular', 'gaussian')
                  edges=edges,            $ ; Values denote edge coordinates 
                                            ;   rather than centers (boolean)
                  nvalues=nvalues,        $ ; Number of center coordinates
                  start=start,            $ ; First center coordinate value
                  increment=increment,    $ ; Increment between consecutive 
                                            ;   coordinates (regular vector)
                  limit=limit,            $ ; the range limit of the grid vector
                  adjust_values=adjust,   $ ; cut vectors if out of range
                  allow_wrap=allow_wrap,  $ ; coordinate may cycle
                  name=name,              $ ; the object name (overwrites BaseObject)
                  _Extra=extra  ; For future additions
                                ; Inherited keywords:
                                ; uvalue    : a user-defined value
                                ; no_dialog : Don't display message dialogs
                                ; debug     : Put object in debugging state


    
   ;; Set default name to 'GridVector'
   IF N_Elements(name) EQ 0 THEN name = 'GridVector'

   ;; Initialize parent object first
   IF not self->MGS_BaseObject::Init(name=name, _Extra=extra) THEN RETURN, 0

;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error initializing grid vector object!'
;      RETURN, 0
;   ENDIF

   ;; Create pointers
   self.centers = Ptr_New(/Allocate)
   self.edges   = Ptr_New(/Allocate)
   self.weights = Ptr_New(/Allocate)

   ;; Call Create method to fill grid vector values
   ok = self->Create(values, type=type, edges=edges, $
      nvalues=nvalues, start=start, increment=increment, $
      limit=limit, adjust_values=adjust, allow_wrap=allow_wrap)

   ;; Free pointers if creation of grid vector was not successful
   IF NOT ok THEN Ptr_Free, self.centers, self.edges, self.weights

   RETURN, ok
END


; -------------------------------------------------------------------------------------
; This is the object definition for the gridvector object.
; NOTE:
; (1) If allow_wrap is set to 1, limit is only used to specify the
; allowed extent of the grid (the wrapping period).

PRO MGS_GridVector__Define

   objectClass = { MGS_GridVector,  $    ; The object class
                   nvalues       : 0L,         $ ; number of center coordinate values
                   centers       : ptr_new(),  $ ; The center coordinate values
                   edges         : ptr_new(),  $ ; The edge coordinate values
                   weights       : ptr_new(),  $ ; relative weights (sum=2.)
                   reversed      : 0,          $ ; flag for reversed direction (north->south)
                   limit         : [0.d0, 0.d0], $ ; range limit
                   allow_wrap    : 0,          $ ; cycling coordinate? (boolean)
                   type          : '',         $ ; coordinate type ('regular', 'irregular', 'gaussian')

                   INHERITS MGS_BaseObject  $
                 }
                   
END


; -------------------------------------------------------------------------------------
; Example: 
;    This example demonstrates a few features of the GridVector
; object. Use the debug=3 option to produce verbose output.
;
; Examples how to use example:
; example
; example, /lat
; example, /lon, /exclusive
; example, /lat, nvalues=18, start=-85., increment=10.
; example, /lat, type='gaussian', nvalues=64
;
; If you want to continue to use the object after running the example,
; you can return the object reference with the object
; keyword. Otherwise, the test object will be automatically deleted.

PRO Example, longitude=longitude, latitude=latitude, $
             exclusive=exclusive, object=o, _Extra=extra


   ;; Create a simple coarse resolution test grid vector with 30
   ;; degrees increment
   IF Obj_Valid(o) THEN BEGIN
      IF Obj_IsA(o, 'MGS_GRIDVECTOR') THEN BEGIN
         test = o 
         o->GetProperty, name=name
         print,'Use available object reference : class='+Obj_Class(o)+'  name='+name
      ENDIF ELSE BEGIN
         print,'*** Object must be of type MGS_GridVector! ***'
         RETURN
      ENDELSE 
   ENDIF ELSE BEGIN
      increment = 30.
      IF Keyword_Set(latitude) THEN BEGIN
         test = obj_new('MGS_GridVector', name='Latitude', $
                        type='regular', $
                        nvalues = 180.d0/increment, $
                        start = -90.d0+0.5*increment, $
                        increment = increment, $
                        _Extra=extra)
      ENDIF ELSE BEGIN
         test = obj_new('MGS_GridVector', name='Longitude', $
                        type='regular', $
                        nvalues = 360.d0/increment, $
                        start = 0.d0, $
                        increment = increment, $
                        /allow_wrap, $
                        _Extra=extra)
      ENDELSE 
   ENDELSE 

   ;; Print some status information
   test->PrintStatus

   ;; Retrieve edge and center coordinates
   test->GetProperty, centers=centers, edges=edges
   print
   print,'Center coordinates = ',centers
   print,'Edge coordinates = ',edges
   print

   ;; Examine a couple of GetIndex and GetValue statements
   ;; Individual points
   print,'P O I N T S'
   point = [ 0., 5., 10., 180., 360., -180., -90., -10. ]
   FOR i=0L, N_Elements(point)-1 DO BEGIN
      index = test->GetIndex(point[i],exclusive=exclusive, $
                             fraction=fraction)
      print,' Index('+StrTrim(point[i],2)+') = ',index
      print,' Fraction = ',fraction
      IF index[0] GE 0 THEN BEGIN
         print,'      Center = ',test->GetValue(index,/order)
         print,'       Edges = ',test->GetValue(index,/Order,/Edges,/Plus_One)
      ENDIF 
   ENDFOR 

   ;; Range vectors
   print,'R A N G E S'
   range = [ [ 0., 30. ], $
             [ -30., 60.], $
             [ -5.00001, 15.], $       ;; ATTENTION: -5. produces roundoff error
             [ -90., 90.] ]

   test->GetProperty, name=name
   IF name EQ 'Longitude' THEN range = [ range, $
                                         [ 0.1, 359.9], $
                                         [ 0., 360. ], $
                                         [ -5., -180.], $
                                         [ -180., 180.], $
                                         [ 180., -180.], $
                                         [ 270., -30.], $
                                         [ 270., 60.] ]


   dims = Size(range,/Dimensions)
   FOR i=0L, dims[1]-1 DO BEGIN
      index = test->GetIndex(range[*,i],exclusive=exclusive, $
                            fraction=fraction)
      print,' Index('+StrTrim(range[0,i],2)+','+ $
         StrTrim(range[1,i],2)+') = ',index
      print,' Fraction = ',fraction
      IF index[0] GE 0 THEN BEGIN
         print,'      Center = ',test->GetValue(index,/order)
         print,'       Edges = ',test->GetValue(index,/Order,/Edges,/Plus_One)
      ENDIF 
   ENDFOR 

   IF NOT Arg_Present(o) THEN Obj_Destroy, test ELSE o=test

END
