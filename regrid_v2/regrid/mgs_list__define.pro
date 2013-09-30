;+
; NAME:
;   MGS_List
;
; PURPOSE:
;
;   This is a widget object that defines a list widget. The user can
;   set, add, or delete values while the widget is on display. Selections
;   are retrieved as values rather than index numbers, and the list
;   can be sorted easily.
;      This widget can be used as a blocking or non-blocking
;   stand-alone widget or as a compound widget (depending on the call
;   to the GUI method; see MGS_BaseGUI). 
;
; AUTHOR:
;
;   Dr. Martin Schultz
;   Max-Planck-Institut fuer Meteorologie
;   Bundesstr. 55, D-20146 Hamburg
;   email: martin.schultz@dkrz.de
;
; CATEGORY:
;   Widget objects
;
; TYPICAL CALLING SEQUENCE:
;   thelist = Obj_New('MGS_List', value=['First choice', 'Second choice'])
;   thelist->GUI, /block
;   print, 'The new value is ',thelist->GetValue()
;   Obj_Destroy, thelist
;
; PARAMETERS:
;
;   None.
;
; KEYWORDS:
;   See the respective method headers. See Init for the keywords to be
;   used with the Obj_New() function.
;
; NOTES:
;
; MODIFICATION HISTORY:
;   mgs, 17 May 2001: Version 0.1
;   mgs, 01 Jun 2001: 
;            - bug fix: GetValue method had wrong name
;   mgs, 28 Feb 2003:
;            - added additive keyword to SelectValue method
;-
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
;


; -----------------------------------------------------------------------------
; Resize:
;    This method resizes the widget by making the list widget fit the new size.

PRO MGS_List::Resize, x=x, y=y

   isactive = Widget_Info(self.tlb, /Valid_ID)

   IF N_Elements(X) GT 0 THEN BEGIN
      self.xsize = float(x)/12.    ;; *** ??? ***
      IF isactive THEN Widget_Control, self.listID, Scr_XSize = x
   ENDIF 
   IF N_Elements(Y) GT 0 THEN BEGIN
      self.ysize = float(y)/12.    ;; *** ??? ***
      IF isactive THEN Widget_Control, self.listID, Scr_YSize = y
   ENDIF 

END


; -----------------------------------------------------------------------------
; SelectValue:
;    Use this method to specify list items that shall be selected by
; their value rather than by the index number. The value argument may
; be a scalar string or string vector and may contain Unix style
; wildcards. Unless multiple selections are allowed, the string match
; must give a unique result. If you don't want multiple matches to
; produce an error, you can set the first_match flag to select the
; first match.
;    Use the fold_case flag to perform a case-independent search.
;    Use the additive flag to add the selection to the current one.

PRO MGS_List::SelectValue, value, fold_case=fold_case, first_match=first_match, $
            additive=additive

   ;; If no value argument is provided return
   IF N_Elements(value) EQ 0 THEN RETURN

   ;; Get list entries 
   entries = self->GetValue(/All)

   ;; Set up select index array
   match = IntArr(N_Elements(entries))

   ;; Loop through value and perform pattern match
   FOR i=0L, N_Elements(value)-1 DO BEGIN
      match = match OR StrMatch(entries, value[i], fold_case=fold_case)
   ENDFOR 

   selected = Where(match GT 0)
   IF keyword_set(additive) THEN BEGIN 
      selected = [ selected, *self.selected ]
      selected = selected[uniq(selected)]
   ENDIF 

   IF NOT self.allow_multiple AND N_Elements(selected) GT 1 THEN BEGIN
      IF Keyword_Set(first_match) THEN BEGIN
         selected = selected[0]
      ENDIF ELSE BEGIN
         self->ErrorMessage, 'More than one match'
         RETURN
      ENDELSE 
   ENDIF

   *self.selected = selected

   ;; Update dialog display if active
   self->Show

END


; -----------------------------------------------------------------------------
; GetValue:
;    This method returns the values of the selected list entries or
; all list values if the All keyword is set. If nothing is selected or
; the selection is invalid, an empty string is returned.
; NOTE:
; (1) As sorting of values is performed here, it is mandatory that all
; methods use GetValue to retrieve the list values rather than
; accessing the values directly.

FUNCTION MGS_List::GetValue, ok=ok, All=all, _Extra=e

   ;; Error handler
   ;; ...

   ok = 0
   retval = ''

   IF self.updatemode EQ 1 THEN self->UpdateObject

   ;; Get all values
   thevalues = *self.value

   ;; Sort the values if sorted flag is set
   IF self.sorted THEN thevalues = thevalues(Sort(thevalues))

   ;; If the all keyword is set, return them
   IF Keyword_Set(all) THEN RETURN, thevalues

   ;; Filter out invalid selection indices
   selected = *self.selected
   wok = Where(selected GE 0 AND selected LE N_Elements(thevalues), count)
   IF count GT 0 THEN retval = thevalues[selected[wok]]

   RETURN, retval
END


; -----------------------------------------------------------------------------
; AddValue:
;    This method allows the addition of values to the list.

PRO MGS_List::AddValue, value

   ;; Return if there is nothing to add
   IF N_Elements(value) EQ 0 THEN RETURN

   ;; Retrieve current values
   entries = self->GetValue(/All)

   ;; Append new values
   self->SetValue, [ entries, String(value) ]

END


; -----------------------------------------------------------------------------
; RemoveValue:
;    This method removes values from the list. Value can be a scalar
; string or string list and may contain Unix style wildcards.

PRO MGS_List::RemoveValue, value

   ;; Return if there is nothing to add
   IF N_Elements(value) EQ 0 THEN RETURN

   ;; Get list entries 
   entries = self->GetValue(/All)

   ;; Set up select index array
   match = IntArr(N_Elements(entries))

   ;; Loop through value and perform pattern match
   FOR i=0L, N_Elements(value)-1 DO BEGIN
      match = match OR StrMatch(entries, value[i], fold_case=fold_case)
   ENDFOR 

   selected = Where(match EQ 0, count)

   IF count GT 0 THEN BEGIN
      entries = entries[selected]

      ;; Set new values
      self->SetValue, entries
   ENDIF ELSE BEGIN
      self->SetValue, ''
   ENDELSE 

END


; -----------------------------------------------------------------------------
; ListEvents:
;    The event handler method for the list widget of the compound
; widget. The only event that is generated is a list selection
; event. We need to replace the current selection. 
; How about scrolling ??

FUNCTION MGS_List::ListEvents, event

   ;; Error Handling.
;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error dealing with text events'
;      RETURN, 0
;   ENDIF

;;;   help,event,/stru

   selected = Widget_Info(event.ID, /LIST_SELECT)
   *self.selected = selected

   ;; Since scrolling does not generate an extra event, let's
   ;; piggypack here and read out the top displayed index as well
   top = Widget_Info(event.ID, /LIST_TOP)
   self.top = top


   RETURN, 0
END


; -----------------------------------------------------------------------------
; ResetDialog:  (private)
;   This method restores the initial value of the dialog. For the list
; object, this simply means that any selections are cancelled and the
; display position is reset to the top of the list.

FUNCTION MGS_List::ResetDialog, event

   ;; Error handler
   ;; ...

   *self.selected = -1L
   self.top = 0L

   self->Show 

   RETURN, 0

END


; -----------------------------------------------------------------------------
; Show:
;    Update the list display by changing the selection or list top

PRO MGS_List::Show

   isactive = Widget_Info(self.tlb, /Valid_ID)

   IF isactive THEN BEGIN
      ;; Set the value
      Widget_Control, self.listID, SET_VALUE=self->GetValue(/All)

      ;; Set the selection
      Widget_Control, self.listID, SET_LIST_SELECT=*self.selected

      ;; Set the top of the displayed items
      Widget_Control, self.listID, SET_LIST_TOP=self.top
   ENDIF 

END   


; -----------------------------------------------------------------------------
; BuildGUI:  (private)
; This method builds the graphical user interface. Here, just a simple
; compound list widget is added to the empty frame provided
; by the generic BaseGUI Object.

PRO MGS_List::BuildGUI

   ;; Create the widgets.
   ;; The label
;   self.labelID = Widget_Label( self.layoutID, $
;                                Value=self.labeltext, $
;                                Font=self.defaultfont, $
;                                Scr_XSize=self.labelsize, $
;                                UValue=self)

   ;; The text widget
   scr_xsize = 0
   scr_ysize = 0

   thevalue = self->GetValue(/all)

   self.listID = Widget_List( self.layoutID, $ 
                              Value=thevalue, $
                              Multiple=self.allow_multiple, $
                              XSize=self.xsize, $
                              YSize=self.ysize, $
                              Font=self.defaultfont, $
                              Event_Pro='MGS_BaseGUI_Widget_Events', $
                              UValue={Method:"ListEvents", Object:self}) 


END


; -----------------------------------------------------------------------------
; GetProperty:
;    This method allows you to obtain various properties of the compound widget via output keywords.

PRO MGS_List::GetProperty, $
   Value=value, $                   ; The "value" of the compound widget (string list).
   Selected=selected, $             ; The predefined selection (may be list if Allow_Multiple is set)
   Top=top, $
   Allow_Multiple=allow_multiple, $ ; Allow multiple selections
   Sorted=sorted, $                 ; Sort the list entries
   XSize=xsize, $                   ; The X size of the Text Widget.
   YSize=ysize, $                   ; The Y size
   _Ref_Extra=extra


   ;; Get properties from base object
   self->MGS_BaseGUI::GetProperty, _Extra=extra

   ;; Error Handling
;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error retrieving object properties'
;      RETURN
;   ENDIF

   ;; Get the properties.
   value = self->GetValue(/all)
   selected = *self.selected
   top = self.top
   allow_multiple = self.allow_multiple
   sorted = self.sorted
   xsize = self.xsize
   ysize = self.ysize

END


; -----------------------------------------------------------------------------
; SetProperty:
;    This method allows you to set various properties of the compound
; widget.
; NOTE:
; (1) The Allow_Multiple property change only takes effect if the
; widget is not active

PRO MGS_List::SetProperty, $
   Value=value, $                   ; The "value" of the compound widget (string list).
   Selected=selected, $             ; The predefined selection (may be list if Allow_Multiple is set)
   Top=top, $                       ; The top position in the list display
   Allow_Multiple=allow_multiple, $ ; Allow multiple selections
   Sorted=sorted, $                 ; Sort the list values
   XSize=xsize, $                   ; The X size of the Text Widget.
   YSize=ysize, $                   ; The Y size
   _Extra=extra


   ;; Set Properties of base object
   self->MGS_BaseGUI::SetProperty, _Extra=extra

   ;; Error Handling
;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error setting object properties'
;      RETURN
;   ENDIF

   ;; Is widget active and needs updating?
   isactive = Widget_Info(self.tlb, /Valid_ID)

   ;; Set the properties, if needed.
   IF N_Elements(allow_multiple) GT 0 THEN self.allow_multiple = Keyword_Set(allow_multiple)
   IF N_Elements(sorted) GT 0 THEN self.sorted = Keyword_Set(sorted)
   IF N_Elements(selected) GT 0 THEN BEGIN
      IF self.allow_multiple EQ 0 AND N_Elements(selected) GT 1 THEN BEGIN
         self->ErrorMessage, 'Only one selection allowed if allow_multiple keyword not set'
      ENDIF ELSE BEGIN
         *self.selected = Long(selected)
      ENDELSE 
   ENDIF 

   IF N_Elements(value) NE 0 THEN BEGIN
      self->SetValue, String(value)
   ENDIF

   IF N_Elements(top) GT 0 THEN BEGIN
      newtop = (Long(top) > 0L) < N_Elements(*self.value)
      self.top = newtop
   ENDIF 

   IF N_Elements(XSize) GT 0 THEN self.xsize = xsize
   IF N_Elements(YSize) GT 0 THEN self.ysize = ysize

   ;; Consistency check
   IF self.allow_multiple EQ 0 AND N_Elements(*self.selected) GT 1 THEN BEGIN
      *self.selected = -1L
   ENDIF 

   ;; Make changes to the widget
   self->Show

END


; -----------------------------------------------------------------------------
; Cleanup:
;    This method makes sure there are not pointers left on the heap.

PRO MGS_List::Cleanup

   Ptr_Free, self.selected

   self->MGS_BaseGUI::Cleanup

END 


; -----------------------------------------------------------------------------
; Init:
;    This method initializes the text input field  

; **** NOTE: xsize or scr_size currently lost in Nirwana ! ****

FUNCTION MGS_List::Init, $ 
   Value=value, $                   ; The "value" of the compound widget (string list).
   Selected=selected, $             ; The predefined selection (may be list if Allow_Multiple is set)
   Allow_Multiple=allow_multiple, $ ; Allow multiple selections
   Sorted=sorted, $                 ; Sort the list values
   XSize=xsize, $                   ; The X size of the Text Widget.
   YSize=ysize, $                   ; The Y size
   _Extra=extra


   ;; Initialize parent object
   IF not self->MGS_BaseGUI::Init(_Extra=extra) THEN RETURN, 0
   
   ;; Error Handling.
;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error initializing object'
;      RETURN, 0
;   ENDIF

   ;; Check keyword values.

   IF N_Elements(selected) EQ 0 THEN selected = -1L
   IF KeywO_]fj\ZAAUVNfCELYPIU\e|	x`t cf|U\CUNYJJ\BUCY\]id mq|`bh~pw4$ JK\F |KKABmHJJX^K 
vWU@_NHJ\UKSTDVWXB\OZ\]PHAAUVNfCELYPIU\[ETNVK]^OYJ\M	=*beylkw -'|`tik43ikwf|BUMHWMJvcIW\|mq|wHSDC\ -'phnr|U\TK^T^`jPTU	|h	dhhw@JGJE	4$=*`O]LUXMKTE\V[DUCY43SHU_JK\ENM\]pYKfw\YlBW^JK\ENM\]=*J\BVYVI l 3]ULKXUUAG@LUMP^\Er\WGO_]fj\ZAAUVNfCELYPIU\=*J\BV^VKM\Jr\@NABDrj\M]_RY\]4$ J\U_ HSDC\HSDC\43 ^\U_WCIW\WCIW\434$ jKD YQ\UGCTOXULK-'phnr|U\TK^T^OXU[U	~m	dhhw{|iyn 3 ^\U_cEYoXULK ~MKPWIVLUL\#: |w}gv huj|lugdw43 J\UH~\MoXBEE4$ |w}|bce4343 |mlk` 43|wj-'4343          4$ `~jfuGCTrf}\_G^E43tEPJP]TE\V[DUCY]\_G^IYPVWH_RMQ\BYSYV[SKST43gD DWQ\KGDS_KVT}g~f{XJKwudNQPMX ]KVOPJUSMQ\IUNHKPZYYDJ\M43FXWZMPA^AAPM@QNI_KVCmjjf{X]UoOS\ZMDHHX[PBYTD\JMASHMXWJ=*HL\\I LWV[DUCYWXTKAC]X[FAAL\zXE[XJ\_BG\ZMO\SBIKVXYDHJX4$ J\W\KO\ @\MQVJFBK]P]@LL@V_UR_VKTKCSL^\JYXINQZX@BH]PKKSTH]43DOXT\]CAJ\]PO\OJVKZ_ YQ\UAW ^ZK\\@VDXMQKNBf]PXB_G_UX^ =* 3ikv}g~fuPJZoi\_PWK=* 3]DRXZMK `~jfugct mQ\ARJHZMZBQS^WXTK-' X^PC\ l  yQ\JGJEV_MFU APJMYYDJ\M43  @JGJE	u-' AAUVNfCELYPIU\ lUUVN]UAMPIUKSHU\ZMG_N^PWZXEUPJM#:  ]_RY\] 	 qLZX@J]_RYMQ\\I^MOXBEE^43  J\U\MDEIiM\onHN MQ\SU_K\WMBI ^\U\ZZUDPM\T]=* DO]	u tE\MV^PBJPPMA^ BW]P]@LL@434$  UGCTd}	b tE\UP]D ZP]^\Zii43  PWQ\\YT^t~jqrA^\~lp] 343|`t  3434$