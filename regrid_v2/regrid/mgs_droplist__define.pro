;+
; NAME:
;	MGS_DROPLIST__DEFINE
;
; PURPOSE:
;	This object implements the WIDGET_DROPLIST for MGS_BaseGUI.
;
; REQUIREMENTS:
;	INHERITS MGS_BaseObject
;
; CATEGORY:
;	Widget Object
;
; USER INTERFACE:
;	ObjRef = OBJ_NEW('MGS_DropList', [List])
;
; ARGUMENTS:
;	List  (Get/Set) A vector of items that can be displayed as text 
;		The droplist data is maintained in its native data type, but is
;		converted to string by WIDGET_DROPLIST for display.  
;		(Default = null string (''))
;		The Value data type should be one of the following data types: BYTE
;		INT, UINT, LONG, ULONG, FLOAT, DOUBLE and, of course, STRING. 
;
; KEYWORDS For Initailization:
;	TITLE (Get/Set)  The title for the droplist (default = '')
;	DROPLIST_SELECT (Get/Set) The index of the current setting.
;		(Default = 0.)
;	VALUE (Get/Set) Set this keyword to an element that matches 
;		an item in the droplist.  This is the same as setting the 
;		index value for DROPLIST_SELECT except by value rather than 
;		by index.
;	FRAME (Get/Set) (See WIDGET_DROPLIST)  (Default = 0)
;	SENSITIVE (Get/Set) (See WIDGET_DROPLIST)  (Default = 1)
;	DYNAMIC_RESIZE (Get/Set) (See WIDGET_DROPLIST)  (Default = 0)
;	NOTIFYID (Get/Set)  
;		
;	NOTIFYOBJ (Get/Set)
;
; EVENT STRUCTURE:
;	This widget generates a named event structure with the following fields:
;		ID:  The Widget ID of the DropList Widget
;		TOP: The Widget ID of the droplist's parent
;		HANDLER: The handler ID for the droplist
;		VALUE:  A pointer to the value of the currently selected item in the list
;		INDEX: A long scalar integer of the currently slected values index 
;				within the LIST
;		OBJECT:  The object reference to the DROPLIST object
;
;	
;	
;	
; EXAMPLE:
;	objref = OBJ_NEW('MGS_DROPLIST', ['Eenie', 'Meenie','Meinie','Moe'],DropList_Select = 1)
;  objref->GUI
;	objref->SetValue, Selection = 'Eenie'
;
;	Try the following from thje command line:
;
;	.compile MGS_DROPLIST__DEFINE
;	EXAMPLE, object = obj, title = 'My List', Debug = 1
;	
;	
; MODIFICATION HISTORY
;	JUNE 2001  written by Ben Tupper
;		btupper@bigelow.org
;		pemaquidriver@tidewater.net
;-


;------
;	MGS_DROPLIST_EVENT
;------
PRO MGS_DROPLIST_EVENT__DEFINE

struct = { MGS_DropList_Event, $ ; The name of the event structure.
             ID: 0L, $          ; The ID of the compound widget's top-level base.
             TOP: 0L, $         ; The widget ID of the top-level base of the hierarchy.
             HANDLER: 0L, $     ; The event handler ID. Filled out by IDL.
             Value: Ptr_New(), $ ; A pointer to the contents of the selection 
             							 ; in the droplist.
             Index:0, $ 			; Index of selection.
             							  
             Object: Obj_New()} ; The "self" object.
   
END	;Event Defintion


;------
;	DropList_Event
;------
FUNCTION MGS_DROPLIST::DropList_Event, event
Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return,0
	EndIf

Self.Index = event.Index
If Self.Index LT 0 Then *Self.Value = '' Else $
	*Self.Value = (*Self.List)[Self.Index]

thisEvent = {MGS_DROPLIST_EVENT, $
	event.id, $
	event.top, $
	event.handler, $
	Self.Value, $
	Self.Index, $
	Self}

	;notify any widgets if called for
	If n_elements(*Self.NotifyID) GT 1 Then Begin
	For i = 0, n_elements(*Self.NotifyID)/2 - 1 Do Begin
		If Widget_Info((*Self.NotifyID)[0,i], /Valid_ID) EQ 1 Then $
		 Widget_Control, (*Self.NotifyID)[0,i] , Send_Event = thisEvent 
		EndFor
	EndIf		
	
	If n_elements(*Self.NotifyObj) GT 0 Then Begin	;defined?
		If SIZE((*Self.NotifyObj)[0], /Type) EQ 8 Then Begin	;a structure?
			For i = 0, n_elements(*self.notifyObj)-1 do $	
				event = CALL_METHOD( (*Self.Notifyobj)[i].Method, $
					(*Self.Notifyobj)[i].Object, thisevent )
				
			EndIf	;not a scalar
		
		EndIF ; something is defined within the pointer
		
		
If Self.Debug EQ 1 Then Help, thisEvent, /str
		
Return, thisevent
END	; event



;------
;	BuildGUI
;------
PRO MGS_DropList::BuildGUI
Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return
	EndIf

				 
	Self.DropListID = Widget_DropList(Self.LayoutID, $
		Value = STRING(*Self.List), Title = Self.Title, $
		Dynamic_Resize = Self.Dynamic_Resize, $
		Event_pro = 'MGS_BaseGUI_Widget_Events', $
		uValue = {Object:Self, Method: 'DropList_Event'})

	Self->SetValue
	
END	;BuildGUI


;-----
;	GetValue
;-----
;	Use this method to return the current selection
;  set Index to a valid index into the list to retrieve the 
;	list value at the specified index
FUNCTION MGS_DropList::GetValue, Index
Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return, -1
	EndIf

If n_elements(Index) NE 0 Then Begin
	
	If Index[0] GT 0 AND Index[0] LT n_elements(*Self.List) Then Begin
	
		Return, (*Self.List)[Index[0]]
		
		EndIF Else Begin
		
		Self->ErrorMessage, 'Invalid index into list'
		Return, -1
		
		EndElse
		
EndIf

Return, *Self.Value

END	;GetValue


;------
; GetState
;------
FUNCTION MGS_DropList::GetState
Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return, -1
	EndIf
	
Return, {List :*Self.List, $
	Value : *Self.Value, $
	Index: Self.Index}
	
END	;GetState


;-----
;	SetValue
;-----
;	Use this method to set the current selection of the 
;	droplist by *value* 
;  Use DROPLIST_SELECT to set the current selection by index
;	Use LIST to set the value for the entire list contents
PRO MGS_DropList::SetValue, Value, $
	DropList_Select = DropList_Select, $
	List = List
Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return
	EndIf


	;first check in the new list
If n_elements(List) NE 0 Then Begin
	*Self.List = List
		;see if the old selection matches the new list
	A = Where (*Self.Value EQ *Self.List, Count)
	If Count NE 0 Then Self.Index = A[0] Else Self.Index = 0
	*Self.Value = (*Self.List)[0]
	EndIf


		;check in the Value
If n_elements(Value) NE 0 Then Begin

	A = Where(Value[0] EQ *Self.List, Count)
	If Count GT 0 Then Begin
		
		Self.index = A[0]
		*Self.Value = (*Self.List)[Self.Index]
		
		EndIf Else Begin
		
		Self->ErrorMessage, $
			'Value, '+ StrTrim(Value[0],2) +', has no match in the Droplist'
		
		EndElse

EndIF



	;check the droplist_select index
If n_elements(DropList_Select) NE 0 then Begin

		;make sure the droplist_select is a valid index for this list
	If DropList_Select[0] GE 0 AND $
		DropList_Select[0] LT n_elements(*Self.List) Then Begin
	
		Self.Index = DropList_Select[0] 
		*Self.Value = (*Self.List)[Self.Index]
		
		EndIf Else Begin
		
		Self->ErrorMessage, 'Invalid DropList_Select:' + StrTrim(Droplist_Select[0],2)
		
		EndElse
	
EndIF	
		
	
If Widget_Info(Self.DroplistID,/Valid_ID) EQ 1 Then Begin
	
		;update the contents
	If n_elements(List) NE 0 Then $
	
	  Widget_Control, Self.DropListID, Set_Value = *Self.List, $
		Set_DropList_Select = Self.Index, $
		Dynamic_Resize = Self.Dynamic_Resize, $
		Sensitive = Self.Sensitive $
		
		ELSE $
	
		Widget_Control, Self.DropListID, $
		Set_DropList_Select = Self.Index, $
		Dynamic_Resize = Self.Dynamic_Resize, $
		Sensitive = Self.Sensitive
		
	
EndIf

END	;Setvalue

;-----
;	SetProperty
;-----
PRO MGS_DropList::SetProperty, Value = Value, $
	List = List, Title = Title, $
	DROPLIST_SELECT = DropList_Select, $
	NOTIFYID = NotifyID, NOTIFYOBJ = NotifyOBJ, $
	DYNAMIC_RESIZE = Dynamic_Resize, FRAME = Frame, $
	SENSITIVE = sensitive, $
	_Ref_Extra = Extra

Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return
	EndIf


If n_elements(Dynamic_Resize) NE 0 Then $
	Self.Dynamic_Resize = KeyWord_Set(Dynamic_Resize)
If n_elements(Frame) NE 0 Then Self.Frame = Frame[0]
If n_elements(Sensitive) NE 0 Then Self.Sensitive = KeyWord_Set(Sensitive)
	;pass selection onto the value/selection
Self->SetValue,Value, DropList_Select = DropList_Select, List = List

	;check in the notification info
If n_elements(NotifyID) NE 0 Then *Self.NotifyID = NotifyID
If n_elements(NotifyObj) NE 0 Then *Self.NotifyObj = NotifyObj

Self->MGS_BaseGUI::SetProperty, _Extra = extra
END ;Getproperty
	

;-----
;	GetProperty
;-----
PRO MGS_DropList::GetProperty, Value = Value, Title = Title, $
	DROPLIST_SELECT = DropList_Select, List = List, $
	NOTIFYID = NotifyID, NOTIFYOBJ = NotifyOBJ, $
	DYNAMIC_RESIZE = Dynamic_Resize, FRAME = Frame, $
	SENSITIVE = sensitive, $
	_Ref_Extra = Extra

Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return
	EndIf


If Arg_present(List) Then List = *Self.List
Title = Self.Title
DropList_Select = Self.Index
Value = Self->GetValue()
Dynamic_Resize = Self.Dynamic_Resize
Frame = Self.Frame
Sensitive = Self.Sensitive

If Arg_present(NotifyID) Then If n_elements(*Self.NotifyID) GT 0 Then $
	NotifyID = *self.NotifyID
If Arg_present(NotifyObj) Then If n_elements(*Self.NotifyObj) GT 0 Then $
	NotifyObj = *self.NotifyObj
	

Self->MGS_BaseGUI::GetProperty, _Extra = extra
END ;Getproperty


;-----
;	INIT
;-----
FUNCTION MGS_DropList::INIT, List, $
	Value = Value, title = title, $
	DROPLIST_SELECT=DropList_Select, $
	NOTIFYID = NotifyID, NOTIFYOBJ = NotifyOBJ, $
	DYNAMIC_RESIZE = Dynamic_Resize, FRAME = Frame, $
	SENSITIVE = sensitive,  $
	_Ref_Extra = Extra
	
If (Self->MGS_BaseObject::INIT(_extra = extra)) NE 1 Then Return, 0

Catch, Error
If Error NE 0 Then Begin
	Self->ErrorMessage
	Catch, /Cancel
	Return, 0
	EndIf

	;items about character of the widget	
If n_elements(List) EQ 0 Then Self.List = Ptr_New('') Else $
	Self.List = Ptr_new(List)
	 ; the first value by default
Self.Value = Ptr_NEW( (*Self.List)[Self.Index] )

If n_elements(Title) EQ 0 Then Self.Title = '' Else $
	Self.Title = Title[0]
Self.Dynamic_Resize = KeyWord_Set(Dynamic_resize)
If n_elements(Frame) EQ 0 Then Self.Frame = 0 Else Self.Frame = Frame[0]
If n_elements(Sensitive) NE 0 Then Self.Sensitive = KeyWord_Set(Sensitive) Else $
	Self.Sensitive = 1
	


If n_elements(NotifyID) EQ 0 Then Self.NotifyID = Ptr_new(/Allocate) Else $
	Self.NotifyID = Ptr_New(NotifyID)
	
If n_elements(NotifyObj) EQ 0 Then Self.NotifyObj = Ptr_new(/Allocate) Else $
	Self.NotifyObj = Ptr_New(NotifyID)

Self->SetValue, Value, DropList_Select = DropList_Select

Return, 1
END


;-----
;	CleanUp
;-----
PRO MGS_DropList::Cleanup

Ptr_free, Self.List, Self.Value
Ptr_Free, Self.NotifyID, Self.NotifyObj

Self->MGS_BaseGUI::CleanUp
END	;CleanUp


;-----
;	Definition
;-----
PRO MGS_Droplist__DEFINE

struct = { MGS_Droplist, $          

	INHERITS MGS_BaseGUI, $

	title:"", $              ; The droplist title.
	list: Ptr_New(), $       ; the contents of the list
	
	droplistID:0L, $         ; widget ID and controling values
	Dynamic_resize: 0, $
	Frame:0, $
	Sensitive: 0, $			
	
	index:0 ,$                ; The index number of the current selecton.
	Value: Ptr_new(), $	      ; the 'value' of the current selection	
										;this is maintained independently from the 
										;droplist widget so the selected item can 
										;be retrieved/set even when GUI is not realized
										;
										;it may seem redundant to maintain this since the 
										;current selection can be found from
										; (*Self.List)[Self.Index]
										;but the *event structure* must be defined to handle
										;any datatype within the droplist.

	NotifyID: Ptr_new(), $    ;set to scalars to disable		
	NotifyObj: Ptr_new() }
	
END ; definition

PRO EXAMPLE, object = objref, _Extra = Extra

objref = OBJ_NEW('MGS_DROPLIST', ['Eenie', 'Meenie','Meinie','Moe'], $
	_Extra = Extra)

ObjRef->GUI


End


--------------B6A3E68D127E14C7789B588F--


