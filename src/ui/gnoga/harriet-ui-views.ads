with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.View;

with Concorde.Sessions;

package Concorde.UI.Views is

   type Root_View_Type is abstract tagged limited private;

   procedure Create_With_Gnoga_View
     (View       : in out Root_View_Type'Class;
      Session    : not null access
        Concorde.Sessions.Root_Concorde_Session'Class;
      Gnoga_View : not null access Gnoga.Gui.View.View_Base_Type'Class);

   procedure Create
     (View    : not null access Root_View_Type;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String := "")
   is abstract;

   procedure Close
     (View : in out Root_View_Type);

   function Accepts_Text_Entry (View : Root_View_Type) return Boolean;

   procedure Handle_Key_Press
     (View    : in out Root_View_Type;
      Meta    : Boolean;
      Alt     : Boolean;
      Control : Boolean;
      Shift   : Boolean;
      Key     : Natural)
   is null;

   procedure Queue_Render
     (View : in out Root_View_Type)
   is null;

   procedure Render
     (View : in out Root_View_Type)
   is null;

   procedure Resize
     (View : in out Root_View_Type)
   is null;

   procedure Add_Child
     (Parent : in out Root_View_Type;
      Child  : not null access Root_View_Type'Class)
   is null;

   function Title
     (View : Root_View_Type)
      return String
      is abstract;

   function Session
     (View : Root_View_Type'Class)
      return Concorde.Sessions.Concorde_Session;

   function Gnoga_View
     (View : Root_View_Type'Class)
      return Gnoga.Gui.View.Pointer_To_View_Base_Class;

   type View_Type is access all Root_View_Type'Class;

   procedure Destroy
     (View : in out View_Type);

private

   type Root_View_Type is abstract tagged limited
      record
         Top_Element : Gnoga.Gui.Element.Pointer_To_Element_Class;
         Gnoga_View  : Gnoga.Gui.View.Pointer_To_View_Base_Class;
         Session     : Concorde.Sessions.Concorde_Session;
      end record;

   function Session
     (View : Root_View_Type'Class)
      return Concorde.Sessions.Concorde_Session
   is (View.Session);

   function Gnoga_View
     (View : Root_View_Type'Class)
      return Gnoga.Gui.View.Pointer_To_View_Base_Class
   is (View.Gnoga_View);

end Concorde.UI.Views;
