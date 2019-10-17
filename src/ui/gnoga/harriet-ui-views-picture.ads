private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

private with Gnoga.Gui.Element.Canvas.Context_2D;
private with Gnoga.Gui.Element.Common;

with Concorde.Color;

package Concorde.UI.Views.Picture is

   Max_Picture_Layers : constant := 16;
   type Layer_Count is range 0 .. Max_Picture_Layers;
   subtype Layer_Index is Layer_Count range 1 .. Layer_Count'Last;

   type Layer_Flags is array (Layer_Index range <>) of Boolean;

   type Layer_Array is array (Positive range <>) of Layer_Index;

   type Point_Type is
      record
         X, Y : Real;
      end record;

   type Point_Array is array (Positive range <>) of Point_Type;

   type Rectangle_Type is
      record
         Left_Top : Point_Type;
         Width    : Non_Negative_Real;
         Height   : Non_Negative_Real;
      end record;

   function Contains
     (R : Rectangle_Type;
      P : Point_Type)
      return Boolean;

   type Root_Picture_View is abstract new Root_View_Type with private;

   type Picture_View is access all Root_Picture_View'Class;

   procedure Create_Picture_View
     (View        : not null access Root_Picture_View'Class;
      Gnoga_View  : in out Gnoga.Gui.View.View_Type'Class;
      Layers      : Layer_Count := 1);

   procedure Draw_Picture
     (View : in out Root_Picture_View;
      Layer : Layer_Index)
   is abstract;

   overriding procedure Close
     (View : in out Root_Picture_View);

   overriding procedure Render
     (View : in out Root_Picture_View);

   overriding procedure Queue_Render
     (View   : in out Root_Picture_View);

   procedure Queue_Render_Layer
     (View   : in out Root_Picture_View'Class;
      Layers : Layer_Array);

   procedure Queue_Render_Layer
     (View   : in out Root_Picture_View'Class;
      Layer  : Layer_Index);

   overriding procedure Resize
     (View : in out Root_Picture_View);

   procedure Set_Viewport
     (Picture  : in out Root_Picture_View'Class;
      Left     : Real;
      Top      : Real;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real);

   procedure On_Mouse_Click
     (Picture : in out Root_Picture_View;
      X, Y    : Real)
   is null;

   procedure Clear
     (Picture : in out Root_Picture_View'Class);

   procedure Draw_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Concorde.Color.Concorde_Color);

   procedure Fill_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Concorde.Color.Concorde_Color);

   procedure Background_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Concorde.Color.Concorde_Color);

   procedure Line_Width
     (Picture : in out Root_Picture_View'Class;
      Width   : Non_Negative_Real);

   procedure Add_Image_Resource
     (Picture : in out Root_Picture_View'Class;
      Name    : String;
      Path    : String);

   procedure Move_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type);

   procedure Move_Relative
     (Picture : in out Root_Picture_View'Class;
      Offset  : Point_Type);

   procedure Line_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type);

   procedure Line
     (Picture  : in out Root_Picture_View'Class;
      From, To : Point_Type);

   procedure Polygon
     (Picture  : in out Root_Picture_View'Class;
      Points   : Point_Array;
      Filled   : Boolean);

   procedure Font
     (Picture : in out Root_Picture_View'Class;
      Family  : String;
      Size    : Non_Negative_Real;
      Bold    : Boolean := False;
      Italic  : Boolean := False);

   procedure Circle
     (Picture : in out Root_Picture_View'Class;
      Centre  : Point_Type;
      Radius  : Non_Negative_Real;
      Filled  : Boolean);

   procedure Rectangle
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real;
      Filled   : Boolean);

   procedure Text
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Text     : String);

   procedure Label
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Offset_X : Integer;
      Offset_Y : Integer;
      Text     : String);

   procedure Image
     (Picture       : in out Root_Picture_View'Class;
      Resource_Name : String;
      Center        : Point_Type;
      Width, Height : Natural);

private

   type Draw_Primitive is
     (Set_Draw_Color, Set_Fill_Color, Set_Font, Set_Line_Width,
      Move_To, Set_Fill,
      Draw_Line, Draw_Line_To, Draw_Polygon,
      Draw_Circle, Draw_Rectangle, Draw_Text, Draw_Image);

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Point_Type);

   type Draw_Command (Primitive : Draw_Primitive) is
      record
         case Primitive is
            when Set_Draw_Color | Set_Fill_Color =>
               Color : Concorde.Color.Concorde_Color;
            when Set_Font =>
               Font_Family : Ada.Strings.Unbounded.Unbounded_String;
               Font_Size   : Non_Negative_Real;
               Font_Bold   : Boolean;
               Font_Italic : Boolean;
            when Set_Line_Width =>
               Line_Width  : Non_Negative_Real;
            when Move_To | Draw_Line_To =>
               Relative    : Boolean;
               Position    : Point_Type;
            when Draw_Line =>
               Line_From   : Point_Type;
               Line_To     : Point_Type;
            when Draw_Polygon =>
               Polygon     : Point_Vectors.Vector;
            when Set_Fill =>
               Fill        : Boolean;
            when Draw_Circle =>
               Radius      : Non_Negative_Real;
            when Draw_Rectangle =>
               Width       : Non_Negative_Real;
               Height      : Non_Negative_Real;
            when Draw_Text =>
               Offset_X    : Integer;
               Offset_Y    : Integer;
               Text        : Ada.Strings.Unbounded.Unbounded_String;
            when Draw_Image =>
               Image_Width  : Natural;
               Image_Height : Natural;
               Image_Name   : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package Draw_Command_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Draw_Command);

   type Picture_Canvas is
     new Gnoga.Gui.Element.Canvas.Canvas_Type with
      record
         View : access Root_Picture_View'Class;
      end record;

   protected type Update_Queue is
      procedure Queue_Update (Layers : Layer_Flags);
      entry Wait_For_Queued_Update
        (Layers : out Layer_Flags);
   private
      Queued        : Boolean := False;
      Queued_Layers : Layer_Flags (1 .. Max_Picture_Layers);
   end Update_Queue;

   type Update_Queue_Access is access Update_Queue;

   task type Render_Task is
      entry Start (View   : Picture_View);
   end Render_Task;

   type Render_Task_Access is access Render_Task;

   package Image_Resource_Maps is
     new WL.String_Maps (Gnoga.Gui.Element.Common.Pointer_To_IMG_Class,
                         Gnoga.Gui.Element.Common."=");

   type Draw_Layer_Record is
      record
         Canvas  : Gnoga.Gui.Element.Canvas.Canvas_Type;
         Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      end record;

   type Draw_Layer is access Draw_Layer_Record;

   package Draw_Layer_Vectors is
     new Ada.Containers.Vectors (Layer_Index, Draw_Layer);

   type Root_Picture_View is abstract new Root_View_Type with
      record
         Background      : Concorde.Color.Concorde_Color := Concorde.Color.Black;
         Commands        : Draw_Command_Lists.List;
         Render_Canvas   : Picture_Canvas;
         Render_Context  : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
         Layers          : Draw_Layer_Vectors.Vector;
         Viewport        : Rectangle_Type := ((0.0, 0.0), 1.0, 1.0);
         X_Axis_Rotation : Real := 45.0;
         Dragging        : Boolean := False;
         Previous_X      : Integer;
         Previous_Y      : Integer;
         Update          : Update_Queue_Access;
         Renderer        : Render_Task_Access;
         Resources       : Image_Resource_Maps.Map;
      end record;

   procedure Add
     (Picture : in out Root_Picture_View'Class;
      Command : Draw_Command);

   function Contains
     (R : Rectangle_Type;
      P : Point_Type)
      return Boolean
   is (P.X in R.Left_Top.X .. R.Left_Top.X + R.Width
       and then P.Y in R.Left_Top.Y .. R.Left_Top.Y + R.Height);

   procedure Render_Layer
     (View  : in out Root_Picture_View'Class;
      Layer : Layer_Index);

end Concorde.UI.Views.Picture;
