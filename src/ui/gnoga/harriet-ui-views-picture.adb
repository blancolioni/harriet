with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Gnoga.Types;

with Concorde.Real_Images;

package body Concorde.UI.Views.Picture is

   procedure On_Canvas_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Canvas_Button_Down
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Canvas_Button_Up
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Canvas_Mouse_Move
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   ---------
   -- Add --
   ---------

   procedure Add
     (Picture : in out Root_Picture_View'Class;
      Command : Draw_Command)
   is
   begin
      Picture.Commands.Append (Command);
   end Add;

   ------------------------
   -- Add_Image_Resource --
   ------------------------

   procedure Add_Image_Resource
     (Picture : in out Root_Picture_View'Class;
      Name    : String;
      Path    : String)
   is
      use Gnoga.Gui.Element.Common;
      Img : constant Pointer_To_IMG_Class :=
              new IMG_Type;
   begin
      Img.Create
        (Parent           => Picture.Gnoga_View.all,
         URL_Source       => Path);
      Img.Display ("none");
      Picture.Resources.Insert (Name, Img);
   end Add_Image_Resource;

   ----------------------
   -- Background_Color --
   ----------------------

   procedure Background_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Concorde.Color.Concorde_Color)
   is
   begin
      Picture.Background := Color;
   end Background_Color;

   ------------
   -- Circle --
   ------------

   procedure Circle
     (Picture : in out Root_Picture_View'Class;
      Centre  : Point_Type;
      Radius  : Non_Negative_Real;
      Filled  : Boolean)
   is
   begin
      Picture.Move_To (Centre);
      Picture.Add ((Set_Fill, Filled));
      Picture.Add ((Draw_Circle, Radius));
   end Circle;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Picture : in out Root_Picture_View'Class)
   is
   begin
      Picture.Commands.Clear;
   end Clear;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (View : in out Root_Picture_View)
   is
      procedure Free_Task is
        new Ada.Unchecked_Deallocation (Render_Task, Render_Task_Access);
      procedure Free_Queue is
        new Ada.Unchecked_Deallocation (Update_Queue, Update_Queue_Access);
   begin
      Free_Task (View.Renderer);
      Free_Queue (View.Update);
   end Close;

   -------------------------
   -- Create_Picture_View --
   -------------------------

   procedure Create_Picture_View
     (View        : not null access Root_Picture_View'Class;
      Gnoga_View  : in out Gnoga.Gui.View.View_Type'Class;
      Layers      : Layer_Count := 1)
   is
   begin
      View.Render_Canvas.View := View;
      View.Render_Canvas.Create
        (Parent => Gnoga_View,
         Width  => Gnoga_View.Width,
         Height => Gnoga_View.Height);

      View.Render_Canvas.On_Mouse_Click_Handler
        (On_Canvas_Click'Access);
      View.Render_Canvas.On_Mouse_Down_Handler
        (On_Canvas_Button_Down'Access);
      View.Render_Canvas.On_Mouse_Up_Handler
        (On_Canvas_Button_Up'Access);
      View.Render_Canvas.On_Mouse_Move_Handler
        (On_Canvas_Mouse_Move'Access);

      View.Render_Context.Get_Drawing_Context_2D (View.Render_Canvas);

      for I in 1 .. Layers loop
         declare
            Layer : constant Draw_Layer := new Draw_Layer_Record;
         begin
            Layer.Canvas.Create
              (Parent => Gnoga_View,
               Width  => Gnoga_View.Width,
               Height => Gnoga_View.Height);
            Layer.Canvas.Hidden (True);
            Layer.Context.Get_Drawing_Context_2D (Layer.Canvas);
            View.Layers.Append (Layer);
         end;
      end loop;

      View.Update := new Update_Queue;
      View.Renderer := new Render_Task;
      View.Renderer.Start (Picture_View (View));

   end Create_Picture_View;

   ----------------
   -- Draw_Color --
   ----------------

   procedure Draw_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Concorde.Color.Concorde_Color)
   is
   begin
      Picture.Add ((Set_Draw_Color, Color));
   end Draw_Color;

   ----------------
   -- Fill_Color --
   ----------------

   procedure Fill_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Concorde.Color.Concorde_Color)
   is
   begin
      Picture.Add ((Set_Fill_Color, Color));
   end Fill_Color;

   ----------
   -- Font --
   ----------

   procedure Font
     (Picture : in out Root_Picture_View'Class;
      Family  : String;
      Size    : Non_Negative_Real;
      Bold    : Boolean := False;
      Italic  : Boolean := False)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Add
        ((Set_Font, To_Unbounded_String (Family), Size, Bold, Italic));
   end Font;

   -----------
   -- Image --
   -----------

   procedure Image
     (Picture       : in out Root_Picture_View'Class;
      Resource_Name : String;
      Center        : Point_Type;
      Width, Height : Natural)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Move_To (Center);
      Picture.Add
        ((Draw_Image, Width, Height, To_Unbounded_String (Resource_Name)));
   end Image;

   -----------
   -- Label --
   -----------

   procedure Label
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Offset_X : Integer;
      Offset_Y : Integer;
      Text     : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Move_To (Position);
      Picture.Add ((Draw_Text, Offset_X, Offset_Y,
                   To_Unbounded_String (Text)));
   end Label;

   ----------
   -- Line --
   ----------

   procedure Line
     (Picture  : in out Root_Picture_View'Class;
      From, To : Point_Type)
   is
   begin
      Picture.Add ((Draw_Line, From, To));
   end Line;

   -------------
   -- Line_To --
   -------------

   procedure Line_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type)
   is
   begin
      Picture.Add ((Draw_Line_To, False, Position));
   end Line_To;

   ----------------
   -- Line_Width --
   ----------------

   procedure Line_Width
     (Picture : in out Root_Picture_View'Class;
      Width   : Non_Negative_Real)
   is
   begin
      Picture.Add ((Set_Line_Width, Width));
   end Line_Width;

   -------------------
   -- Move_Relative --
   -------------------

   procedure Move_Relative
     (Picture : in out Root_Picture_View'Class;
      Offset  : Point_Type)
   is
   begin
      Picture.Add ((Move_To, True, Offset));
   end Move_Relative;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type)
   is
   begin
      Picture.Add ((Move_To, False, Position));
   end Move_To;

   ---------------------------
   -- On_Canvas_Button_Down --
   ---------------------------

   procedure On_Canvas_Button_Down
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;
   begin
      View.Dragging := True;
      View.Previous_X := Event.X;
      View.Previous_Y := Event.Y;
   end On_Canvas_Button_Down;

   -------------------------
   -- On_Canvas_Button_Up --
   -------------------------

   procedure On_Canvas_Button_Up
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      pragma Unreferenced (Event);
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;
   begin
      View.Dragging := False;
   end On_Canvas_Button_Up;

   ---------------------
   -- On_Canvas_Click --
   ---------------------

   procedure On_Canvas_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;

      View_Size : constant Non_Negative_Real :=
                    Real'Min (View.Viewport.Width, View.Viewport.Height);
      Screen_Width   : constant Natural := View.Render_Canvas.Width;
      Screen_Height  : constant Natural := View.Render_Canvas.Height;

      To_View_Scale  : constant Non_Negative_Real :=
                         View_Size
                           / Real (Natural'Min (Screen_Width, Screen_Height));

      Center_X        : constant Real :=
                   View.Viewport.Left_Top.X
                     + View.Viewport.Width / 2.0;
      Center_Y : constant Real :=
                   View.Viewport.Left_Top.Y
                     + View.Viewport.Height / 2.0;

      function To_View_X (X : Integer) return Real
      is (Real (X - Screen_Width / 2) * To_View_Scale
          + Center_X);

      function To_View_Y (Y : Integer) return Real
      is (Real (Y - Screen_Height / 2) * To_View_Scale
          + Center_Y);

      X : constant Real := To_View_X (Event.X);
      Y : constant Real := To_View_Y (Event.Y);

   begin
      Ada.Text_IO.Put_Line
        ("canvas click" & Event.X'Image & Event.Y'Image
         & " -> "
         & Concorde.Real_Images.Approximate_Image (X)
         & " "
         & Concorde.Real_Images.Approximate_Image (Y));
      View.On_Mouse_Click (X, Y);

   end On_Canvas_Click;

   --------------------------
   -- On_Canvas_Mouse_Move --
   --------------------------

   procedure On_Canvas_Mouse_Move
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;
   begin
      if View.Dragging then
         declare
            DY : constant Integer := Event.Y - View.Previous_Y;
         begin
            View.X_Axis_Rotation := View.X_Axis_Rotation + Real (DY);
         end;
      end if;
   end On_Canvas_Mouse_Move;

   -------------
   -- Polygon --
   -------------

   procedure Polygon
     (Picture  : in out Root_Picture_View'Class;
      Points   : Point_Array;
      Filled   : Boolean)
   is
      V : Point_Vectors.Vector;
   begin
      for P of Points loop
         V.Append (P);
      end loop;
      Picture.Add ((Set_Fill, Filled));
      Picture.Add ((Draw_Polygon, V));
   end Polygon;

   ------------------
   -- Queue_Render --
   ------------------

   overriding procedure Queue_Render
     (View : in out Root_Picture_View)
   is
      All_Layers : constant Layer_Flags (1 .. View.Layers.Last_Index) :=
                     (others => True);
   begin
      View.Update.Queue_Update (All_Layers);
   end Queue_Render;

   ------------------------
   -- Queue_Render_Layer --
   ------------------------

   procedure Queue_Render_Layer
     (View   : in out Root_Picture_View'Class;
      Layers : Layer_Array)
   is
      Flags : Layer_Flags (1 .. View.Layers.Last_Index) :=
                (others => False);
   begin
      for Layer of Layers loop
         Flags (Layer) := True;
      end loop;
      View.Update.Queue_Update (Flags);
   end Queue_Render_Layer;

   ------------------------
   -- Queue_Render_Layer --
   ------------------------

   procedure Queue_Render_Layer
     (View   : in out Root_Picture_View'Class;
      Layer  : Layer_Index)
   is
   begin
      View.Queue_Render_Layer ((1 => Layer));
   end Queue_Render_Layer;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real;
      Filled   : Boolean)
   is
   begin
      Picture.Add ((Set_Fill, Filled));
      Picture.Move_To (Position);
      Picture.Add ((Draw_Rectangle, Width, Height));
   end Rectangle;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (View : in out Root_Picture_View)
   is
   begin
      for Layer in 1 .. View.Layers.Last_Index loop
         View.Render_Context.Draw_Image
           (View.Layers.Element (Layer).Canvas, 0, 0);
      end loop;
   end Render;

   ------------------
   -- Render_Layer --
   ------------------

   procedure Render_Layer
     (View  : in out Root_Picture_View'Class;
      Layer : Layer_Index)
   is
      View_Size : constant Non_Negative_Real :=
                    Real'Min (View.Viewport.Width, View.Viewport.Height);
      Screen_Width   : constant Natural := View.Render_Canvas.Width;
      Screen_Height  : constant Natural := View.Render_Canvas.Height;

      To_Screen_Scale : constant Non_Negative_Real :=
                          Real (Natural'Min (Screen_Width, Screen_Height))
                          / View_Size;

      Center_X : constant Real :=
                   View.Viewport.Left_Top.X
                     + View.Viewport.Width / 2.0;
      Center_Y : constant Real :=
                   View.Viewport.Left_Top.Y
                     + View.Viewport.Height / 2.0;

      function To_Screen_Distance
        (Distance : Non_Negative_Real)
         return Natural
      is (Natural (Distance * To_Screen_Scale));

      function To_Screen_X (X : Real) return Integer
      is (Integer ((X - Center_X) * To_Screen_Scale));

      function To_Screen_Y (Y : Real) return Integer
      is (Integer ((Y - Center_Y) * To_Screen_Scale));

      Current_Position         : Point_Type := (0.0, 0.0);
      Current_Fill             : Boolean := False;
      Current_Fill_Color       : Concorde.Color.Concorde_Color;
      Current_Draw_Color       : Concorde.Color.Concorde_Color;
      Fill_Color_Changed       : Boolean := False;
      Draw_Color_Changed       : Boolean := False;

      Current_Font_Family      : Ada.Strings.Unbounded.Unbounded_String;
      Current_Font_Size        : Non_Negative_Real;
      Current_Font_Changed     : Boolean := False;

      Current_Line_Width       : Non_Negative_Real := 1.0;
      Line_Width_Changed       : Boolean := False;

      Drawing_Line             : Boolean := False;

      Count : Natural := 0;

      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type renames
                  View.Layers.Element (Layer).Context;

      function Is_Visible (P : Point_Type) return Boolean
      is (Contains (View.Viewport, P));

      procedure Check_Path;

      procedure Execute (Command : Draw_Command);

      ----------------
      -- Check_Path --
      ----------------

      procedure Check_Path is
      begin
         if Drawing_Line then
            Drawing_Line := False;
            Context.Stroke;
         end if;
      end Check_Path;

      -------------
      -- Execute --
      -------------

      procedure Execute (Command : Draw_Command) is
         use Ada.Strings.Unbounded;
         use Concorde.Color;
         use Concorde.Real_Images;

         procedure Check_Colors (Fill : Boolean);
         procedure Check_Lines;

         ------------------
         -- Check_Colors --
         ------------------

         procedure Check_Colors (Fill : Boolean) is
         begin
            if Fill then
               if Fill_Color_Changed then
                  Context.Fill_Color
                    (Concorde.Color.To_Html_String (Current_Fill_Color));
                  Fill_Color_Changed := False;
               end if;
            else
               if Draw_Color_Changed then
                  Check_Path;
                  Context.Stroke_Color
                    (Concorde.Color.To_Html_String (Current_Draw_Color));
                  Draw_Color_Changed := False;
               end if;
            end if;

         end Check_Colors;

         -----------------
         -- Check_Lines --
         -----------------

         procedure Check_Lines is
         begin
            if Line_Width_Changed then
               Context.Line_Width (Natural (Current_Line_Width));
               Line_Width_Changed := False;
            end if;
         end Check_Lines;

      begin
         case Command.Primitive is
            when Set_Draw_Color =>
               Current_Draw_Color := Command.Color;
               Draw_Color_Changed := True;
            when Set_Fill_Color =>
               if Command.Color /= Current_Fill_Color then
                  Current_Fill_Color := Command.Color;
                  Fill_Color_Changed := True;
               end if;
            when Set_Font =>
               if Command.Font_Family /= Current_Font_Family then
                  Current_Font_Family := Command.Font_Family;
                  Current_Font_Changed := True;
               end if;
               if Command.Font_Size /= Current_Font_Size then
                  Current_Font_Size := Command.Font_Size;
                  Current_Font_Changed := True;
               end if;
            when Set_Line_Width =>
               if Command.Line_Width /= Current_Line_Width then
                  Current_Line_Width := Command.Line_Width;
                  Line_Width_Changed := True;
               end if;

            when Draw_Line_To =>
               Check_Colors (False);
               Check_Lines;

               if not Drawing_Line then
                  Context.Begin_Path;
               end if;

               Context.Move_To
                 (To_Screen_X (Current_Position.X),
                  To_Screen_Y (Current_Position.Y));

               Current_Position := Command.Position;

               Context.Line_To
                 (To_Screen_X (Current_Position.X),
                  To_Screen_Y (Current_Position.Y));

               Drawing_Line := True;

            when Draw_Line =>

               if Is_Visible (Command.Line_From)
                 or else Is_Visible (Command.Line_To)
               then
                  Check_Colors (False);
                  Check_Lines;

                  if not Drawing_Line then
                     Context.Begin_Path;
                  end if;

                  Current_Position := Command.Line_From;
                  Context.Move_To
                    (To_Screen_X (Current_Position.X),
                     To_Screen_Y (Current_Position.Y));

                  Current_Position := Command.Line_To;

                  Context.Line_To
                    (To_Screen_X (Current_Position.X),
                     To_Screen_Y (Current_Position.Y));

                  Context.Stroke;
                  Drawing_Line := False;
               end if;

            when Draw_Polygon =>
               Check_Path;
               Check_Colors (Current_Fill);
               Check_Lines;

               Context.Begin_Path;
               declare
                  First : Boolean := True;
               begin
                  for P of Command.Polygon loop
                     if First then
                        Context.Move_To
                          (To_Screen_X (P.X),
                           To_Screen_Y (P.Y));
                        First := False;
                     else
                        Context.Line_To
                          (To_Screen_X (P.X),
                           To_Screen_Y (P.Y));
                     end if;
                  end loop;
               end;

               Context.Close_Path;
               if Current_Fill then
                  Context.Fill;
               else
                  Context.Stroke;
               end if;

               Current_Position := Command.Polygon.Last_Element;

            when Draw_Image =>
               Check_Path;
               Check_Lines;

               declare
                  use Gnoga.Gui.Element.Common;
                  Img : constant Pointer_To_IMG_Class :=
                          View.Resources.Element
                            (To_String (Command.Image_Name));
                  W   : constant Natural := Command.Image_Width;
                  H   : constant Natural := Command.Image_Height;
                  X   : constant Integer :=
                          To_Screen_X (Current_Position.X) - W / 2;
                  Y   : constant Integer :=
                          To_Screen_Y (Current_Position.Y) - H / 2;
               begin
                  Context.Draw_Image (Img.all, X, Y, W, H);
               end;

            when Move_To =>
               if Command.Relative then
                  Current_Position.X :=
                    Current_Position.X + Command.Position.X;
                  Current_Position.Y :=
                    Current_Position.Y + Command.Position.Y;
               else
                  Current_Position := Command.Position;
               end if;

            when Set_Fill =>
               Current_Fill := Command.Fill;

            when Draw_Circle =>
               if Is_Visible (Current_Position) then
                  Check_Path;
                  Check_Lines;
                  Check_Colors (Current_Fill);
                  Context.Begin_Path;
                  Context.Arc_Degrees
                    (X                 => To_Screen_X (Current_Position.X),
                     Y                 => To_Screen_Y (Current_Position.Y),
                     Radius            => To_Screen_Distance (Command.Radius),
                     Starting_Angle    => 0.0,
                     Ending_Angle      => 360.0);
                  if Current_Fill then
                     Context.Fill;
                  else
                     Context.Stroke;
                  end if;
                  Count := Count + 1;
               end if;

            when Draw_Rectangle =>

               if Is_Visible (Current_Position)
                 or else Is_Visible ((Current_Position.X + Command.Width,
                                     Current_Position.Y + Command.Height))
               then
                  Check_Path;
                  Check_Lines;
                  Check_Colors (Current_Fill);
                  Context.Rectangle
                    (Rectangle => Gnoga.Types.Rectangle_Type'
                       (X      => To_Screen_X (Current_Position.X),
                        Y      => To_Screen_Y (Current_Position.Y),
                        Width  => To_Screen_Distance (Command.Width),
                        Height => To_Screen_Distance (Command.Height)));
                  if Current_Fill then
                     Context.Fill;
                  else
                     Context.Stroke;
                  end if;
                  Count := Count + 1;
               end if;

            when Draw_Text =>
               if Is_Visible (Current_Position) then
                  if Current_Font_Changed then
                     Context.Font
                       (Family  => To_String (Current_Font_Family),
                        Height  =>
                          Approximate_Image (Current_Font_Size) & "pt");
                     Current_Font_Changed := False;
                  end if;
                  Check_Path;
                  Check_Lines;
                  Check_Colors (True);
                  Context.Fill_Text
                    (Text       => To_String (Command.Text),
                     X          =>
                       Command.Offset_X + To_Screen_X (Current_Position.X),
                     Y          =>
                       Command.Offset_Y + To_Screen_Y (Current_Position.Y));
                  Count := Count + 1;
               end if;
         end case;
      end Execute;

      Start : constant Ada.Calendar.Time :=
                Ada.Calendar.Clock;

   begin
--        Ada.Text_IO.Put_Line
--          ("picture render: width"
--           & Natural'Image (View.Gnoga_View.Width)
--           & " height"
--           & Natural'Image (View.Gnoga_View.Height)
--           & " viewport "
--           & Concorde.Real_Images.Approximate_Image (View.Viewport.Left_Top.X)
--           & " "
--           & Concorde.Real_Images.Approximate_Image (View.Viewport.Left_Top.Y)
--           & " "
--           & Concorde.Real_Images.Approximate_Image (View.Viewport.Width)
--           & "x"
--           & Concorde.Real_Images.Approximate_Image (View.Viewport.Height));

      if Layer = 1 then
         Context.Fill_Color
           (Concorde.Color.To_Html_String (View.Background));
         Context.Fill_Rectangle
           (Gnoga.Types.Rectangle_Type'
              (X      => 0,
               Y      => 0,
               Width  => Screen_Width,
               Height => Screen_Height));
      else
         Context.Clear_Rectangle
           (Gnoga.Types.Rectangle_Type'
              (X      => 0,
               Y      => 0,
               Width  => Screen_Width,
               Height => Screen_Height));
      end if;

      Context.Save;

      Context.Translate (Screen_Width / 2, Screen_Height / 2);

      for Command of View.Commands loop
         Execute (Command);
      end loop;

      Check_Path;

      Context.Restore;

      declare
         use type Ada.Calendar.Time;
         Finish : constant Ada.Calendar.Time :=
                    Ada.Calendar.Clock;
      begin
         if False then
            Ada.Text_IO.Put_Line
              ("executed"
               & Count'Image
               & " primitives in "
               & Concorde.Real_Images.Approximate_Image
                 (Real (Finish - Start))
               & "s");
         end if;
      end;

   end Render_Layer;

   -----------------
   -- Render_Task --
   -----------------

   task body Render_Task is
      Target : Picture_View;
   begin
      select
         accept Start (View   : Picture_View)
         do
            Target := View;
         end Start;
      or
         terminate;
      end select;

      loop
         declare
            Layers : Layer_Flags (1 .. Target.Layers.Last_Index);
         begin
            Target.Update.Wait_For_Queued_Update (Layers);
            for Layer in Layers'Range loop
               if Layers (Layer) then
                  Target.Draw_Picture (Layer);
                  Target.Render_Layer (Layer);
               end if;
            end loop;
            Target.Render;
         end;
      end loop;
   end Render_Task;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (View : in out Root_Picture_View)
   is
      function Image (X : Natural) return String
      is (Ada.Strings.Fixed.Trim (X'Image, Ada.Strings.Left));

   begin
      View.Render_Canvas.Attribute ("width", Image (View.Gnoga_View.Width));
      View.Render_Canvas.Attribute ("height", Image (View.Gnoga_View.Height));

      for Layer of View.Layers loop
         Layer.Canvas.Attribute ("width", Image (View.Gnoga_View.Width));
         Layer.Canvas.Attribute ("height", Image (View.Gnoga_View.Height));
      end loop;

      View.Queue_Render;
   end Resize;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport
     (Picture  : in out Root_Picture_View'Class;
      Left     : Real;
      Top      : Real;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real)
   is
   begin
      Picture.Viewport := ((Left, Top), Width, Height);
   end Set_Viewport;

   ----------
   -- Text --
   ----------

   procedure Text
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Text     : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Move_To (Position);
      Picture.Add ((Draw_Text, 0, 0, To_Unbounded_String (Text)));
   end Text;

   ------------------
   -- Update_Queue --
   ------------------

   protected body Update_Queue is

      ------------------
      -- Queue_Update --
      ------------------

      procedure Queue_Update (Layers : Layer_Flags) is
      begin
         Queued := True;
         Queued_Layers (1 .. Layers'Length) := Layers;
      end Queue_Update;

      ----------------------------
      -- Wait_For_Queued_Update --
      ----------------------------

      entry Wait_For_Queued_Update
        (Layers : out Layer_Flags)
        when Queued
      is
      begin
         Queued := False;
         Layers := Queued_Layers (1 .. Layers'Length);
         Queued_Layers := (others => False);
      end Wait_For_Queued_Update;

   end Update_Queue;

end Concorde.UI.Views.Picture;
