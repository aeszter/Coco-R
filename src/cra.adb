--------------------------------------
--                                  --
-- Automaton and Scanner Generation --
--                                  --
--------------------------------------

--   (1) Convert_To_States translates a top-down graph into a NFA.
--       Match_DFA tries to match literal strings against the DFA
--   (2) Convert_NDFA_into_DFA converts the NFA into a DFA
--   (3) Write_Scanner generates the scanner source file

-- with ada.text_io;


package body CRA is


   procedure Report_Semantic_Error (The_Error_Number: in     Integer)
   is
   begin
      CRS.Error (The_Error_Number + 100, CRS.line, CRS.col, CRS.pos);
   end Report_Semantic_Error;


   procedure Put (C: in     Character)
   is
   begin
      FileIO.Write (Cur_Out_File, C);
   end Put;


   procedure Put_Ln
   is
   begin
      FileIO.Write_Ln (Cur_Out_File);
   end Put_Ln;


   procedure Put_Blank (Blank_Count: in     Integer)
   is
   begin
      FileIO.Write_Text (Cur_Out_File, " ", Blank_Count);
   end Put_Blank;


   procedure Indent (Width: in     CARDINAL)
   is
   begin
      if  Is_New_Line  then
         Put_Blank (Width);
      else
         Is_New_Line := TRUE;
      end if;
   end Indent;


   procedure Put_S (S: in     String)
   is
   begin
      for  I  in S'Range  loop
         if  S (I) = '$'  then
            FileIO.Write_Ln (Cur_Out_File);
         else
            FileIO.Write (Cur_Out_File, S (I));
         end  if;
      end loop;
   end Put_S;


   procedure Put_I (I: in     Integer)
   is
   begin
      FileIO.Write_Int (Cur_Out_File, I, 1);
   end Put_I;


   procedure Put_I2
     (I     : in     Integer;
      Width : in     Integer)
   is
   begin
      FileIO.Write_Int (Cur_Out_File, I, Width);
   end Put_I2;


   procedure Put_C (C: in     Character)
   is
   begin
      case  C  is
         when  Character'Val (8#0#)   .. Character'Val (8#37#) |
               Character'Val (8#177#) .. Character'Val (8#377#)   =>
            Put_S ("Character'Val (");
            Put_I (Character'Pos (C));
            Put (')');

         when others =>
            Put (''');
            Put (C);
            Put (''');
      end case;
   end Put_C;


   procedure Put_SN (The_Symbol_Node_Index: in     Integer)
   is
      use FileIO;
      The_Symbol_Node: CRT.Symbol_Node;
   begin
      CRT.Get_Symbol_Node (The_Symbol_Node_Index, The_Symbol_Node);

      if  (The_Symbol_Node.Named_Constant /= null) and then
          (FileIO.SLENGTH (The_Symbol_Node.Named_Constant.all) > 0)  then
         Put_S (The_Symbol_Node.Named_Constant.all);
      else
         Put_I (The_Symbol_Node_Index);
      end if;
   end Put_SN;


   procedure Put_SE (The_Symbol_Node_Index: in     Integer)
   is
   begin
      Put_S  ("sym := ");
      Put_SN (The_Symbol_Node_Index);
      Put_S  ("; ");
   end Put_SE;


   procedure Put_Range
     (The_Set     : in     CRT.CRT_Set;
      The_Indent  : in     CARDINAL)
   is
      type HL_Array is array (Integer range 0..31) of Character;
      pragma Pack (HL_Array);

      Lo    : HL_Array;
      Hi    : HL_Array;
      Top   : Integer;
      I     : Integer;
      The_Set_1    : CRT.CRT_Set;
   begin
      -- fill Lo and Hi
      Top   := -1;
      I     := 0;
      while  I <= 256  loop -- PDT
         if  Sets.Is_In (The_Set, I)  then
            Top := Top + 1;
            Lo (Top) := Character'Val (I);
            I := I + 1;
            while  ((I < 256)           and then --PDT
                    (Sets.Is_In (The_Set, I)))         loop
               I := I + 1;
            end loop;
            Hi (Top) := Character'Val (I - 1);
         else
            I := I + 1;
         end if;
      end loop;

      -- print ranges
      if    (Top    = 1                     ) and then
            (Lo (0) = Character'Val (8#0#  )) and then
            (Hi (1) = Character'Val (8#377#)) and then -- PDT
            (Character'Val (Character'Pos(Hi (0)) + 2) = Lo (1)) then
         Sets.Fill   (The_Set_1);
         Sets.Differ (The_Set_1, The_Set);
         Put_S       ("not ");
         Put_Range   (The_Set_1, The_Indent);
      else
         Put_S ("(");
         I := 0;
         while  (I <= Top)  loop
            if  (Hi (I) = Lo (I))  then
               Put_S ("(ch = ");
               Put_C (Lo (I));
            elsif  (Lo (I) = Character'Val (8#0#))  then
               Put_S ("(ch <= ");
               Put_C (Hi (I));
            elsif  (Hi (I) = Character'Val (8#377#))  then   -- PDT
               Put_S ("(ch >= ");
               Put_C (Lo (I));
            else
               Put_S ("((ch >= ");
               Put_C (Lo (I));
               Put_S (") and then (ch <= ");
               Put_C (Hi (I));
               Put (')');
            end if;
            Put (')');
            if  I < Top  then
              Put_S (" or else$");
              Put_Blank (The_Indent);
            end if;
            I := I + 1;
         end loop;
         Put (')');
      end if;
   end Put_Range;


   procedure Put_Character_Condition (C: in     Character)
   is
   begin
      Put_S ("(ch = ");
      Put_C (C);
      Put   (')');
   end Put_Character_Condition;


   --
   -- Print a symbol for tracing
   --
   procedure Print_Symbol_For_Tracing
    (The_Type  : in     Integer;
     The_Value : in     Integer;
     The_Widht : in     Integer)
   is
      Character_Class_Name : CRT.Name_Ptr;
      Length               : Integer;
   begin
      if  (The_Type = CRT.NT_Character_Class)  then
         CRT.Get_Character_Class_Name (The_Value, Character_Class_Name);
         Put_S (Character_Class_Name.all);
         Length := FileIO.SLENGTH (Character_Class_Name.all);
      elsif  (The_Value >= Character'Pos (' ')) and then
             (The_Value < 127) and then
             (The_Value /= 34)  then
         Put ('"');
         Put (Character'Val (The_Value));
         Put ('"');
         Length := 3;
      else
         Put_S  ("Character'Val (");
         Put_I2 (The_Value, 2);
         Put   (')');
         Length := 7;
      end if;

      while  (Length < The_Widht)  loop
         Put(' ');
         Length := Length + 1;
      end loop;
   end Print_Symbol_For_Tracing;


   -------------------------------------------------------------------------


   --
   -- List the automaton for tracing
   --
   procedure Print_Automaton_States
   is
      The_Action     : Action_Node_Ptr;
      Is_First       : Boolean;
      The_State      : Integer;
      The_Target_Node: Target_Node_Ptr;
      The_Set        : CRT.CRT_Set;
      The_Name       : CRT.Name_Ptr;
   begin
      Cur_Out_File := CRS.Listing;
      Put_S ("$-------- states ---------$");
      The_State := Root_State;

      while  The_State <= Last_Allocated_State  loop
         The_Action := Automaton_States (The_State).First_Action;
         Is_First := TRUE;
         if  Automaton_States (The_State).End_Of_Token = CRT.No_Symbol  then
            Put_S ("     ");
         else
            Put_S ("E(");
            Put_I2 (Automaton_States (The_State).End_Of_Token, 2);
            Put (')');
         end if;
         Put_I2 (The_State, 3);
         Put (':');
         if  The_Action = null  then
            Put_S (" $");
         end if;

         while  The_Action /= null  loop
            if  Is_First  then
               Put (' ');
               Is_First := FALSE;
            else
               Put_S ("          ");
            end if;
            Print_Symbol_For_Tracing (The_Action.Node_Type, The_Action.Symbol_Action, 0);
            Put (' ');
            The_Target_Node := The_Action.Target;
            while  The_Target_Node /= null  loop
               Put_I (The_Target_Node.State);
               Put (' ');
               The_Target_Node := The_Target_Node.Next;
            end loop;
            if  The_Action.Transition_Code = CRT.contextTrans  then
               Put_S (" context$");
            else
               Put_S ("$");
            end if;
            The_Action := The_Action.Next;
         end loop;

         The_State := The_State + 1;
      end loop;

      Put_S ("$-------- character classes ---------$");

      for  I in 0..CRT.maxC  loop
         CRT.Get_Character_Class       (I, The_Set);
         CRT.Get_Character_Class_Name  (I, The_Name);
         FileIO.Write_Text             (Cur_Out_File, The_Name.all, 10);
         FileIO.Write_String           (Cur_Out_File, ": ");
         Sets.Print                    (Cur_Out_File, The_Set, 80, 13);
         FileIO.Write_Ln               (Cur_Out_File);
      end loop;
   end Print_Automaton_States;


   --
   -- Add a action to the action list of a State
   --
   procedure Add_Action
     (The_Action  : in     Action_Node_Ptr;
      The_Head    : in out Action_Node_Ptr)
   is
      A      : Action_Node_Ptr := The_Head;
      Last_A : Action_Node_Ptr := null;
   begin
      loop
         if  (A = null) or else
             (The_Action.Node_Type < A.Node_Type)  then
            -- collecting classes at the front improves performance
            The_Action.Next := A;
            if  Last_A = null  then
               The_Head := The_Action;
            else
               Last_A.Next := The_Action;
            end if;
            return;
         end if;
         Last_A := A;
         A      := A.Next;
      end loop;
   end Add_Action;


   --
   -- Detach action a from list L
   --
   procedure Detach_Action
     (The_Action: in Action_Node_Ptr;
      The_List: in out Action_Node_Ptr)
   is
   begin
      if  The_List = The_Action then
         The_List := The_Action.Next;
      elsif  The_List /= null  then
         Detach_Action (The_Action, The_List.Next);
      end if;
   end Detach_Action;


   function Get_Action
     (The_State: in     Automaton_State;
      C        : in     Character)
     return Action_Node_Ptr
   is
      The_Action  : Action_Node_Ptr;
      The_Set     : CRT.CRT_Set;
   begin
      The_Action := The_State.First_Action;
      while  The_Action /= null  loop
         if  The_Action.Node_Type = CRT.NT_Single_Character  then
            if  Character'Pos (C) = The_Action.Symbol_Action  then
               return The_Action;
            end if;
         elsif  The_Action.Node_Type = CRT.NT_Character_Class  then
            CRT.Get_Character_Class (The_Action.Symbol_Action, The_Set);
            if  Sets.Is_In (The_Set, Character'Pos (C)) then
               return The_Action;
            end if;
         end if;
         The_Action := The_Action.Next;
      end loop;
      return null;
   end Get_Action;


   procedure Add_Target_List
     (The_List_A: in out Target_Node_Ptr;
      The_List_B: in out Target_Node_Ptr)
   is

      procedure Add_Target_Node
        (The_Target_Node   : in     Target_Node_Ptr;
         The_List          : in out Target_Node_Ptr)
      is
         T     : Target_Node_Ptr := The_Target_Node;
         P     : Target_Node_Ptr := The_List;
         Last_P: Target_Node_Ptr := null;
      begin
         loop
            exit when (P = null) or else (T.State < P.State); -- GOTO 999
            if  P.State = T.State  then
               Dispose (T);
               return;
            end if;
            Last_P := P;
            P      := P.Next;
         end loop;
      -- 999:
         T.Next := P;
         if  Last_P = null  then
            The_List := T;
         else
            Last_P.Next := T;
         end if;
       end Add_Target_Node;

      P: Target_Node_Ptr := The_List_A;
      T: Target_Node_Ptr;

   begin -- Add_Target_List
      while  P /= null  loop
         T       := new Target_Node;
         T.State := P.State;
         Add_Target_Node (T, The_List_B);
         P       := P.Next;
      end loop;
   end Add_Target_List;



   --
   -- Generate new info about a melted State
   --
   function New_Melted_Node
     (The_State_Set  : in     CRT.CRT_Set;
      The_State      : in     Integer)
     return Melted_Node_Ptr
   is
      The_Melted_Node: Melted_Node_Ptr;
   begin
      The_Melted_Node                := new Melted_Node;
      The_Melted_Node.Old_States_Set := The_State_Set;
      The_Melted_Node.New_State      := The_State;
      The_Melted_Node.Next           := List_Of_Melted_States;
      List_Of_Melted_States          := The_Melted_Node;
      return The_Melted_Node;
   end New_Melted_Node;


   --
   -- Return a new State node
   --
   function New_State_Node_Index return Integer
   is
   begin
      Last_Allocated_State := Last_Allocated_State + 1;
      if  Last_Allocated_State > maxStates  then
         CRT.Restriction (7, maxStates);
      end if;

      Automaton_States (Last_Allocated_State).First_Action       := null;
      Automaton_States (Last_Allocated_State).End_Of_Token       := CRT.No_Symbol;
      Automaton_States (Last_Allocated_State).Reached_By_Context := FALSE;

      return Last_Allocated_State;
   end New_State_Node_Index;


   --
   -- Generate transition (The_Graph_Node.State, The_Graph_Node.Index_1) --> toState
   --
   procedure New_Transition
     (From_State     : in     Integer;
      The_Graph_Node : in     CRT.Graph_Node;
      To_State       : in     Integer)
   is
      A: Action_Node_Ptr;
      T: Target_Node_Ptr;
   begin
      if  To_State = Root_State  then
         Report_Semantic_Error (21);
      end if;

      T        := new Target_Node;
      T.State  := To_State;
      T.Next   := null;

      A                 := new Action_Node;
      A.Node_Type       := The_Graph_Node.Node_Type;
      A.Symbol_Action   := The_Graph_Node.Index_1;
      A.Transition_Code := The_Graph_Node.Index_2;
      A.Target          := T;

      Add_Action (A, Automaton_States (From_State).First_Action);
   end New_Transition;


   --
   -- Defines a new comment for the scanner. The comment brackets are
   -- represented by the mini top-down graphs with the roots from and to.
   --
   procedure Define_New_Comment
     (The_Start_From : in     Integer;
      The_Stop_To    : in     Integer;
      The_Nested     : in     Boolean)
   is

      New_Comment_Def: Comment_Node_Ptr;

      procedure Make_Comment_String
        (The_Graph_Index_Arg  : in     Integer;
         The_Comment_String   :    out String; -- String (1..2)
         The_Last             :    out Positive)
      is
         The_Graph_Node    : CRT.Graph_Node;
         The_Set           : CRT.CRT_Set;
         The_Graph_Index   : Integer := The_Graph_Index_Arg;
         I                 : Integer := 1;
         N                 : Integer;
      begin
         while  The_Graph_Index /= 0  loop
            CRT.Get_Graph_Node (The_Graph_Index, The_Graph_Node);
            if  The_Graph_Node.Node_Type = CRT.NT_Single_Character  then
               if  I < 3  then
                  The_Comment_String (I) := Character'Val (The_Graph_Node.Index_1);
               end if;
               I := I + 1;
            elsif  The_Graph_Node.Node_Type = CRT.NT_Character_Class  then
               CRT.Get_Character_Class (The_Graph_Node.Index_1, The_Set);
               if  Sets.Elements (The_Set) /= 1 then
                  Report_Semantic_Error (26);
               else
                  N := Sets.Last_Element (The_Set);
               end if;
               if  I < 3  then
                  The_Comment_String (I) := Character'Val (N);
               end if;
               I := I + 1;
            else
               Report_Semantic_Error (22);
            end if;
            The_Graph_Index := The_Graph_Node.Next;
         end loop;

         if  (I = 1) or else (I > 3)  then
            Report_Semantic_Error (25);
         else
            The_Last := I - 1;
         end if;
      end Make_Comment_String;

   begin -- Define_New_Comment
      New_Comment_Def := new Comment_Node;
      Make_Comment_String
        (The_Start_From,
         New_Comment_Def.Start_Comment,
         New_Comment_Def.Start_Last);
      Make_Comment_String
        (The_Stop_To,
         New_Comment_Def.Stop_Comment,
         New_Comment_Def.Stop_Last);
      New_Comment_Def.Nested := The_Nested;
      New_Comment_Def.Next   := List_Of_Comments;
      List_Of_Comments       := New_Comment_Def;
   end Define_New_Comment;


   --
   -- Delete a target list
   --
   procedure Delete_Target_List (The_List_Arg : in     Target_Node_Ptr)
   is
      The_List : Target_Node_Ptr := The_List_Arg;
   begin
      if  The_List /= null  then
         Delete_Target_List (The_List.Next);
         Dispose            (The_List);
      end if;
   end Delete_Target_List;


   --
   -- Delete an action list
   --
   procedure Delete_Action_List (The_Action_Arg : in     Action_Node_Ptr)
   is
      The_Action : Action_Node_Ptr := The_Action_Arg;
   begin
      if  The_Action /= null  then
         Delete_Action_List (The_Action.Next);
         Delete_Target_List (The_Action.Target);
         Dispose            (The_Action);
      end if;
   end Delete_Action_List;


   --
   -- Expand action symbol into symbol Set
   --
   procedure Make_Set
     (The_Action_Node   : in     Action_Node_Ptr;
      The_Set           : in out CRT.CRT_Set)
   is
   begin
      if  The_Action_Node.Node_Type = CRT.NT_Character_Class  then
         CRT.Get_Character_Class (The_Action_Node.Symbol_Action, The_Set);
      else
         Sets.Clear (The_Set);
         Sets.Incl  (The_Set, The_Action_Node.Symbol_Action);
      end if;
   end Make_Set;


   --
   -- Change the action symbol to Set
   --
   procedure Change_Action
     (The_Action_Node   : in     Action_Node_Ptr;
      The_Set           : in     CRT.CRT_Set)
   is
      Last_Element: aliased Integer;
   begin
      if  Sets.Elements (The_Set) = 1  then
         Last_Element                  := Sets.Last_Element (The_Set);
         The_Action_Node.Node_Type     := CRT.NT_Single_Character;
         The_Action_Node.Symbol_Action := Last_Element;
      else
         Last_Element := CRT.Get_Class_By_Set (The_Set);
         if  Last_Element < 0  then
            Last_Element := CRT.New_Character_Class (new String'("##"), The_Set);
         end if;
         The_Action_Node.Node_Type     := CRT.NT_Character_Class;
         The_Action_Node.Symbol_Action := Last_Element;
      end if;
   end Change_Action;


   --
   -- Combine shifts with different symbols into same State
   --
   procedure Combine_Shifts
   is
      The_State         : Integer;
      The_Action_Node_A : Action_Node_Ptr;
      The_Action_Node_B : Action_Node_Ptr;
      The_Action_Node_C : Action_Node_Ptr;
      The_Set_A         : CRT.CRT_Set;
      The_Set_B         : CRT.CRT_Set;
   begin
      The_State := Root_State;
      while  The_State <= Last_Allocated_State  loop
         The_Action_Node_A := Automaton_States (The_State).First_Action;
         while  The_Action_Node_A /= null  loop
            The_Action_Node_B := The_Action_Node_A.Next;
            while  The_Action_Node_B /= null  loop
               if  (The_Action_Node_A.Target.State = The_Action_Node_B.Target.State) and then
                   (The_Action_Node_A.Transition_Code = The_Action_Node_B.Transition_Code) then
                  Make_Set       (The_Action_Node_A, The_Set_A);
                  Make_Set       (The_Action_Node_B, The_Set_B);
                  Sets.Unite     (The_Set_A, The_Set_B);
                  Change_Action  (The_Action_Node_A, The_Set_A);
                  The_Action_Node_C := The_Action_Node_B;
                  The_Action_Node_B := The_Action_Node_B.Next;
                  Detach_Action  (The_Action_Node_C, The_Action_Node_A);
               else
                  The_Action_Node_B := The_Action_Node_B.Next;
               end if;
            end loop;
            The_Action_Node_A := The_Action_Node_A.Next;
         end loop;
         The_State := The_State + 1;
      end loop;
   end Combine_Shifts;


   --
   -- Delete unused and equal States
   --
   procedure Delete_Redundant_States
   is
      The_Action_Node   : Action_Node_Ptr;
      A_State_1         : Integer;
      A_State_2         : Integer;
      Next_State        : Integer;
      Used_States       : Sets.BitArray (0 .. maxStates / Sets.size);
      New_State_Numbers : array (Integer range 0 .. maxStates) of Integer;

      procedure Find_Used_States (The_State: Integer)
      is
         An_Action: Action_Node_Ptr;
      begin
         if  Sets.Is_In (Used_States, The_State)  then
            return;
         end if;
         Sets.Incl (Used_States, The_State);
         An_Action := Automaton_States (The_State).First_Action;
         while  An_Action /= null  loop
            Find_Used_States (An_Action.Target.State);
            An_Action := An_Action.Next;
         end loop;
      end Find_Used_States;

   begin -- Delete_Redundant_States
      Sets.Clear (Used_States);
      Find_Used_States (Root_State);

      -- combine equal final States
      A_State_1 := Root_State + 1;  -- Root_State cannot be final
      while  A_State_1 <= Last_Allocated_State  loop
         if  Sets.Is_In (Used_States, A_State_1) and then
             (Automaton_States (A_State_1).End_Of_Token /= CRT.No_Symbol)  then
            if  (Automaton_States (A_State_1).First_Action = null) and then
                not Automaton_States (A_State_1).Reached_By_Context then

               A_State_2 := A_State_1 + 1;
               while  A_State_2 <= Last_Allocated_State  loop
                  if  Sets.Is_In (Used_States, A_State_2) and then
                      (Automaton_States (A_State_1).End_Of_Token =
                       Automaton_States (A_State_2).End_Of_Token)  then
                     if  (Automaton_States (A_State_2).First_Action = null) and then
                         not Automaton_States (A_State_2).Reached_By_Context  then
                        Sets.Excl (Used_States, A_State_2);
                        New_State_Numbers (A_State_2) := A_State_1;
                     end if;
                  end if;
                  A_State_2 := A_State_2 + 1;
               end loop;

            end if;
         end if;
         A_State_1 := A_State_1 + 1;
      end loop;

      A_State_1 := Root_State;   --  + 1 ?
                                 -- PDT - was Root_State, but Oberon had .Next ie +1
                                 -- seems to work both ways??
      while  A_State_1 <= Last_Allocated_State  loop
         if  Sets.Is_In (Used_States, A_State_1)  then
            The_Action_Node := Automaton_States (A_State_1).First_Action;
            while  The_Action_Node /= null  loop
               if  not Sets.Is_In (Used_States, The_Action_Node.Target.State)  then
                  The_Action_Node.Target.State := New_State_Numbers (The_Action_Node.Target.State);
               end if;
               The_Action_Node := The_Action_Node.Next;
            end loop;
         end if;
         A_State_1 := A_State_1 + 1;
      end loop;

      -- delete unused States
      A_State_1   := Root_State + 1;
      Next_State  := A_State_1;

      while  A_State_1 <= Last_Allocated_State  loop
         if  Sets.Is_In (Used_States, A_State_1) then
            if  Next_State < A_State_1  then
               Automaton_States (Next_State) := Automaton_States (A_State_1);
            end if;
            New_State_Numbers (A_State_1) := Next_State;
            Next_State                    := Next_State + 1;
         else
            Delete_Action_List (Automaton_States (A_State_1).First_Action);
         end if;
         A_State_1 := A_State_1 + 1;
      end loop;

      Last_Allocated_State := Next_State - 1;
      A_State_1            := Root_State;

      while  A_State_1 <= Last_Allocated_State  loop
         The_Action_Node := Automaton_States (A_State_1).First_Action;
         while  The_Action_Node /= null  loop
           The_Action_Node.Target.State := New_State_Numbers (The_Action_Node.Target.State);
           The_Action_Node              := The_Action_Node.Next;
         end loop;
         A_State_1 := A_State_1 + 1;
      end loop;
   end Delete_Redundant_States;


   --
   -- Converts top-down graph (TDG) with root The_TDG_Index
   -- into a subautomaton of the DFA
   -- that recognizes token The_Symbol_Index
   --
   procedure Convert_To_States
     (The_TDG_Index     : in    Integer;
      The_Symbol_Index  : in    Integer)
   -- note: The_Graph_Node.Source_Line is abused as a State number!
   is
      Stepped  : CRT.Mark_List;
      Visited  : CRT.Mark_List;

      procedure Number_Nodes
        (The_Graph_Index   : in     Integer;
         The_State_Number  : in     Integer)
      is
         The_Graph_Node : CRT.Graph_Node;
         The_SNR        : Integer := The_State_Number;
      begin
         if  The_Graph_Index = 0 then  -- end of graph
            return;
         end if;

         CRT.Get_Graph_Node (The_Graph_Index, The_Graph_Node);
         if  The_Graph_Node.Source_Line >= 0  then -- already Visited
            return;
         end if;
         if  The_SNR < Root_State then
            The_SNR := New_State_Node_Index;
         end if;

         The_Graph_Node.Source_Line := The_SNR;
         CRT.Put_Graph_Node (The_Graph_Index, The_Graph_Node);

         if  CRT.Is_Deletable_Graph (The_Graph_Index) then
            Automaton_States (The_SNR).End_Of_Token := The_Symbol_Index;
         end if;
         -- The_SNR is end State
         case  The_Graph_Node.Node_Type  is
            when CRT.NT_Character_Class | CRT.NT_Single_Character =>
               Number_Nodes (abs (The_Graph_Node.Next), Root_State - 1);
            when CRT.NT_Optional =>
               Number_Nodes (abs (The_Graph_Node.Next), Root_State - 1);
               Number_Nodes (The_Graph_Node.Index_1, The_SNR);
            when CRT.NT_Iteration =>
               Number_Nodes (abs (The_Graph_Node.Next), The_SNR);
               Number_Nodes (The_Graph_Node.Index_1, The_SNR);
            when CRT.NT_Alternative =>
               Number_Nodes (The_Graph_Node.Index_1, The_SNR);
               Number_Nodes (The_Graph_Node.Index_2, The_SNR);
            when others =>
               null;
         end case;
      end Number_Nodes;

      function Get_State
        (The_Graph_Index: in     Integer)
        return Integer
      is
         The_State_Index: Integer;
         The_Graph_Node : CRT.Graph_Node;
      begin
         if  The_Graph_Index = 0  then
            The_State_Index := New_State_Node_Index;
            Automaton_States (The_State_Index).End_Of_Token := The_Symbol_Index;
            return The_State_Index;
         else
            CRT.Get_Graph_Node (The_Graph_Index, The_Graph_Node);
            return The_Graph_Node.Source_Line;
         end if;
      end Get_State;

      procedure Step
        (From_State        : in     Integer;
         The_Graph_Index   : in     Integer)
      is
         The_Graph_Node : CRT.Graph_Node;
         Next           : Integer;
      begin
         if  The_Graph_Index = 0  then
            return;
         end if;

         Sets.Incl          (Stepped, The_Graph_Index);
         CRT.Get_Graph_Node (The_Graph_Index, The_Graph_Node);

         case  The_Graph_Node.Node_Type  is
            when CRT.NT_Character_Class | CRT.NT_Single_Character =>
               New_Transition
                 (From_State,
                  The_Graph_Node,
                  Get_State (abs (The_Graph_Node.Next)));
            when CRT.NT_Alternative =>
               Step (From_State, The_Graph_Node.Index_1);
               Step (From_State, The_Graph_Node.Index_2);
            when CRT.NT_Optional | CRT.NT_Iteration =>
               Next := abs (The_Graph_Node.Next);
               if  not Sets.Is_In (Stepped, Next)  then
                  Step (From_State, Next);
               end if;
               Step (From_State, The_Graph_Node.Index_1);
            when others =>
               null;
         end case;
      end Step;

      procedure Find_Transition
        (The_Graph_Index   : in     Integer;
         Is_Start          : in     Boolean)
      is
         The_Graph_Node: CRT.Graph_Node;
      begin
         if  (The_Graph_Index = 0) or else
             Sets.Is_In (Visited, The_Graph_Index)  then
            return;
         end if;

         Sets.Incl          (Visited, The_Graph_Index);
         CRT.Get_Graph_Node (The_Graph_Index, The_Graph_Node);

         if  Is_Start  then   -- start of group of equally numbered nodes
            CRT.Clear_Mark_List (Stepped);
            Step (The_Graph_Node.Source_Line, The_Graph_Index);
         end if;

         case  The_Graph_Node.Node_Type  is
            when CRT.NT_Character_Class | CRT.NT_Single_Character =>
               Find_Transition (abs (The_Graph_Node.Next), TRUE);
            when CRT.NT_Optional =>
               Find_Transition (abs (The_Graph_Node.Next), TRUE);
               Find_Transition (The_Graph_Node.Index_1, FALSE);
            when CRT.NT_Iteration =>
               Find_Transition (abs (The_Graph_Node.Next), FALSE);
               Find_Transition (The_Graph_Node.Index_1, FALSE);
            when CRT.NT_Alternative =>
               Find_Transition (The_Graph_Node.Index_1, FALSE);
               Find_Transition (The_Graph_Node.Index_2, FALSE);
            when others =>
               null;
         end case;
      end Find_Transition;

      The_Graph_Node : CRT.Graph_Node;

   begin -- Convert_To_States
      if  CRT.Is_Deletable_Graph (The_TDG_Index)  then
         Report_Semantic_Error (20);
      end if;

      for  I in  0..CRT.nNodes  loop
         CRT.Get_Graph_Node (I, The_Graph_Node);
         The_Graph_Node.Source_Line := -1;
         CRT.Put_Graph_Node (I, The_Graph_Node);
      end loop;

      Number_Nodes        (The_TDG_Index, Root_State);
      CRT.Clear_Mark_List (Visited);
      Find_Transition     (The_TDG_Index, TRUE);
   end Convert_To_States;


   --
   -- Returns TRUE (???), if string S can be recognized by the current DFA.
   -- Matched_Symbol_Index point to token as that S can be recognized.
   --
   procedure Match_DFA
     (S                    : in     String;
      The_Symbol_Index     : in     Integer;
      Matched_Symbol_Index : in out Integer)
   -- note: S with quotes
   is
      State             :  Integer; -- State
      To_State          :  Integer; -- State
      The_Action_Node   :  Action_Node_Ptr;
      The_Graph_Node    :  CRT.Graph_Node;
      I                 :  Integer;
      Length            :  Integer;
      Weak_Match        :  Boolean;
   begin
      State       := Root_State;
      I           := 2;
      Length      := FileIO.SLENGTH (S);
      Weak_Match  := FALSE;

      loop                          -- try to match S against existing DFA
         exit when I = Length;
         The_Action_Node := Get_Action (Automaton_States (State), S (I));
         exit when The_Action_Node = null;
         if  The_Action_Node.Node_Type = CRT.NT_Character_Class  then
            Weak_Match := TRUE;
         end if;
         State := The_Action_Node.Target.State;
         I := I + 1;
      end loop;

      if  Weak_Match and then
          ((I /= Length) or else
           (Automaton_States (State).End_Of_Token = CRT.No_Symbol))  then
         State        := Root_State;
         I            := 2;
         Is_Dirty_DFA := TRUE;
      end if;

      while  I < Length  loop       -- make new DFA for S (I .. Length - 1)
         To_State                 := New_State_Node_Index;
         The_Graph_Node.Node_Type := CRT.NT_Single_Character;
         The_Graph_Node.Index_1   := Character'Pos (S (I));
         The_Graph_Node.Index_2   := CRT.normTrans;
         New_Transition (State, The_Graph_Node, To_State);
         State := To_State;
         I     := I + 1;
      end loop;

      Matched_Symbol_Index := Automaton_States (State).End_Of_Token;

      if  Automaton_States (State).End_Of_Token = CRT.No_Symbol  then
         Automaton_States (State).End_Of_Token := The_Symbol_Index;
      end if;
   end Match_DFA;


   --
   -- Generate unique actions from two overlapping actions
   --
   procedure Split_Actions
     (The_Action_Node_A : in     Action_Node_Ptr;
      The_Action_Node_B : in     Action_Node_Ptr)
   is
      The_Action_A   : Action_Node_Ptr := The_Action_Node_A;
      The_Action_B   : Action_Node_Ptr := The_Action_Node_B;
      The_Action_C   : Action_Node_Ptr;
      The_Set_A      : CRT.CRT_Set;
      The_Set_B      : CRT.CRT_Set;
      The_Set_C      : CRT.CRT_Set;

      procedure Combine_Transition_Codes
        (Transition_Code_1 : in     Integer;
         Transition_Code_2 : in     Integer;
         Result            :    out Integer)
      is
      begin
         if  Transition_Code_1 = CRT.contextTrans  then
            Result := Transition_Code_1;
         else
            Result := Transition_Code_2;
         end if;
      end Combine_Transition_Codes;

   begin -- Split_Actions
      Make_Set (The_Action_A, The_Set_A);
      Make_Set (The_Action_B, The_Set_B);

      if  Sets.Equal (The_Set_A, The_Set_B)  then
         Add_Target_List (The_Action_B.Target, The_Action_A.Target);
         Delete_Target_List (The_Action_B.Target);
         Combine_Transition_Codes
           (The_Action_A.Transition_Code,
            The_Action_B.Transition_Code,
            The_Action_A.Transition_Code);
         Detach_Action (The_Action_B, The_Action_A);
         Dispose (The_Action_B);
      elsif  Sets.Includes (The_Set_A, The_Set_B)  then
         The_Set_C := The_Set_A;
         Sets.Differ     (The_Set_C, The_Set_B);
         Add_Target_List (The_Action_A.Target, The_Action_B.Target);
         Combine_Transition_Codes
           (The_Action_A.Transition_Code,
            The_Action_B.Transition_Code,
            The_Action_B.Transition_Code);
         Change_Action (The_Action_A, The_Set_C);
      elsif  Sets.Includes (The_Set_B, The_Set_A)  then
         The_Set_C := The_Set_B;
         Sets.Differ     (The_Set_C, The_Set_A);
         Add_Target_List (The_Action_B.Target, The_Action_A.Target);
         Combine_Transition_Codes
           (The_Action_A.Transition_Code,
            The_Action_B.Transition_Code,
            The_Action_A.Transition_Code);
         Change_Action (The_Action_B, The_Set_C);
      else
         Sets.Intersect (The_Set_A, The_Set_B, The_Set_C);
         Sets.Differ    (The_Set_A, The_Set_C);
         Sets.Differ    (The_Set_B, The_Set_C);
         Change_Action  (The_Action_A, The_Set_A);
         Change_Action  (The_Action_B, The_Set_B);

         The_Action_C        := new Action_Node;
         The_Action_C.Target := null;

         Combine_Transition_Codes
           (The_Action_A.Transition_Code,
            The_Action_B.Transition_Code,
            The_Action_C.Transition_Code);
         Add_Target_List (The_Action_A.Target, The_Action_C.Target);
         Add_Target_List (The_Action_B.Target, The_Action_C.Target);
         Change_Action   (The_Action_C, The_Set_C);
         Add_Action      (The_Action_C, The_Action_A);
      end if;
   end Split_Actions;


   --
   -- Make all actions in this The_State unique
   --
   procedure Make_Unique
     (The_State   : in     Integer;
      Changed     :    out Boolean)
   is
      The_Action_Node_A: Action_Node_Ptr;
      The_Action_Node_B: Action_Node_Ptr;

      function Is_Overlap
        (The_Action_Node_A: Action_Node_Ptr;
         The_Action_Node_B: Action_Node_Ptr)
        return Boolean
      is
         The_Set_A: CRT.CRT_Set;
         The_Set_B: CRT.CRT_Set;
      begin
         if  The_Action_Node_A.Node_Type = CRT.NT_Single_Character  then
            if  The_Action_Node_B.Node_Type = CRT.NT_Single_Character  then
               return The_Action_Node_A.Symbol_Action = The_Action_Node_B.Symbol_Action;
            else
               CRT.Get_Character_Class (The_Action_Node_B.Symbol_Action, The_Set_B);
               return Sets.Is_In (The_Set_B, The_Action_Node_A.Symbol_Action);
            end if;
         else
            CRT.Get_Character_Class (The_Action_Node_A.Symbol_Action, The_Set_A);
            if  The_Action_Node_B.Node_Type = CRT.NT_Single_Character  then
               return Sets.Is_In (The_Set_A, The_Action_Node_B.Symbol_Action);
            else
               CRT.Get_Character_Class (The_Action_Node_B.Symbol_Action, The_Set_B);
               return not Sets.Different (The_Set_A, The_Set_B);
            end if;
         end if;
      end Is_Overlap;

   begin -- Make_Unique
      The_Action_Node_A := Automaton_States (The_State).First_Action;
      Changed           := FALSE;

      while The_Action_Node_A /= null  loop
         The_Action_Node_B := The_Action_Node_A.Next;
         while  The_Action_Node_B /= null  loop
            if  Is_Overlap (The_Action_Node_A, The_Action_Node_B)  then
               Split_Actions (The_Action_Node_A, The_Action_Node_B);
               Changed := TRUE;
               return;
                  -- originally no RETURN.  FST blows up if we leave RETURN out.
                  -- Somewhere there is a field that is not properly Set, but I
                  -- have not chased this down completely Fri  08-20-1993
            end if;
            The_Action_Node_B := The_Action_Node_B.Next;
         end loop;
         The_Action_Node_A := The_Action_Node_A.Next;
      end loop;
   end Make_Unique;


   --
   -- Melt States appearing with a shift of the same symbol
   --
   procedure Melt_States
     (The_State   : in     Integer;
      Correct     : in out Boolean)
   is

      The_Action_Node      : Action_Node_Ptr;
      The_Melted_Node      : Melted_Node_Ptr;
      The_State_Set        : CRT.CRT_Set;
      A_State              : Integer;
      End_Of_Token         : Integer;
      Reached_By_Context   : Boolean;
      Changed              : Boolean;
      Known_Melted         : Boolean;

      procedure Add_Melted_Set
        (The_State      : in     Integer;
         The_State_Set  : in out CRT.CRT_Set)
      is
         The_Melted_Node: Melted_Node_Ptr;
      begin
         The_Melted_Node := List_Of_Melted_States;
         while  (The_Melted_Node /= null) and then
                (The_Melted_Node.New_State /= The_State)  loop
            The_Melted_Node := The_Melted_Node.Next;
         end loop;
         if  The_Melted_Node = null  then    -- compiler error
            CRT.Restriction (-1, 0);
         end if;
         Sets.Unite (The_State_Set, The_Melted_Node.Old_States_Set);
      end Add_Melted_Set;

      procedure Get_State_Set
        (The_Target_Node      : in     Target_Node_Ptr;
         The_State_Set        : in out CRT.CRT_Set;
         End_Of_Token         : in out Integer;
         Reached_By_Context   : in out Boolean)
      is
      --
      --   Modified back to match Oberon version Fri  08-20-1993
      --   This seemed to cause problems with some larger automata
      --
      --   new bug fix Wed  11-24-1993  from ETHZ incorporated
      --
         The_Target     : Target_Node_Ptr := The_Target_Node;
         The_Last_State : Integer;
      begin
         Sets.Clear (The_State_Set);
         End_Of_Token             := CRT.No_Symbol;
         Reached_By_Context := FALSE;
         The_Last_State     := Last_Allocated_State;  -- Fri  08-20-1993

         while  The_Target /= null  loop
            if  The_Target.State <= Last_Non_Melted_State  then
               Sets.Incl (The_State_Set, The_Target.State);
            else
               Add_Melted_Set (The_Target.State, The_State_Set);
            end if;
            if  Automaton_States (The_Target.State).End_Of_Token /= CRT.No_Symbol  then
               if  (End_Of_Token = CRT.No_Symbol) or else
                   (End_Of_Token = Automaton_States (The_Target.State).End_Of_Token)  then
                  End_Of_Token   := Automaton_States (The_Target.State).End_Of_Token;
                  The_Last_State := The_Target.State;
               else
                  Put_S ("$Tokens ");
                  Put_I (End_Of_Token);
                  Put_S (" and ");
                  Put_I (Automaton_States (The_Target.State).End_Of_Token);
                  Put_S (" cannot be distinguished.$");
                  Correct := FALSE;
               end if;
            end if;
            if  Automaton_States (The_Target.State).Reached_By_Context  then
               Reached_By_Context := TRUE;
      --  removed this test Fri  08-30-02
      --             if Automaton_States (The_Target.State).End_Of_Token /= CRT.No_Symbol  then
      --                Put_S ("$Ambiguous CONTEXT clause.$");
      --                Correct := FALSE;
      --             end if;
            end if;
            The_Target := The_Target.Next;
         end loop;
      end Get_State_Set;

      procedure Fill_With_Actions
        (The_State_Index : in     Integer;
         The_Target_Node : in     Target_Node_Ptr)
      is
         The_Target     : Target_Node_Ptr := The_Target_Node;
         The_Action_1   : Action_Node_Ptr;
         The_Action_2   : Action_Node_Ptr;
      begin
         while  The_Target /= null  loop
            The_Action_1 := Automaton_States (The_Target.State).First_Action;
            while  The_Action_1 /= null  loop
               The_Action_2        := new Action_Node;
               The_Action_2.all    := The_Action_1.all;
               The_Action_2.Target := null;
               Add_Target_List (The_Action_1.Target, The_Action_2.Target);
               Add_Action      (The_Action_2, Automaton_States (The_State_Index).First_Action);
               The_Action_1 := The_Action_1.Next;
            end loop;
            The_Target := The_Target.Next;
         end loop;
      end Fill_With_Actions;

      procedure Check_Known_Melted                    -- originally it is was
        (The_State_Set     : in     CRT.CRT_Set;      -- the function Known_Melted,
         The_Melted_Node   : in out Melted_Node_Ptr;  -- but for Ada language
         Known_Melted      :    out Boolean)          -- it is modified to procedure
      is
      begin
         Known_Melted    := FALSE;
         The_Melted_Node := List_Of_Melted_States;
         while  The_Melted_Node /= null  loop
            if  Sets.Equal (The_State_Set, The_Melted_Node.Old_States_Set) then
               Known_Melted := TRUE;
               exit;
            end if;
            The_Melted_Node := The_Melted_Node.Next;
         end loop;
      end Check_Known_Melted;


   begin -- Melt_States
      The_Action_Node := Automaton_States (The_State).First_Action;
      while  The_Action_Node /= null  loop
         if  The_Action_Node.Target.Next /= null  then
            Get_State_Set
              (The_Action_Node.Target,
               The_State_Set,
               End_Of_Token,
               Reached_By_Context);
            Check_Known_Melted
              (The_State_Set,
               The_Melted_Node,
               Known_Melted);
            if  not Known_Melted  then
               A_State := New_State_Node_Index;
               Automaton_States (A_State).End_Of_Token       := End_Of_Token;
               Automaton_States (A_State).Reached_By_Context := Reached_By_Context;
               Fill_With_Actions (A_State, The_Action_Node.Target);
               loop
                  Make_Unique (A_State, Changed);
                  exit when not Changed;
               end loop;
               The_Melted_Node := New_Melted_Node (The_State_Set, A_State);
            end if;

            Delete_Target_List (The_Action_Node.Target.Next);
            The_Action_Node.Target.Next  := null;
            The_Action_Node.Target.State := The_Melted_Node.New_State;
         end if;
         The_Action_Node := The_Action_Node.Next;
      end loop;
   end Melt_States;


   --
   -- Converts the NDFA into a DFA. "Correct" indicates if an error occurred.
   --
   procedure Convert_NDFA_into_DFA (Correct: in out Boolean)
   is

      The_State   : Integer;
      Changed     : Boolean;

      --
      -- Find States reached by a context transition
      --
      procedure Find_Context_Transition_States
      is
         The_Action_Node: Action_Node_Ptr;
         The_State      : Integer;
      begin
         The_State := Root_State;
         while  The_State <= Last_Allocated_State  loop
            The_Action_Node := Automaton_States (The_State).First_Action;
            while  The_Action_Node /= null  loop
               if  The_Action_Node.Transition_Code = CRT.contextTrans  then
                  Automaton_States (The_Action_Node.Target.State).Reached_By_Context := TRUE;
               end if;
               The_Action_Node := The_Action_Node.Next;
            end loop;
            The_State := The_State + 1;
         end loop;
      end Find_Context_Transition_States;

   begin -- Convert_NDFA_into_DFA
      Cur_Out_File          := CRS.Listing;
      Last_Non_Melted_State := Last_Allocated_State;

      Find_Context_Transition_States;

      The_State := Root_State;
      while  The_State <= Last_Allocated_State  loop
         loop
            Make_Unique (The_State, Changed);
            exit when not Changed;
         end loop;
         The_State := The_State + 1;
      end loop;

      Correct   := TRUE;
      The_State := Root_State;
      while  The_State <= Last_Allocated_State  loop
         Melt_States (The_State, Correct);
         The_State := The_State + 1;
      end loop;

      Delete_Redundant_States;
      Combine_Shifts;

      -- if  CRT.ddt ('A')  then
      --    Print_Automaton_States;
      -- end if;
   end Convert_NDFA_into_DFA;


   --
   -- Generate a procedure to scan comments
   --
   procedure Gen_Comment
     (The_Left_Margin   : in     CARDINAL;
      The_Comment_Node  : in     Comment_Node_Ptr)
   is

      procedure Gen_Body (The_Left_Margin : in     CARDINAL)
      is
      begin
         Put_Blank (The_Left_Margin);
         Put_S ("loop$");
         Put_Blank (The_Left_Margin + 3);
         Put_S ("if  ");
         Put_Character_Condition (The_Comment_Node.Stop_Comment (1));
         Put_S ("  then$");

         if  The_Comment_Node.Stop_Last = 1  then
            Put_Blank (The_Left_Margin + 6); Put_S ("level   := level - 1;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("oldEols := curLine - startLine;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("Next_Ch;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("if  level = 0  then$");
            Put_Blank (The_Left_Margin + 6); Put_S ("   return TRUE;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("end if;$");
         else
            Put_Blank (The_Left_Margin + 6); Put_S ("Next_Ch;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("if ");
            Put_Character_Condition (The_Comment_Node.Stop_Comment (2));
            Put_S ("  then$");
            Put_Blank (The_Left_Margin + 6); Put_S ("   level := level - 1;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("   Next_Ch;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("   if  level = 0  then$");
            Put_Blank (The_Left_Margin + 6); Put_S ("      return TRUE;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("   end if;$");
            Put_Blank (The_Left_Margin + 6); Put_S ("end if;$");
         end if;

         if  The_Comment_Node.Nested  then
            Put_Blank (The_Left_Margin + 3); Put_S ("elsif  ");
            Put_Character_Condition (The_Comment_Node.Start_Comment (1));
            Put_S ("  then$");

            if  The_Comment_Node.Start_Last = 1  then
               Put_Blank (The_Left_Margin + 6); Put_S ("level := level + 1;$");
               Put_Blank (The_Left_Margin + 6); Put_S ("Next_Ch;$");
            else
               Put_Blank (The_Left_Margin + 6); Put_S ("Next_Ch;$");
               Put_Blank (The_Left_Margin + 6); Put_S ("if  ");
               Put_Character_Condition (The_Comment_Node.Start_Comment (2));
               Put_S (" then$");
               Put_Blank (The_Left_Margin + 6); Put_S ("   level := level + 1;$");
               Put_Blank (The_Left_Margin + 6); Put_S ("   Next_Ch;$");
               Put_Blank (The_Left_Margin + 6); Put_S ("end if;$");
            end if;
         end if;

         Put_Blank (The_Left_Margin + 3); Put_S ("elsif  ch = EOF  then$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   return FALSE;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("else$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   Next_Ch;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("end if;$");
         Put_Blank (The_Left_Margin);
         Put_S ("end loop;$");
      end Gen_Body;

   begin -- Gen_Comment
      Put_S ("if  ");
      Put_Character_Condition (The_Comment_Node.Start_Comment (1));
      Put_S ("  then$");

      if  The_Comment_Node.Start_Last = 1  then
         Put_Blank (The_Left_Margin + 3); Put_S ("Next_Ch;$");
         Gen_Body (The_Left_Margin + 3);
      else
         Put_Blank (The_Left_Margin + 3); Put_S ("Next_Ch;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("if  ");
         Put_Character_Condition (The_Comment_Node.Start_Comment (2));
         Put_S ("  then$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   Next_Ch;$");
         Gen_Body (The_Left_Margin + 6);
         Put_Blank (The_Left_Margin + 3); Put_S ("else$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   if  (ch = CR) or else (ch = LF)  then$");
         Put_Blank (The_Left_Margin + 3); Put_S ("      curLine   := curLine - 1;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("      lineStart := oldLineStart;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   end if;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   bp := bp - 1;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   ch := lastCh;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("   return FALSE;$");
         Put_Blank (The_Left_Margin + 3); Put_S ("end if;$");
      end if;

      Put_Blank (The_Left_Margin);
      Put_S ("end if;$");
      Put_Blank (The_Left_Margin);
   end Gen_Comment;


   --
   -- Copy from file <Frame_In> to file <Frame_Out> until <Stop_String>
   --
   -- "Stop_String" must not contain "FileIO.EOL".
   -- "Left_Margin" is in/out-parameter - it has to be Set once by the calling program.
   --
   procedure Copy_Frame_Part
     (Stop_String : in     String;
      Left_Margin : in out CARDINAL;
      Frame_In    : in     FileIO.File;
      Frame_Out   : in     FileIO.File)
   is
      C           : Character;
      I           : Natural;
      Temp_Str    : String (Stop_String'First..Stop_String'Last);
   begin

      loop
         exit when FileIO.End_Of_File (Frame_In);
         FileIO.Read (Frame_In, C);
         if  (C = FileIO.EOL) or else
             (C = FileIO.CR) or else
             (C = FileIO.LF)  then
            Left_Margin := 0;
         else
            Left_Margin := Left_Margin + 1;
         end if;

         if  C = Stop_String (Stop_String'First)  then   -- check if begin of Stop_String occurs
            I := 0;
            while  (Stop_String'First + I < Stop_String'Last) and then
                   (C = Stop_String (Stop_String'First + I)) and then
                   not FileIO.End_Of_File (Frame_In)  loop
               Temp_Str (Temp_Str'First + I) := C;
               I := I + 1;
               FileIO.Read (Frame_In, C);
            end loop;

            if  C = Stop_String (Stop_String'First + I)  then  -- Stop_String found
               Left_Margin := Left_Margin - 1;                 --
               return;                                         -- ==> exit,
            end if;                                            --
                                                               -- else continue
            Temp_Str (Temp_Str'First + I) := C;
            FileIO.Write_Text (Frame_Out, Temp_Str, (I + 1));
            Left_Margin := Left_Margin + I;
         else
            FileIO.Write (Frame_Out, C);
         end if;
      end loop;
   end Copy_Frame_Part;



   --
   -- Generates the import list for the eventually existing named symbol constants.
   --
   procedure Import_Named_Symbol_Constants (putS : in     PutS_Proc)
   is
      The_Graph_Node       : CRT.Graph_Node;
      The_Symbol_Node      : CRT.Symbol_Node;
      --
      -- A NOTE: next local variables and internal routine
      --         from original release
      --         really not needed for Ada version
      --
      -- The_Named_Constant   : CRT.Name_Ptr;
      -- I        : Integer;
      -- len      : Integer;
      -- oldLen   : Integer;
      -- pos      : Integer;
      -- The_Grammar_Name : String (1..32);
      --
      -- procedure Put_Imported_Symbol
      -- is
      -- begin
      --    if  pos + oldLen > Max_Source_Line_Length  then
      --       putS ("$  ");
      --       pos := 2;
      --    end if;
      --    putS (The_Named_Constant.all);
      --    pos := pos + (oldLen + 1);
      --       -- This is not strictly correct, as the increase of 2 should be
      --       -- lower. I omitted it, because to separate it would be too
      --       -- complicated, and no unexpected side effects are likely, since it
      --       -- is only called again outside the loop - after which "pos" is not
      --       -- used again
      --
      -- end Put_Imported_Symbol;

   begin -- Import_Named_Symbol_Constants
      -- Import list of the generated Named Symbol Constants Module
      CRT.Get_Graph_Node (CRT.root, The_Graph_Node);
      CRT.Get_Symbol_Node (The_Graph_Node.Index_1, The_Symbol_Node);
      putS
        ("$$" &
         "with  " & The_Symbol_Node.Symbol_Name.all & "G;   " &
         "use  "  & The_Symbol_Node.Symbol_Name.all & "G;" &
         " -- symbol constants$$");
   end Import_Named_Symbol_Constants;


   --
   -- Generate CASE for the recognition of literals
   --
   procedure Gen_Literals (The_Left_Margin: in     CARDINAL)
   is
      use FileIO;

      Keys              : array (Integer range 0..CRT.maxLiterals) of CRT.Name_Ptr;
      Key_Numbers       : array (Integer range 0..CRT.maxLiterals) of Integer;
      The_Symbol_Node   : CRT.Symbol_Node;
      Ch                : Character;
      I                 : Integer;
      J                 : Integer;
      K                 : Integer;
      
      function To_Ada_Quotes (S : in     String) return String -- ? ? ? ? ?
      is
      begin
         if  (S (S'First) = ''') and then
             (S (S'Last)  = ''') then
            return ('"' & S ((S'First + 1)..(S'Last - 1)) & '"');
         else
            return S;
         end if;
      end To_Ada_Quotes;

   begin
      Keys        := (others => null);
      Key_Numbers := (others => 0); -- not strictly needed, but placed here
                                    -- just to prevent GNAT compilation warning

      -- sort literal list
      --
      -- NOTE: the Symbol_Name_Ptr to smaller Symbol_Name
      --       will placed in the Keys (0)
      I := 0;
      K := 0;
      while  I <= CRT.maxT  loop
         CRT.Get_Symbol_Node (I, The_Symbol_Node);
         if  The_Symbol_Node.Struct = CRT.TK_Literal_Token  then
            J := K - 1;
            while  (J >= 0) and then
                   (The_Symbol_Node.Symbol_Name /= null) and then
                   (Keys (J) /= null) and then
                   (The_Symbol_Node.Symbol_Name.all < Keys (J).all)  loop
               Keys        (J + 1) := Keys        (J);
               Key_Numbers (J + 1) := Key_Numbers (J);
               J                   := J - 1;
            end loop;
            Keys        (J + 1) := The_Symbol_Node.Symbol_Name;
            Key_Numbers (J + 1) := I;
            K                   := K + 1;    -- here the K is the literals counter
            if  K > CRT.maxLiterals  then    -- check restrictions for CRT.maxLiterals
               CRT.Restriction (10, CRT.maxLiterals);
            end if;
         end if;
         I := I + 1;
      end loop;

      -- print CASE statement
      if  K /= 0  then
         Put_S ("case  Current_Ch (bp0)  is$");
         Put_Blank (The_Left_Margin);
         I := 0;
         while  I < K  loop
            Ch := Keys (I)(2);   -- "Keys (I)(1)" = quote character
            if  I /= 0  then
               Put_Ln;
               Put_Blank (The_Left_Margin + 3);
            end if;
            Put_S ("when ");
            Put_C (Ch);
            J := I;
            loop
               if  I = J  then
                  Put_S (" =>$");
                  Put_Blank (The_Left_Margin + 6);
                  Put_S ("if");
               else
                  Put_Blank (The_Left_Margin + 6);
                  Put_S ("elsif");
               end if;
               Put_S (" Equal (" & To_Ada_Quotes (Keys (I).all) & ") then$");
               Put_Blank (The_Left_Margin + 9);
               Put_SE (Key_Numbers (I));
               Put_Ln;
               I := I + 1;
               exit when ((I = K) or else (Keys (I)(2) /= Ch));
            end loop;

            Put_Blank (The_Left_Margin + 6);
            Put_S ("end if;");
         end loop;
         Put_Ln;
         Put_Blank (The_Left_Margin + 3);
         Put_S ("when others =>$");
         Put_Blank (The_Left_Margin + 6);
         Put_S ("null;$");
         Put_Blank (The_Left_Margin);
         Put_S ("end case;");
      end if;
   end Gen_Literals;



   --
   -- Write the source text of a scanner State
   --
   procedure Write_State
     (The_Left_Margin   : in     Integer;
      The_State         : in     Integer;
      The_First_State   : in out Boolean)
   is
      The_Action        : Action_Node_Ptr;
      The_Symbol_Node   : CRT.Symbol_Node;
      The_State_Set     : CRT.CRT_Set;
      End_Of_Token      : Integer;
      The_Indent        : Integer;
      Reached_By_Context: Boolean;
      First             : Boolean;
   begin
      End_Of_Token := Automaton_States (The_State).End_Of_Token;

      if  (End_Of_Token > CRT.maxT) and then    -- pragmas have been moved
          (End_Of_Token /= CRT.No_Symbol)  then
         End_Of_Token := CRT.maxT + CRT.maxSymbols - End_Of_Token;
      end if;

      Indent (The_Left_Margin);

      if  The_First_State  then
         The_First_State := FALSE;
      end if;

      Put_S ("   when ");
      Put_I2 (The_State, 2);
      Put_S (" =>$");
      First := TRUE;
      Reached_By_Context := Automaton_States (The_State).Reached_By_Context;
      The_Action := Automaton_States (The_State).First_Action;

      while  The_Action /= null  loop

         Put_Blank (The_Left_Margin + 6);
         if  First  then
            Put_S ("if  ");
            First := FALSE;
            The_Indent := The_Left_Margin + 3;
         else
            Put_S ("elsif  ");
            The_Indent := The_Left_Margin + 6;
         end if;

         if  The_Action.Node_Type = CRT.NT_Single_Character  then
            Put_Character_Condition (Character'Val (The_Action.Symbol_Action));
         else
            CRT.Get_Character_Class (The_Action.Symbol_Action, The_State_Set);
            Put_Range (The_State_Set, The_Left_Margin + The_Indent);
         end if;

         Put_S ("  then$");

         Put_Blank (The_Left_Margin + 9);
         if  The_Action.Target.State /= The_State  then
            Put_S ("State := ");
            Put_I (The_Action.Target.State);
            Put (';');
         else
            Put_S ("null;");
         end if;

         if  The_Action.Transition_Code = CRT.contextTrans  then
            Put_S (" apx := apx + 1;");
            Reached_By_Context := FALSE;
         elsif Automaton_States (The_State).Reached_By_Context  then
            Put_S (" apx := 0;");
         end if;

         Put_S (" $");
         The_Action := The_Action.Next;
      end loop;

      if  Automaton_States (The_State).First_Action /= null  then
         Put_Blank (The_Left_Margin + 6);
         Put_S ("else$");
      end if;

      if  End_Of_Token = CRT.No_Symbol  then
         Put_Blank (The_Left_Margin + 9); Put_S ("sym := noSYMB;$");
      else  -- final State
         CRT.Get_Symbol_Node (End_Of_Token, The_Symbol_Node);
         if  Reached_By_Context  then  -- cut appendix
            Put_Blank (The_Left_Margin + 9);
            Put_S ("bp      := bp - apx - 1;$");
            Put_Blank (The_Left_Margin + 9);
            Put_S ("nextLen := nextLen - FileIO.ORDL (apx);$");
            Put_Blank (The_Left_Margin + 9);
            Put_S ("Next_Ch;$");
         end if;
         Put_Blank (The_Left_Margin + 9); Put_SE (End_Of_Token); Put_S ("$");
         if  The_Symbol_Node.Struct = CRT.TK_Class_Literal_Token  then
            Put_Blank (The_Left_Margin + 9); Put_S ("Check_Literal;$");
         end if;
      end if;

      Put_Blank (The_Left_Margin + 9); Put_S ("return;$");

      if  Automaton_States (The_State).First_Action /= null  then
         Put_Blank (The_Left_Margin + 6);
         Put_S ("end if;$");
      end if;
   end Write_State;




   --
   -- Emits the source code of the generated scanner
   -- using the frame file "scanner.frm"
   --
   procedure Write_Scanner (Ok: in out Boolean)
   is

      -- The_Listing_Width : constant := 78;

      Start_Tab               : array (Integer range 0..255) of Integer;
      The_Graph_Node          : CRT.Graph_Node;
      The_Symbol_Node         : CRT.Symbol_Node;
      The_Comment_Node        : Comment_Node_Ptr;
      The_Grammar_Name        : CRT.Name_Ptr;
      The_State               : Integer;

      procedure Fill_Start_Tab
      is
         The_Action_Node      : Action_Node_Ptr;
         The_Character_Class  : CRT.CRT_Set;
         The_Target_State     : Integer;
         The_Undefined_State  : Integer;
      begin
         The_Undefined_State := Last_Allocated_State + 2;

         Start_Tab := (0      => Last_Allocated_State + 1,  -- eof
                       others => The_Undefined_State);      -- PDT

         The_Action_Node := Automaton_States (Root_State).First_Action;

         while  The_Action_Node /= null  loop
            The_Target_State := The_Action_Node.Target.State;
            if  The_Action_Node.Node_Type = CRT.NT_Single_Character  then
               Start_Tab (The_Action_Node.Symbol_Action) := The_Target_State;
            else
               CRT.Get_Character_Class (The_Action_Node.Symbol_Action, The_Character_Class);
               for  I in Start_Tab'Range  loop  -- PDT
                  if  Sets.Is_In (The_Character_Class, I)  then
                     Start_Tab (I) := The_Target_State;
                  end if;
               end loop;
            end if;
            The_Action_Node := The_Action_Node.Next;
         end loop;
      end Fill_Start_Tab;

      The_Left_Margin   : CARDINAL;
      The_First_State   : Boolean;

   begin -- Write_Scanner

      if  Is_Dirty_DFA  then
         Convert_NDFA_into_DFA (ok);
      end if;

      Fill_Start_Tab;

      -- NOTE: next commented lines are reflected original version
      --
      -- FileIO.Concat (CRS.directory, "scanner.frm", The_Scanner_Frame);
      -- FileIO.Open   (Frame, The_Scanner_Frame, FALSE);
      --
      FileIO.Open   (The_Frame_File, "scanner.frm", FALSE);
      if  not FileIO.Okay  then
         FileIO.Search_File (The_Frame_File, "CRFRAMES", "scanner.frm", FALSE);
         if  not FileIO.Okay  then
            FileIO.Write_Ln      (FileIO.StdOut);
            FileIO.Write_String  (FileIO.StdOut, '"' & "scanner.frm" & '"' & "not found.");
            FileIO.Write_Ln      (FileIO.StdOut);
            FileIO.Write_String  (FileIO.StdOut, "Aborted.");
            FileIO.Quit_Execution;
         end if;
      end if;

      The_Left_Margin := 0;

      CRT.Get_Graph_Node  (CRT.root, The_Graph_Node);                -- determine
      CRT.Get_Symbol_Node (The_Graph_Node.Index_1, The_Symbol_Node); -- Grammar
      The_Grammar_Name := The_Symbol_Node.Symbol_Name;               -- Name

      -- NOTE: next commented lines are reflected original version
      --
      -- FileIO.Extract (The_Symbol_Node.Symbol_Name.all, 0, 7, The_Grammar_Name.all);
      -- FileIO.Concat  (CRS.directory, The_Grammar_Name.all, The_Grammar_File_Name.all);
      -- FileIO.Concat  (The_Grammar_File_Name.all, "The_State", fn);
      -- FileIO.Concat  (fn, FileIO.BodyExt, fn);

      --
      -- *s.adb (GNAT Ada body file)
      --
      declare  -- opening The_Scanner_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & 's' & FileIO.BodyExt;
      begin
         FileIO.Open (The_Scanner_File, The_File_Name, TRUE);
      end;

      Cur_Out_File := The_Scanner_File;

      if  CRT.ddt ('N') or else CRT.symNames  then
         Import_Named_Symbol_Constants (Put_S'Access);
      end if;

      Copy_Frame_Part ("-->modulename", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Put_S (The_Grammar_Name.all & 'S');

      Copy_Frame_Part ("-->unknownsym", The_Left_Margin, The_Frame_File, Cur_Out_File);

      if  CRT.ddt ('N') or else CRT.symNames  then
         Put_SN (CRT.maxT);
      else
         Put_I (CRT.maxT);
      end if;

      Copy_Frame_Part ("-->comment", The_Left_Margin, The_Frame_File, Cur_Out_File);
      The_Comment_Node := List_Of_Comments;
      while  The_Comment_Node /= null  loop
         Gen_Comment (The_Left_Margin, The_Comment_Node);
         The_Comment_Node := The_Comment_Node.Next;
      end loop;

      Copy_Frame_Part ("-->literals", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Gen_Literals (The_Left_Margin);

      Copy_Frame_Part ("-->GetSy1", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Is_New_Line := FALSE;
      if  not Sets.Is_In (CRT.ignored, Character'Pos (CR))  then
         Indent (The_Left_Margin);
         Put_S ("if  oldEols > 0  then$");
         Put_S ("   bp      := bp - 1;$");
         Put_S ("   oldEols := oldEols - 1;$");
         Put_S ("   ch      := CR;$");
         Put_S ("end if;$");
      end if;
      Indent (The_Left_Margin);
      Put_S  ("while  (ch = ' ')");
      if  not Sets.Empty (CRT.ignored)  then
         Put_S (" or else$");
         Indent (The_Left_Margin + 6);
         Put_Range (CRT.ignored, The_Left_Margin + 6);
      end if;
      Put_S (" loop  Next_Ch;  end loop;");
      if  List_Of_Comments /= null  then
         Put_Ln;
         Put_Blank (The_Left_Margin);
         Put_S ("if  (");
         The_Comment_Node := List_Of_Comments;
         while  The_Comment_Node /= null  loop
            Put_Character_Condition (The_Comment_Node.Start_Comment (1));
            if  The_Comment_Node.Next /= null  then
               Put_S (" or else ");
            end if;
            The_Comment_Node := The_Comment_Node.Next;
         end loop;
         Put_S (") and then Comment  then$");
         Put_Blank (The_Left_Margin); Put_S ("   Get (sym);$");
         Put_Blank (The_Left_Margin); Put_S ("   return;$");
         Put_Blank (The_Left_Margin); Put_S ("end if;");
      end if;

      Copy_Frame_Part ("-->GetSy2", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Is_New_Line     := FALSE;
      The_State       := Root_State + 1;
      The_First_State := TRUE;
      while  The_State <= Last_Allocated_State  loop
         Write_State (The_Left_Margin, The_State, The_First_State);
         The_State := The_State + 1;
      end loop;
      Put_Blank (The_Left_Margin); Put_S ("   when ");
      Put_I2 (Last_Allocated_State + 1, 2); Put_S (" =>$");
      Put_Blank (The_Left_Margin + 6); Put_SE (0); Put_S ("$");
      Put_Blank (The_Left_Margin); Put_S ("      ch := Character'Val (0);$");
      Put_Blank (The_Left_Margin); Put_S ("      bp := bp - 1;$");
      Put_Blank (The_Left_Margin); Put_S ("      return;");

      Copy_Frame_Part ("-->initializations", The_Left_Margin, The_Frame_File, Cur_Out_File);
      if  CRT.ignoreCase  then
         Put_S ("Current_Ch := Up_Char_At'Access;$");
      else
         Put_S ("Current_Ch := Char_At'Access;$");
      end if;
      Put_Blank (The_Left_Margin);

      for  I in Start_Tab'First..(((Start_Tab'Last + 1) / 4) - 1)  loop
         if  I /= 0  then
            Put_Ln;
            Put_Blank (The_Left_Margin);
         end if;
         for  J in 0..3  loop
            Put_S ("Start ("); Put_I2 (4 * I + J, 3); Put_S (") := ");
            Put_I2 (Start_Tab (4 * I + J), 2); Put_S ("; ");
         end loop;
      end loop;

      Copy_Frame_Part ("-->modulename;", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Put_S (The_Grammar_Name.all & "S;$$$$$");
      FileIO.Close (The_Scanner_File);


      --
      -- *s.ads (GNAT Ada specification file)
      --
      declare  -- opening The_Scanner_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & 's' & FileIO.SpecExt;
      begin
         FileIO.Open (The_Scanner_File, The_File_Name, TRUE);
      end;

      Cur_Out_File := The_Scanner_File;

      Copy_Frame_Part ("-->modulename", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Put_S (The_Grammar_Name.all & 'S');

      Copy_Frame_Part ("-->modulename", The_Left_Margin, The_Frame_File, Cur_Out_File);
      Put_S (The_Grammar_Name.all & "S;$$$$$");
      FileIO.Close (The_Scanner_File);

      FileIO.Close (The_Frame_File);

   end Write_Scanner;



begin -- CRA

   Last_Allocated_State  := -1;
   Root_State            := New_State_Node_Index;
   List_Of_Melted_States := null;
   List_Of_Comments      := null;
   Is_New_Line           := TRUE;
   Is_Dirty_DFA          := FALSE;

end CRA;



