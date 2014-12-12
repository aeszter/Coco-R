-----------------------------------------------------------------------------
--                                                                         --
-- CRX   Parser Generation                                                 --
-- ===   =================                                                 --
--                                                                         --
-- Uses the top-down graph and the computed sets of terminal start symbols --
-- from CRT to generate recursive descent parsing procedures.              --
--                                                                         --
-- Errors are reported by error numbers. The corresponding error messages  --
-- are written to <grammar name>.ERR.                                      --
--                                                                         --
-----------------------------------------------------------------------------

with ada.text_io;

with  CRS;
with  CRT;
with  CRA;
with  FileIO;
with  Sets;

package body CRX is

   --
   -- Write Character "C"
   --
   procedure Put (C: in     Character)
   is
   begin
      FileIO.Write (The_Parser_File, C);
   end Put;

   --
   -- Write line mark
   --
   procedure Put_Ln
   is
   begin
      FileIO.Write_Ln (The_Parser_File);
   end Put_Ln;

   --
   -- Write Blank_Count blanks
   --
   procedure Put_Blank (Blank_Count: in     Integer)
   is
   begin
      if  Blank_Count > 0  then
         FileIO.Write_Text (The_Parser_File, " ", Blank_Count);
      end if;
   end Put_Blank;

   --
   -- Indent Width characters
   --
   procedure Indent (Width: in     Integer)
   is
   begin
      if  Is_New_Line  then
         Put_Blank (Width);
      else
         Is_New_Line := TRUE;
      end if;
   end Indent;

   --
   -- Indent_Proc Width characters with additional IndDisp
   --
   procedure Indent_Proc (Width: in     Integer)
   is
   begin
      Indent (Width + IndDisp);
   end Indent_Proc;

   --
   -- Shortcut for Write (The_Parser_File, ..)
   --
   procedure Put_S (S: in     String)
   is
   begin
      for  I  in S'Range  loop
         if  S (I) = '$'  then
            FileIO.Write_Ln (The_Parser_File);
         else
            FileIO.Write (The_Parser_File, S (I));
         end  if;
      end loop;
   end Put_S;

   --
   -- Shortcut for Write_Int (The_Parser_File, I, 1)
   --
   procedure Put_I (I: in     Integer)
   is
   begin
      FileIO.Write_Int (The_Parser_File, I, 1);
   end Put_I;

   --
   -- Shortcut for Write_Int (The_Parser_File, I, 2)
   --
   procedure Put_I2 (I: in     Integer)
   is
   begin
      FileIO.Write_Int (The_Parser_File, I, 2);
   end Put_I2;

   --
   -- Writes Index or named constant of Index-ed symbol
   --
   procedure Put_SI (Index: in     Integer)
   is
      use FileIO;
      The_Symbol_Node: CRT.Symbol_Node;
   begin
      CRT.Get_Symbol_Node (Index, The_Symbol_Node);
      if  (The_Symbol_Node.Named_Constant /= null) and then
          (FileIO.SLENGTH (The_Symbol_Node.Named_Constant.all) > 0)  then
         Put_S (The_Symbol_Node.Named_Constant.all);
      else
         Put_I (Index);
      end if;
   end Put_SI;

   --
   -- Enumerate BITSET
   --
   procedure Put_Set
     (The_Set     : in     BITSET;
      The_Offset  : in     CARDINAL)
   is
      use FileIO;

      The_Max_Line      : constant := 76;

      The_Symbol_Node   : CRT.Symbol_Node;
      The_Length        : CARDINAL;
      The_NCLength      : CARDINAL;
      I                 : CARDINAL;

      Is_First          : Boolean;
      Is_Empty_Set      : Boolean; -- added for Ada code generation
      Is_Full_Set       : Boolean; -- added for Ada code generation


   begin
      The_Length     := 20;
      I              := 0;
      Is_First       := TRUE;
      Is_Empty_Set   := TRUE;
      Is_Full_Set    := TRUE;


      while  (I < Sets.Size) and then
             ((The_Offset + I) <= CRT.maxT)  loop
         if  Sets.Is_In (I, The_Set)  then
            if  Is_First  then
               Is_First := FALSE;
            else
               Put_S (", ");
               The_Length := The_Length + 2;
            end if;
            CRT.Get_Symbol_Node (The_Offset + I, The_Symbol_Node);
            if  The_Symbol_Node.Named_Constant /= null  then
               The_NCLength := FileIO.SLENGTH (The_Symbol_Node.Named_Constant.all);
            else
               The_NCLength := 0;
            end if;
            if  The_NCLength > 0  then
               if  The_Length + The_NCLength > The_Max_Line  then
                  Put_S ("$                    ");
                  The_Length := 20;
               end if;
               Put_S (The_Symbol_Node.Named_Constant.all);
               The_Length := The_Length + The_NCLength;
               if  The_Offset > 0  then
                  Put ('-');
                  Put_I (The_Offset);
                  The_Length := The_Length + 3;
               end if;
            else
               if The_Length + The_NCLength > The_Max_Line then
                  Put_S ("$                    ");
                  The_Length := 20;
               end if;
               Put_I (I);
               The_Length := The_Length + (I / (10 + 1));
            end if;
            Put_S (" => TRUE");
            Is_Empty_Set := FALSE;  -- added for Ada code generation
         else
            Is_Full_Set := FALSE;   -- added for Ada code generation
         end if;
         I := I + 1;
      end loop;
      if  Is_Empty_Set  then
         Put_S ("others => FALSE");
      elsif  (not Is_Full_Set) or else (I < Sets.Size)  then
         Put_S (", others => FALSE");
      end if;
   end Put_Set;

   --
   -- Enumerate long set
   --
   procedure Put_Set1 (The_Set: in     CRT.CRT_Set)
   is
      Is_First : Boolean;
   begin
      Is_First := TRUE;
      for  I in 0..CRT.maxT  loop
         if  Sets.Is_In (The_Set, I)  then
            if  Is_First  then
               Is_First := FALSE;
            else
               Put_S (" | ");
            end if;
            Put_SI (I);
         end if;
      end loop;
   end Put_Set1;

   --
   -- Count alternatives of gp
   --
   function Alternatives (The_Graph_Node_Index: in     Integer) return Integer
   is
      Index          : Integer := The_Graph_Node_Index;
      The_Graph_Node : CRT.Graph_Node;
      Alt_Counter    : Integer;
   begin
      Alt_Counter := 0;
      while  Index > 0  loop
         CRT.Get_Graph_Node (Index, The_Graph_Node);
         Index  := The_Graph_Node.Index_2;
         Alt_Counter := Alt_Counter + 1;
      end loop;
      return Alt_Counter;
   end Alternatives;

   --
   -- Copy from file <The_Frame_File> to file <The_Parser_File> until <stopStr>
   --
   procedure Copy_Frame_Part
     (Stop_String : in     String;
      Left_Margin : in out CARDINAL)
   is
   begin
      CRA.Copy_Frame_Part (Stop_String, Left_Margin, The_Frame_File, The_Parser_File);
   end Copy_Frame_Part;


   type Indent_Proc_Type is access procedure (I : in     Integer);

   --
   -- Copy sequence <The_Position> from input file to file <The_Parser_File>
   --
   procedure Copy_Source_Part
     (The_Position      : in     CRT.Position;
      The_Indent        : in     Integer;
      The_Indent_Proc   : in     Indent_Proc_Type)
   is
      C        : Character;
      Last_C   : Character;
      Column   : Integer;
      Extra    : Integer;
      I        : Integer;
      nChars   : CARDINAL;
      bp       : INT32;
   begin
      if The_Position.Start >= 0 then
         bp     := The_Position.Start;
         nChars := The_Position.Length;
         Column := The_Position.Column - 1;
         C      := ' ';
         Extra  := 0;

         while  (nChars > 0) and then
                ((C = ' ') or else                 -- skip leading white spaces
                 (C = Character'Val (9)))  loop    -- skip leading blanks
            C      := CRS.Char_At (bp);
            bp     := bp + 1;
            nChars := nChars - 1;
            Column := Column + 1;
         end loop;

         The_Indent_Proc (The_Indent);

         loop
            while  (C = FileIO.CR) or else   -- Write blank lines with the correct
                   (C = FileIO.LF)  loop     -- number of leading blanks
               FileIO.Write_Ln (The_Parser_File);
               Last_C := C;
               if  nChars > 0  then
                  C      := CRS.Char_At (bp);
                  bp     := bp + 1;
                  nChars := nChars - 1;
               else
                  return; -- GOTO 999
               end if;
               if  (C = FileIO.LF) and then (Last_C = FileIO.CR)  then
                  Extra := 1; -- must be MS-DOS format
                  if  nChars > 0  then
                     C      := CRS.Char_At (bp);
                     bp     := bp + 1;
                     nChars := nChars - 1;
                  else
                     return; -- EXIT
                  end if;
               end if;
               if  (C /= FileIO.CR) and then
                   (C /= FileIO.LF)  then -- we have something on this line
                  The_Indent_Proc (The_Indent);
                  I := Column - 1 - Extra;
                  while  ((C = ' ') or else (C = Character'Val (9))) and then
                         (I > 0)  loop -- skip at most "Column-1" white space chars at start of line
                     if  nChars > 0  then
                        C      := CRS.Char_At (bp);
                        bp     := bp + 1;
                        nChars := nChars - 1;
                     else
                        return; -- EXIT
                     end if;
                     I := I - 1;
                  end loop;
               end if;
            end loop;
            -- Handle extra blanks
            I := 0;
            while  C = ' '  loop
               if  nChars > 0  then
                  C      := CRS.Char_At (bp);
                  bp     := bp + 1;
                  nChars := nChars - 1;
               else
                  return; -- EXIT
               end if;
               I := I + 1;
            end loop;
            if  (C /= FileIO.CR) and then
                (C /= FileIO.LF) and then
                (C /= FileIO.EOF)  then
               if  I > 0  then
                  Put_Blank (I);
               end if;
               FileIO.Write (The_Parser_File, C);
               if  nChars > 0  then
                  C      := CRS.Char_At (bp);
                  bp     := bp + 1;
                  nChars := nChars - 1;
               else
                  return; -- GOTO 999
               end if;
            end if;
         end loop;
-- 999:
      end if;
   end Copy_Source_Part;

   --
   -- Generate an error message and return its number
   --
   procedure Gen_Error_Message
     (The_Error_Type    : in     Integer;
      The_Error_Symbol  : in     Integer;
      The_Error_Number  : in out Integer)
   is
      The_Symbol_Node   : CRT.Symbol_Node;
      The_Name          : CRT.Name_Ptr;
   begin
      errorNr           := errorNr + 1;
      The_Error_Number  := errorNr;
      CRT.Get_Symbol_Node (The_Error_Symbol, The_Symbol_Node);
      The_Name          := The_Symbol_Node.Symbol_Name;

      for  I in The_Name.all'Range  loop
         if  The_Name (I) = '"'  then
            The_Name (I) := ''';
         end if;
      end loop;

      FileIO.Write_String (The_Err_Msgs_File, "when ");
      FileIO.Write_Int    (The_Err_Msgs_File, The_Error_Number, 3);
      FileIO.Write_String (The_Err_Msgs_File, " => Msg (" & '"');

      case  The_Error_Type  is
         when tErr      =>
            FileIO.Write_String
              (The_Err_Msgs_File,
               The_Name.all & " expected");
         when altErr    =>
            FileIO.Write_String
              (The_Err_Msgs_File,
               "invalid " & The_Name.all);
         when syncErr   =>
            FileIO.Write_String
              (The_Err_Msgs_File,
               "this symbol not expected in " & The_Name.all);
         when others    =>
            null;
      end case;

      FileIO.Write_String (The_Err_Msgs_File, '"' & ");");
      FileIO.Write_Ln     (The_Err_Msgs_File);
   end Gen_Error_Message;

   --
   -- Generate a new condition set, if set not yet exists
   --
   function New_Condition_Set (The_Set: in     CRT.CRT_Set) return Integer
   is
   begin
      for  I in 1..maxSS  loop   --    I := 1; -- skip symSet (0)
         if  Sets.Equal (The_Set, symSet (I))  then
            return I;
         end if;
      end loop;

      maxSS := maxSS + 1;

      if  maxSS > symSetSize  then
         CRT.Restriction (5, symSetSize);
      end if;

      symSet (maxSS) := The_Set;
      return maxSS;
   end New_Condition_Set;

   --
   -- Generate code to check if sym is in set
   --
   procedure Gen_Condition
     (The_New_Set : in     CRT.CRT_Set;
      The_Indent  : in     Integer)
   is
      I  : Integer;
      N  : Integer;

      function Is_Small (The_New_Set: in     CRT.CRT_Set) return Boolean
      is
      begin
         I := Sets.Size;
         while  I <= CRT.maxT  loop
            if  Sets.Is_In (The_New_Set, I)  then
               return FALSE;
            end if;
            I := I + 1;
         end loop;
         return TRUE;
      end Is_Small;

   begin -- Gen_Condition
      N := Sets.Elements (The_New_Set);
      if  N = 0  then
         Put_S (" FALSE"); -- this branch should never be taken
      elsif  N <= maxTerm  then
         I := 0;
         while  I <= CRT.maxT  loop
            if  Sets.Is_In (The_New_Set, I)  then
               Put_S (" (sym = "); Put_SI (I); Put (')');
               N := N - 1;
               if  N > 0  then
                  Put_S (" or else");
                  if  CRT.ddt ('N')  then
                     Put_Ln;
                     Indent_Proc (The_Indent);
                  end if;
               end if;
            end if;
            I := I + 1;
         end loop;
      elsif  Is_Small (The_New_Set)  then
         Put_S (" (sym < ");
         Put_I2 (Sets.Size);
         Put_S (") and then -- prevent range error$");
         Indent_Proc (The_Indent);
         Put_S (" (sym Sets.Is_In (Sets.BITSET'(");
         Put_Set (The_New_Set (0), 0);
         Put_S (")) ");
      else
         Put_S (" Is_In (symSet (");
         Put_I (New_Condition_Set (The_New_Set));
         Put_S ("), sym)");
      end if;
   end Gen_Condition;

   --
   -- Generate code for The_Graph in production curSy
   --
   procedure Gen_Code
     (The_Graph   : in     Integer;
      The_Indent  : in     Integer;
      Checked     : in     CRT.CRT_Set)
   is

      The_Graph_Node_Index_1  : Integer            := The_Graph;
      The_Graph_Node_Index_2  : Integer;
      The_Graph_Node_1        : CRT.Graph_Node;
      The_Graph_Node_2        : CRT.Graph_Node;
      The_Symbol_Node         : CRT.Symbol_Node;
      The_Checked_Set         : CRT.CRT_Set        := Checked;
      The_Set_1               : CRT.CRT_Set;
      The_Set_2               : CRT.CRT_Set;

      The_Error_Number        : Integer;
      The_Alternatives_Amount : Integer;
      The_Additional_Indent   : Integer;

      Is_First_Case           : Boolean;
      Is_Sets_Equal           : Boolean;

   begin -- Gen_Code

      while  The_Graph_Node_Index_1 > 0  loop
         CRT.Get_Graph_Node (The_Graph_Node_Index_1, The_Graph_Node_1);

         case  The_Graph_Node_1.Node_Type  is

            when  CRT.NT_Nonterminal_Symbol  =>
               Indent_Proc         (The_Indent);
               CRT.Get_Symbol_Node (The_Graph_Node_1.Index_1, The_Symbol_Node);
               Put_S               (The_Symbol_Node.Symbol_Name.all);
               if  The_Graph_Node_1.Source_Pos.Start >= 0  then
                  Put_S (" (");
                  Is_New_Line := FALSE;
                  Copy_Source_Part
                    (The_Graph_Node_1.Source_Pos,
                     The_Indent + FileIO.SLENGTH (The_Symbol_Node.Symbol_Name.all) + 1,
                     Indent_Proc'Access);
                  Put (')');
               end if;
               Put_S (";$");

            when  CRT.NT_Terminal_Symbol  =>
               CRT.Get_Symbol_Node (The_Graph_Node_1.Index_1, The_Symbol_Node);
               Indent_Proc         (The_Indent);
               if  Sets.Is_In (The_Checked_Set, The_Graph_Node_1.Index_1)  then
                  Put_S ("Get;$");
               else
                  Put_S ("Expect (");
                  Put_SI (The_Graph_Node_1.Index_1);
                  Put_S (");$");
               end if;

            when  CRT.NT_Weak_Terminal_Symbol  =>
               CRT.Compute_Expected (abs (The_Graph_Node_1.Next), curSy, The_Set_1);
               CRT.Get_Symbol_Set   (0, The_Set_2);
               Sets.Unite           (The_Set_1, The_Set_2);
               CRT.Get_Symbol_Node  (The_Graph_Node_1.Index_1, The_Symbol_Node);
               Indent_Proc          (The_Indent);
               Put_S  ("Expect_Weak (");
               Put_SI (The_Graph_Node_1.Index_1);
               Put_S  (", ");
               Put_I  (New_Condition_Set (The_Set_1));
               Put_S  (");$");

            when  CRT.NT_ANY_Symbol  =>
               Indent_Proc (The_Indent);
               Put_S       ("Get;$");

            when  CRT.NT_Empty_Alternative  =>    -- nothing
               null;

            when  CRT.NT_Semantic_Action  =>
               Copy_Source_Part
                 (The_Graph_Node_1.Source_Pos,
                  The_Indent,
                  Indent_Proc'Access);
               -- Put_S (";$");
               Put_S ("$");

            when  CRT.NT_SYNC_Symbol  =>
               CRT.Get_Symbol_Set (The_Graph_Node_1.Index_1, The_Set_1);
               Gen_Error_Message  (syncErr, curSy, The_Error_Number);
               Indent_Proc (The_Indent); Put_S ("while  not (");
               Gen_Condition (The_Set_1, The_Indent + 9); Put_S (")  loop$");
               Indent_Proc (The_Indent); Put_S ("   Syntax_Error (");
               Put_I (The_Error_Number); Put_S (");$");
               Indent_Proc (The_Indent); Put_S ("   Get;$");
               Indent_Proc (The_Indent); Put_S ("end loop;$");

            when  CRT.NT_Alternative  =>
               CRT.Compute_First_Set (The_Graph_Node_Index_1, The_Set_1);
               Is_Sets_Equal           := Sets.Equal   (The_Set_1, The_Checked_Set);
               The_Alternatives_Amount := Alternatives (The_Graph_Node_Index_1);
               if  The_Alternatives_Amount > maxAlter  then
                  Indent_Proc (The_Indent);
                  Put_S       ("case  sym  is$");
               end if;
               The_Graph_Node_Index_2 := The_Graph_Node_Index_1;
               if  The_Alternatives_Amount > maxAlter  then
                  The_Additional_Indent := 6;
               else
                  The_Additional_Indent := 3;
               end if;
               Is_First_Case := TRUE;
               while  The_Graph_Node_Index_2 /= 0  loop
                  CRT.Get_Graph_Node   (The_Graph_Node_Index_2, The_Graph_Node_2);
                  CRT.Compute_Expected (The_Graph_Node_2.Index_1, curSy, The_Set_1);
                  Indent_Proc          (The_Indent);
                  if  The_Alternatives_Amount > maxAlter  then
                     Put_S ("   when "); Put_Set1 (The_Set_1); Put_S (" =>$");
                  elsif  The_Graph_Node_Index_2 = The_Graph_Node_Index_1  then
                     Put_S         ("if ");
                     Gen_Condition (The_Set_1, The_Indent + 3);
                     Put_S         ("  then$");
                  elsif  (The_Graph_Node_2.Index_2 = 0) and then
                         Is_Sets_Equal  then
                     Put_S ("else$");
                  else
                     Put_S         ("elsif ");
                     Gen_Condition (The_Set_1, The_Indent + 6);
                     Put_S         ("  then$");
                  end if;
                  Sets.Unite (The_Set_1, The_Checked_Set);
                  Gen_Code
                    (The_Graph_Node_2.Index_1,
                     The_Indent + The_Additional_Indent, The_Set_1);
                  Is_New_Line            := TRUE;
                  The_Graph_Node_Index_2 := The_Graph_Node_2.Index_2;
               end loop;
               if  not Is_Sets_Equal  then
                  Gen_Error_Message (altErr, curSy, The_Error_Number);
                  Indent_Proc (The_Indent);
                  if  (The_Alternatives_Amount > maxAlter)  then
                     Put_S       ("   when others =>$");
                     Indent_Proc (The_Indent + 3);
                  else
                     Put_S       ("else$");
                     Indent_Proc (The_Indent);
                  end if;
                  Put_S ("   Syntax_Error (");
                  Put_I (The_Error_Number);
                  Put_S (");$");
                  if  (The_Alternatives_Amount > maxAlter)  then
                     Put_S ("$");
                  end if;
               end if;
               Indent_Proc (The_Indent);
               if  (The_Alternatives_Amount > maxAlter)  then
                  if  Is_Sets_Equal  then
                     Put_S ("   when others =>$");
                     Indent_Proc (The_Indent);
                     Put_S ("      null;$");
                     Indent_Proc (The_Indent);
                  end if;
                  Put_S ("end case;-----$");
               else
                  Put_S ("end if;$");
               end if;

            when  CRT.NT_Iteration  =>
               CRT.Get_Graph_Node (The_Graph_Node_1.Index_1, The_Graph_Node_2);
               Indent_Proc (The_Indent);
               Put_S       ("while ");
               if  The_Graph_Node_2.Node_Type = CRT.NT_Weak_Terminal_Symbol  then
                  CRT.Compute_Expected (abs (The_Graph_Node_2.Next), curSy, The_Set_1);
                  CRT.Compute_Expected (abs (The_Graph_Node_1.Next), curSy, The_Set_2);
                  CRT.Get_Symbol_Node  (The_Graph_Node_2.Index_1, The_Symbol_Node);
                  Put_S  (" Weak_Separator (");
                  Put_SI (The_Graph_Node_2.Index_1);
                  Put_S  (", ");
                  Put_I  (New_Condition_Set (The_Set_1));
                  Put_S  (", ");
                  Put_I  (New_Condition_Set (The_Set_2));
                  Put    (')');
                  Sets.Clear (The_Set_1);
                  -- for inner structure
                  if  The_Graph_Node_2.Next > 0  then
                     The_Graph_Node_Index_2 := The_Graph_Node_2.Next;
                  else
                     The_Graph_Node_Index_2 := 0;
                  end if;
               else
                  The_Graph_Node_Index_2 := The_Graph_Node_1.Index_1;
                  CRT.Compute_First_Set (The_Graph_Node_Index_2, The_Set_1);
                  Gen_Condition         (The_Set_1, The_Indent + 6); -- "+ 5" ???
               end if;
               Put_S       ("  loop$");
               Gen_Code    (The_Graph_Node_Index_2, The_Indent + 3, The_Set_1);
               Indent_Proc (The_Indent);
               Put_S       ("end loop;$");

            when  CRT.NT_Optional  =>
               CRT.Compute_First_Set (The_Graph_Node_1.Index_1, The_Set_1);
               if  Sets.Equal (The_Checked_Set, The_Set_1)  then
                  Gen_Code (The_Graph_Node_1.Index_1, The_Indent, The_Checked_Set);
               else
                  Indent_Proc (The_Indent);
                  Put_S       ("if ");
                  Gen_Condition (The_Set_1, The_Indent + 3);
                  Put_S       ("  then$");
                  Gen_Code    (The_Graph_Node_1.Index_1, The_Indent + 3, The_Set_1);
                  Indent_Proc (The_Indent);
                  Put_S       ("end if;$");
               end if;

            when  others  =>  -- ? ? ? ? ?
               null;

         end case;

         if  (The_Graph_Node_1.Node_Type /= CRT.NT_Empty_Alternative) and then
             (The_Graph_Node_1.Node_Type /= CRT.NT_Semantic_Action) and then
             (The_Graph_Node_1.Node_Type /= CRT.NT_SYNC_Symbol)  then
           Sets.Clear (The_Checked_Set);
         end if;
         The_Graph_Node_Index_1 := The_Graph_Node_1.Next;
      end loop; -- while  The_Graph_Node_Index_1 > 0
   end Gen_Code;

   --
   -- Generate code for pragmas
   --
   procedure Gen_Pragma_Code
     (The_Left_Margin   : in     CARDINAL;
      The_Grammar_Name  : in     String)
   is
       The_Symbol_Node  : CRT.Symbol_Node;
       I                : Integer;
   begin
      I := CRT.maxT + 1;
      if  I > CRT.maxP  then
         Put_S ("null;");
         return;
      end if;
      Put_S     ("case  sym  is$");
      Put_Blank (The_Left_Margin);
      loop
         CRT.Get_Symbol_Node (I, The_Symbol_Node);
         Put_S ("   when "); Put_SI (I); Put_S  (" =>$");
         Is_New_Line := TRUE;
         Copy_Source_Part
           (The_Symbol_Node.Sem_Action_Pos,
            The_Left_Margin + 6,
            Indent'Access);
         exit when I = CRT.maxP;
         I := I + 1;
         Put_Ln;
         Put_Blank (The_Left_Margin);
      end loop;
      Put_Ln;
      Put_Blank (The_Left_Margin); Put_S ("   when others =>$");  -- maybe gen "PRAGMA ERROR"
      Put_Blank (The_Left_Margin); Put_S ("      null;$");        -- will be better here?
      Put_Blank (The_Left_Margin); Put_S ("end case;$");

      Put_Blank (The_Left_Margin);
      Put_S     (The_Grammar_Name & "S.nextPos  := " & The_Grammar_Name & "S.pos;$");
      Put_Blank (The_Left_Margin);
      Put_S     (The_Grammar_Name & "S.nextCol  := " & The_Grammar_Name & "S.col;$");
      Put_Blank (The_Left_Margin);
      Put_S     (The_Grammar_Name & "S.nextLine := " & The_Grammar_Name & "S.line;$");
      Put_Blank (The_Left_Margin);
      Put_S     (The_Grammar_Name & "S.nextLen  := " & The_Grammar_Name & "S.len;");
   end Gen_Pragma_Code;

   --
   -- Generate procedure heading (specification)
   --
   procedure Gen_Proc_Heading (The_Symbol_Node: in     CRT.Symbol_Node)
   is
   begin
      Put_S ("procedure " & The_Symbol_Node.Symbol_Name.all);
      if  The_Symbol_Node.Attr_Pos.Start >= 0  then
         Put_S (" (");
         Is_New_Line := FALSE;
         Copy_Source_Part
           (The_Symbol_Node.Attr_Pos,
            13 + FileIO.SLENGTH (The_Symbol_Node.Symbol_Name.all),
            Indent'Access);
         Put (')');
      end if;
   end Gen_Proc_Heading;

   --
   -- Generate forward references for one-pass compilers
   --
   procedure Gen_Forward_References
   is
      The_Symbol_Node   : CRT.Symbol_Node;
      Is_First          : Boolean := TRUE;
   begin
      -- if  CRT.ddt ('M')  then
      --    Put_S ("-------- FORWARD not needed in multipass compilers$$");
      -- end if;
      for  I in CRT.firstNt..CRT.lastNt  loop   -- for all nonterminals
         CRT.Get_Symbol_Node (I, The_Symbol_Node);
         -- if  CRT.ddt ('M')  then
         --    Put_S ("-- ");
         -- end if;
         if  Is_First  then
            Is_First := FALSE;
         else
            Put_Blank (3);
         end if;
         Gen_Proc_Heading (The_Symbol_Node);
         Put_S (";$");
      end loop;
      Put_Ln;
   end Gen_Forward_References;

   --
   -- Generate code for all productions
   --
   procedure Gen_Productions
   is
      The_Symbol_Node   : CRT.Symbol_Node;
      The_Checked_Set   : CRT.CRT_Set;
   begin
      curSy       := CRT.firstNt;
      Is_New_Line := TRUE;
      while  curSy <= CRT.lastNt  loop    -- for all nonterminals
         CRT.Get_Symbol_Node (curSy, The_Symbol_Node);
         Put_Blank (3); Gen_Proc_Heading (The_Symbol_Node);
         Put_Ln;
         Put_Blank (3); Put_S ("is$");
         if  The_Symbol_Node.Sem_Action_Pos.Start >= 0  then
           Copy_Source_Part
             (The_Symbol_Node.Sem_Action_Pos,
              6,
              Indent_Proc'Access);
           Put_Ln;
         end if;
         Put_Blank (3); Put_S ("begin -- " & The_Symbol_Node.Symbol_Name.all & "$");
         Sets.Clear (The_Checked_Set);
         Gen_Code   (The_Symbol_Node.Struct, 6, The_Checked_Set);
         Put_Blank (3); Put_S ("end " & The_Symbol_Node.Symbol_Name.all & ";$$");
         curSy := curSy + 1;
      end loop;
   end Gen_Productions;

   --
   -- Generate code to initialise all sets
   --
   procedure Gen_Init_Sets
   is
      I: Integer;
      J: Integer;
   begin
      CRT.Get_Symbol_Set (0, symSet (0));
      Is_New_Line := FALSE;
      I := 0;
      while  I <= maxSS  loop
         if  I /= 0  then
            Put_Ln;
         end if;
         J := 0;
         while  J <= (CRT.maxT / Sets.Size)  loop
            if  J /= 0 then
               Put_Ln;
            end if;
            Indent (3); Put_S ("symSet ("); Put_I2 (I); Put_S (")("); Put_I (J);
            Put_S (") := Sets.BITSET'(");
            Put_Set (symSet (I)(J), J * Sets.Size);
            Put_S (");");
            J := J + 1;
         end loop;
         I := I + 1;
      end loop;
   end Gen_Init_Sets;



   --
   -- Generates the target compiler (parser).
   --
   procedure Gen_Compiler
   is
      use FileIO;

      The_Checked_Set   : CRT.CRT_Set;
      The_Graph_Node    : CRT.Graph_Node;
      The_Symbol_Node   : CRT.Symbol_Node;
      The_Grammar_Name  : CRT.Name_Ptr;

      The_Digits        : CARDINAL;
      The_NCLength      : CARDINAL;
      The_Position      : CARDINAL;
      The_Left_Margin   : CARDINAL;
      The_Error_Number  : Integer;

      I                 : Integer;

--       End_File_Pos      : INT32;
--       SS_File_Pos       : INT32;

   begin
      -- NOTE: next commented lines are reflected original version
      --
      -- FileIO.Concat (CRS.directory, "parser.frm", ParserFrame);
      -- FileIO.Open   (The_Frame_File, ParserFrame, FALSE);
      --
      FileIO.Open   (The_Frame_File, "parser.frm", FALSE);
      if  not FileIO.Okay  then
         FileIO.Search_File (The_Frame_File, "CRFRAMES", "parser.frm", FALSE);
         if  not FileIO.Okay  then
           FileIO.Write_Ln       (FileIO.StdOut);
           FileIO.Write_String   (FileIO.StdOut, "'parser.frm' not found.");
           FileIO.Write_String   (FileIO.StdOut, "Aborted.");
           FileIO.Quit_Execution;
         end if;
      end if;

      The_Left_Margin := 0;

      CRT.Get_Graph_Node  (CRT.root, The_Graph_Node);                -- determine
      CRT.Get_Symbol_Node (The_Graph_Node.Index_1, The_Symbol_Node); -- Grammar
      The_Grammar_Name := The_Symbol_Node.Symbol_Name;               -- Name

      --
      -- *.err
      --
      declare  -- opening The_Err_Msgs_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & FileIO.ErrExt;
      begin
         FileIO.Open (The_Err_Msgs_File, The_File_Name, TRUE);
      end;

      for  I in 0..CRT.maxT  loop
         Gen_Error_Message (tErr, I, The_Error_Number);
      end loop;


      if  (CRT.ddt ('N') or else CRT.symNames) -- and then not CRT.ddt ('D')
         then
         --
         -- Write *g.ads (GNAT Ada specification file)
         --
         declare  -- opening The_Parser_File
            The_File_Name : constant String :=
               FileIO.Lo_Case (The_Grammar_Name.all) & 'g' & FileIO.SpecExt;
         begin
            FileIO.Open (The_Parser_File, The_File_Name, TRUE);
         end;

         Put_S ("package " & The_Grammar_Name.all & "G is$$");

         I := 0;
         The_Position := CRA.Max_Source_Line_Length + 1;
         loop
            CRT.Get_Symbol_Node (I, The_Symbol_Node);
            if  The_Symbol_Node.Named_Constant /= null  then
               The_NCLength := FileIO.SLENGTH (The_Symbol_Node.Named_Constant.all);
            else
               The_NCLength := 0;
            end if;
            if  The_NCLength > 0  then
               The_Error_Number := I;
               The_Digits       := 1;
               while  The_Error_Number >= 10  loop
                  The_Digits       := The_Digits + 1;
                  The_Error_Number := The_Error_Number / 10;
               end loop;
               The_NCLength := The_NCLength + (3 + The_Digits + 1);
               -- if  (The_Position + The_NCLength) > CRA.Max_Source_Line_Length  then
                  Put_Ln;
                  The_Position := 0;
               -- end if;
               Put_S ("   " & The_Symbol_Node.Named_Constant.all & " : constant := ");
               Put_I (I); Put (';');
               The_Position := The_Position + (The_NCLength + 2);
            end if;
            I := I + 1;
            exit when  I > CRT.maxP;
         end loop;
         Put_S ("$$end " & The_Grammar_Name.all & "G;$$$$$");
         FileIO.Close (The_Parser_File);
      end if; -- if (CRT.ddt ('N') or else CRT.symNames) -- and then not CRT.ddt ('D')

      --
      -- Write *p.adb (GNAT Ada body file)
      --
      declare  -- opening The_Parser_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & 'p' & FileIO.BodyExt;
      begin
         FileIO.Open (The_Parser_File, The_File_Name, TRUE);
      end;

      if  CRT.ddt ('N') or else CRT.symNames  then
         CRA.Import_Named_Symbol_Constants (Put_S'Access);
      end if;

      Copy_Frame_Part ("-->scanner;", The_Left_Margin);
      Put_S (The_Grammar_Name.all & "S;");

      Copy_Frame_Part ("-->modulename", The_Left_Margin);
      Put_S (The_Grammar_Name.all & 'P');

      Copy_Frame_Part ("-->declarations", The_Left_Margin);
      Copy_Source_Part (CRT.semDeclPos, 0, Put_Blank'Access);

      Copy_Frame_Part ("-->error", The_Left_Margin);
      Put_S (The_Grammar_Name.all & "S.Error (errNo, " &
             The_Grammar_Name.all & "S.line, "         &
             The_Grammar_Name.all & "S.col, "       &
             The_Grammar_Name.all & "S.pos);");

      Copy_Frame_Part ("-->error", The_Left_Margin);
      Put_S (The_Grammar_Name.all & "S.Error (errNo, " &
             The_Grammar_Name.all & "S.nextLine, "     &
             The_Grammar_Name.all & "S.nextCol, "      &
             The_Grammar_Name.all & "S.nextPos);");

      Copy_Frame_Part ("-->scanner", The_Left_Margin);
      Put_S (The_Grammar_Name.all & 'S');

      Copy_Frame_Part ("-->pragmas", The_Left_Margin);
      Gen_Pragma_Code (The_Left_Margin, The_Grammar_Name.all);

      for  I  in  1..13  loop
         Copy_Frame_Part ("-->scanner", The_Left_Margin);
         Put_S (The_Grammar_Name.all & 'S');
      end loop;

      Copy_Frame_Part ("-->productions", The_Left_Margin);
      Gen_Forward_References;
      Gen_Productions;

      Copy_Frame_Part ("-->parseRoot", The_Left_Margin);
      Put_S      (The_Grammar_Name.all & "S.Reset;$");
      Put_Blank  (The_Left_Margin); Put_S ("Get;$");
      Sets.Clear (The_Checked_Set);
      Gen_Code   (CRT.root, The_Left_Margin, The_Checked_Set);

      Copy_Frame_Part ("-->initialization", The_Left_Margin);
      Gen_Init_Sets;

      Copy_Frame_Part ("-->modulename;", The_Left_Margin);
      Put_S (The_Grammar_Name.all & "P;$$$$$");

      FileIO.Close (The_Parser_File);

      --
      -- *p.ads (GNAT Ada specification file)
      --
      declare  -- opening The_Parser_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & 'p' & FileIO.SpecExt;
      begin
         FileIO.Open (The_Parser_File, The_File_Name, TRUE);
      end;

      Copy_Frame_Part ("-->modulename", The_Left_Margin);
      Put_S (The_Grammar_Name.all & 'P');

      -- BEGIN of private specification part
      Copy_Frame_Part ("-->constants", The_Left_Margin);
      Put_S ("maxT : constant := "); Put_I (CRT.maxT); Put (';');
      if  CRT.maxP > CRT.maxT  then
         Put_Ln;
         Put_Blank (The_Left_Margin);
         Put_S ("maxP : constant := "); Put_I (CRT.maxP); Put (';');
      end if;

      Copy_Frame_Part ("-->symSetSize", The_Left_Margin);
      if  maxSS < 0  then
         maxSS := 0;
      end if;
      FileIO.Write_Int (The_Parser_File, maxSS, 3);
      -- END of private specification part

      Copy_Frame_Part ("-->modulename", The_Left_Margin);
      Put_S (The_Grammar_Name.all & "P;$$$$$");

      FileIO.Close (The_Parser_File);


      FileIO.Close (The_Frame_File);
      FileIO.Close (The_Err_Msgs_File);
   end Gen_Compiler;



   --
   -- Writes statistics about compilation to list file.
   --
   procedure Write_Statistics
   is

      procedure Write_Numbers
        (Used     : in    Integer;
         Available: in    Integer)
      is
      begin
         FileIO.Write_Int    (CRS.Listing, Used + 1, 6);
         FileIO.Write_String (CRS.Listing, " (limit ");
         FileIO.Write_Int    (CRS.Listing, Available, 5);
         FileIO.Write        (CRS.Listing, ')');
         FileIO.Write_Ln     (CRS.Listing);
      end Write_Numbers;

   begin
      FileIO.Write_String  (CRS.Listing, "Statistics:");
      FileIO.Write_Ln      (CRS.Listing);
      FileIO.Write_Ln      (CRS.Listing);
      FileIO.Write_String  (CRS.Listing, "  nr of terminals:    ");
      Write_Numbers        (CRT.maxT, CRT.maxTerminals);

      FileIO.Write_String  (CRS.Listing, "  nr of non-terminals:");
      Write_Numbers        (CRT.lastNt-CRT.firstNt, CRT.maxNt);

      FileIO.Write_String  (CRS.Listing, "  nr of pragmas:      ");
      Write_Numbers        (CRT.maxSymbols-CRT.lastNt-2, CRT.maxSymbols-CRT.maxT-1);

      FileIO.Write_String  (CRS.Listing, "  nr of symbolnodes:  ");
      Write_Numbers        (CRT.maxSymbols-CRT.firstNt+CRT.maxT, CRT.maxSymbols);

      FileIO.Write_String  (CRS.Listing, "  nr of graphnodes:   ");
      Write_Numbers        (CRT.nNodes, CRT.maxNodes);

      FileIO.Write_String  (CRS.Listing, "  nr of conditionsets:");
      Write_Numbers        (maxSS, symSetSize);

      FileIO.Write_String  (CRS.Listing, "  nr of charactersets:");
      Write_Numbers        (CRT.maxC, CRT.maxClasses);

      FileIO.Write_Ln      (CRS.Listing);
      FileIO.Write_Ln      (CRS.Listing);
   end Write_Statistics;


begin -- CRX

   errorNr     := -1;
   maxSS       := 0;    -- symSet (0) reserved for allSyncSyms
   Is_New_Line := TRUE;
   IndDisp     := 0;

end CRX;


