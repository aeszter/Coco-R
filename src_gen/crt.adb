------------------------------------------------------------------------
--                                                                    --
--  CRT   Table Handler                                               --
--  ===   =============                                               --
--                                                                    --
-- (1) handles a symbol table for terminals, pragmas and nonterminals --
-- (2) handles a table for character classes (for scanner generation) --
-- (3) handles a top-down graph for productions                       --
-- (4) computes various sets (start symbols, followers, any sets)     --
-- (5) contains procedures for grammar tests                          --
--                                                                    --
------------------------------------------------------------------------

with ada.text_io;

with  CRS;
with  FileIO;
with  Sets;

-- with Ada.Strings.Fixed;

package body CRT is

   --
   -- Implementation restriction. Signal compiler restriction
   -- and abort program
   --
   procedure Restriction
     (n     : in     Integer;
      limit : in     Integer)
   is
   begin
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_String (FileIO.StdOut, "Restriction  ");
      FileIO.Write_Int    (FileIO.StdOut, n, 1);
      FileIO.Write_Ln     (FileIO.StdOut);
      case  n  is
         when  1     =>    FileIO.Write_String (FileIO.StdOut, "Too many graph nodes");
         when  2     =>    FileIO.Write_String (FileIO.StdOut, "Too many symbols");
         when  3     =>    FileIO.Write_String (FileIO.StdOut, "Too many sets");
         when  4     =>    FileIO.Write_String (FileIO.StdOut, "Too many character classes");
         when  5     =>    FileIO.Write_String (FileIO.StdOut, "Too many symbol sets");
         when  6     =>    FileIO.Write_String (FileIO.StdOut, "Too many token names");
         when  7     =>    FileIO.Write_String (FileIO.StdOut, "Too many states");
         when  8     =>    FileIO.Write_String (FileIO.StdOut, "Semantic text buffer overflow");
         when  9     =>    FileIO.Write_String (FileIO.StdOut, "Circular check buffer overflow");
         when  10    =>    FileIO.Write_String (FileIO.StdOut, "Too many literal terminals");
         when  11    =>    FileIO.Write_String (FileIO.StdOut, "Too many non-terminals");
         when  -1    =>    FileIO.Write_String (FileIO.StdOut, "Compiler error");
         when others =>    null;
      end case;
      if  n > 0  then
         FileIO.Write_String (FileIO.StdOut, " (limited to ");
         FileIO.Write_Int    (FileIO.StdOut, limit, 1);
         FileIO.Write        (FileIO.StdOut, ')');
      end if;
   -- maybe we want CRX.WriteStatistics;
      FileIO.Quit_Execution;
   end Restriction;

   --
   -- Move pragmas after terminals
   --
   procedure Move_Pragmas
   is
       I: Integer;
   begin
      if  maxP > firstNt  then
         I := maxSymbols - 1;
         maxP := maxT;
         while  I > lastNt  loop
            maxP := maxP + 1;
            if  maxP >= firstNt  then
               Restriction (2, maxSymbols);
            end if;
            The_Symbol_Table (maxP) := The_Symbol_Table (I);
            I := I - 1;
         end loop;
      end if;
   end Move_Pragmas;

   --
   -- Clears all elements (mark list) of The_Mark_List
   --
   procedure Clear_Mark_List (The_Mark_List : in out Mark_List)
   is
   begin
      for  I in The_Mark_List'Range  loop
         The_Mark_List (I) := Sets.Empty_BitSet;
      end loop;
   end Clear_Mark_List;

   --
   -- Gets graph node with The_Index in The_Graph_Node.
   --
   procedure Get_Graph_Node
     (The_Index      : in     Integer;
      The_Graph_Node :    out Graph_Node)
   is
   begin
      The_Graph_Node := The_Top_Down_Graph (The_Index);
   end Get_Graph_Node;
   pragma Inline (Get_Graph_Node);

   --
   -- Replaces graph node with The_Index by The_Graph_Node.
   --
   procedure Put_Graph_Node
     (The_Index      : in     Integer;
      The_Graph_Node : in     Graph_Node)
   is
   begin
      The_Top_Down_Graph (The_Index) := The_Graph_Node;
   end Put_Graph_Node;
   pragma Inline (Put_Graph_Node);

   --
   -- Collects a user defined token name.
   -- Inserts the pair (The_Name, The_Name_Definition)
   -- in The_Token_Names_Table
   --
   procedure New_Name
     (The_Name            : in     Name_Ptr;
      The_Name_Definition : in     Name_Ptr)
   is
   begin
      if  lastName = maxNames  then
         Restriction (6, maxNames);
      else
         lastName := lastName + 1;
         symNames := TRUE;
--          The_Token_Names_Table (lastName).Symbol_Name := new Name'(The_Name);
--          The_Token_Names_Table (lastName).Definition  := new String'(The_Name_Definition);
         The_Token_Names_Table (lastName).Symbol_Name := The_Name;
         The_Token_Names_Table (lastName).Definition  := The_Name_Definition;
      end if;
   end New_Name;

   --
   -- Generates a new symbol with The_Type and The_Name and returns its
   -- The_Index in The_Symbol_Table
   --
   function New_Sym
     (The_Type       : in     Integer;
      The_Name       : in     Name_Ptr;
      The_Source_Line: in     Integer)
     return Integer
   is
      The_Index: Integer;
   begin
      if  maxT + 1 = firstNt  then
         Restriction (2, maxSymbols);
      else
         case  The_Type  is
            when   NT_Terminal_Symbol =>
               maxT     := maxT + 1;
               The_Index:= maxT;
            when  NT_Pragma_Symbol =>
               maxP     := maxP - 1;
               firstNt  := firstNt - 1;
               lastNt   := lastNt - 1;
               The_Index:= maxP;
            when  NT_Nonterminal_Symbol | NT_Unknown_Symbol =>
               firstNt  := firstNt - 1;
               The_Index:= firstNt;
            when others => null; -- ???
         end case;
         if  maxT + 1 >= firstNt  then
            Restriction (2, maxSymbols);
         end if;
         The_Symbol_Table (The_Index).Node_Type            := The_Type;
--          The_Symbol_Table (The_Index).Symbol_Name          := new Name'(The_Name);
         The_Symbol_Table (The_Index).Symbol_Name          := The_Name;
         The_Symbol_Table (The_Index).Named_Constant       := null;  -- Bug fix - PDT
         The_Symbol_Table (The_Index).Struct               := 0;
         The_Symbol_Table (The_Index).Deletable            := FALSE;
         The_Symbol_Table (The_Index).Attr_Pos.Start       := -1;
         The_Symbol_Table (The_Index).Sem_Action_Pos.Start := -1;
         The_Symbol_Table (The_Index).Source_Line          := The_Source_Line;
      end if;
      return The_Index;
   end New_Sym;

   --
   -- Gets symbol node with The_Index in The_Symbol_Node.
   --
   procedure Get_Symbol_Node
     (The_Index       : in     Integer;
      The_Symbol_Node :    out Symbol_Node)
   is
   begin
      The_Symbol_Node := The_Symbol_Table (The_Index);
   end Get_Symbol_Node;
   pragma Inline (Get_Symbol_Node);

   --
   -- Replaces symbol node with The_Index by The_Symbol_Node.
   --
   procedure Put_Symbol_Node
     (The_Index       : in     Integer;
      The_Symbol_Node : in     Symbol_Node)
   is
   begin
      The_Symbol_Table (The_Index) := The_Symbol_Node;
   end Put_Symbol_Node;
   pragma Inline (Put_Symbol_Node);

   --
   -- Gets symbol The_Index for identifier with name The_Name.
   --
   function Find_Symbol (The_Name : in  Name) return Integer
   is
      The_Index: Integer;
   begin
      The_Index := 0;         -- search in terminal list
      while  (The_Index <= maxT) and then
             (The_Symbol_Table (The_Index).Symbol_Name.all /= The_Name)  loop
         The_Index := The_Index + 1;
      end loop;
      if  The_Index <= maxT  then
         return The_Index;
      end if;
      The_Index := firstNt;   -- search in nonterminal/pragma list
      while  (The_Index < maxSymbols) and then
             (The_Symbol_Table (The_Index).Symbol_Name.all /= The_Name)  loop
         The_Index := The_Index + 1;
      end loop;
      if  The_Index < maxSymbols  then
         return The_Index;
      else
         return No_Symbol;
      end if;
   end Find_Symbol;

   --
   -- Print set The_Set
   --
   procedure Print_Set
     (F        : in     FileIO.File;
      The_Set  : in     Sets.BitArray;
      Indent   : in     Integer)
   is
      maxLineLen  : constant := 80;

      Column            : Integer := Indent;
      SLength           : Integer;
      Empty             : Boolean := TRUE;
      The_Symbol_Node   : Symbol_Node;
   begin
      for  I in  0..maxT  loop
         if  Sets.Is_In (The_Set, I)  then
           Empty := FALSE;
           Get_Symbol_Node (I, The_Symbol_Node);
           SLength := FileIO.SLENGTH (The_Symbol_Node.Symbol_Name.all);
           if  Column + SLength + 2 > maxLineLen  then
             FileIO.Write_Ln (F);
             Column := 1;
             while  Column < Indent  loop
               FileIO.Write (F, ' ');
               Column := Column + 1;
             end loop;
           end if;
           FileIO.Write_String (F, The_Symbol_Node.Symbol_Name.all);
           FileIO.Write_String (F, "  ");
           Column := Column + (SLength + 2);
         end if;
      end loop;

      if  Empty  then
         FileIO.Write_String (F, "-- empty set --");
      end if;
      FileIO.Write_Ln (F);
   end Print_Set;

   --
   -- Stores The_Set in The_Symbol_Sets as a new set and returns its index.
   --
   function New_Set (The_Set : in     CRT_Set) return Integer
   -- any-set computation requires not to search if s is already in set
   is
   begin
      maxSet := maxSet + 1;
      if  maxSet > maxSetNr  then
         Restriction (3, maxSetNr);
      end if;
      The_Symbol_Sets (maxSet) := The_Set;
      return maxSet;
   end New_Set;

   --
   -- Computes start symbols of graph gp.
   --
   procedure Compute_First_Set
     (The_Graph_Index: in     Integer;
      The_First_Set  :    out CRT_Set)
   is
      Visited: Mark_List;

      procedure Compute_First
        (The_Graph_Index: in     Integer;
         The_First_Set  :    out CRT_Set)
      is
         Graph_Index    : Integer      := The_Graph_Index;
         The_Set        : CRT_Set;
         The_Graph_Node : Graph_Node;
         The_Symbol_Node: Symbol_Node;
      begin
         Sets.Clear (The_First_Set);
         while  (Graph_Index /= 0) and then
                not Sets.Is_In (Visited, Graph_Index)  loop
            Get_Graph_Node (Graph_Index, The_Graph_Node);
            Sets.Incl      (Visited, Graph_Index);
            case  The_Graph_Node.Node_Type  is
               when  NT_Nonterminal_Symbol    =>
                  if  The_First_Sets (The_Graph_Node.Index_1 - firstNt).Ready  then
                     Sets.Unite
                       (The_First_Set,
                        The_First_Sets (The_Graph_Node.Index_1 - firstNt).Terminal_Symbols);
                  else
                     Get_Symbol_Node   (The_Graph_Node.Index_1, The_Symbol_Node);
                     Compute_First     (The_Symbol_Node.Struct, The_Set);
                     Sets.Unite        (The_First_Set, The_Set);
                  end if;
               when NT_Terminal_Symbol | NT_Weak_Terminal_Symbol =>
                  Sets.Incl (The_First_Set, The_Graph_Node.Index_1);
               when NT_ANY_Symbol    =>
                  Sets.Unite (The_First_Set, The_Symbol_Sets (The_Graph_Node.Index_1));
               when NT_Alternative | NT_Iteration | NT_Optional   =>
                  Compute_First (The_Graph_Node.Index_1, The_Set);
                  Sets.Unite    (The_First_Set, The_Set);
                  if  The_Graph_Node.Node_Type = NT_Alternative  then
                     Compute_First (The_Graph_Node.Index_2, The_Set);
                     Sets.Unite    (The_First_Set, The_Set);
                  end if;
               when others => -- NT_Empty_Alternative, NT_Semantic_Action, NT_SYNC_Symbol, ind: nothing
                  null;
            end case;
            if  not Is_Deletable_Node (The_Graph_Node)  then
               return ;
            end if;
            Graph_Index := abs (The_Graph_Node.Next);
         end loop;
      end Compute_First;

   begin -- Compute_First_Set
      Clear_Mark_List (Visited);
      Compute_First   (The_Graph_Index, The_First_Set);
      if  ddt ('I')  then
         FileIO.Write_Ln     (FileIO.StdOut);
         FileIO.Write_String (FileIO.StdOut, "ComputeFirstSet: The_Graph_Index = ");
         FileIO.Write_Int    (FileIO.StdOut, The_Graph_Index, 1);
         FileIO.Write_Ln     (FileIO.StdOut);
         Print_Set           (FileIO.StdOut, The_First_Set, 0);
      end if;
--       if  ddt ('I')  then
--          FileIO.Write_Ln     (CRS.Listing);
--          FileIO.Write_String (CRS.Listing, "ComputeFirstSet: The_Graph_Index = ");
--          FileIO.Write_Int    (CRS.Listing, The_Graph_Index, 1);
--          FileIO.Write_Ln     (CRS.Listing);
--          Print_Set           (CRS.Listing, The_First_Set, 0);
--       end if;
   end Compute_First_Set;

   --
   -- Compute The_First_Sets symbols of nonterminals
   --
   procedure Compute_First_Sets
   is
      The_Index      : Integer      := firstNt;
      The_Symbol_Node: Symbol_Node;
   begin
      if  (lastNt - firstNt) > maxNt  then
         Restriction (11, maxNt);
      end if;
      while  The_Index <= lastNt  loop
         The_First_Sets (The_Index - firstNt).Ready := FALSE;
         The_Index := The_Index + 1;
      end loop;
      The_Index := firstNt;
      while  The_Index <= lastNt  loop  -- for all nonterminals
         Get_Symbol_Node (The_Index, The_Symbol_Node);
         Compute_First_Set
           (The_Symbol_Node.Struct,
            The_First_Sets (The_Index - firstNt).Terminal_Symbols);
         The_First_Sets (The_Index - firstNt).Ready := TRUE;
         The_Index := The_Index + 1;
      end loop;
   end Compute_First_Sets;

   --
   -- Computes all symbols expected at location gp in graph of symbol sp.
   --
   procedure Compute_Expected
     (The_Graph_Index   : in     Integer;
      The_Symbol_Index  : in     Integer;
      The_Expected_Set  :    out CRT_Set)
   is
   begin
      Compute_First_Set (The_Graph_Index, The_Expected_Set);
      if  Is_Deletable_Graph (The_Graph_Index)  then
         Sets.Unite
           (The_Expected_Set,
            The_Follow_Sets (The_Symbol_Index - firstNt).Terminal_Symbols);
      end if;
   end Compute_Expected;

   --
   -- Get The_Follow_Sets symbols of nonterminals
   --
   procedure Compute_Follow_Sets
   is
      The_Symbol_Node   : Symbol_Node;
      Cur_Symbol_Index  : Integer;
      Size              : Integer;
      Visited           : Mark_List;

      procedure Compute_Follow (The_Graph_Index: in     Integer)
      is
         Graph_Index    : Integer := The_Graph_Index;
         The_Set        : CRT_Set;
         The_Graph_Node : Graph_Node;
      begin
         while  (Graph_Index > 0)  and then
                not Sets.Is_In (Visited, Graph_Index)  loop
            Get_Graph_Node (Graph_Index, The_Graph_Node);
            Sets.Incl      (Visited, Graph_Index);
            if  The_Graph_Node.Node_Type = NT_Nonterminal_Symbol  then
               Compute_First_Set (abs (The_Graph_Node.Next), The_Set);
               Sets.Unite
                 (The_Follow_Sets (The_Graph_Node.Index_1 - firstNt).Terminal_Symbols,
                  The_Set);
               if  Is_Deletable_Graph (abs (The_Graph_Node.Next))  then
                  Sets.Incl
                    (The_Follow_Sets (The_Graph_Node.Index_1 - firstNt).Non_Terminals,
                     (Cur_Symbol_Index - firstNt));
               end if;
            elsif  (The_Graph_Node.Node_Type = NT_Optional) or else
                   (The_Graph_Node.Node_Type = NT_Iteration)  then
               Compute_Follow (The_Graph_Node.Index_1);
            elsif  The_Graph_Node.Node_Type = NT_Alternative  then
               Compute_Follow (The_Graph_Node.Index_1);
               Compute_Follow (The_Graph_Node.Index_2);
            end if;
            Graph_Index := The_Graph_Node.Next;
         end loop;
      end Compute_Follow;

      procedure Complete (The_Index: in     Integer)
      is
      begin
         if  Sets.Is_In (Visited, The_Index)  then
            return;
         end if;
         Sets.Incl (Visited, The_Index);
         for  I in 0..(lastNt - firstNt)  loop  -- for all nonterminals
            if  Sets.Is_In (The_Follow_Sets (The_Index).Non_Terminals, I)  then
               Complete (I);
               Sets.Unite
                 (The_Follow_Sets (The_Index).Terminal_Symbols,
                  The_Follow_Sets (I).Terminal_Symbols);
               if  The_Index = Cur_Symbol_Index  then
                  Sets.Excl (The_Follow_Sets (The_Index).Non_Terminals, I);
               end if;
            end if;
         end loop;
      end Complete;

   begin -- Compute_Follow_Sets
      Size             := (lastNt - firstNt + 2) / Sets.Size;
      Cur_Symbol_Index := firstNt;
      while  Cur_Symbol_Index <= lastNt  loop
         Sets.Clear (The_Follow_Sets (Cur_Symbol_Index - firstNt).Terminal_Symbols);
         for  I in 0..Size  loop
            The_Follow_Sets (Cur_Symbol_Index - firstNt).Non_Terminals (I) := Sets.Empty_BitSet;
         end loop;
         Cur_Symbol_Index := Cur_Symbol_Index + 1;
      end loop;

      Clear_Mark_List (Visited);
      Cur_Symbol_Index := firstNt;  -- get direct successors of nonterminals
      while  Cur_Symbol_Index <= lastNt  loop
         Get_Symbol_Node   (Cur_Symbol_Index, The_Symbol_Node);
         Compute_Follow    (The_Symbol_Node.Struct);
         Cur_Symbol_Index := Cur_Symbol_Index + 1;
      end loop;

      Cur_Symbol_Index := 0;        -- add indirect successors to The_Follow_Sets.Terminal_Symbols
      while  Cur_Symbol_Index <= (lastNt - firstNt)  loop
         Clear_Mark_List (Visited);
         Complete        (Cur_Symbol_Index);
         Cur_Symbol_Index := Cur_Symbol_Index + 1;
      end loop;
   end Compute_Follow_Sets;

   --
   -- Compute all any-sets
   --
   procedure Compute_ANY_Sets
   is

      The_Symbol_Node   : Symbol_Node;

      procedure Find_ANY_Set
        (The_Graph_Index   : in     Integer;
         The_Symbol_Index  : in     Integer)
      is
         Graph_Index    : Integer := The_Graph_Index;
         The_Graph_Node : Graph_Node;
         The_Graph_Node2: Graph_Node;
         A_Graph_Node   : Graph_Node;  -- used & modified by Is_Leading_ANY !!!
         The_Set_1      : CRT_Set;
         The_Set_2      : CRT_Set;
         Index          : Integer;

         function Is_Leading_ANY
           (The_Graph_Index: in     Integer)
           return Boolean
         is
            The_Graph_Node: Graph_Node;
         begin
            if  The_Graph_Index <= 0  then
               return FALSE;
            end if;
            Get_Graph_Node (The_Graph_Index, The_Graph_Node);
            if  (The_Graph_Node.Node_Type = NT_ANY_Symbol)  then
               A_Graph_Node := The_Graph_Node;
               return TRUE;
            else
               return
                  ((The_Graph_Node.Node_Type = NT_Alternative) and then
                   (Is_Leading_ANY (The_Graph_Node.Index_1) or else
                    Is_Leading_ANY (The_Graph_Node.Index_2)))
                  or else
                  (((The_Graph_Node.Node_Type = NT_Optional) or else
                    (The_Graph_Node.Node_Type = NT_Iteration)) and then
                   Is_Leading_ANY (The_Graph_Node.Index_1))
                  or else
                  (Is_Deletable_Node (The_Graph_Node) and then
                   Is_Leading_ANY (The_Graph_Node.Next));
            end if;
         end Is_Leading_ANY;

      begin -- Find_ANY_Set
         while  Graph_Index > 0  loop
            Get_Graph_Node (Graph_Index, The_Graph_Node);
            if  (The_Graph_Node.Node_Type = NT_Optional) or else
                (The_Graph_Node.Node_Type = NT_Iteration)  then
               Find_ANY_Set (The_Graph_Node.Index_1, The_Symbol_Index);
               if  Is_Leading_ANY (The_Graph_Node.Index_1)  then
                  Compute_Expected
                    (abs (The_Graph_Node.Next),
                     The_Symbol_Index,
                     The_Set_1);
                  Sets.Differ
                    (The_Symbol_Sets (A_Graph_Node.Index_1),
                     The_Set_1);
               end if;
            elsif  The_Graph_Node.Node_Type = NT_Alternative  then
               Index := Graph_Index;
               Sets.Clear (The_Set_1);
               while  Index /= 0  loop
                  Get_Graph_Node (Index, The_Graph_Node2);
                  Find_ANY_Set (The_Graph_Node2.Index_1, The_Symbol_Index);
                  if  Is_Leading_ANY (The_Graph_Node2.Index_1)  then
                     Compute_Expected  (The_Graph_Node2.Index_2, The_Symbol_Index, The_Set_2);
                     Sets.Unite        (The_Set_2, The_Set_1);
                     Sets.Differ       (The_Symbol_Sets (A_Graph_Node.Index_1), The_Set_2);
                  else
                     Compute_First_Set (The_Graph_Node2.Index_1, The_Set_2);
                     Sets.Unite        (The_Set_1, The_Set_2);
                  end if;
                  Index := The_Graph_Node2.Index_2;
               end loop;
            end if;
           Graph_Index := The_Graph_Node.Next;
         end loop;
      end Find_ANY_Set;

   begin -- Compute_ANY_Sets
      for  Symbol_Index in firstNt..lastNt  loop   -- for all nonterminals
         Get_Symbol_Node   (Symbol_Index, The_Symbol_Node);
         Find_ANY_Set      (The_Symbol_Node.Struct, Symbol_Index);
      end loop;
   end Compute_ANY_Sets;

   --
   -- Compute The_Follow_Sets symbols of sync-nodes
   --
   procedure Compute_SYNC_Sets
   is

      The_Symbol_Node   : Symbol_Node;
      Visited           : Mark_List;

      procedure Compute_SYNC
        (The_Graph_Index   : Integer;
         The_Symbol_Index  : Integer)
      is
         Graph_Index    : Integer   := The_Graph_Index;
         The_Set        : CRT_Set;
         The_Graph_Node : Graph_Node;
      begin
         while  (Graph_Index > 0) and then not Sets.Is_In (Visited, Graph_Index)  loop
            Get_Graph_Node (Graph_Index, The_Graph_Node);
            Sets.Incl      (Visited, Graph_Index);
            if  The_Graph_Node.Node_Type = NT_SYNC_Symbol  then
               Compute_Expected
                 (abs (The_Graph_Node.Next),
                  The_Symbol_Index,
                  The_Set);
               Sets.Incl  (The_Set, EOF_Symbol);
               Sets.Unite (The_Symbol_Sets (0), The_Set);
               The_Graph_Node.Index_1 := New_Set (The_Set);
               Put_Graph_Node (Graph_Index, The_Graph_Node);
            elsif  The_Graph_Node.Node_Type = NT_Alternative  then
               Compute_SYNC (The_Graph_Node.Index_1, The_Symbol_Index);
               Compute_SYNC (The_Graph_Node.Index_2, The_Symbol_Index);
            elsif  (The_Graph_Node.Node_Type = NT_Optional) or else
                   (The_Graph_Node.Node_Type = NT_Iteration)  then
               Compute_SYNC (The_Graph_Node.Index_1, The_Symbol_Index);
            end if;
            Graph_Index := The_Graph_Node.Next;
         end loop;
      end Compute_SYNC;

   begin -- Compute_SYNC_Sets
      Clear_Mark_List (Visited);
      for  Symbol_Index in firstNt..lastNt  loop
         Get_Symbol_Node   (Symbol_Index, The_Symbol_Node);
         Compute_SYNC      (The_Symbol_Node.Struct, Symbol_Index);
      end loop;
   end Compute_SYNC_Sets;

   --
   -- Marks deletable nonterminals and prints them.
   --
   procedure Compute_Deletable_Symbols
   is
      Changed           : Boolean;
      None              : Boolean;
      The_Symbol_Node   : Symbol_Node;
   begin
      loop
         Changed := FALSE;
         for  I in firstNt..lastNt  loop  -- for all nonterminals
            Get_Symbol_Node (I, The_Symbol_Node);
            if  not The_Symbol_Node.Deletable and then
                (The_Symbol_Node.Struct /= 0) and then
                Is_Deletable_Graph (The_Symbol_Node.Struct)  then
               The_Symbol_Node.Deletable := TRUE;
               Put_Symbol_Node (I, The_Symbol_Node);
               Changed := TRUE;
            end if;
         end loop;
         exit when not Changed;
      end loop;

      FileIO.Write_String (CRS.Listing, "Deletable symbols:");

      None := TRUE;
      for  I in firstNt..lastNt  loop
         Get_Symbol_Node (I, The_Symbol_Node);
         if  The_Symbol_Node.Deletable  then
            None := FALSE;
            FileIO.Write_Ln     (CRS.Listing);
            FileIO.Write_String (CRS.Listing, "     ");
            FileIO.Write_String (CRS.Listing, The_Symbol_Node.Symbol_Name.all);
         end if;
      end loop;

      if  None  then
         FileIO.Write_String (CRS.Listing, "        -- None --");
      end if;
      FileIO.Write_Ln (CRS.Listing);
   end Compute_Deletable_Symbols;

   --
   -- Collects The_First_Sets, The_Follow_Sets, ANY-sets, and SYNC-sets.
   --
   procedure Compute_Symbol_Sets
   is
      The_Symbol_Node : Symbol_Node;
   begin
      Move_Pragmas;
      Compute_Deletable_Symbols;
      Compute_First_Sets;
      Compute_Follow_Sets;
      Compute_ANY_Sets;
      Compute_SYNC_Sets;
      if  ddt ('F')  then
         FileIO.Write_String (CRS.Listing, "List of FIRST and FOLLOW symbols:");
         FileIO.Write_Ln     (CRS.Listing);
         FileIO.Write_Ln     (CRS.Listing);
         for  I in firstNt..lastNt  loop  -- for all nonterminals
            Get_Symbol_Node       (I, The_Symbol_Node);
            FileIO.Write_String   (CRS.Listing, The_Symbol_Node.Symbol_Name.all);
            FileIO.Write_Ln       (CRS.Listing);
            FileIO.Write_String   (CRS.Listing, "FIRST:   ");
            Print_Set             (CRS.Listing, The_First_Sets (I - firstNt).Terminal_Symbols, 10);
            FileIO.Write_String   (CRS.Listing, "FOLLOW:  ");
            Print_Set             (CRS.Listing, The_Follow_Sets (I - firstNt).Terminal_Symbols, 10);
            FileIO.Write_Ln       (CRS.Listing);
         end loop;

         FileIO.Write_Ln     (CRS.Listing);
         FileIO.Write_Ln     (CRS.Listing);
         FileIO.Write_String (CRS.Listing, "List of sets (ANY, SYNC): ");

         if  maxSet < 0  then
            FileIO.Write_String (CRS.Listing, "        -- None --");
         else
            FileIO.Write_Ln     (CRS.Listing);
         end if;

         for  I in 0..maxSet  loop
            FileIO.Write_String (CRS.Listing, "     set[");
            FileIO.Write_Int    (CRS.Listing, I, 2);
            FileIO.Write_String (CRS.Listing, "] = ");
            Print_Set           (CRS.Listing, The_Symbol_Sets (I), 16);
         end loop;
         FileIO.Write_Ln (CRS.Listing);
      end if;
   end Compute_Symbol_Sets;

   --
   -- Get precomputed FIRST set for nonterminal with The_Index
   --
   procedure Get_First_Set
     (The_Index: in     Integer;
      The_Set  :    out CRT_Set)
   is
   begin
      The_Set := The_First_Sets (The_Index - firstNt).Terminal_Symbols;
   end Get_First_Set;
   pragma Inline (Get_First_Set);

   --
   -- Get precomputed FOLLOW set for nonterminal with The_Index
   --
   procedure Get_Follow_Set
     (The_Index: in     Integer;
      The_Set  :    out CRT_Set)
   is
   begin
      The_Set := The_Follow_Sets (The_Index - firstNt).Terminal_Symbols;
   end Get_Follow_Set;
   pragma Inline (Get_Follow_Set);

   --
   -- Gives access to precomputed symbol sets
   --
   procedure Get_Symbol_Set
     (The_Index: in     Integer;
      The_Set  :    out CRT_Set)
   is
   begin
      The_Set := The_Symbol_Sets (The_Index);
   end Get_Symbol_Set;
   pragma Inline (Get_Symbol_Set);

   --
   -- Print symbol table (for tracing).
   --
   procedure Print_Symbol_Table
   is
      I: Integer;

      procedure Write_Boolean (Item: in     Boolean)
      is
      begin
         if  Item  then
            FileIO.Write_String (CRS.Listing, "  TRUE ");
         else
            FileIO.Write_String (CRS.Listing, "  FALSE");
         end if;
      end Write_Boolean;

      procedure Write_Node_Type (The_Node_Type: in     Integer)
      is
      begin
         case  The_Node_Type  is
            when NT_Unknown_Symbol     => FileIO.Write_String (CRS.Listing, " NT_Unknown_Symbol     ");
            when NT_Terminal_Symbol    => FileIO.Write_String (CRS.Listing, " NT_Terminal_Symbol    ");
            when NT_Pragma_Symbol      => FileIO.Write_String (CRS.Listing, " NT_Pragma_Symbol      ");
            when NT_Nonterminal_Symbol => FileIO.Write_String (CRS.Listing, " NT_Nonterminal_Symbol ");
            when others                => FileIO.Write_String (CRS.Listing, " --------------------- ");
         end case;
      end Write_Node_Type;

   begin -- Print_Symbol_Table
      FileIO.Write_String (CRS.Listing, "Symbol_Table:");
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_String (CRS.Listing, "nr    definition                ");
      if  ddt ('N') or else symNames  then
         FileIO.Write_String (CRS.Listing, "constant                           ");
                          
      end if;
      FileIO.Write_String (CRS.Listing, " type                    hasAttrs Struct del   line");
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_Ln     (CRS.Listing);
      I := 0;
      while  I < maxSymbols  loop
         FileIO.Write_Int  (CRS.Listing, I, 3);
         FileIO.Write_Text (CRS.Listing, "   ", 3);
         FileIO.Write_Text (CRS.Listing, The_Symbol_Table (I).Symbol_Name.all, 26);
         if   ddt ('N') or else  symNames  then
            if  I <= maxT  then
               FileIO.Write_Text (CRS.Listing, The_Symbol_Table (I).Named_Constant.all, 35);
            else
               FileIO.Write_Text (CRS.Listing, " ", 35);
            end if;
         end if;
         Write_Node_Type  (The_Symbol_Table (I).Node_Type);
         Write_Boolean    (The_Symbol_Table (I).Attr_Pos.Start >= 0);
         FileIO.Write_Int (CRS.Listing, The_Symbol_Table (I).Struct, 9);
         Write_Boolean    (The_Symbol_Table (I).Deletable);
         FileIO.Write_Int (CRS.Listing, The_Symbol_Table (I).Source_Line, 5);
         FileIO.Write_Ln  (CRS.Listing);
         if  I = maxT  then
            I := firstNt;
         else
            I := I + 1;
         end if;
      end loop;
      FileIO.Write_Ln (CRS.Listing);
      FileIO.Write_Ln (CRS.Listing);
   end Print_Symbol_Table;

   --
   -- Defines a new Character_Class and returns its index
   --
   function New_Character_Class
     (The_Class_Name : in     Name_Ptr;
      The_Set        : in     CRT_Set)
     return Integer
   is
   begin
      maxC := maxC + 1;
      if  maxC > maxClasses  then
         Restriction (4, maxClasses);
      end if;
      if  The_Class_Name (1) = '#'  then
         The_Class_Name (2) := Character'Val (Character'Pos ('A') + dummyName);
         dummyName := dummyName + 1;
      end if;
      The_Char_Classes_Table (maxC).Class_Name := The_Class_Name;
      The_Char_Classes_Table (maxC).Set_Index  := New_Set (The_Set);
      return maxC;
   end New_Character_Class;

   --
   -- Searches for a Character_Class with the given name.
   -- Returns its index or -1
   --
   function Get_Class_By_Name
     (The_Class_Name : in     Name)
     return Integer
   is
      I: Integer;
   begin
      I := maxC;
      while  (I >= 0) and then
             (The_Char_Classes_Table (I).Class_Name.all /= The_Class_Name)  loop
         I := I - 1;
      end loop;
      return I;
   end Get_Class_By_Name;

   --
   -- Searches for a Character_Class with the given set. Returns its index or -1
   --
   function Get_Class_By_Set (The_Set : in  CRT_Set) return Integer
   is
      I: Integer;
   begin
      I := maxC;
      while  (I >= 0) and then
             not Sets.Equal
                  (The_Symbol_Sets (The_Char_Classes_Table (I).Set_Index),
                   The_Set)  loop
         I := I - 1;
      end loop;
      return I;
   end Get_Class_By_Set;

   --
   -- Returns Character_Class n
   --
   procedure Get_Character_Class
     (The_Index: in     Integer;
      The_Set  :    out CRT_Set)
   is
   begin
      Get_Symbol_Set (The_Char_Classes_Table (The_Index).Set_Index, The_Set);
   end Get_Character_Class;
   pragma Inline (Get_Character_Class);

   --
   -- Returns the name of Character_Class n
   --
   procedure Get_Character_Class_Name
     (The_Index   : in     Integer;
      Class_Name  :    out Name_Ptr)
   is
   begin
      Class_Name := The_Char_Classes_Table (The_Index).Class_Name;
   end Get_Character_Class_Name;
   pragma Inline (Get_Character_Class_Name);

   --
   -- Produces a cross reference listing of all symbols.
   --
   procedure XRef
   is
      maxLineLen : constant := 80;

      type  List_Node;
      type  List_Node_Ptr  is access all List_Node;
      type  List_Node is
         record
            Next        : List_Node_Ptr;
            Source_Line : Integer;
         end record;

      type  List_Header  is
         record
            Symbol_Name : Name_Ptr;
            List_Nodes  : List_Node_Ptr;
         end record;

      The_Graph_Node : Graph_Node;
      The_Symbol_Node: Symbol_Node;
      Index          : Integer;

      l     : List_Node_Ptr;
      p     : List_Node_Ptr;
      q     : List_Node_Ptr;

      xList : array (Integer range 0..maxSymbols) of List_Header;

      Column: Integer;

   begin -- XRef
      if  maxT <= 0  then
         return;
      end if;
      Move_Pragmas;
      -- initialize cross reference list
      Index := 0;
      while  Index <= lastNt  loop   -- for all symbols
         Get_Symbol_Node (Index, The_Symbol_Node);
         xList (Index).Symbol_Name := The_Symbol_Node.Symbol_Name;
         xList (Index).List_Nodes  := null;
         if  Index = maxP  then
            Index := firstNt;
         else
            Index := Index + 1;
         end if;
      end loop;

      -- search lines where symbol has been referenced
      Index := 1;
      while  Index <= nNodes  loop  -- for all graph nodes
         Get_Graph_Node (Index, The_Graph_Node);
         if  (The_Graph_Node.Node_Type = NT_Terminal_Symbol) or else
             (The_Graph_Node.Node_Type = NT_Weak_Terminal_Symbol) or else
             (The_Graph_Node.Node_Type = NT_Nonterminal_Symbol)  then
            l := new List_Node;
            l.Next := xList (The_Graph_Node.Index_1).List_Nodes;
            l.Source_Line := The_Graph_Node.Source_Line;
            xList (The_Graph_Node.Index_1).List_Nodes := l;
         end if;
         Index := Index + 1;
      end loop;

      -- search lines where symbol has been defined and insert in order
      Index := 1;
      while  Index <= lastNt  loop  -- for all symbols
         Get_Symbol_Node (Index, The_Symbol_Node);
         p := xList (Index).List_Nodes;
         q := null;
         while  (p /= null) and then (p.Source_Line > The_Symbol_Node.Source_Line)  loop
            q := p;
            p := p.Next;
         end loop;
         l := new List_Node;
         l.Next := p;
         l.Source_Line := -The_Symbol_Node.Source_Line;
         if  q /= null  then
            q.Next := l;
         else
            xList (Index).List_Nodes := l;
         end if;
         if  Index = maxP  then
            Index := firstNt;
         else
            Index := Index + 1;
         end if;
      end loop;

      -- print cross reference listing
      FileIO.Write_String (CRS.Listing, "Cross reference list:");
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_String (CRS.Listing, "Terminals:");
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_String (CRS.Listing, "  0  EOF");
      FileIO.Write_Ln     (CRS.Listing);
      Index := 1;

      while  Index <= lastNt  loop -- for all symbols
         if  Index = maxT  then
            FileIO.Write_Ln     (CRS.Listing);
            FileIO.Write_String (CRS.Listing, "Pragmas:");
            FileIO.Write_Ln     (CRS.Listing);
         else
            FileIO.Write_Int    (CRS.Listing, Index, 3);
            FileIO.Write_String (CRS.Listing, "  ");
            FileIO.Write_Text   (CRS.Listing, xList (Index).Symbol_Name.all, 25);
            l := xList (Index).List_Nodes;
            Column := 35;
            while  l /= null  loop
               if  Column + 5 > maxLineLen  then
                  FileIO.Write_Ln   (CRS.Listing);
                  FileIO.Write_Text (CRS.Listing, "", 30);
                  Column := 35;
               end if;
               if  l.Source_Line = 0  then
                  FileIO.Write_String (CRS.Listing, "undef");
               else
                  FileIO.Write_Int (CRS.Listing, l.Source_Line, 5);
               end if;
               Column := Column + 5;
               l := l.Next;
            end loop;
            FileIO.Write_Ln (CRS.Listing);
         end if;
         if  Index = maxP  then
            FileIO.Write_Ln     (CRS.Listing);
            FileIO.Write_String (CRS.Listing, "Nonterminals:");
            FileIO.Write_Ln     (CRS.Listing);
            Index := firstNt;
         else
            Index := Index + 1;
         end if;
      end loop;

      FileIO.Write_Ln (CRS.Listing);
      FileIO.Write_Ln (CRS.Listing);
   end XRef;

   --
   -- Generates a new graph node with The_Type, Index_1, and Source_Line number
   -- and returns its index.
   --
   function New_Graph_Node
     (The_Type    : in     Integer;
      Index_1     : in     Integer;
      Source_Line : in     Integer)
     return Integer
   is
   begin
      nNodes := nNodes + 1;
      if  nNodes > maxNodes  then
         Restriction (1, maxNodes);
      end if;
      The_Top_Down_Graph (nNodes).Node_Type         := The_Type;
      The_Top_Down_Graph (nNodes).Next              := 0;
      The_Top_Down_Graph (nNodes).Index_1           := Index_1;
      The_Top_Down_Graph (nNodes).Index_2           := 0;
      The_Top_Down_Graph (nNodes).Source_Pos.Start  := -1;
      The_Top_Down_Graph (nNodes).Source_Pos.Length := 0;
      The_Top_Down_Graph (nNodes).Source_Pos.Column := 0;
      The_Top_Down_Graph (nNodes).Source_Line       := Source_Line;
      return nNodes;
   end New_Graph_Node;

   --
   -- Lets right ends of graph gp be 0
   --
   procedure Complete_Graph (The_Graph_Index : in  Integer)
   is
      Graph_Index : Integer;
      Index       : Integer;
   begin
      Graph_Index := The_Graph_Index;
      while  Graph_Index /= 0  loop
         Index                                 := The_Top_Down_Graph (Graph_Index).Next;
         The_Top_Down_Graph (Graph_Index).Next := 0;
         Graph_Index                           := Index;
      end loop;
   end Complete_Graph;

   --
   -- Makes (The_Graph_L2, The_Graph_R2) an NT_Alternative
   -- of the graph (The_Graph_L1, The_Graph_R1).
   -- The resulting graph is identified by (The_Graph_L1, The_Graph_R1).
   --
   procedure Concatenate_Alternative
     (The_Graph_L1   : in out Integer;
      The_Graph_R1   : in out Integer;
      The_Graph_L2   : in     Integer;
      The_Graph_R2   : in     Integer)
   is
      Graph_L2 : Integer;
      Index    : Integer;
   begin
      Graph_L2 := New_Graph_Node (NT_Alternative, The_Graph_L2, 0);

      Index := The_Graph_L1;
      while  The_Top_Down_Graph (Index).Index_2 /= 0  loop
         Index := The_Top_Down_Graph (Index).Index_2;
      end loop;
      The_Top_Down_Graph (Index).Index_2 := Graph_L2;

      Index         := The_Graph_R1;
      while  The_Top_Down_Graph (Index).Next /= 0  loop
         Index := The_Top_Down_Graph (Index).Next;
      end loop;
      The_Top_Down_Graph (Index).Next := The_Graph_R2;
   end Concatenate_Alternative;

   --
   -- Concatenates graph (The_Graph_L1, The_Graph_R1)
   -- with graph (The_Graph_L2, The_Graph_R2) via next-chain.
   -- The resulting graph is identified by (The_Graph_L1, The_Graph_R1).
   --
   procedure Concatenate_Sequential
     (The_Graph_L1   : in out Integer;
      The_Graph_R1   : in out Integer;
      The_Graph_L2   : in     Integer;
      The_Graph_R2   : in     Integer)
   is
      Index_1  : Integer;
      Index_2  : Integer;
   begin
      Index_1                                := The_Top_Down_Graph (The_Graph_R1).Next;
      The_Top_Down_Graph (The_Graph_R1).Next := The_Graph_L2;  -- head node

      while  Index_1 /= 0  loop  -- substructure
         Index_2                           := The_Top_Down_Graph (Index_1).Next;
         The_Top_Down_Graph (Index_1).Next := -The_Graph_L2;
         Index_1                           := Index_2;
      end loop;
      The_Graph_R1 := The_Graph_R2;
   end Concatenate_Sequential;

   --
   -- Generates an alt-node with (The_Graph_L, The_Graph_R)
   -- as its first and only alternative
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure Make_First_Alternative
     (The_Graph_L : in out Integer;
      The_Graph_R : in out Integer)
   is
   begin
      The_Graph_L                           := New_Graph_Node (NT_Alternative, The_Graph_L, 0);
      The_Top_Down_Graph (The_Graph_L).Next := The_Graph_R;
      The_Graph_R                           := The_Graph_L;
   end Make_First_Alternative;

   --
   -- Encloses the graph (The_Graph_L, The_Graph_R)
   -- into an iteration construct.
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure Make_Iteration
     (The_Graph_L : in out Integer;
      The_Graph_R : in out Integer)
   is
      Index_1  : Integer;
      Index_2  : Integer;
   begin
      The_Graph_L := New_Graph_Node (NT_Iteration, The_Graph_L, 0);
      Index_1  := The_Graph_R;
      The_Graph_R := The_Graph_L;
      while  Index_1 /= 0  loop
         Index_2                           := The_Top_Down_Graph (Index_1).Next;
         The_Top_Down_Graph (Index_1).Next := - The_Graph_L;
         Index_1                           := Index_2;
      end loop;
   end Make_Iteration;

   --
   -- Encloses the graph (The_Graph_L, The_Graph_R)
   -- into an option construct.
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure Make_Option
     (The_Graph_L : in out Integer;
      The_Graph_R : in out Integer)
   is
   begin
      The_Graph_L                           := New_Graph_Node (NT_Optional, The_Graph_L, 0);
      The_Top_Down_Graph (The_Graph_L).Next := The_Graph_R;
      The_Graph_R                           := The_Graph_L;
   end Make_Option;

   --
   -- Generates linear graph from characters in s
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure String_To_Graph
     (S           : in     String;
      The_Graph_L : in out Integer;
      The_Graph_R : in out Integer)
   is
   begin
      The_Graph_R  := 0;
      for  I in (S'First + 1)..(S'Last - 1)  loop  -- strip quotes
         The_Top_Down_Graph (The_Graph_R).Next :=
               New_Graph_Node (NT_Single_Character, Character'Pos (S (I)), 0);
         The_Graph_R := The_Top_Down_Graph (The_Graph_R).Next;
      end loop;
      The_Graph_L                 := The_Top_Down_Graph (0).Next;
      The_Top_Down_Graph (0).Next := 0;
   end String_To_Graph;

   --
   -- TRUE, if (sub) graph with root in The_Graph_Index is deletable.
   --
   function Is_Deletable_Graph
     (The_Graph_Index : in  Integer)
     return Boolean
   is
      The_Graph_Node: Graph_Node;
   begin
      if  The_Graph_Index = 0  then  -- end of graph found
         return TRUE;
      end if;
      Get_Graph_Node (The_Graph_Index, The_Graph_Node);
      return Is_Deletable_Node  (The_Graph_Node) and then
             Is_Deletable_Graph (abs (The_Graph_Node.Next));
   end Is_Deletable_Graph;

   --
   -- TRUE, if graph node The_Graph_Node is deletable,
   -- i.e. can be derived into the empty string.
   --
   function Is_Deletable_Node
     (The_Graph_Node : in  Graph_Node)
     return Boolean
   is

      The_Symbol_Node: Symbol_Node;

      function Is_Deletable_Alternatives
        (The_Graph_Node_Index: Integer)
         return Boolean
      is
         The_Graph_Node: Graph_Node;
      begin
         if  The_Graph_Node_Index <= 0  then -- end of graph found
            return TRUE;
         end if;
         Get_Graph_Node (The_Graph_Node_Index, The_Graph_Node);
         return Is_Deletable_Node (The_Graph_Node) and then
                Is_Deletable_Alternatives (The_Graph_Node.Next);
      end Is_Deletable_Alternatives;

   begin
      if  The_Graph_Node.Node_Type = NT_Nonterminal_Symbol  then
         Get_Symbol_Node (The_Graph_Node.Index_1, The_Symbol_Node);
         return The_Symbol_Node.Deletable;
      elsif  The_Graph_Node.Node_Type = NT_Alternative  then
         return Is_Deletable_Alternatives (The_Graph_Node.Index_1) or else
                ((The_Graph_Node.Index_2 /= 0) and then  -- maybe this test can be deleted ???
                 Is_Deletable_Alternatives (The_Graph_Node.Index_2));
      else
         return (The_Graph_Node.Node_Type = NT_Empty_Alternative) or else
                (The_Graph_Node.Node_Type = NT_Iteration)         or else
                (The_Graph_Node.Node_Type = NT_Optional)          or else
                (The_Graph_Node.Node_Type = NT_Semantic_Action)   or else
                (The_Graph_Node.Node_Type = NT_SYNC_Symbol);
      end if;
   end Is_Deletable_Node;

   --
   -- Prints the graph (for tracing).
   --
   procedure Print_Graph
   is

      procedure Write_Node_Type (The_Node_Type: Integer)
      is
      begin
         case  The_Node_Type  is
            when NT_Nonterminal_Symbol    => FileIO.Write_String (CRS.Listing, "Nonterminal_Symbol  ");
            when NT_Terminal_Symbol       => FileIO.Write_String (CRS.Listing, "Terminal_Symbol     ");
            when NT_Weak_Terminal_Symbol  => FileIO.Write_String (CRS.Listing, "Weak_Terminal_Symbol");
            when NT_ANY_Symbol            => FileIO.Write_String (CRS.Listing, "ANY_Symbol          ");
            when NT_Empty_Alternative     => FileIO.Write_String (CRS.Listing, "Empty_Alternative   ");
            when NT_Semantic_Action       => FileIO.Write_String (CRS.Listing, "Semantic_Action     ");
            when NT_SYNC_Symbol           => FileIO.Write_String (CRS.Listing, "SYNC_Symbol         ");
            when NT_Alternative           => FileIO.Write_String (CRS.Listing, "Alternative         ");
            when NT_Iteration             => FileIO.Write_String (CRS.Listing, "Iteration           ");
            when NT_Optional              => FileIO.Write_String (CRS.Listing, "Optional            ");
            when others                   => FileIO.Write_String (CRS.Listing, "- - - - -           ");
         end case;
      end Write_Node_Type;

   begin -- Print_Graph
      FileIO.Write_String (CRS.Listing, "Graph_List:");
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_Ln     (CRS.Listing);
      FileIO.Write_String
        (CRS.Listing,
         " nr   type                   next  Index_1  Index_2   line");
      -- useful for debugging - PDT
      FileIO.Write_String (CRS.Listing, " posbeg poslen poscol");

      FileIO.Write_Ln (CRS.Listing);
      FileIO.Write_Ln (CRS.Listing);

      for  I in 0..nNodes  loop
         FileIO.Write_Int    (CRS.Listing, I, 3);
         FileIO.Write_String (CRS.Listing, "   ");
         Write_Node_Type (The_Top_Down_Graph (I).Node_Type);
         FileIO.Write_Int    (CRS.Listing, The_Top_Down_Graph (I).Next, 7);
         FileIO.Write_Int    (CRS.Listing, The_Top_Down_Graph (I).Index_1, 9);
         FileIO.Write_Int    (CRS.Listing, The_Top_Down_Graph (I).Index_2, 9);
         FileIO.Write_Int    (CRS.Listing, The_Top_Down_Graph (I).Source_Line, 7);
         -- useful for debugging - PDT
         FileIO.Write_Int    (CRS.Listing, FileIO.INTL (The_Top_Down_Graph (I).Source_Pos.Start), 7);
         FileIO.Write_Card   (CRS.Listing, The_Top_Down_Graph (I).Source_Pos.Length, 7);
         FileIO.Write_Int    (CRS.Listing, The_Top_Down_Graph (I).Source_Pos.Column, 7);
         --
         FileIO.Write_Ln     (CRS.Listing);
      end loop;

      FileIO.Write_Ln (CRS.Listing);
      FileIO.Write_Ln (CRS.Listing);
   end Print_Graph;



   -------------------------------------------------------------------
   --                                                               --
   --                Procedures for grammar tests                   --
   --                                                               --
   -------------------------------------------------------------------

   --
   -- Finds and prints the circular part of the grammar.
   -- Ok = TRUE means no circular part.
   --
   procedure Find_Circular_Productions (Ok : in out Boolean)
   is
      type  List_Entry   is
         record
            Left     : Integer;
            Right    : Integer;
            Deleted  : Boolean;
         end record;

      Changed        : Boolean;
      On_Left_Side   : Boolean;
      On_Right_Side  : Boolean;

      List_Length    : Integer;

      The_List          : array (Integer range 0..maxList) of List_Entry;
      Singles           : Mark_List;
      The_Symbol_Node   : Symbol_Node;

      procedure Get_Singles
        (The_Graph_Index   : in     Integer;
         The_Singles       : in out Mark_List)
      is
         The_Graph_Node: Graph_Node;
      begin
         if  The_Graph_Index <= 0  then -- end of graph found
            return;
         end if;

         Get_Graph_Node (The_Graph_Index, The_Graph_Node);

         if  The_Graph_Node.Node_Type = NT_Nonterminal_Symbol  then
            if  Is_Deletable_Graph (abs (The_Graph_Node.Next))  then
               Sets.Incl (The_Singles, The_Graph_Node.Index_1);
            end if;
         elsif  (The_Graph_Node.Node_Type = NT_Alternative) or else
                (The_Graph_Node.Node_Type = NT_Iteration) or else
                (The_Graph_Node.Node_Type = NT_Optional)  then
            if  Is_Deletable_Graph (abs (The_Graph_Node.Next))  then
               Get_Singles (The_Graph_Node.Index_1, The_Singles);
               if  The_Graph_Node.Node_Type = NT_Alternative  then
                  Get_Singles (The_Graph_Node.Index_2, The_Singles);
               end if;
            end if;
         end if;

         if  Is_Deletable_Node (The_Graph_Node)  then
            Get_Singles (The_Graph_Node.Next, The_Singles);
         end if;
      end Get_Singles;

   begin -- Find_Circular_Productions

      List_Length := 0;

      for  I in firstNt..lastNt  loop     -- for all nonterminals I
         Clear_Mark_List (Singles);
         Get_Symbol_Node (I, The_Symbol_Node);
         Get_Singles     (The_Symbol_Node.Struct, Singles); -- get NT_Nonterminal_Symbol's
                                                            -- J such that I-->J
         for  J in firstNt..lastNt  loop  -- for all nonterminals J
            if  Sets.Is_In (Singles, J)  then
               The_List (List_Length).Left := I;
               The_List (List_Length).Right := J;
               The_List (List_Length).Deleted := FALSE;
               List_Length := List_Length + 1;
               if  List_Length > maxList  then
                  Restriction (9, maxList);  -- ??? Restriction (9, List_Length);
               end if;
            end if;
         end loop;
      end loop;

      loop
         Changed := FALSE;
         for  I in 0..(List_Length - 1)  loop
            if  not The_List (I).Deleted  then
               On_Left_Side  := FALSE;
               On_Right_Side := FALSE;

               for  J in 0..(List_Length - 1)  loop
                  if  not The_List (J).Deleted  then
                     if  The_List (I).Left = The_List (J).Right  then
                        On_Right_Side := TRUE;
                     end if;
                     if  The_List (J).Left = The_List (I).Right  then
                        On_Left_Side := TRUE;
                     end if;
                  end if;
               end loop;

               if  not On_Right_Side or else
                   not On_Left_Side  then
                  The_List (I).Deleted := TRUE;
                  Changed := TRUE;
               end if;
            end if;
         end loop;
         exit when not Changed;
      end loop;

      FileIO.Write_String (CRS.Listing, "Circular derivations:    ");

      Ok := TRUE;
      for  I in 0..(List_Length - 1)  loop
         if  not The_List (I).Deleted  then
            Ok := FALSE;
            FileIO.Write_Ln     (CRS.Listing);
            FileIO.Write_String (CRS.Listing, "     ");
            Get_Symbol_Node (The_List (I).Left, The_Symbol_Node);
            FileIO.Write_Text   (CRS.Listing, The_Symbol_Node.Symbol_Name.all, 20);
            FileIO.Write_String (CRS.Listing, " --> ");
            Get_Symbol_Node (The_List (I).Right, The_Symbol_Node);
            FileIO.Write_Text   (CRS.Listing, The_Symbol_Node.Symbol_Name.all, 20);
         end if;
      end loop;

      if  Ok  then
         FileIO.Write_String (CRS.Listing, " -- None --");
      end if;
      FileIO.Write_Ln (CRS.Listing);
   end Find_Circular_Productions;

   --
   -- Collect terminal sets and
   -- checks if the grammar satisfies the LL(1) conditions.
   -- Ok = TRUE means no LL(1)-conflicts.
   --
   procedure LL1_Test (Ok : in out Boolean)
   is

      The_Symbol_Node   : Symbol_Node;

      procedure LL1_Error
        (The_Condition        : in     Integer;
         The_Terminal_Symbol  : in     Integer;
         The_Symbol_Index     : in     Integer)
      is
         The_Symbol_Node: Symbol_Node;
      begin
         Ok := FALSE;
         FileIO.Write_Ln     (CRS.Listing);
         FileIO.Write_String (CRS.Listing, " LL(1) error in ");
         Get_Symbol_Node (The_Symbol_Index, The_Symbol_Node);
         FileIO.Write_String (CRS.Listing, The_Symbol_Node.Symbol_Name.all);
         FileIO.Write_String (CRS.Listing, ": ");

         if  The_Terminal_Symbol > 0  then
            Get_Symbol_Node (The_Terminal_Symbol, The_Symbol_Node);
            FileIO.Write_String (CRS.Listing, The_Symbol_Node.Symbol_Name.all);
            FileIO.Write_String (CRS.Listing, " is ");
         end if;

         case  The_Condition  is
            when 1   =>
               FileIO.Write_String (CRS.Listing, "the start of several alternatives.");
            when 2   =>
               FileIO.Write_String (CRS.Listing, "the start & successor of a deletable structure");
            when 3   =>
               FileIO.Write_String (CRS.Listing, "an ANY node that matches no symbol");
            when others =>
               null;
         end case;
      end LL1_Error;

      procedure Check
        (The_Condition     : in     Integer;
         The_Symbol_Index  : in     Integer;
         The_Set_1         : in out CRT_Set;
         The_Set_2         : in out CRT_Set)
      is
      begin
         for  I in 0..maxT  loop
            if  Sets.Is_In (The_Set_1, I) and then
                Sets.Is_In (The_Set_2, I)  then
               LL1_Error (The_Condition, I, The_Symbol_Index);
            end if;
         end loop;
      end Check;

      procedure Check_Alternatives
        (The_Graph_Index   : in     Integer;
         The_Symbol_Index  : in     Integer)
      is
         Graph_Index       : Integer := The_Graph_Index;
         Index             : Integer;
         The_Graph_Node    : Graph_Node;
         The_Graph_Node_1  : Graph_Node;
         The_Set_1         : CRT_Set;
         The_Set_2         : CRT_Set;
      begin
         while  Graph_Index > 0  loop
            Get_Graph_Node (Graph_Index, The_Graph_Node);
            if  The_Graph_Node.Node_Type = NT_Alternative  then
               Index := Graph_Index;
               Sets.Clear (The_Set_1);
               while  Index /= 0  loop  -- for all alternatives
                  Get_Graph_Node     (Index, The_Graph_Node_1);
                  Compute_Expected   (The_Graph_Node_1.Index_1, The_Symbol_Index, The_Set_2);
                  Check              (1, The_Symbol_Index, The_Set_1, The_Set_2);
                  Sets.Unite         (The_Set_1, The_Set_2);
                  Check_Alternatives (The_Graph_Node_1.Index_1, The_Symbol_Index);
                  Index := The_Graph_Node_1.Index_2;
               end loop;
            elsif  (The_Graph_Node.Node_Type = NT_Optional) or else
                   (The_Graph_Node.Node_Type = NT_Iteration)  then
               Compute_Expected   (The_Graph_Node.Index_1, The_Symbol_Index, The_Set_1);
               Compute_Expected   (abs (The_Graph_Node.Next), The_Symbol_Index, The_Set_2);
               Check              (2, The_Symbol_Index, The_Set_1, The_Set_2);
               Check_Alternatives (The_Graph_Node.Index_1, The_Symbol_Index);
            elsif  The_Graph_Node.Node_Type = NT_ANY_Symbol  then
               Get_Symbol_Set (The_Graph_Node.Index_1, The_Set_1);
               if  Sets.Empty (The_Set_1)  then -- e.g. {ANY} ANY or [ANY] ANY
                  LL1_Error (3, 0, The_Symbol_Index);
               end if;
            end if;
            Graph_Index := The_Graph_Node.Next;
         end loop;
      end Check_Alternatives;

   begin -- LL1_Test
      FileIO.Write_String (CRS.Listing, "LL(1) conditions:");
      Ok := TRUE;
      for  Symbol_Index in firstNt..lastNt  loop  -- for all nonterminals
         Get_Symbol_Node (Symbol_Index, The_Symbol_Node);
         Check_Alternatives (The_Symbol_Node.Struct, Symbol_Index);
      end loop;
      if  Ok  then
         FileIO.Write_String (CRS.Listing, "         --  Ok  --");
      end if;
      FileIO.Write_Ln (CRS.Listing);
   end LL1_Test;

   --
   -- Test if all nonterminals have productions
   -- Ok = TRUE, if all nonterminals have productions.
   --
   procedure Test_Completeness (Ok : in out Boolean)
   is
      sp: Integer;
      The_Symbol_Node: Symbol_Node;
   begin
      FileIO.Write_String (CRS.Listing, "Undefined nonterminals:  ");
      sp := firstNt;
      Ok := TRUE;
      while  sp <= lastNt  loop  -- for all nonterminals
         Get_Symbol_Node (sp, The_Symbol_Node);
         if  The_Symbol_Node.Struct = 0  then
           Ok := FALSE;
           FileIO.Write_Ln     (CRS.Listing);
           FileIO.Write_String (CRS.Listing, "     ");
           FileIO.Write_String (CRS.Listing, The_Symbol_Node.Symbol_Name.all);
         end if;
         sp := sp + 1;
      end loop;
      if  Ok  then
         FileIO.Write_String (CRS.Listing, " -- None --");
      end if;
      FileIO.Write_Ln (CRS.Listing);
   end Test_Completeness;

   --
   -- Ok = TRUE, if all nonterminals can be Reached from the start symbol.
   --
   procedure Test_If_All_Nt_Reached (Ok : in out Boolean)
   is
      The_Graph_Node    : Graph_Node;
      The_Symbol_Node   : Symbol_Node;
      Reached           : Mark_List;

      procedure Mark_Reached_Nonterminals (The_Graph_Index: in     Integer)
      is
         Graph_Index    : Integer := The_Graph_Index;
         The_Graph_Node : Graph_Node;
         The_Symbol_Node: Symbol_Node;
      begin
         while  Graph_Index > 0  loop
           Get_Graph_Node (Graph_Index, The_Graph_Node);
           if  The_Graph_Node.Node_Type = NT_Nonterminal_Symbol  then
               if  not Sets.Is_In (Reached, The_Graph_Node.Index_1)  then -- new NT_Nonterminal_Symbol Reached
                  Sets.Incl (Reached, The_Graph_Node.Index_1);
                  Get_Symbol_Node (The_Graph_Node.Index_1, The_Symbol_Node);
                  Mark_Reached_Nonterminals (The_Symbol_Node.Struct);
               end if;
           elsif  (The_Graph_Node.Node_Type = NT_Alternative) or else
                  (The_Graph_Node.Node_Type = NT_Iteration) or else
                  (The_Graph_Node.Node_Type = NT_Optional)  then
               Mark_Reached_Nonterminals (The_Graph_Node.Index_1);
               if  The_Graph_Node.Node_Type = NT_Alternative  then
                  Mark_Reached_Nonterminals (The_Graph_Node.Index_2);
               end if;
           end if;
           Graph_Index := The_Graph_Node.Next;
         end loop;
      end Mark_Reached_Nonterminals;

   begin -- Test_If_All_Nt_Reached
      Clear_Mark_List (Reached);

      Get_Graph_Node             (root, The_Graph_Node);
      Sets.Incl                  (Reached, The_Graph_Node.Index_1);
      Get_Symbol_Node            (The_Graph_Node.Index_1, The_Symbol_Node);
      Mark_Reached_Nonterminals  (The_Symbol_Node.Struct);

      FileIO.Write_String (CRS.Listing, "Unreachable nonterminals:");

      Ok := TRUE;
      for  Symbol_Index in firstNt..lastNt  loop  -- for all nonterminals
         if  not Sets.Is_In (Reached, Symbol_Index)  then
           Ok := FALSE;
           Get_Symbol_Node (Symbol_Index, The_Symbol_Node);
           FileIO.Write_Ln     (CRS.Listing);
           FileIO.Write_String (CRS.Listing, "     ");
           FileIO.Write_String (CRS.Listing, The_Symbol_Node.Symbol_Name.all);
         end if;
      end loop;

      if  Ok  then
         FileIO.Write_String (CRS.Listing, " -- None --");
      end if;

      FileIO.Write_Ln (CRS.Listing);
   end Test_If_All_Nt_Reached;

   --
   -- Ok = TRUE, if all nonterminals can be reduced to terminals.
   --
   procedure Test_If_Nt_Reduced_to_Term (Ok : in out Boolean)
   is

      The_Symbol_Node   : Symbol_Node;
      Terminated_List   : Mark_List;
      Changed           : Boolean;

      function Is_Terminated (The_Graph_Index: in     Integer) return Boolean
      is
         Graph_Index    : Integer := The_Graph_Index;
         The_Graph_Node : Graph_Node;
      begin
         while  Graph_Index > 0  loop
            Get_Graph_Node (Graph_Index, The_Graph_Node);
            if  ( (The_Graph_Node.Node_Type = NT_Nonterminal_Symbol) and then
                   not Sets.Is_In (Terminated_List, The_Graph_Node.Index_1) )
                or else
                ( (The_Graph_Node.Node_Type = NT_Alternative) and then
                  not Is_Terminated (The_Graph_Node.Index_1) and then
                  ( (The_Graph_Node.Index_2 = 0) or else 
                    not Is_Terminated (The_Graph_Node.Index_2)))  then
               return FALSE;
            end if;
            Graph_Index := The_Graph_Node.Next;
         end loop;
         return TRUE;
      end Is_Terminated;

   begin -- Test_If_Nt_Reduced_to_Term
      Clear_Mark_List (Terminated_List);

      loop
         Changed := FALSE;

         for  Symbol_Index in firstNt..lastNt  loop
            if  not Sets.Is_In (Terminated_List, Symbol_Index)  then
               Get_Symbol_Node (Symbol_Index, The_Symbol_Node);
               if  Is_Terminated (The_Symbol_Node.Struct)  then
                  Sets.Incl (Terminated_List, Symbol_Index);
                  Changed := TRUE;
               end if;
            end if;
         end loop;
         exit when not Changed;
      end loop;

      FileIO.Write_String (CRS.Listing, "Underivable nonterminals:");

      Ok := TRUE;
      for  Symbol_Index in firstNt..lastNt  loop
         if  not Sets.Is_In (Terminated_List, Symbol_Index)  then
            Ok := FALSE;
            Get_Symbol_Node (Symbol_Index, The_Symbol_Node);
            FileIO.Write_Ln     (CRS.Listing);
            FileIO.Write_String (CRS.Listing, "     " & The_Symbol_Node.Symbol_Name.all);
         end if;
      end loop;

      if  Ok  then
         FileIO.Write_String (CRS.Listing, " -- None --");
      end if;

      FileIO.Write_Ln (CRS.Listing);
   end Test_If_Nt_Reduced_to_Term;




   --
   -- Assigns the wellknown ASCII-Name in lowercase
   --
   procedure Assign_ASCII_Name
     (ASCII_Char     : in     Character;
      The_ASCII_Name : in out Name_Ptr)
   is
      I : Integer;
   begin
      case  ASCII_Char  is
         when  Character'Val (8#00#)   => The_ASCII_Name := new String'("ASCII_nul");
         when  Character'Val (8#01#)   => The_ASCII_Name := new String'("ASCII_soh");
         when  Character'Val (8#02#)   => The_ASCII_Name := new String'("ASCII_stx");
         when  Character'Val (8#03#)   => The_ASCII_Name := new String'("ASCII_etx");
         when  Character'Val (8#04#)   => The_ASCII_Name := new String'("ASCII_eot");
         when  Character'Val (8#05#)   => The_ASCII_Name := new String'("ASCII_enq");
         when  Character'Val (8#06#)   => The_ASCII_Name := new String'("ASCII_ack");
         when  Character'Val (8#07#)   => The_ASCII_Name := new String'("ASCII_bel");
         when  Character'Val (8#10#)   => The_ASCII_Name := new String'("ASCII_bs");
         when  Character'Val (8#11#)   => The_ASCII_Name := new String'("ASCII_ht");
         when  Character'Val (8#12#)   => The_ASCII_Name := new String'("ASCII_lf");
         when  Character'Val (8#13#)   => The_ASCII_Name := new String'("ASCII_vt");
         when  Character'Val (8#14#)   => The_ASCII_Name := new String'("ASCII_ff");
         when  Character'Val (8#15#)   => The_ASCII_Name := new String'("ASCII_cr");
         when  Character'Val (8#16#)   => The_ASCII_Name := new String'("ASCII_so");
         when  Character'Val (8#17#)   => The_ASCII_Name := new String'("ASCII_si");
         when  Character'Val (8#20#)   => The_ASCII_Name := new String'("ASCII_dle");
         when  Character'Val (8#21#)   => The_ASCII_Name := new String'("ASCII_dc1");
         when  Character'Val (8#22#)   => The_ASCII_Name := new String'("ASCII_dc2");
         when  Character'Val (8#23#)   => The_ASCII_Name := new String'("ASCII_dc3");
         when  Character'Val (8#24#)   => The_ASCII_Name := new String'("ASCII_dc4");
         when  Character'Val (8#25#)   => The_ASCII_Name := new String'("ASCII_nak");
         when  Character'Val (8#26#)   => The_ASCII_Name := new String'("ASCII_syn");
         when  Character'Val (8#27#)   => The_ASCII_Name := new String'("ASCII_etb");
         when  Character'Val (8#30#)   => The_ASCII_Name := new String'("ASCII_can");
         when  Character'Val (8#31#)   => The_ASCII_Name := new String'("ASCII_em");
         when  Character'Val (8#32#)   => The_ASCII_Name := new String'("ASCII_sub");
         when  Character'Val (8#33#)   => The_ASCII_Name := new String'("ASCII_esc");
         when  Character'Val (8#34#)   => The_ASCII_Name := new String'("ASCII_fs");
         when  Character'Val (8#35#)   => The_ASCII_Name := new String'("ASCII_gs");
         when  Character'Val (8#36#)   => The_ASCII_Name := new String'("ASCII_rs");
         when  Character'Val (8#37#)   => The_ASCII_Name := new String'("ASCII_us");
         when  ' '                     => The_ASCII_Name := new String'("ASCII_sp");
         when  '!'                     => The_ASCII_Name := new String'("ASCII_bang");
         when  '"'                     => The_ASCII_Name := new String'("ASCII_dquote");
         when  '#'                     => The_ASCII_Name := new String'("ASCII_hash");
         when  '$'                     => The_ASCII_Name := new String'("ASCII_dollar");
         when  '%'                     => The_ASCII_Name := new String'("ASCII_percent");
         when  '&'                     => The_ASCII_Name := new String'("ASCII_and");
         when  '''                     => The_ASCII_Name := new String'("ASCII_squote");
         when  '('                     => The_ASCII_Name := new String'("ASCII_lparen");
         when  ')'                     => The_ASCII_Name := new String'("ASCII_rparen");
         when  '*'                     => The_ASCII_Name := new String'("ASCII_star");
         when  '+'                     => The_ASCII_Name := new String'("ASCII_plus");
         when  ','                     => The_ASCII_Name := new String'("ASCII_comma");
         when  '-'                     => The_ASCII_Name := new String'("ASCII_minus");
         when  '.'                     => The_ASCII_Name := new String'("ASCII_point");
         when  '/'                     => The_ASCII_Name := new String'("ASCII_slash");
         when  '0'                     => The_ASCII_Name := new String'("ASCII_zero");
         when  '1'                     => The_ASCII_Name := new String'("ASCII_one");
         when  '2'                     => The_ASCII_Name := new String'("ASCII_two");
         when  '3'                     => The_ASCII_Name := new String'("ASCII_three");
         when  '4'                     => The_ASCII_Name := new String'("ASCII_four");
         when  '5'                     => The_ASCII_Name := new String'("ASCII_five");
         when  '6'                     => The_ASCII_Name := new String'("ASCII_six");
         when  '7'                     => The_ASCII_Name := new String'("ASCII_seven");
         when  '8'                     => The_ASCII_Name := new String'("ASCII_eight");
         when  '9'                     => The_ASCII_Name := new String'("ASCII_nine");
         when  ':'                     => The_ASCII_Name := new String'("ASCII_colon");
         when  ';'                     => The_ASCII_Name := new String'("ASCII_semicolon");
         when  '<'                     => The_ASCII_Name := new String'("ASCII_less");
         when  '='                     => The_ASCII_Name := new String'("ASCII_equal");
         when  '>'                     => The_ASCII_Name := new String'("ASCII_greater");
         when  '?'                     => The_ASCII_Name := new String'("ASCII_query");
         when  '@'                     => The_ASCII_Name := new String'("ASCII_at");
         when  'A' .. 'Z' | 'a' .. 'z' => The_ASCII_Name := new String'("ASCII_" & ASCII_Char);
         when  '['                     => The_ASCII_Name := new String'("ASCII_lbrack");
         when  '\'                     => The_ASCII_Name := new String'("ASCII_backslash");
         when  ']'                     => The_ASCII_Name := new String'("ASCII_rbrack");
         when  '^'                     => The_ASCII_Name := new String'("ASCII_uparrow");
         when  '_'                     => The_ASCII_Name := new String'("ASCII_underscore");
         when  '`'                     => The_ASCII_Name := new String'("ASCII_accent");
         when  '{'                     => The_ASCII_Name := new String'("ASCII_lbrace");
         when  '|'                     => The_ASCII_Name := new String'("ASCII_bar");
         when  '}'                     => The_ASCII_Name := new String'("ASCII_rbrace");
         when  '~'                     => The_ASCII_Name := new String'("ASCII_tilde");
         when  Character'Val (8#177#)  => The_ASCII_Name := new String'("ASCII_delete");
         when others =>
            I := Character'Pos (ASCII_Char);
            The_ASCII_Name := new String'("ASCII_" &
                                          Character'Val (((I / 100) mod 10) + Character'Pos ('0')) &
                                          Character'Val (((I / 10)  mod 10) + Character'Pos ('0')) &
                                          Character'Val ((I         mod 10) + Character'Pos ('0')));
      end case;
   end Assign_ASCII_Name;

   --
   -- Build new Name to represent The_Old_Name string
   --
   procedure Build_New_Name
     (The_Old_Name: in     Name;
      The_New_Name:    out Name_Ptr)
   is
      Target_Index   : Positive;
      ASCII_Name     : Name_Ptr;
      New_Name       : String (1..256);
   begin
      Target_Index := New_Name'First;

      for  I in (The_Old_Name'First + 1)..(The_Old_Name'Last - 1)  loop
         case  The_Old_Name (I)  is
            when 'A'..'Z' | 'a'..'z'   =>
               if  Target_Index <= New_Name'Last  then
                  New_Name (Target_Index) := The_Old_Name (I);
                  Target_Index := Target_Index + 1;
               end if;
            when others =>
               Assign_ASCII_Name (The_Old_Name (I), ASCII_Name);
               for  K in ASCII_Name.all'Range  loop
                  if  Target_Index <= New_Name'Last  then
                     New_Name (Target_Index) := ASCII_Name (K);
                     Target_Index := Target_Index + 1;
                  end if;
               end loop;
               FileIO.Free_String (ASCII_Name);   -- MUST be cleaned !!!
         end case;
      end loop;

      The_New_Name := new Name'(New_Name (New_Name'First..(Target_Index - 1)));
   end Build_New_Name;

   --
   -- Generates a new name for a symbol constant
   --
   procedure Generate_New_Symbol_Name
    (The_Symbol_Name    : in     Name;
     The_Constant_Name  :    out Name_Ptr)
   is
      Symbol_Name    : Name := The_Symbol_Name;
      Constant_Name  : Name_Ptr;
   begin
      if  (Symbol_Name (1) = '"') or else (Symbol_Name (1) = ''')  then
         if  FileIO.SLENGTH (Symbol_Name) = 3  then
            Assign_ASCII_Name (Symbol_Name (2), Constant_Name);
         else
            Build_New_Name (Symbol_Name, Constant_Name);
         end if;
      else
         Constant_Name := new Name'(Symbol_Name);
      end if;
      The_Constant_Name := new Name'(Constant_Name.all & "_Sym");
      FileIO.Free_String (Constant_Name); -- MUST be cleaned !!!
   end Generate_New_Symbol_Name;

   --
   -- Assigns the user defined or generated token names
   --
   procedure Assign_Symbol_Names
     (Default     : in     Boolean;
      There_Exists:    out Boolean)
   is

      procedure Assign_Default
        (The_Name          : in     Name; -- is not modified
         The_Constant_Name :    out Name_Ptr)
      is
         use FileIO;
      begin
         for  I in 1..lastName  loop
            if  (The_Token_Names_Table (I).Definition /= null) and then
                (The_Name = The_Token_Names_Table (I).Definition.all)  then
               The_Constant_Name := new Name'(The_Token_Names_Table (I).Symbol_Name.all);
               There_Exists      := TRUE;
               return;
            end if;
         end loop;

         if  Default  then
            Generate_New_Symbol_Name (The_Name, The_Constant_Name);
         else
            -- The_Constant_Name := "";
            The_Constant_Name := null;   -- is it correct ???
         end if;
      end Assign_Default;

   begin -- Assign_Symbol_Names
      There_Exists := Default;
      The_Symbol_Table (0).Named_Constant := new String'("EOFSYM");
      for  I in 1..maxP  loop
         Assign_Default
           (The_Symbol_Table (I).Symbol_Name.all,
            The_Symbol_Table (I).Named_Constant);
      end loop;
      The_Symbol_Table (maxT).Named_Constant := new String'("NOSYM");
   end Assign_Symbol_Names;



begin -- CRT

   ddt      := (others => FALSE);

   maxSet   := 0;
   Sets.Clear (The_Symbol_Sets (0));
   Sets.Incl  (The_Symbol_Sets (0), EOF_Symbol);
   firstNt  := maxSymbols;
   maxP     := maxSymbols;
   maxT     := -1;
   maxC     := -1;
   lastNt   := maxP - 1;
   dummyName:= 0;
   lastName := 0;
   symNames := FALSE;

   -- The dummy node The_Top_Down_Graph (0) ensures that None of the procedures
   -- above have to check for 0 indices.

   nNodes   := 0;
   The_Top_Down_Graph (0).Node_Type          := -1; -- No_Symbol constant (?)
   The_Top_Down_Graph (0).Index_1            := 0;
   The_Top_Down_Graph (0).Index_2            := 0;
   The_Top_Down_Graph (0).Next               := 0;
   The_Top_Down_Graph (0).Source_Line        := 0;
   The_Top_Down_Graph (0).Source_Pos.Start   := -1;
   The_Top_Down_Graph (0).Source_Pos.Length  := 0;
   The_Top_Down_Graph (0).Source_Pos.Column  := 0;




end CRT;


