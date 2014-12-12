--------------------------------------------------
--                                              --
-- General set handling primitives / HM 1.11.89 --
--                                              --
--------------------------------------------------

--with FileIO;

package body Sets is


   ---------------------------------
   -- The BITSET type definitions --
   ---------------------------------

   -- This function determines whether Element is a member of Set
   function Is_In
     (Element : in     BITSET_Range;
      Set     : in     BITSET)
     return Boolean
   is
   begin
      return Set (Element);
   end Is_In;

   -- Return the Number of Elements in the set S
   function Elements_Count (Set : in     BITSET) return Integer
   is
      Result : Integer := 0;
   begin
      for  I in BITSET_Range  loop
         if  Is_In (I, Set)  then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Elements_Count;
   
   -- Returns the union of the two sets
   -- (elements that are in either or both sets)
   function "+" (Left, Right: in BITSET) return BITSET
   is
   begin
      return (Left or Right);
   end "+";

   -- Adds an element to a set
   -- (the set is not changed if the element is already a member)
   function "+"
     (Left : in     BITSET;
      Right: in     BITSET_Range)
     return BITSET
   is
      Result: BITSET := Left;
   begin
      Result (Right) := True;   -- Add the new element to the set
      return Result;
   end "+";

   -- Returns the intersection of the two sets
   -- (elements occurring in both sets)
   function "*" (Left, Right: in BITSET) return BITSET
   is
   begin
      return (Left and Right);
   end "*";

   -- Returns the difference of the two sets
   -- (elements in Left and not in Right)
   function "-" (Left, Right: in BITSET) return BITSET is
   begin
      return (Left and not Right);
   end "-";

   -- Removes an element from the set
   -- (the set is not changed if the element is not a member)
   function "-"
     (Left : in     BITSET;
      Right: in     BITSET_Range)
     return BITSET
   is
      Result: BITSET := Left;
   begin
      Result(Right) := False;   -- Remove the element from the set
      return Result;
   end "-";


   -- Now subset and proper subset operations ...

   -- If the union of the sets Left and Right is equal to Right,
   -- then the set Left is a subset of Right.
   function "<=" (Left, Right: in BITSET) return Boolean
   is
   begin
      return ((Left+Right) = Right);
   end "<=";

   -- A proper subset is a subset but at least one element must
   -- be missing making the sets unequal
   function "<" (Left, Right: in BITSET) return Boolean
   is
   begin
      return ((Left <= Right) and then Left /= Right);
   end "<";


   -- The set Left is a superset of Right, if Right is a subset of Left
   function ">=" (Left, Right: in BITSET) return Boolean
   is
   begin
      return (Right <= Left);
   end ">=";

   -- Similarly for a proper superset
   function ">" (Left, Right: in BITSET) return Boolean
   is
   begin
      return (Right < Left);
   end ">";



   ---------------------------------------------
   -- Clear all elements in set S  ( S := {} )--
   ---------------------------------------------
   procedure Clear (S: in out BitArray)
   is
   begin
--       for  I in S'Range  loop
--          S (I) := Empty_BITSET;
--       end loop;
      S := (others => Empty_BITSET);
   end Clear;

   -------------------------------------------------
   -- Set all elements in set S  ( S := full set )--
   -------------------------------------------------
   procedure Fill (S: in out BitArray)
   is
   begin
--       for  I in S'Range  loop
--          S (I) := Full_BITSET;
--       end loop;
      S := (others => Full_BITSET);
   end Fill;

   ----------------------------------------------------------------------
   -- Return TRUE, if element X is contained in the set S  ( x IN s ? )--
   ----------------------------------------------------------------------
   function  Is_In
     (S: in     BitArray;
      X: in     CARDINAL)
     return  Boolean
   is
   -- procedure In (VAR s: ARRAY OF BITSET; x: CARDINAL): Boolean;
   --   BEGIN
   --     RETURN x MOD size IN s[x DIV size]
   --   END In;

--       X_mod_size := X mod size;
--       X_size     := X / size;
   begin
      return (Is_In ((X mod size), S (X / size)));
   end Is_In;

   ----------------------------------------------------
   -- Include element X in the set S  ( INCL(s, x) ) --
   ----------------------------------------------------
   procedure Incl
     (S: in out BitArray;
      X: in     CARDINAL)
   is
   begin
      S (X / size) := S (X / size) + (X mod size);
   end Incl;

   ------------------------------------------------------
   -- Exclude element X from the set S  ( EXCL(s, x) ) --
   ------------------------------------------------------
   procedure Excl
     (S: in out BitArray;
      X: in     CARDINAL)
   is
   begin
      S (X / size) := S (X / size) - (X mod size);
   end Excl;

   ---------------------------------------------------------------------------
   -- Return TRUE, if the set S2 is contained in the set S1  ( s2 <= s1 ? ) --
   ---------------------------------------------------------------------------
   function  Includes
     (S1: in     BitArray;
      S2: in     BitArray)
     return Boolean
   is
   begin
      for  I in S1'Range  loop
         if  (S2 (I) /= (S2 (I) * S1 (I))) then
            return False;
         end if;
      end loop;
      return True;
   end Includes;

   -------------------------------------------------------
   -- Return number of elements in the set S  ( | s | ) --
   -------------------------------------------------------
   function  Elements
     (S         : in     BitArray)
     return  Integer
   is
      Result : Integer := 0;
   begin
      for  I in S'Range  loop
         Result := Result + Elements_Count (S (I));
      end loop;
      return Result;
   end Elements;

   ---------------------------------------------------------------
   -- Return number of the last element contained in the set S, --
   -- or -1 when the set S is empty                             --
   ---------------------------------------------------------------
   function Last_Element
     (S         : in     BitArray)
     return  Integer
   is
      Result : Integer := -1;
   begin
      for  I in reverse 0..(((S'Last + 1) * size) - 1)  loop
         if  Is_In (S, I)  then
            Result := I;
            exit;
         end if;
      end loop;
      return Result;
   end Last_Element;

   ----------------------------------------------------
   -- Returns TRUE, if set S is empty  ( s1 = {} ? ) --
   ----------------------------------------------------
   function  Empty (S: in     BitArray) return Boolean
   is
   begin
      for  I in S'Range  loop
         if  (S (I) /= Empty_BITSET)  then
            return False;
         end if;
      end loop;
      return True;
   end Empty;

   -------------------------------------------------------------
   -- Return TRUE, if sets S1 and S2 are equal  ( s1 = s2 ? ) --
   -------------------------------------------------------------
   function  Equal
     (S1: in     BitArray;
      S2: in     BitArray)
     return Boolean
   is
--       D : Integer := 0;
      D : Integer := S2'First - S1'First;
   begin
      if  ((S1'Last - S1'First) = (S2'Last - S2'First))  then
--          if  S1'First /= S2'First  then
--             D := S2'First - S1'First;
--          end if;
         for  I in S1'Range  loop
            if  (S1 (I) /= S2 (I + D))  then
               return False;
            end if;
         end loop;
         return True;
      else
         return False;
      end if;
   end Equal;

   -----------------------------------------------------------------------------
   -- Return TRUE, if sets S1 and S2 are totally different  ( s1 * s2 = 0 ? ) --
   -----------------------------------------------------------------------------
   function  Different
     (S1: in     BitArray;
      S2: in     BitArray)
     return Boolean
   is
   begin
      for  I in S1'Range  loop
         if  (S1 (I) * S2 (I) /= Empty_BITSET)  then
            return False;
         end if;
      end loop;
      return True;
   end Different;

   -------------------
   -- S1 := S1 + S2 --
   -------------------
   procedure Unite
     (S1: in out BitArray;
      S2: in     BitArray)
   is
   begin
      for  I in S1'Range  loop
         S1 (I) := S1 (I) + S2 (I);
      end loop;
   end Unite;

   -------------------
   -- S1 := S1 - S2 --
   -------------------
   procedure Differ
     (S1: in out BitArray;
      S2: in     BitArray)
   is
   begin
      for  I in S1'Range  loop
         S1 (I) := S1 (I) - S2 (I);
      end loop;
   end Differ;

   -------------------
   -- S1 := S1 * S2 --
   -------------------
   procedure Intersect
     (S1: in     BitArray;
      S2: in     BitArray;
      S3: in out BitArray)
   is
   begin
      for  I in S1'Range  loop
         S3 (I) := S1 (I) * S2 (I);
      end loop;
   end Intersect;

   -----------------
   -- Print set S --
   -----------------
   procedure Print
     (F     : in     FileIO.File;
      S     : in out BitArray;
      W     : in     Integer;
      Indent: in     Integer)
   is
      col: Integer := indent;
      i  : Integer := 0;
      max: Integer := (S'Last + 1) * size;
   begin
    FileIO.Write(f, '{');
    while i < max  loop
         if  Is_In (s, i) then
            if  (col + 4) > w then
               FileIO.Write_Ln (f); 
               FileIO.Write_Text (f, "", indent); 
               col := indent;
            end if;
            FileIO.Write_Int (f, i, 3); 
            FileIO.Write (f, ',');
            col := col + 4;
         end if;
         I := I + 1;
      end loop;
      FileIO.Write (f, '}');
   end Print;


end Sets;


