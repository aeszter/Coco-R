--------------------------------------------------
--                                              --
-- General set handling primitives / HM 1.11.89 --
--                                              --
--------------------------------------------------

with FileIO;

package Sets is

--    subtype CARDINAL is Integer;
--    size : constant := 16;

   subtype CARDINAL is FileIO.CARDINAL;
   size : constant := FileIO.BITSET_Size;

   type Integer_Ptr is access all Integer;


   ---------------------------------
   -- The BITSET type definitions --
   ---------------------------------

   subtype BITSET_Range   is Integer range 0..(size - 1);

--    type  BITSET      is private;
   type  BITSET      is array (BITSET_Range)   of Boolean;
   pragma Pack (BITSET);

   -- The equality operators (= and /=) are made available
   -- implicitly, since type "BITSET" is nonlimited.

   Empty_BITSET   : constant BITSET;  -- Set with no elements
   Full_BITSET    : constant BITSET;  -- Set with all elements

   --
   -- This function determines whether Element is a member of BITSET
   --
   function Is_In
     (Element : in     BITSET_Range;
      Set     : in     BITSET)
     return Boolean;

--    -- Return the Number of Elements in the set S
--    function Elements_Count (Set : in     BITSET) return Integer;

   --
   -- Returns the union of the two sets
   -- (elements that are in either or both sets)
   --
   function "+" (Left, Right: in BITSET) return BITSET;

   --
   -- Adds an element to a set
   -- (the set is not changed if the element is already a member)
   --
   function "+"
     (Left : in     BITSET;
      Right: in     BITSET_Range)
     return BITSET;

   --
   -- Returns the intersection of the two sets
   -- (elements occurring in both sets)
   --
   function "*" (Left, Right: in BITSET) return BITSET;

   --
   -- Returns the difference of the two sets
   -- (elements in Left and not in Right)
   --
   function "-" (Left, Right: in BITSET) return BITSET;

   --
   -- Removes an element from the set
   -- (the set is not changed if the element is not a member)
   --
   function "-"
     (Left : in     BITSET;
      Right: in     BITSET_Range)
     return BITSET;

   --
   -- Now subset and proper subset operations ...
   --
   function "<=" (Left, Right: in BITSET) return Boolean;
   function "<"  (Left, Right: in BITSET) return Boolean;
   function ">=" (Left, Right: in BITSET) return Boolean;
   function ">"  (Left, Right: in BITSET) return Boolean;




--   Limit : constant := 32;
--   subtype BitArray_Range is Integer range 0..Limit;
--   type  BitArray    is array (BitArray_Range) of Bit_Set;
   type  BitArray    is array (Integer range <>) of BITSET;



   ---------------------------------------------
   -- Clear all elements in set S  ( S := {} )--
   ---------------------------------------------
   procedure Clear (S: in out BitArray);

   -------------------------------------------------
   -- Set all elements in set S  ( S := full set )--
   -------------------------------------------------
   procedure Fill (S: in out BitArray);

   ----------------------------------------------------------------------
   -- Return TRUE, if element X is contained in the set S  ( x IN s ? )--
   ----------------------------------------------------------------------
   function  Is_In
     (S: in     BitArray;
      X: in     CARDINAL)
     return  Boolean;

   ----------------------------------------------------
   -- Include element X in the set S  ( INCL(s, x) ) --
   ----------------------------------------------------
   procedure Incl
     (S: in out BitArray;
      X: in     CARDINAL);

   ------------------------------------------------------
   -- Exclude element X from the set S  ( EXCL(s, x) ) --
   ------------------------------------------------------
   procedure Excl
     (S: in out BitArray;
      X: in     CARDINAL);

   ---------------------------------------------------------------------------
   -- Return TRUE, if the set S2 is contained in the set S1  ( s2 <= s1 ? ) --
   ---------------------------------------------------------------------------
   function  Includes
     (S1: in     BitArray;
      S2: in     BitArray)
     return Boolean;

   -------------------------------------------------------
   -- Return number of elements in the set S  ( | s | ) --
   -------------------------------------------------------
   function  Elements
     (S         : in     BitArray)
     return  Integer;

   ---------------------------------------------------------------
   -- Return number of the last element contained in the set S, --
   -- or -1 when the set S is empty                             --
   ---------------------------------------------------------------
   function Last_Element
     (S         : in     BitArray)
     return  Integer;

   ----------------------------------------------------
   -- Returns TRUE, if set S is empty  ( s1 = {} ? ) --
   ----------------------------------------------------
   function  Empty (S: in     BitArray) return Boolean;

   -------------------------------------------------------------
   -- Return TRUE, if sets S1 and S2 are equal  ( s1 = s2 ? ) --
   -------------------------------------------------------------
   function  Equal
     (S1: in     BitArray;
      S2: in     BitArray)
     return Boolean;

   -----------------------------------------------------------------------------
   -- Return TRUE, if sets S1 and S2 are totally different  ( s1 * s2 = 0 ? ) --
   -----------------------------------------------------------------------------
   function  Different
     (S1: in     BitArray;
      S2: in     BitArray)
     return Boolean;

   -------------------
   -- S1 := S1 + S2 --
   -------------------
   procedure Unite
     (S1: in out BitArray;
      S2: in     BitArray);

   -------------------
   -- S1 := S1 - S2 --
   -------------------
   procedure Differ
     (S1: in out BitArray;
      S2: in     BitArray);

   -------------------
   -- S3 := S1 * S2 --
   -------------------
   procedure Intersect
     (S1: in     BitArray;
      S2: in     BitArray;
      S3: in out BitArray);

   -----------------
   -- Print set S --
   -----------------
   procedure Print
     (F     : in     FileIO.File;
      S     : in out BitArray;
      W     : in     Integer;
      Indent: in     Integer);



private


--    type  BITSET      is array (BITSET_Range)   of Boolean;
--    pragma Pack (BITSET);

   -- Completion of deferred constants
   Empty_BITSET   : constant BITSET := (BITSET_Range => False);
   Full_BITSET    : constant BITSET := (BITSET_Range => True);


end Sets;


