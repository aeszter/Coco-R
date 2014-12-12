--------------------------------------
--                                  --
-- Automaton and Scanner Generation --
--                                  --
--------------------------------------

with FileIO;


-- for private part
with CRS;
with CRT;
with Sets;

with Ada.Unchecked_Deallocation;



package CRA is

   subtype CARDINAL is FileIO.CARDINAL;

   Max_Source_Line_Length  : constant := 78;

   type PutS_Proc is access procedure (S : in     String);


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
      Frame_Out   : in     FileIO.File);


   --
   -- Generates the import list for the eventually existing named symbol constants.
   --
   procedure Import_Named_Symbol_Constants (putS : in     PutS_Proc);


   --
   -- Converts top-down graph (TDG) with root The_TDG_Index
   -- into a subautomaton of the DFA
   -- that recognizes token The_Symbol_Index
   --
   procedure Convert_To_States
     (The_TDG_Index     : in    Integer;
      The_Symbol_Index  : in    Integer);


   --
   -- Returns TRUE (???), if string S can be recognized by the current DFA.
   -- Matched_Symbol_Index point to token as that S can be recognized.
   --
   procedure Match_DFA
     (S                    : in     String;
      The_Symbol_Index     : in     Integer;
      Matched_Symbol_Index : in out Integer);


   --
   -- Converts the NDFA into a DFA. "Correct" indicates if an error occurred.
   --
   procedure Convert_NDFA_into_DFA (correct: in out Boolean);


   --
   -- Defines a new comment for the scanner. The comment brackets are
   -- represented by the mini top-down graphs with the roots from and to.
   --
   procedure Define_New_Comment
     (The_Start_From : in     Integer;
      The_Stop_To    : in     Integer;
      The_Nested     : in     Boolean);


   --
   -- Emits the source code of the generated scanner
   -- using the frame file scanner.frm
   --
   procedure Write_Scanner (Ok: in out Boolean);


   --
   -- List the automaton for tracing
   --
   procedure Print_Automaton_States;


-------------------------------------------------------------------------

private

--CONST's

   maxStates : constant           := 500;
   CR        : constant Character := FileIO.CR;

--TYPE's

   type  Action_Node;
   type  Action_Node_Ptr  is access all Action_Node;

   type  Target_Node;
   type  Target_Node_Ptr  is access all Target_Node;

   type  Action_Node  is      -- action of finite automaton
      record
         Node_Type         : Integer;           -- type of action symbol: char, class
         Symbol_Action     : Integer;           -- action symbol
         Transition_Code   : Integer;           -- transition code: normTrans, contextTrans
         Target            : Target_Node_Ptr;   -- States after transition with input symbol
         Next              : Action_Node_Ptr;
      end record;
   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Action_Node,
      Name   => Action_Node_Ptr);

   type  Target_Node  is      -- State after transition with input symbol
      record
         State : Integer;        -- target State
         Next  : Target_Node_Ptr;
      end record;
   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Target_Node,
      Name   => Target_Node_Ptr);

   type  Automaton_State  is  -- State of finite automaton
      record
         First_Action         : Action_Node_Ptr;  -- to first action of this State
         End_Of_Token         : Integer;     -- nr. of recognized token if State is final
         Reached_By_Context   : Boolean;     -- TRUE: State reached by contextTrans
      end record;

   type  Comment_Node;        -- info about a comment syntax
   type  Comment_Node_Ptr is access all Comment_Node;
   type  Comment_Node is
      record
         Start_Comment  : String (1..2) := "  ";
         Start_Last     : Positive;
         Stop_Comment   : String (1..2) := "  ";
         Stop_Last      : Positive;
         Nested         : Boolean;
         Next           : Comment_Node_Ptr;
      end record;
--    procedure Dispose is new Ada.Unchecked_Deallocation
--      (Object => Comment_Node,
--       Name   => Comment_Node_Ptr);

   type  Melted_Node;         -- info about melted States
   type  Melted_Node_Ptr  is access all Melted_Node;
   type  Melted_Node  is
      record
         Old_States_Set : CRT.CRT_Set;  -- Set of old States
         New_State      : Integer;      -- new State
         Next           : Melted_Node_Ptr;
      end record;
   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Melted_Node,
      Name   => Melted_Node_Ptr);


--VAR's

   Automaton_States        : array (Integer range 0 .. maxStates) of Automaton_State;

   Last_Non_Melted_State   : Integer;     -- last non melted State
   Last_Allocated_State    : Integer;     -- last allocated State
   Root_State              : Integer;     -- Start State of DFA

   List_Of_Melted_States   : Melted_Node_Ptr;   -- list of melted States
   List_Of_Comments        : Comment_Node_Ptr;  -- list of comments

   Is_Dirty_DFA            :  Boolean;          -- DFA may become non-deterministic
   Is_New_Line             :  Boolean;

   The_Scanner_File        : aliased FileIO.File;  -- generated scanner file
   The_Frame_File          : aliased FileIO.File;  -- scanner frame file

   Cur_Out_File            : FileIO.File; -- current output file




end CRA;
