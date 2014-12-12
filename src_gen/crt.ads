-------------------------------------
--                                 --
-- Symbol Table and Top-Down Graph --
--                                 --
-------------------------------------

with FileIO;
with Sets;

package CRT is


   subtype INT32     is FileIO.INT32;
   subtype CARDINAL  is FileIO.CARDINAL;



-- CONSTANTS

   -----------------------------------------------------------------------------
   -- The following are chosen to ensure that data segments remain within the --
   -- 64K limit imposed by Dos 16 bit systems.  Manipulate them at your peril --
   -- if you need to handle large grammars!                                   --
   -----------------------------------------------------------------------------

   maxSymbols   : constant :=    500; -- max number of symbols
                                      -- (terminals+nonterminals+pragmas)
   maxTerminals : constant :=    400; -- max number of terminals
   maxNt        : constant :=    210; -- max number of nonterminals
   maxNodes     : constant :=   1500; -- max number of top-down graph nodes
   maxClasses   : constant :=    250; -- max number of character classes
   maxSemLen    : constant :=  64000; -- max length of a semantic text
   normTrans    : constant :=      0; -- DFA transition during normal scanning
   contextTrans : constant :=      1; -- DFA transition during scanning of right context
   maxList      : constant :=    150; -- max array size in Find_Circular_Productions
   maxLiterals  : constant :=    127; -- max number of literal terminals


   ----------------
   -- node types --
   ----------------
   NT_Unknown_Symbol       : constant :=   0;
   NT_Terminal_Symbol      : constant :=   1; -- terminal symbol
   NT_Pragma_Symbol        : constant :=   2; -- pragma
   NT_Nonterminal_Symbol   : constant :=   3; -- nonterminal symbol
   NT_Character_Class      : constant :=   4; -- character class
   NT_Single_Character     : constant :=   5; -- single character
   NT_Weak_Terminal_Symbol : constant :=   6; -- weak terminal symbol
   NT_ANY_Symbol           : constant :=   7; -- symbol ANY
   NT_Empty_Alternative    : constant :=   8; -- empty alternative
   NT_SYNC_Symbol          : constant :=   9; -- symbol SYNC
   NT_Semantic_Action      : constant :=  10; -- semantic action
   NT_Alternative          : constant :=  11; -- alternative
   NT_Iteration            : constant :=  12; -- iteration
   NT_Optional             : constant :=  13; -- option


   No_Symbol            : constant :=  -1;
   EOF_Symbol           : constant :=   0;


--    type  Node_Types is (
--       Unknown_Symbol,
--       Terminal_Symbol,
--       Pragma_Symbol, -- pragma
--       Nonterminal_Symbol,
--       Character_Class,
--       Single_Character,
--       Weak_Terminal_Symbol,
--       ANY_Symbol,
--       Empty_Alternative,
--       SYNC_Symbol,
--       Semantic_Action,
--       Alternative,
--       Iteration,
--       Option -- ,
-- --       No_Symbol
--    );
-- --    EOF_Symbol : constant Node_Types := Unknown_Node;


   -----------------
   -- token kinds --
   -----------------
   TK_Class_Token          : constant :=  0;  -- token class
   TK_Literal_Token        : constant :=  1;  -- literal (e.g. keyword) not recognized by DFA
   TK_Class_Literal_Token  : constant :=  2;  -- token class that can also match a literal


--    type  Token_Kinds is (
--       Class_Token,         -- token class
--       Literal_Token,       -- literal (e.g. keyword) not recognized by DFA
--       Class_Literal_Token  -- token class that can also match a literal
--    );


-- TYPES

   subtype String_Ptr   is FileIO.String_Ptr;

   subtype Name         is String;
   subtype Name_Ptr     is FileIO.String_Ptr;

   type Position is     -- position of stretch of source text
      record
         Start    :  INT32;      -- start relative to beginning of file
         Length   :  CARDINAL;   -- length
         Column   :  Integer;    -- column number of start position
      end record;

   type Symbol_Node is   -- node of symbol table
      record
         Node_Type      : Integer;     -- nt, t, pr, unknown
         Symbol_Name    : Name_Ptr;    -- symbol name
         Named_Constant : Name_Ptr;    -- named constant of symbol
         Struct         : Integer;     -- Node_Type = nt: index of first node of syntax graph
                                       -- Node_Type = t: token kind: literal, class, ...
         Deletable      : Boolean;     -- Node_Type = nt: TRUE, if nonterminal is deletable
         Attr_Pos       : Position;    -- position of attributes in source text
         Sem_Action_Pos : Position;    -- Node_Type = pr: pos of sem action in source text
                                       -- Node_Type = nt: pos of local decls in source text
         Source_Line    : Integer;     -- source text line number of symbol in this node
      end record;

   type Graph_Node is
      record     -- node of top-down graph
         Node_Type      : Integer;     -- nt,sts,wts,char,class,any,eps,sem,sync,alt,
                                       --   iter,opt
         Next           : Integer;     -- to successor node
                                       -- next < 0: to successor of enclosing structure
         Index_1        : Integer;     -- Node_Type IN {nt, t, wt}: index to symbol table
                                       -- Node_Type = any: index to anyset
                                       -- Node_Type = sync: index to syncset
                                       -- Node_Type = alt:
                                       --         index of first node of first alternative
                                       -- Node_Type IN {iter, opt}: first node in subexpression
                                       -- Node_Type = char: ordinal character value
                                       -- Node_Type = class: index of character class
         Index_2        : Integer;     -- Node_Type = alt:
                                       --         index of first node of second alternative
                                       -- Node_Type IN {char, class}: transition code
         Source_Pos     : Position;    -- Node_Type IN {nt, t, wt}:
                                       --         source pos of actual attributes
                                       -- Node_Type = sem: source pos of sem action
         Source_Line    : Integer;     -- source text line number of item in this node
      end record;


   subtype CRT_Set   is Sets.BitArray (Integer range 0 .. maxTerminals / Sets.size);
   subtype Mark_List is Sets.BitArray (Integer range 0 .. maxNodes / Sets.size);



-- VARIABLES

   maxT        :  Integer;    -- terminals stored from 0 .. maxT in symbol table
   maxP        :  Integer;    -- pragmas stored from maxT+1..maxP in symbol table
   firstNt     :  Integer;    -- index of first nt: available after Compute_Symbol_Sets
   lastNt      :  Integer;    -- index of last nt: available after Compute_Symbol_Sets
   maxC        :  Integer;    -- index of last character class
   nNodes      :  Integer;    -- index of last top-down graph node
   root        :  Integer;    -- index of root node, filled by ATG

   semDeclPos  :  Position;   -- position of global semantic declarations
   genScanner  :  Boolean;    -- TRUE: a scanner shall be generated
   ignoreCase  :  Boolean;    -- TRUE: scanner treats lower case as upper case
   symNames    :  Boolean;    -- TRUE: symbol names have to be assigned
   ignored     :  CRT_Set;    -- characters ignored by the scanner

   ddt         :  array (Character range 'A' .. 'Z') of Boolean;
                              -- parameter, debug and test switches


   --
   -- Collects a user defined token name.
   -- Inserts the pair (The_Name, The_Name_Definition)
   -- in The_Token_Names_Table
   --
   procedure New_Name
     (The_Name            : in     Name_Ptr;
      The_Name_Definition : in     Name_Ptr);

   --
   -- Generates a new symbol with The_Type and The_Name and returns its
   -- Index in The_Symbol_Table
   --
   function New_Sym
     (The_Type       : in     Integer;
      The_Name       : in     Name_Ptr;
      The_Source_Line: in     Integer)
     return Integer;

   --
   -- Gets symbol node with The_Index in The_Symbol_Node.
   --
   procedure Get_Symbol_Node
     (The_Index       : in     Integer; 
      The_Symbol_Node :    out Symbol_Node);

   --
   -- Replaces symbol node with The_Index by The_Symbol_Node.
   --
   procedure Put_Symbol_Node
     (The_Index       : in     Integer;
      The_Symbol_Node : in     Symbol_Node);

   --
   -- Gets symbol index for identifier with name The_Name.
   --
   function Find_Symbol (The_Name : in  Name) return Integer;

   --
   -- Stores The_Set in The_Symbol_Sets as a new set and returns its index.
   --
   function New_Set (The_Set : in     CRT_Set) return Integer;

   --
   -- Computes start symbols of graph gp.
   --
   procedure Compute_First_Set
     (The_Graph_Index: in     Integer;
      The_First_Set  :    out CRT_Set);

   --
   -- Computes all symbols expected at location gp in graph of symbol sp.
   --
   procedure Compute_Expected
     (The_Graph_Index   : in     Integer;
      The_Symbol_Index  : in     Integer;
      The_Expected_Set  :    out CRT_Set);

   --
   -- Marks deletable nonterminals and prints them.
   --
   procedure Compute_Deletable_Symbols;

   --
   -- Collects The_First_Sets, The_Follow_Sets, any-sets, and sync-sets.
   --
   procedure Compute_Symbol_Sets;

   --
   -- Prints the symbol table (for tracing).
   --
   procedure Print_Symbol_Table;

   --
   -- Produces a cross reference listing of all symbols.
   --
   procedure XRef;

   --
   -- Defines a new character class and returns its index
   --
   function New_Character_Class
     (The_Class_Name : in     Name_Ptr;
      The_Set        : in     CRT_Set)
     return Integer;

   --
   -- Searches for a Character_Class with the given name.
   -- Returns its index or -1
   --
   function Get_Class_By_Name
     (The_Class_Name : in     Name)
     return Integer;

   --
   -- Searches for a class with the given set.
   -- Returns its index or -1
   --
   function Get_Class_By_Set
     (The_Set : in     CRT_Set)
     return Integer;

   --
   -- Returns character class n
   --
   procedure Get_Character_Class
     (The_Index: in     Integer;
      The_Set  :    out CRT_Set);

   --
   -- Returns the name of class n
   --
   procedure Get_Character_Class_Name
     (The_Index   : in     Integer;
      Class_Name  :    out Name_Ptr);

   --
   -- Gives access to precomputed symbol sets
   --
   procedure Get_Symbol_Set
     (The_Index: in     Integer;
      The_Set  :    out CRT_Set);

   --
   -- Generates a new graph node with The_Type, Index_1, and Source_Line number
   -- and returns its index.
   --
   function New_Graph_Node
     (The_Type    : in     Integer;
      Index_1     : in     Integer;
      Source_Line : in     Integer)
     return Integer;

   --
   -- Clears all elements of m
   --
   procedure Clear_Mark_List (The_Mark_List : in out Mark_List);

   --
   -- Gets graph node with The_Index in The_Graph_Node.
   --
   procedure Get_Graph_Node
     (The_Index      : in     Integer;
      The_Graph_Node :    out Graph_Node);

   --
   -- Replaces graph node with The_Index by The_Graph_Node.
   --
   procedure Put_Graph_Node
     (The_Index      : in     Integer;
      The_Graph_Node : in     Graph_Node);

   --
   -- Makes (The_Graph_L2, The_Graph_R2) an NT_Alternative
   -- of the graph (The_Graph_L1, The_Graph_R1).
   -- The resulting graph is identified by (The_Graph_L1, The_Graph_R1).
   --
   procedure Concatenate_Alternative
     (The_Graph_L1   : in out Integer;
      The_Graph_R1   : in out Integer;
      The_Graph_L2   : in     Integer;
      The_Graph_R2   : in     Integer);

   --
   -- Concatenates graph (The_Graph_L1, The_Graph_R1)
   -- with graph (The_Graph_L2, The_Graph_R2) via next-chain.
   -- The resulting graph is identified by (The_Graph_L1, The_Graph_R1).
   --
   procedure Concatenate_Sequential
     (The_Graph_L1   : in out Integer;
      The_Graph_R1   : in out Integer;
      The_Graph_L2   : in     Integer;
      The_Graph_R2   : in     Integer);

   --
   -- Generates an alt-node with (The_Graph_L, The_Graph_R)
   -- as its first and only alternative
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure Make_First_Alternative
     (The_Graph_L : in out Integer;
      The_Graph_R : in out Integer);

   --
   -- Encloses the graph (The_Graph_L, The_Graph_R)
   -- into an iteration construct.
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure Make_Iteration
     (The_Graph_L : in out Integer;
      The_Graph_R : in out Integer);

   --
   -- Encloses the graph (The_Graph_L, The_Graph_R)
   -- into an option construct.
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure Make_Option
     (The_Graph_L : in out Integer;
      The_Graph_R : in out Integer);

   --
   -- Lets right ends of graph gp be 0
   --
   procedure Complete_Graph (The_Graph_Index : in  Integer);

   --
   -- Generates linear graph from characters in s
   -- The resulting graph is identified by (The_Graph_L, The_Graph_R).
   --
   procedure String_To_Graph
     (S           : in     String;
      The_Graph_L : in out Integer;
      The_Graph_R : in out Integer);

   --
   -- TRUE, if (sub) graph with root gp is deletable.
   --
   function Is_Deletable_Graph
     (The_Graph_Index : in  Integer)
     return Boolean;

   --
   -- TRUE, if graph node The_Graph_Node is deletable,
   -- i.e. can be derived into the empty string.
   --
   function Is_Deletable_Node
     (The_Graph_Node : in  Graph_Node)
     return Boolean;

   --
   -- Prints the graph (for tracing).
   --
   procedure Print_Graph;




   -------------------------------------------------------------------
   --                                                               --
   --                Procedures for grammar tests                   --
   --                                                               --
   -------------------------------------------------------------------

   --
   -- Finds and prints the circular part of the grammar.
   -- Ok = TRUE means no circular part.
   --
   procedure Find_Circular_Productions (Ok : in out Boolean);

   --
   -- Checks if the grammar satisfies the LL(1) conditions.
   -- Ok = TRUE means no LL(1)-conflicts.
   --
   procedure LL1_Test (Ok : in out Boolean);

   --
   -- Ok = TRUE, if all nonterminals have productions.
   --
   procedure Test_Completeness (Ok: in out Boolean);

   --
   -- Ok = TRUE, if all nonterminals can be reached from the start symbol.
   --
   procedure Test_If_All_Nt_Reached (Ok : in out Boolean);

   --
   -- Ok = TRUE, if all nonterminals can be reduced to terminals.
   --
   procedure Test_If_Nt_Reduced_to_Term (Ok : in out Boolean);

   --
   -- Assigns the user defined or generated token names
   --
   procedure Assign_Symbol_Names
     (Default     : in     Boolean;
      There_Exists:    out Boolean);

   --
   -- Signal compiler restriction and abort program
   --
   procedure Restriction
     (n     : in     Integer;
      limit : in     Integer);


private --------------------------------------------------------------


   ----------------------
   -- INTERNAL CONST's --
   ----------------------

   maxSetNr    : constant := 256;   -- max. number of symbol sets
   maxNames    : constant := 100;   -- max. number of declared token names



   ---------------------
   -- INTERNAL TYPE's --
   ---------------------

   type  First_Set    is
      record
         Terminal_Symbols  : CRT_Set;  -- terminal symbols
         Ready             : Boolean;  -- TRUE = Terminal_Symbols is complete
      end record;

   type  Follow_Set  is
      record
         Terminal_Symbols  : CRT_Set;  -- terminal symbols
         Non_Terminals     : CRT_Set;  -- Non_Terminals whose start set is 
                                       -- to be included in Terminal_Symbols
      end record;

   type  Char_Class  is
      record
         Class_Name  : Name_Ptr;     -- Character_Class name
         Set_Index   : Integer;  -- ptr to set representing the Char_Class
      end record;

   type  Name_Table_Entry is
      record
         Symbol_Name : Name_Ptr;
         Definition  : String_Ptr;
      end record;


   type Symbols      is array (Natural range <>)   of Symbol_Node;
   type Char_Classes is array (Natural range <>)   of Char_Class;
   type Names        is array (Positive range <>)  of Name_Table_Entry;
   type Graph        is array (Natural range <>)   of Graph_Node;


   type First_Sets   is array (Natural range <>)   of First_Set;
   type Follow_Sets  is array (Natural range <>)   of Follow_Set;
   type Symbol_Sets  is array (Natural range <>)   of CRT_Set;



   -------------------------
   -- INTERNAL VARIABLE's --
   -------------------------


   -- symbol table for terminals, pragmas, and nonterminals
   The_Symbol_Table        : Symbols (0..maxSymbols);

   -- character classes table
   The_Char_Classes_Table  : Char_Classes (0..maxClasses);

   -- table of token name declarations
   The_Token_Names_Table   : Names (1..maxNames);

   -- top-down graph
   The_Top_Down_Graph      : Graph (0..maxNodes);


   -- The_First_Sets (I) = first symbols of The_Symbol_Table (I + firstNt) 
   The_First_Sets          : First_Sets (0..maxNt);

   -- The_Follow_Sets (I) = followers of The_Symbol_Table (I + firstNt) 
   The_Follow_Sets         : Follow_Sets (0..maxNt);

   -- The_Symbol_Sets (0) = all SYNC symbols 
   The_Symbol_Sets         : Symbol_Sets (0..maxSetNr);



   maxSet      : Integer;     -- index of last symbol set
   lastName    : CARDINAL;
   dummyName   : CARDINAL;    -- for unnamed character classes



end CRT;


