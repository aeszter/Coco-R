--
--    CR   Main Module of Coco/R
--    ==   =====================
-- 
--    This is a compiler generator that produces a scanner and a parser
--    from an attributed grammar, and optionally a complete small compiler.
-- 
--    Original code in Oberon by Hanspeter Moessenboeck, ETH Zurich
--    Ported at ETH to Apple Modula, and thence to JPI-2 Modula.
-- 
--    JPI version of 27 January 1991 was then modified to make more
--    portable by Pat Terry, January - October 1992
-- 
--    This is the Linux/GNAT based version for genegating Ada code.
--    The port done by O.Havva, 11 January 2005
-- 
-- 
-- Usage:
--           COCOR [-options] GrammarName[.atg] [$options]
-- 
-- Input:
--   attributed grammar   input grammar
--   scanner.frm          frame file
--   parser.frm           frame file
--   compiler.frm         frame file (optional)
-- 
-- (Frame files must be in the sme directory as the grammar, or may be
-- found on a path specified by environment variable CRFRAMES).
-- 
-- Output:
--   <GrammarName>S.def + mod  generated scanner
--   <GrammarName>P.def + mod  generated parser
--   <GrammarName>.err         error numbers and corresponding error messages
--   <GrammarName>.lst         source listing with error messages and trace output
-- 
-- Optionally
-- 
--   <GrammarName>G.def + mod  generated symbolic names
--   <GrammarName>.mod         generated compiler main module
-- 
-- Implementation restrictions
--   1  too many nodes in graph (>1500)                 CRT.NewNode
--   2  too many symbols (>500)                         CRT.NewSym, MovePragmas
--   3  too many sets (>256 ANY-syms or SYNC syms)      CRT.NewSet,
--   4  too many character classes (>250)               CRT.NewClass
--   5  too many conditions in generated code (>100)    CRX.NewCondSet
--   6  too many token names in "NAMES" (>100)          CRT.NewName
--   7  too many states in automata (>500)              CRA.NewState
-- 
-- Trace output
-- (To activate a trace switch, write "${letter}" in the input grammar, or
-- invoke Coco with a second command line parameter)
-- 
--   A  Prints states of automaton
-- 
--   C  Generates complete compiler module
-- 
--   D  Suppresses Def Mod generation
--      NOTE: it is not released in Ada!
-- 
--   F  Prints start symbols and followers of nonterminals.
-- 
--   G  Prints the top-down graph.
-- 
--   I  Trace of start symbol set computation.
-- 
--   L  Forces a listing (otherwise a listing is only printed if errors are found).
--   
--   M  Suppresses FORWARD declarations in parser (for multipass compilers).
--      NOTE: it is not released in Ada!
-- 
--   N  Uses default names for symbol value constants.  This generates an
--      extra module <grammar name>G, and corresponding import statements
--      using constant names instead of numbers for symbols in parser and
--      scanner.
--      The constants are used unqualified and hence all needed constants
--      have to be imported; so a complete import list for these constants
--      is generated.
--      There is no decision whether a constant is actually needed.
-- 
--      The default conventions are (only terminals or pragmas can have names):
--      single character   -->  <ASCII name (lowercase)>Sym
--           eg. "+"       -->  plusSym
--      character string   -->  <string>Sym
--           eg. "PROGRAM" -->  PROGRAMSym
--      scanner token      -->  <token name>Sym
--           eg. ident     -->  identSym
-- 
--   O  Trace of follow set computation (not yet implemented).
-- 
--   P  Generates parser only
-- 
--   S  Prints the symbol list.
-- 
--   T  Suppresses generation of def and mod files (grammar tests only).
-- 
--   X  Prints a cross reference list.
-- 
-- ==========================================================================


with  CRC;        use   CRC;
with  CRT;        use   CRT;
with  CRA;        use   CRA;
with  CRP;        use   CRP;
with  CRS;        use   CRS;
with  CRX;        use   CRX;
with  FileIO;     use   FileIO;

with Ada.Command_Line;

procedure CR is

   ATGExt      : constant String := ".atg";
   LSTExt      : constant String := ".lst";
   Version     : constant String := "1.53.1";
   ReleaseDate : constant String := "11 January 2005";


   subtype  INT32    is FileIO.INT32;
   subtype  CARDINAL is FileIO.CARDINAL;


   Options     : String (1..64);
   GrammarName : String (1..64);
   ATGFileName : String (1..64);
   lstFileName : String (1..64);
   LL1         : Boolean;  -- TRUE, if grammar is LL(1)
   Ok          : Boolean;  -- TRUE, if grammar tests ok so far


   type  ErrDesc;
   type  Err is access ErrDesc;
   type  ErrDesc  is
      record
         nr    : Integer;
         line  : Integer;
         col   : Integer;
         next  : Err;
      end record;

   tab   : constant Character := Character'Val (8#11#);


   firstErr   : Err;
   lastErr    : Err;
   Extra      : Integer;



   --
   -- Store an error message for later printing
   --
   procedure Store_Error
     (nr    : in     Integer;
      line  : in     Integer;
      col   : in     Integer;
      pos   : in     INT32)
   is
      nextErr: Err;
   begin
      nextErr      := new ErrDesc;
      nextErr.nr   := nr;
      nextErr.line := line;
      nextErr.col  := col;
      nextErr.next := null;

      if  firstErr = null  then
         firstErr := nextErr;
      else
         lastErr.next := nextErr;
      end if;

      lastErr := nextErr;
      errors := errors + 1;
   end Store_Error;


   --
   -- Read a source line. Return empty line if eof
   --
   procedure Get_Line
     (pos   : in out INT32;
      line  :    out String;
      eof   :    out Boolean)
   is
      ch : Character;
      I  : CARDINAL;
   begin
      I   := line'First;   -- := 1
      eof := FALSE;
      ch  := Char_At (pos);
      pos := pos + 1;
      while  (ch /= FileIO.CR) and then
             (ch /= FileIO.LF) and then
             (ch /= FileIO.EOF)  loop
         line (I) := ch;
         I := I + 1;
         ch := Char_At (pos);
         pos := pos + 1;
      end loop;
      eof := (i = 1) and then (ch = FileIO.EOF);
      line (I) := Character'Val (8#0#);
      if  ch = FileIO.CR  then   -- check for MsDos (???)
         ch := Char_At (pos);
         if  ch = FileIO.LF  then
            pos := pos + 1;
            Extra := 0;
         end if;
      end if;
   end Get_Line;


   --
   -- Print an error message 
   --
   procedure Print_Err
     (line  : in     String;
      nr    : in     Integer;
      col   : in     Integer)
   is

      procedure Msg (S: in     String)
      is
      begin
         FileIO.Write_String (Listing, S);
      end Msg;

      procedure Pointer
      is
         I: Integer := line'First;
      begin
         FileIO.Write_String (Listing, "*****  ");
         while  I < (col + Extra - 2)  loop
            if  line (I) = tab  then 
               FileIO.Write (Listing, tab);
            else 
               FileIO.Write (Listing, ' ');
            end if;
            I := I + 1;
         end loop;
         FileIO.Write_String (Listing, "^ ");
      end Pointer;

   begin -- Print_Err
      Pointer;

      case  nr  is

         when   0 => Msg ("EOF expected");
         when   1 => Msg ("identifier expected");
         when   2 => Msg ("string expected");
         when   3 => Msg ("badstring expected");
         when   4 => Msg ("number expected");
         when   5 => Msg ("'COMPILER' expected");
         when   6 => Msg ("'PRODUCTIONS' expected");
         when   7 => Msg ("'=' expected");
         when   8 => Msg ("'.' expected");
         when   9 => Msg ("'END' expected");
         when  10 => Msg ("'CHARACTERS' expected");
         when  11 => Msg ("'TOKENS' expected");
         when  12 => Msg ("'NAMES' expected");
         when  13 => Msg ("'PRAGMAS' expected");
         when  14 => Msg ("'COMMENTS' expected");
         when  15 => Msg ("'FROM' expected");
         when  16 => Msg ("'TO' expected");
         when  17 => Msg ("'NESTED' expected");
         when  18 => Msg ("'IGNORE' expected");
         when  19 => Msg ("'CASE' expected");
         when  20 => Msg ("'+' expected");
         when  21 => Msg ("'-' expected");
         when  22 => Msg ("'..' expected");
         when  23 => Msg ("'ANY' expected");
         when  24 => Msg ("'CHR' expected");
         when  25 => Msg ("'(' expected");
         when  26 => Msg ("')' expected");
         when  27 => Msg ("'|' expected");
         when  28 => Msg ("'WEAK' expected");
         when  29 => Msg ("'[' expected");
         when  30 => Msg ("']' expected");
         when  31 => Msg ("'{' expected");
         when  32 => Msg ("'}' expected");
         when  33 => Msg ("'SYNC' expected");
         when  34 => Msg ("'CONTEXT' expected");
         when  35 => Msg ("'<' expected");
         when  36 => Msg ("'>' expected");
         when  37 => Msg ("'<.' expected");
         when  38 => Msg ("'.>' expected");
         when  39 => Msg ("'(.' expected");
         when  40 => Msg ("'.)' expected");
         when  41 => Msg ("not expected");
         when  42 => Msg ("invalid Token_Factor");
         when  43 => Msg ("invalid Factor");
         when  44 => Msg ("invalid Factor");
         when  45 => Msg ("invalid Term");
         when  46 => Msg ("invalid Symbol");
         when  47 => Msg ("invalid Single_Character");
         when  48 => Msg ("invalid Symbol_Set");
         when  49 => Msg ("invalid Name_Declaration");
         when  50 => Msg ("this symbol not expected in Token_Declaration");
         when  51 => Msg ("invalid Token_Declaration");
         when  52 => Msg ("invalid Attributes");
         when  53 => Msg ("invalid Declaration");
         when  54 => Msg ("invalid Declaration");
         when  55 => Msg ("invalid Declaration");
         when  56 => Msg ("this symbol not expected in CR");
         when  57 => Msg ("invalid CR");
         

         

         when 101    => Msg ("character set may not be empty");
         when 102    => Msg ("string literal may not extend over line end");
         when 103    => Msg ("a literal must not have attributes");
         when 104    => Msg ("this symbol kind not allowed in production");
         when 105    => Msg ("attribute mismatch between declaration and use");
         when 106    => Msg ("undefined string in production");
         when 107    => Msg ("name declared twice");
         when 108    => Msg ("this type not allowed on left side of production");
         when 109    => Msg ("earlier semantic action was not terminated");
         when 111    => Msg ("no production found for grammar name");
         when 112    => Msg ("grammar symbol must not have attributes");
         when 113    => Msg ("a literal must not be declared with a structure");
         when 114    => Msg ("semantic action not allowed here");
         when 115    => Msg ("undefined name");
         when 116    => Msg ("attributes not allowed in token declaration");
         when 117    => Msg ("name does not match grammar name");
         when 118    => Msg ("unacceptable constant value");
         when 119    => Msg ("may not ignore CHR(0)");
         when 120    => Msg ("token might be empty");
         when 121    => Msg ("token must not start with an iteration");
         when 122    => Msg ("comment delimiters may not be structured");
         when 123    => Msg ("only terminals may be weak");
         when 124    => Msg ("literal tokens may not contain white space");
         when 125    => Msg ("comment delimiter must be 1 or 2 characters long");
         when 126    => Msg ("character set contains more than one character");
         when 127    => Msg ("could not make deterministic automaton");
         when 128    => Msg ("semantic action text too long - please split it");
         when 129    => Msg ("literal tokens may not be empty");
         when 130    => Msg ("IGNORE CASE must appear earlier");
         when others => Msg ("Error: "); FileIO.Write_Int (Listing, nr, 1);
      end case;

      FileIO.Write_Ln (Listing);
   end Print_Err;


   --
   -- Print a source listing with error messages 
   --
   procedure Print_Listing
   is
      nextErr  : Err;
      eof      : Boolean;
      lnr      : Integer;
      errC     : Integer;
      srcPos   : INT32;
      line     : String (1..256);
   begin
      FileIO.Write_String (Listing, "Listing:");
      FileIO.Write_Ln     (Listing); 
      FileIO.Write_Ln     (Listing);
      srcPos  := 0; 
      nextErr := firstErr;
      Get_Line (srcPos, line, eof); 
      lnr := 1; 
      errC := 0;

      while  not eof  loop
         FileIO.Write_Int    (Listing, lnr, 5); 
         FileIO.Write_String (Listing, "  ");
         FileIO.Write_String (Listing, line); 
         FileIO.Write_Ln     (Listing);
         while  (nextErr /= null) and then (nextErr.line = lnr)  loop
            Print_Err (line, nextErr.nr, nextErr.col);
            errC := errC + 1;
            nextErr := nextErr.next;
         end loop;
         Get_Line (srcPos, line, eof);
         lnr := lnr + 1;
      end loop;

      if  nextErr /= null  then
         FileIO.Write_Int (Listing, lnr, 5); 
         FileIO.Write_Ln  (Listing);
         while  nextErr /= null  loop
            Print_Err (line, nextErr.nr, nextErr.col);
            errC := errC + 1;
            nextErr := nextErr.next;
         end loop;
      end if;

      FileIO.Write_Ln     (Listing);
      FileIO.Write_Int    (Listing, errC, 5); 
      FileIO.Write_String (Listing, " error");
      if  errC /= 1  then 
         FileIO.Write (Listing, 's');
      end if;

      FileIO.Write_Ln (Listing); 
      FileIO.Write_Ln (Listing); 
      FileIO.Write_Ln (Listing);
   end Print_Listing;


   --
   -- Set compiler options 
   --
   procedure Set_Option (S: in     String)
   is
      C: Character;
   begin
      for  I in S'Range  loop
         exit when s (I) = Character'Val (8#0#);
         C := Up_Case (S (I));
         if  (C >= 'A') and then (C <= 'Z')  then 
            CRT.ddt (C) := TRUE;
         end if;
      end loop;
   end Set_Option;


   procedure Msg (S: in     String)
   is
   begin
      FileIO.Write_String (FileIO.StdOut, S); 
      FileIO.Write_Ln     (FileIO.StdOut);
   end Msg;


   ------------------------------ Help ----------------------------------
   procedure Help
   is
   begin
      Msg ("Usage: COCOR [-Options] [Grammar_Name] [-Options]");
      Msg ("Example: COCOR -mcs Test");
      Msg ("");
      Msg ("Options are");
      Msg ("a  - Trace automaton");
      Msg ("c  - Generate compiler module");
      -- Msg ("d  - Suppress generation of Definition Modules");
      Msg ("f  - Give Start and Follower sets");
      Msg ("g  - Print top-down graph");
      Msg ("i  - Trace start set computations");
      Msg ("l  - Force listing");
      -- Msg ("m  - (Multipass) Suppress FORWARD declarations");
      Msg ("n  - Generate symbolic names");
      Msg ("p  - Generate parser only");
      Msg ("s  - Print symbol table");
      Msg ("t  - Grammar tests only - no code generated");
      Msg ("x  - Print cross reference list");
      Msg ("");
      Msg ("Frame files: " &
           '"' & "compiler.frm" & '"' & ", " &
           '"' & "scanner.frm"  & '"' & " and " &
           '"' & "parser.frm"   & '"');
      Msg ("must be in the working directory or on the path specified");
      Msg ("by the environment variable " & '"' & "CRFRAMES" & '"');
   end Help;


begin -- CR
   firstErr := null; 
   Extra := 1;

   FileIO.Write_String (FileIO.StdOut, "Coco/R (Linux) - Compiler-Compiler V-");
   FileIO.Write_String (FileIO.StdOut, Version);
   FileIO.Write_Ln (FileIO.StdOut);
   FileIO.Write_String (FileIO.StdOut, "Released by O.Havva ");
   FileIO.Write_String (FileIO.StdOut, ReleaseDate);
   FileIO.Write_Ln (FileIO.StdOut);
   FileIO.Next_Parameter (GrammarName);

   if  (GrammarName (1) = '?') or else
       (((GrammarName (1) = '-') or else
         (GrammarName (1) = '/')) and then (GrammarName (2) = '?'))  then
      Help; 
      FileIO.Write_Ln (FileIO.StdOut);
      FileIO.Quit_Execution;
   end if;

   if  GrammarName (1) = ' '  then
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_String (FileIO.StdOut, "Input Grammar_Name not present");
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_String (FileIO.StdOut, "COCOR ? gives short help screen");
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Quit_Execution;
   end if;

   while  (GrammarName (1) = '-') or else (GrammarName (1) = '/')  loop
      -- accept options before filename 
      Set_Option (GrammarName); 
      FileIO.Next_Parameter (GrammarName);
   end loop;
   
   Ok := GrammarName (1) /= ' ';
   if  Ok  then
      FileIO.Append_Extension (GrammarName, ATGExt, ATGFileName);
      GrammarName := ATGFileName;
      FileIO.Open (Source, GrammarName, FALSE);
      if  not FileIO.Okay  then
         FileIO.Write_String (FileIO.StdOut, "File <");
         FileIO.Write_String (FileIO.StdOut, GrammarName);
         FileIO.Write_String (FileIO.StdOut, "> not found.");
         FileIO.Write_Ln     (FileIO.StdOut);
         FileIO.Quit_Execution;
      end if;
   else
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_String (FileIO.StdOut, "Input Grammar_Name not present");
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_String (FileIO.StdOut, "COCOR ? gives short help screen");
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Quit_Execution;
   end if;
   
   -- accept options after filename 
   FileIO.Next_Parameter (Options);
   if  Options (1) /= ' '  then 
      Set_Option (Options);
   end if;

   FileIO.Extract_Directory
     (fullName  => GrammarName,
      directory => directory);
   FileIO.Change_Extension
     (oldName => GrammarName,
      ext     => LSTExt,
      newName => lstFileName);

   FileIO.Open
     (f        => Listing,
      fileName => lstFileName,
      newFile  => TRUE);

   FileIO.Write_String (Listing, "Coco/R (Linux) - Compiler-Compiler V-");
   FileIO.Write_String (Listing, Version);
   FileIO.Write_Ln     (Listing);
   FileIO.Write_String (Listing, "Released by O.Havva ");
   FileIO.Write_String (Listing, ReleaseDate);
   FileIO.Write_Ln     (Listing);
   FileIO.Write_String (Listing, "Source file: ");
   FileIO.Write_String (Listing, GrammarName);
   FileIO.Write_Ln     (Listing);
   FileIO.Write_Ln     (Listing);

   FileIO.Write_Ln     (FileIO.StdOut);
   FileIO.Write_String (FileIO.StdOut, "parsing file ");
   FileIO.Write_String (FileIO.StdOut, GrammarName);
   FileIO.Write_Ln     (FileIO.StdOut);

   CRS.Error := Store_Error'Unrestricted_Access;

   CRP.Parse;

   if  errors = 0  then
      Msg ("testing grammar");
      FileIO.Write_String (Listing, "Grammar Tests:");
      FileIO.Write_Ln     (Listing); 
      FileIO.Write_Ln     (Listing);
      CRT.Compute_Symbol_Sets;
      CRT.Test_Completeness (Ok);
      if  Ok  then CRT.Test_If_All_Nt_Reached     (Ok); end if;
      if  Ok  then CRT.Find_Circular_Productions  (Ok); end if;
      if  Ok  then CRT.Test_If_Nt_Reduced_to_Term (Ok); end if;
      if  Ok  then CRT.LL1_Test                   (LL1); end if;
      FileIO.Write_Ln (Listing);
      if  not Ok or else not LL1 or else CRT.ddt ('L') or else CRT.ddt ('X')  then
         Msg ("listing");
         Print_Listing; 
         if  CRT.ddt ('X')  then
            CRT.XRef;
         end if;
      end if;
      if  CRT.ddt ('N') or else CRT.symNames  then
         Msg ("symbol name assignment");
         CRT.Assign_Symbol_Names (CRT.ddt ('N'), CRT.symNames);
      end if;
      if  Ok and then not CRT.ddt ('T')  then
         Msg ("generating parser");
         CRX.Gen_Compiler;
         if  CRT.genScanner and then not CRT.ddt ('P')  then
            Msg ("generating scanner");
            CRA.Write_Scanner (Ok);
            if  CRT.ddt ('A')  then
               CRA.Print_Automaton_States;
            end if;
         end if;
         if  CRT.ddt ('C')  then
            Msg ("generating compiler");
            CRC.Write_Driver;
         end if;
         CRX.Write_Statistics;
      end if;
      if  not Ok  then 
         Msg ("Compilation ended with errors in grammar tests.");
      elsif  not LL1  then 
         Msg ("Compilation ended with LL(1) errors.");
      else 
         Msg ("Compilation completed. No errors detected.");
      end if;
   else
      Msg ("listing");
      Print_Listing; 
      if  CRT.ddt ('X')  then
         CRT.XRef;
      end if;
      Msg ("*** errors detected ***");
   end if;

   if  CRT.ddt ('G')  then
      CRT.Print_Graph;
   end if;
   if  CRT.ddt ('S')  then
      CRT.Print_Symbol_Table;
   end if;
   FileIO.Close (Listing); 
   FileIO.Close (Source);
end CR;





