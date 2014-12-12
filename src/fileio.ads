-- This module attempts to provide several potentially non-portable
-- facilities for Coco/R.
--
-- (a)  A general file input/output module, with all routines required for
--      Coco/R itself, as well as several other that would be useful in
--      Coco-generated applications.
-- (b)  Definition of the "Long_Integer" type needed by Coco.
-- (c)  Some conversion functions to handle this long type.
-- (d)  Some "long" and other constant literals that may be problematic
--      on some implementations.
-- (e)  Some string handling primitives needed to interface to a variety
--      of known implementations.
--
-- The intention is that the rest of the code of Coco and its generated
-- parsers should be as portable as possible.  Provided the definition
-- module given, and the associated implementation, satisfy the
-- specification given here, this should be almost 100% possible (with
-- the exception of a few constants, avoid changing anything in this
-- specification).
--
-- FileIO is based on code by MB 1990/11/25; heavily modified and extended
-- by PDT and others between 1992/1/6 and the present day.

with Ada.Unchecked_Deallocation;

package FileIO is


-- TYPES
   Buffer_Error : exception;

   type File is private;

   subtype INT32     is Long_Integer;        -- This may require a special import;
                                             -- on 32 bit systems INT32 = Integer
                                             -- may even suffice.

   subtype CARDINAL  is Natural;

   type  String_Ptr is access all String;
   procedure Free_String is new Ada.Unchecked_Deallocation
                                   (Object => String,
                                    Name   => String_Ptr);


-- CONSTANTS

   EOF : constant Character := Character'Val (8#0#);
   -- FileIO.Read returns EOF when eof is reached.

   EOL : constant Character := Character'Val (8#36#);
   -- FileIO.Read maps line marks onto EOL
   -- FileIO.Write maps EOL onto cr, lf, or cr/lf
   -- as appropriate for filing system.

   ESC : constant Character := Character'Val (8#33#);
   -- Standard ASCII escape.

   CR  : constant Character := Character'Val (8#15#);
   -- Standard ASCII carriage return.

   LF  : constant Character := Character'Val (8#12#);
   -- Standard ASCII line feed.

   BS  : constant Character := Character'Val (8#10#);
   -- Standard ASCII backspace.

   DEL : constant Character := Character'Val (8#177#);
   -- Standard ASCII DEL (rub-out).



   BITSET_Size : constant := 16;    -- number of bits actually used in BITSET type
   Name_Length : constant := 256;   -- (is it needs ???)



   -------------------------
   -- Filename extentions --
   -------------------------

   FrmExt : constant String := ".frm"; -- supplied frame files have this extension.
   TxtExt : constant String := ".txt"; -- generated text files may have this extension.
   ErrExt : constant String := ".err"; -- generated error files may have this extension.
   PasExt : constant String := ".pas"; -- generated Pascal units have this extension.
   DefExt : constant String := ".def"; -- generated definition modules have this extension.
   ModExt : constant String := ".mod"; -- generated implementation/program modules have this
                                       -- extension.
   -- GNAT Ada files extentions
   SpecExt: constant String := ".ads"; -- Ada Unit Specification
   BodyExt: constant String := ".adb"; -- Ada Unit Body


   PathSep: constant Character := ':'; -- separate components in path environment variables
                                       -- DOS = ";"  UNIX = ":"
   DirSep : constant Character := '/'; -- separate directory element of file specifiers
                                       -- DOS = "\"  UNIX = "/"

-- VARIABLES

   Okay   : Boolean;     -- Status of last I/O operation


   StdIn    : constant File;
   StdOut   : constant File;
   StdErr   : constant File;

   EOFChar  : Character ; -- Signal EOF interactively



   --------------------------------------------------------------------------
   -- The following routines provide access to command line parameters and --
   -- the environment.                                                     --
   --------------------------------------------------------------------------

   --
   -- Extracts next parameter from command line.
   -- Returns empty string (S = "") if no further parameter can be found.
   --
   procedure Next_Parameter (S: in out String);



   ---------------------------------------------------------------------------
   -- The following routines provide a minimal set of file opening routines --
   -- and closing routines.                                                 --
   ---------------------------------------------------------------------------

   --
   -- Opens file F whose full name is specified by fileName.
   -- Opening mode is specified by newFile:
   --     TRUE:  the specified file is opened for output only.  Any existing
   --            file with the same name is deleted.
   --    FALSE:  the specified file is opened for input only.
   -- FileIO.Okay indicates whether the file F has been opened successfully.
   --
   procedure Open
     (F       : in out File;
      fileName: in     String;
      newFile : in     Boolean);

   --
   -- As for Open, but tries to open file of given fileName by searching each
   -- directory specified by the environment variable named by envVar.
   --
   procedure Search_File
     (F       : in out File;
      envVar  : in     String;
      fileName: in     String;
      newFile : in     Boolean);

   --
   -- Closes file F.  F becomes NIL.
   -- If possible, Close should be called automatically for all files that
   -- remain open when the application terminates.  This will be possible on
   -- implementations that provide some sort of termination or at-exit
   -- facility.
   --
   procedure Close (F: in     File);



   -----------------------------------------------------------------------------
   -- The following routines provide a minimal set of file name manipulation  --
   -- routines.  These are modelled after MS-DOS conventions, where a file    --
   -- specifier is of a form exemplified by D:\DIR\SUBDIR\PRIMARY.EXT         --
   -- Other conventions may be introduced; these routines are used by Coco to --
   -- derive names for the generated modules from the grammar name and the    --
   -- directory in which the grammar specification is located.                --
   -----------------------------------------------------------------------------

   --
   -- Extracts D:\DIRECTORY\ portion of fullName.
   --
   procedure Extract_Directory
     (fullName : in     String;
      directory: in out String);

   --
   -- Constructs newName as complete file name by appending ext to oldName
   -- if it doesn't end with "."  Examples: (assume ext = "EXT")
   --       old.any ==> OLD.EXT
   --       old.    ==> OLD.
   --       old     ==> OLD.EXT
   -- This is not a file renaming facility, merely a string manipulation
   -- routine.
   --
   procedure Append_Extension
     (oldName: in     String;
      ext    : in     String;
      newName:    out String);

   --
   -- Constructs newName as a complete file name by changing extension of
   -- oldName to ext.  Examples: (assume ext = "EXT")
   --       old.any ==> OLD.EXT
   --       old.    ==> OLD.EXT
   --       old     ==> OLD.EXT
   -- This is not a file renaming facility, merely a string manipulation
   -- routine.
   --
   procedure Change_Extension
     (oldName: in     String;
      ext    : in     String;
      newName:    out String);



   --------------------------------------------------------------------------------
   -- The following routines provide a minimal set of file positioning routines. --
   -- Others may be introduced, but at least these should be implemented.        --
   -- Success of each operation is recorded in FileIO.Okay.                      --
   --------------------------------------------------------------------------------

   --
   -- Returns length of file F.
   --
   function Length (F: in     File) return INT32;

   --
   -- Returns the current read/write position in F.
   --
   function Get_Pos (F: in     File) return INT32;

   --
   -- Sets the current position for F to pos.
   --
   procedure Set_Pos
     (F  : in     File;
      pos: in     INT32);




   -------------------------------------------------------------------------
   -- The following routines provide a minimal set of input routines.     --
   -- Others may be introduced, but at least these should be implemented. --
   -- Success of each operation is recorded in FileIO.Okay.               --
   -------------------------------------------------------------------------

   --
   -- TRUE if F is currently at the end of file.
   --
   function End_Of_File (F: in     File) return Boolean;

   --
   -- Reads a character ch from file F.
   -- Maps filing system line mark sequence to FileIO.EOL.
   --
   procedure Read
     (F : in     File;
      C :    out Character);

   --
   -- Reads to start of next line on file F, or to end of file if no next
   -- line.  Skips to, and consumes next line mark.
   --
   procedure Read_Ln (F: in     File);

   --
   -- Reads a string of characters from file F.
   -- Leading blanks are skipped, and str is delimited by line mark.
   -- Line mark is not consumed.
   --
   procedure Read_String
     (F : in     File;
      S : in out String);

   --
   -- Reads a string of characters from file F.
   -- Leading blanks are not skipped, and str is terminated by line mark or
   -- control character, which is not consumed.
   --
   procedure Read_Line
     (F : in     File;
      S : in out String);

   --
   -- Attempts to read len bytes from the current file position into buf.
   -- After the call, len contains the number of bytes actually read.
   --
   procedure Read_Bytes
     (F  : in     File;
      buf:    out String;
      len: in out CARDINAL);



   -------------------------------------------------------------------------
   -- The following routines provide a minimal set of output routines.    --
   -- Others may be introduced, but at least these should be implemented. --
   -------------------------------------------------------------------------

   --
   -- Writes a character C to file F.
   -- If C = FileIO.EOL, writes line mark appropriate to filing system.
   --
   procedure Write
     (F : in   File;
      C : in   Character);

   --
   -- Skips to the start of the next line on file F.
   -- Writes line mark appropriate to filing system.
   --
   procedure Write_Ln (F: File);

   --
   -- Writes entire string str to file F.
   --
   procedure Write_String
     (F : in   File;
      S : in   String);

   --
   -- Writes text to file F.
   -- At most len characters are written.  Trailing spaces are introduced
   -- if necessary (thus providing left justification).
   --
   procedure Write_Text
     (F   : in   File;
      text: in   String;
      len : in   Integer);

   --
   -- Writes an Integer int into a field of wid characters width.
   -- If the number does not fit into wid characters, wid is expanded.
   -- If wid = 0, exactly one leading space is introduced.
   --
   procedure Write_Int
     (F     : in   File;
      Item  : in   Integer;
      Width : in   Natural);

   --
   -- Writes a CARDINAL card into a field of wid characters width.
   -- If the number does not fit into wid characters, wid is expanded.
   -- If wid = 0, exactly one leading space is introduced.
   --
   procedure Write_Card
     (F     : in   File;
      Item  : in   CARDINAL;
      Width : in   CARDINAL);

--    ???
--    --
--    -- Writes len bytes from buf to F at the current file position.
--    --
--    procedure WriteBytes
--      (F: File;
--       VAR buf: ARRAY OF SYSTEM.BYTE;
--       len: CARDINAL);



   --------------------------------------------------------------------------
   -- The following procedures are a minimal set used within Coco for      --
   -- string manipulation.  They almost follow the conventions of the ISO  --
   -- routines, and are provided here to interface onto whatever Strings   --
   -- library is available.  On ISO compilers it should be possible to     --
   -- implement most of these with CONST declarations, and even replace    --
   -- SLENGTH with the pervasive function LENGTH at the points where it is --
   -- called.                                                              --
   --                                                                      --
   -- CONST                                                                --
   --   SLENGTH = Strings.Length;                                          --
   --   Assign  = Strings.Assign;                                          --
   --   Extract = Strings.Extract;                                         --
   --   Concat  = Strings.Concat;                                          --
   --                                                                      --
   --------------------------------------------------------------------------

   --
   -- Returns number of characters in stringVal, not including nul
   --
   function SLENGTH (S: String) return CARDINAL;

   --
   -- Copies as much of source to destination as possible, truncating if too
   -- long, and nul terminating if shorter.
   -- Be careful - some libraries have the parameters reversed!
   --
   procedure Assign
     (source     : in     String;
      destination: in out String);

   --
   -- Extracts at most numberToExtract characters from source[startIndex]
   -- to destination.  If source is too short, fewer will be extracted, even
   -- zero perhaps
   --
   procedure Extract
     (source         : in     String;
      startIndex     : in     CARDINAL;
      numberToExtract: in     CARDINAL;
      destination    : in out String);

   --
   -- Concatenates stringVal1 and stringVal2 to form destination.
   -- Nul terminated if concatenation is short enough, truncated if it is
   -- too long
   --
   procedure Concat
     (stringVal1 : in     String;
      stringVal2 : in     String;
      destination: in out String);

   --
   -- Returns -1, 0, 1 depending whether stringVal1 < = > stringVal2.
   -- This is not directly ISO compatible
   --
   function Compare
     (stringVal1: in     String;
      stringVal2: in     String)
     return Integer;

   --------------------------------------------------------------------------
   -- The following routineses present oversimplified Character and String --
   -- case manipulation. Really they are support for ASCII character set.  --
   --------------------------------------------------------------------------

   --
   -- Returns Character C in Low Case
   --
   function Lo_Case (C: Character) return Character;

   --
   -- Returns Character C in Hight Case
   --
   function Up_Case (C: Character) return Character;

   --
   -- Returns string S in Low Case
   --
   function Lo_Case (S: String) return String;

   --
   -- Returns string S in Hight Case
   --
   function Up_Case (S: String) return String;


   ------------------------------------------------------------------------------
   -- The following routines are for conversions to and from the INT32 type.   --
   -- Their names are modelled after the ISO pervasive routines that would     --
   -- achieve the same end.  Where possible, replacing calls to these routines --
   -- by the pervasives would improve performance markedly.  As used in Coco,  --
   -- these routines should not give range problems.                           --
   ------------------------------------------------------------------------------

   --
   -- Convert long integer n to corresponding (short) cardinal value.
   -- Potentially FileIO.ORDL(n) = VAL(CARDINAL, n)
   --
   function ORDL (n: in     INT32) return CARDINAL;

   --
   -- Convert long integer n to corresponding short integer value.
   -- Potentially FileIO.INTL(n) = VAL(Integer, n)
   --
   function INTL (n: in     INT32) return Integer;

   --
   -- Convert cardinal n to corresponding long integer value.
   -- Potentially FileIO.INT(n) = VAL(INT32, n)
   --
   function INT (n: in     CARDINAL) return INT32;

   --
   -- Close all files and halt execution.
   -- On some implementations QuitExecution will be simply implemented as HALT
   --
   procedure Quit_Execution;

private

   type File is range -2 .. 32;

   StdIn    : constant File := 0;
   StdOut   : constant File := -1;
   StdErr   : constant File := -2;

end FileIO;


