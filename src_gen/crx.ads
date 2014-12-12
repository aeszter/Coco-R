-----------------------
--                   --
-- Parser Generation --
--                   --
-----------------------

-- for private part
--
with  CRT;
with  FileIO;
with  Sets;


package CRX is

   --
   -- Generates the target compiler (parser).
   --
   procedure Gen_Compiler;

   --
   -- Writes statistics about compilation to list file.
   --
   procedure Write_Statistics;

--------------------------------------------------------------------
private

--CONST'S
   symSetSize  : constant := 100;   -- max.number of symbol sets of the generated parser
   maxTerm     : constant :=   5;   -- sets of size < maxTerm are enumerated
   maxAlter    : constant :=   5;   -- more than maxAlter alternatives are handled with
                                    -- a case statement

   -- kinds of generated error messages --

   tErr        : constant :=   0;   -- unmatched terminal symbol
   altErr      : constant :=   1;   -- unmatched alternatives
   syncErr     : constant :=   2;   -- error reported at synchronization point


--TYPE'S
   subtype  INT32    is FileIO.INT32;
   subtype  CARDINAL is FileIO.CARDINAL;
   subtype  BITSET   is Sets.BitSet;


--VAR'S
   symSet   :  array (Integer range 0 .. symSetSize) of CRT.CRT_Set;
               -- symbol sets in the generated parser

   maxSS    :  Integer;       -- number of symbol sets
   errorNr  :  Integer;       -- number of last generated error message
   curSy    :  Integer;       -- symbol whose production is currently generated

   The_Err_Msgs_File :  FileIO.File;   -- output: error message texts
   The_Frame_File    :  FileIO.File;   -- input:  parser frame parser.frm
   The_Parser_File   :  FileIO.File;   -- output: generated parser

   Is_New_Line       :  Boolean;
   IndDisp  :  Integer;





end CRX;
