--
-- This is an example of a rudimentary main module for use with COCO/R.
-- It assumes FileIO library will be available.
-- The auxiliary modules <Grammar>S (scanner) and <Grammar>P (parser)
-- are assumed to have been constructed with COCO/R compiler generator.
--

with  -->Parser;        use   -->Parser;
with  -->Scanner;        use   -->Scanner;

with  FileIO;     use   FileIO;


procedure -->Grammar is

   subtype  INT32    is FileIO.INT32;
   subtype  CARDINAL is FileIO.CARDINAL;


   type  ErrDesc;
   type  Err is access ErrDesc;
   type  ErrDesc  is
      record
         nr    : Integer;
         line  : Integer;
         col   : Integer;
         next  : Err;
      end record;

   tab      : constant Character := Character'Val (8#11#);

   firstErr : Err;
   lastErr  : Err;
   Extra    : Integer;


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
      if  ch = FileIO.CR  then   -- check for MsDos
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

         -->Errors

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


   Source_File_Name  : String (1..256);
   Listing_File_Name : String (1..256);

begin -- CR
   firstErr := null; 
   Extra := 1;

   -- check on correct parameter usage
   FileIO.Next_Parameter (Source_File_Name);
   if  Source_File_Name (1) = ' '  then
      FileIO.Write_String (FileIO.StdOut, "No input file specified");
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Qiot_Execution;
   end if;

   -- open the source file with "Source_File_Name"
   FileIO.Open (Source, Source_File_Name, FALSE);
   if  not FileIO.Okay  then
      FileIO.Write_String (FileIO.StdOut, "Could not open input file");
      FileIO.Write_Ln     (FileIO.StdOut);
      FileIO.Qiot_Execution;
   end if;

   -- open the output file for source listing
   FileIO.Append_Extension (Source_File_Name, ".lst", Listing_File_Name);
   FileIO.Open (Listing, Listing_File_Name, TRUE);
   if  not FileIO.Okay  then
      FileIO.Write_String (FileIO.StdOut, "Could not open listing file");
      FileIO.Write_Ln     (FileIO.StdOut);
      -- in this case default listing will go to the screen
      Listing := FileIO.StdOut;
   end if;


   -- install error reporting procedure
   -->Scanner.Error := Store_Error'Unrestricted_Access;

   -- instigate the compilation
   FileIO.Write_String (FileIO.StdOut, "Parsing");
   FileIO.Write_Ln     (FileIO.StdOut);
   Parse;

   -- generate the source listing
   Print_Listing;
   FileIO.Close (Listing); 
    
   -- examine the outcome
   if  not Successful  then
      FileIO.Write_String (FileIO.StdOut, "Incorrect source");
   else
      FileIO.Write_String (FileIO.StdOut, "Parsed correctly");
      -----   Add further activities if required   -----
   end if;
   FileIO.Close (Source);

end -->Grammar;


