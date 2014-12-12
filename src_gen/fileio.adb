--
-- This package attempts to provide several low level facilities
-- for Coco/R.
--
-- Note: this release uses some services from FLORIST
--       (standard POSIX 1003.5 binding for Ada).
--
--       However, in case if You don't want to use FLORIST
--       or if You have no FLORIST at all, it is possible
--       to make a bit code modifications to do so.
--       I hope it will be not so hard :-)
--


with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with Interfaces.C;
with Interfaces.C_Streams;
with Ada.Text_IO.C_Streams;
with POSIX.IO;
with POSIX.Process_Primitives;
with GNAT.OS_Lib;

package body FileIO is

   pragma Linker_Options ("-lflorist");



   -------------------   Internal File IO Datas   --------------------

   type Open_Mode is (Read_Mode, Write_Mode, Not_Open);

   Buf_Len     : constant Natural := 1024;   -- buffer size
   Rec_Len     : constant Natural := 256;    -- minimum buffer filling degree
   Buf_Middle  : constant Natural := 512;    -- border for buffer transfer
   Buf_Switch  : constant Natural := 768;    -- maximum buffer filling degree
   -- When the input buffer has been filled up to the limit Buf_Switch,
   -- the part from Buf_Middle up to Buf_Switch (2nd half of buffer)
   -- is copied to the begin of buffer. So there are always Rec_Len
   -- characters minimum in the buffer for possible Un_Read operations
   -- (of course, only if so many characters have already been read in).


   subtype T_Disk_File is File range 1 .. File'Last;

   subtype T_Buf_Pos is Natural range 0 .. Buf_Len;

   subtype T_Buffer is String (1 .. Buf_Len);
   type T_Buffer_Ptr is access T_Buffer;

   type T_Filedata is
      record
         File_Mode   : Open_Mode;
         Buffer      : T_Buffer_Ptr;  -- input/output buffer
         Act_Char    : T_Buf_Pos;
         -- actual (normally: last delivered) input character
         Last_Char   : T_Buf_Pos;
         -- last character read/written into input/output buffer
         End_Of_File : Boolean;
      end record;

   FDL : array (File) of T_Filedata := ((Write_Mode, null, 0, 0, False),
                                          (Write_Mode, null, 0, 0, False),
                                          (Read_Mode , null, 0, 0, False),
                                          others => (Not_Open, null, 0, 0, False));

   Filedesclist : array (T_Disk_File) of Ada.Text_IO.File_Type;



   ------------------   Internal File IO Routines   ------------------

   procedure Move_Buffer (F : File) is
      -- if the last character is in the last quarter of the buffer,
      -- move the second half of the buffer into the first half
      No_Chars : Natural;
   begin
      if FDL (F).Last_Char >= Buf_Switch then
         -- reached last quarter of buffer:
         -- move 2nd half into 1st half
         No_Chars := FDL (F).Last_Char - Buf_Middle;
         FDL (F).Buffer (1 .. No_Chars) :=
               FDL (F).Buffer (Buf_Middle + 1 .. Buf_Middle + No_Chars);
         FDL (F).Act_Char  := FDL (F).Act_Char - Buf_Middle;
         FDL (F).Last_Char := FDL (F).Last_Char - Buf_Middle;
      end if;
   end Move_Buffer;

   procedure Read_Buffer (F : File) is
      -- read the next line from input file into the buffer
      -- !!! We assume:
      -- (1) get_line sets Last_Char to the actual buffer index pointing
      --     to the position where the last character has been transferred to.
      -- (2) get_line sets Last_Char to the buffer start position minus 1 if
      --     the actual line contains only LF.
      -- (3) get_line reads a line without END_ERROR if there is
      --     at least one character (with or without LF) before EOF.
      Last_Char : Natural;
   begin
      Move_Buffer (F);
      if F = StdIn then
         Ada.Text_IO.Get_Line
           (FDL (F).Buffer (FDL (F).Last_Char + 1 .. FDL (F).Last_Char + Rec_Len),
            Last_Char);
      else
         Ada.Text_IO.Get_Line
           (Filedesclist (F),
            FDL (F).Buffer (FDL (F).Last_Char + 1 .. FDL (F).Last_Char + Rec_Len),
            Last_Char);
      end if;
      if Last_Char - FDL (F).Last_Char < Rec_Len then
         -- buffer not completely filled, i.e. LF occurred (but not transferred)
         Last_Char := Last_Char + 1;
         FDL (F).Buffer (Last_Char) := LF;
      end if;
      FDL (F).Last_Char := Last_Char;
   exception
      when Ada.Text_IO.End_Error =>
         FDL (F).End_Of_File := True;
         raise;
   end Read_Buffer;

   -- makes last read character unread from F.
   -- The next Read call begins its reading at this unread character
   --   where F.<File_Mode> = Not_Open => raise STATUS_ERROR,
   --         F.<File_Mode> = Write_Mode => raise MODE_ERROR,
   --         in (Buffer_Pos (F) = Buf_Pos'first)  => raise BUFFER_ERROR;
   procedure Un_Read (F : File) is
   begin
      if FDL (F).File_Mode = Write_Mode then
         raise Ada.Text_IO.Mode_Error;
      elsif FDL (F).File_Mode = Not_Open then
         raise Ada.Text_IO.Status_Error;
      elsif FDL (F).Act_Char = 0 then
         raise Buffer_Error;
      end if;
      FDL (F).Act_Char    := FDL (F).Act_Char - 1;
      FDL (F).End_Of_File := False;
   end Un_Read;

   -- writes buffer of F to disk
   --   where F.<File_Mode> = Not_Open => raise STATUS_ERROR,
   --         F.<File_Mode> = Read_Mode => raise MODE_ERROR;
   procedure Write_Flush (F : File := StdOut) is
   begin
      if F = StdOut or F = StdErr then
         return;
      elsif FDL (F).File_Mode = Read_Mode then
         raise Ada.Text_IO.Mode_Error;
      elsif FDL (F).File_Mode = Not_Open then
         raise Ada.Text_IO.Status_Error;
      elsif FDL (F).Last_Char > 0 then
         Ada.Text_IO.Put
           (Filedesclist (F),
            FDL (F).Buffer (1 .. FDL (F).Last_Char));
         FDL (F).Last_Char := 0;
      end if;
   end Write_Flush;


   -- returns the state of file F.
   function Is_Open (F: File) return Open_Mode is
   begin
      return FDL (F).File_Mode;
   end Is_Open;

   -------------------------------------------------------------------




   --
   -- Extracts next parameter from command line.
   -- Returns empty string (S = "") if no further parameter can be found.
   --
   Param : CARDINAL := 0;
   numPar: CARDINAL := Ada.Command_Line.Argument_Count;
   procedure Next_Parameter (S: in out String)
   is
   begin
      Param := Param + 1;
      if  Param <= numPar  then
         Ada.Strings.Fixed.Move
           (Source => Ada.Command_Line.Argument (Param),
            Target => S,
            Pad    => ' ');
      else
         Ada.Strings.Fixed.Move
           (Source => "",
            Target => S,
            Pad    => ' ');
      end if;
   end Next_Parameter;
   
   -- --
   -- -- Extracts parameters from command line.
   -- -- Returns Command_Name when Number = 0
   -- -- Returns empty string (S = "") if no further parameter can be found.
   -- --
   -- function Param_Str (Number : in     Natural) return String
   -- is
   -- begin
   --    if  Number = 0  then
   --       return Ada.Command_Line.Command_Name;
   --    elsif Number <= Ada.Command_Line.Argument_Count  then
   --       return Ada.Command_Line.Argument (Number);
   --    else
   --       return "";
   --    end if;
   -- end Param_Str;

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
      newFile : in     Boolean)
   is
      File_Name : constant String :=
            Ada.Strings.Fixed.Trim
              (Source => fileName,
               Side   => Ada.Strings.Both);

      -- opens an input file File_Name for read-only access
      -- and returns an identifier of the file
      --   where No_Valid_File (File_Name) => raise NAME_ERROR,
      --         Is_Open (File_Name) /= Not_Open => raise STATUS_ERROR,
      --         File_Protection_Error (File_Name) => raise USE_ERROR,
      --         in (Open_Files = File'last) => raise USE_ERROR,
      --         return F : File => out (F.<File_Mode> = Read_Mode);
      function Read_Open return File is
      begin
         for F in T_Disk_File loop
            if FDL (F).File_Mode = Not_Open then
               begin
                  Ada.Text_IO.Open
                    (Filedesclist (F),
                     Ada.Text_IO.In_File,
                     File_Name);
                  FDL (F).File_Mode := Read_Mode;
                  if FDL (F).Buffer = null then
                     FDL (F).Buffer := new T_Buffer;
                  else
                     FDL (F).Act_Char    := 0;
                     FDL (F).Last_Char   := 0;
                     FDL (F).End_Of_File := False;
                  end if;
                  Okay := TRUE;
               exception
                  when Ada.Text_IO.Name_Error =>
                     Okay := FALSE;
               end;
               return F;
            end if;
         end loop;
         -- too many files open
         raise Ada.Text_IO.Use_Error;
      end Read_Open;

      -- opens a new or existing output file File_Name for write-only access
      -- (for an existing file: to write from the beginning, deleting old contents)
      -- and returns an identifier of the file
      --   where No_Valid_File (File_Name) => raise NAME_ERROR,
      --         Is_Open (File_Name) /= Not_Open => raise STATUS_ERROR,
      --         File_Protection_Error (File_Name) => raise USE_ERROR,
      --         in (Open_Files = File'last) => raise USE_ERROR,
      --         return F : File => out (F.<File_Mode> = Write_Mode);
      function Write_Open return File is
      begin
         for  F in T_Disk_File  loop
            if  FDL (F).File_Mode = Not_Open  then
               begin
                  Ada.Text_IO.Open
                    (Filedesclist (F),
                     Ada.Text_IO.Out_File,
                     File_Name);
               exception
                  when Ada.Text_IO.Name_Error =>
                     Ada.Text_IO.Create
                       (Filedesclist (F),
                        Ada.Text_IO.Out_File,
                        File_Name);
               end;
               FDL (F).File_Mode := Write_Mode;
               if  FDL (F).Buffer = null  then
                  FDL (F).Buffer := new T_Buffer;
               else
                  FDL (F).Act_Char  := 0;
                  FDL (F).Last_Char := 0;
               end if;
               Okay := TRUE;
               return F;
            end if;
         end loop;
         -- too many files open
         raise Ada.Text_IO.Use_Error;
      end Write_Open;

   begin -- Open
      if  newFile  then
         F := Write_Open;
      else
         F := Read_Open;
      end if;
   end Open;

   --
   -- As for Open, but tries to open file of given fileName by searching each
   -- directory specified by the environment variable named by envVar.
   --
   procedure Search_File
     (F       : in out File;
      envVar  : in     String;
      fileName: in     String;
      newFile : in     Boolean)
   is
      The_File_Name  : constant String := Ada.Strings.Fixed.Trim
                                            (Source => fileName,
                                             Side   => Ada.Strings.Both);
      Path_Ptr : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv (envVar);

      Dir_Scan_Pos   : Integer;
      Dir_Start_Pos  : Integer;
      Dir_Stop_Pos   : Integer;
   begin
      Okay := FALSE;
      if  Path_Ptr.all /= ""  then
         Dir_Scan_Pos := Path_Ptr.all'First;
         while  Dir_Scan_Pos <= Path_Ptr.all'Last  loop
            Dir_Start_Pos := Dir_Scan_Pos;
            Dir_Stop_Pos  := Dir_Scan_Pos;
            loop
               Dir_Stop_Pos := Dir_Stop_Pos + 1;
               exit when (Dir_Stop_Pos > Path_Ptr.all'Last) or else
                         (Path_Ptr.all (Dir_Stop_Pos) = PathSep);
            end loop;
            declare
               The_Open_File_Name : constant String :=
                     Path_Ptr.all (Dir_Start_Pos..(Dir_Stop_Pos - 1)) & DirSep & The_File_Name;
            begin
               Open
                 (F        => F,
                  fileName => The_Open_File_Name,
                  newFile  => newFile);
            end;
            exit when Okay;
            Dir_Scan_Pos := Dir_Stop_Pos + 1;
         end loop;
      end if;
      if  not Okay  then
         Open
           (F        => F,
            fileName => The_File_Name,
            newFile  => newFile);
      end if;

      GNAT.OS_Lib.Free (Path_Ptr);
   end Search_File;

   --
   -- Closes file F.  F becomes NIL.
   -- If possible, Close should be called automatically for all files that
   -- remain open when the application terminates.  This will be possible on
   -- implementations that provide some sort of termination or at-exit
   -- facility.
   --
   procedure Close (F : in     File) is
   begin
      case F is
         when StdIn | StdOut | StdErr =>
            raise Ada.Text_IO.Mode_Error;
         when others =>
            case FDL (F).File_Mode is
               when Read_Mode =>
                  Ada.Text_IO.Close (Filedesclist (F));
                  FDL (F).File_Mode := Not_Open;
               when Write_Mode =>
                  Write_Flush (F);
                  Ada.Text_IO.Close (Filedesclist (F));
                  FDL (F).File_Mode := Not_Open;
               when Not_Open =>
                  raise Ada.Text_IO.Status_Error;
            end case;
      end case;
   end Close;

   -- closes all open files (input or output files);
   -- in case of output files the buffer is flushed if necessary.
   procedure Close_IO is
   begin
      for F in T_Disk_File loop
         if FDL (F).File_Mode /= Not_Open then
            Close (F);
         end if;
      end loop;
      return;
   end Close_IO;

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
      directory: in out String)
   is
      I     : CARDINAL := 1;
      Start : CARDINAL := 1;
   begin
      while  (I <= fullName'Last) and then (fullName (I) /= ' ')  loop
         if  (I <= directory'Last)  then
            directory (I) := fullName (I);
         end if;
         if  (fullName (I) = ':') or else (fullName (I) = DirSep)  then
            Start := I + 1;
         end if;
         I := I + 1;
      end loop;
      for  I in Start..directory'Last  loop
         directory (I) := ' ';
      end loop;
   end Extract_Directory;

   --
   -- Extracts PRIMARY.EXT portion of fullName.
   --
   procedure Extract_File_Name
     (fullName: in     String;
      fileName: in out String)
   is
      I     : CARDINAL := 1;
      L     : CARDINAL := 1;
      Start : CARDINAL := 1;
   begin
      while  (L <= fullName'Last) and then (fullName (L) /= ' ')  loop
         if  (fullName (L) = ':') or else (fullName (L) = DirSep)  then
            Start := L + 1;
         end if;
         L := L + 1;
      end loop;
      while  (Start < L) and then (I <= fileName'Last)  loop
         fileName (I) := fullName (Start);
         Start := Start + 1;
         I     := I + 1;
      end loop;
      for  J in I..fileName'Last  loop
         fileName (J) := ' ';
      end loop;
   end Extract_File_Name;

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
      newName:    out String)
   is
      the_name : constant String := Ada.Strings.Fixed.Trim
                                      (Source => oldName,
                                       Side   => Ada.Strings.Both);
      the_ext  : constant String := Ada.Strings.Fixed.Trim
                                      (Source => ext,
                                       Side   => Ada.Strings.Both);
   begin
      Ada.Strings.Fixed.Move
        (Source => the_name & the_ext,
         Target => newName,
         Pad    => ' ');
   end Append_Extension;

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
      newName:    out String)
   is
      I  : CARDINAL := 1;
      J  : CARDINAL := 0;
      fn : String (1..Name_Length);
   begin
      Extract_Directory (oldName, newName);
      Extract_File_Name (oldName, fn);
      while  (I <= Name_Length) and then (fn (I) /= ' ')  loop
         if  (fn (I) = '.')  then
            J := I + 1;
         end if;
         I := I + 1;
      end loop;
      if  J /= 0 then
         I := J - 1;
      end if;
      if (ext (1) /= '.') and then (ext (1) /= ' ') then
         if I <= Name_Length  then
            fn (I) := '.';
            I := I + 1;
         end if;
      end if;
      J := 1;
      while  (J <= ext'Last) and then (ext (J) /= ' ') and then (I <= fn'Last)  loop
         fn (I) := ext (J);
         I := I + 1;
         J := J + 1;
      end loop;
      for  K in I..fn'Last  loop
         fn (K) := ' ';
      end loop;
      Concat (newName, fn, newName);
   end Change_Extension;



   --------------------------------------------------------------------------------
   -- The following routines provide a minimal set of file positioning routines. --
   -- Others may be introduced, but at least these should be implemented.        --
   -- Success of each operation is recorded in FileIO.Okay.                      --
   --------------------------------------------------------------------------------

   --
   -- Returns length of file F.
   --
   function Length (F: in     File) return INT32
   is
      Descriptor : constant POSIX.IO.File_Descriptor :=
         POSIX.IO.File_Descriptor (Interfaces.C_Streams.fileno
           (Ada.Text_IO.C_Streams.C_Stream (Filedesclist (F))));
   begin
      return INT32 (POSIX.IO.File_Size (Descriptor));
   end Length;

   --
   -- Returns the current read/write position in F.
   --
   function Get_Pos (F: in     File) return INT32
   is
      Descriptor : constant POSIX.IO.File_Descriptor :=
         POSIX.IO.File_Descriptor (Interfaces.C_Streams.fileno
           (Ada.Text_IO.C_Streams.C_Stream (Filedesclist (F))));
   begin
      return INT32 (POSIX.IO.File_Position (Descriptor));
   end Get_Pos;

   --
   -- Sets the current position for F to pos.
   --
   procedure Set_Pos
     (F  : in     File;
      pos: in     INT32)
   is
      Descriptor : constant POSIX.IO.File_Descriptor :=
         POSIX.IO.File_Descriptor (Interfaces.C_Streams.fileno
           (Ada.Text_IO.C_Streams.C_Stream (Filedesclist (F))));
      Result : POSIX.IO.IO_Offset;
   begin
      POSIX.IO.Seek
        (File   => Descriptor,
         Offset => POSIX.IO.IO_Offset (pos),
         Result => Result);
   end Set_Pos;








   -------------------------------------------------------------------------
   -- The following routines provide a minimal set of input routines.     --
   -- Others may be introduced, but at least these should be implemented. --
   -- Success of each operation is recorded in FileIO.Okay.               --
   -------------------------------------------------------------------------

   --
   -- TRUE if F is currently at the end of file.
   --
   function End_Of_File
     (F : in     File)
      return Boolean
   is
      C : Character;
   begin
      if FDL (F).End_Of_File then
         return True;
      end if;
      Read (F, C);
      Un_Read (F);
      return False;
   exception
      when Ada.Text_IO.End_Error =>
         return True;
   end End_Of_File;

   --
   -- Reads a character ch from file F.
   -- Maps filing system line mark sequence to FileIO.EOL.
   --
   procedure Read
     (F : in     File;
      C :    out Character)
      -- CHARACTER
      -- read the next character from input buffer resp. input file;
      -- on EOF condition from file set End_Of_File to True
   is
   begin
      if FDL (F).File_Mode = Write_Mode then
         raise Ada.Text_IO.Mode_Error;
      elsif FDL (F).File_Mode = Not_Open then
         raise Ada.Text_IO.Status_Error;
      end if;
      if F = StdIn and then FDL (F).Buffer = null then
         FDL (F).Buffer := new T_Buffer;
      end if;
      if FDL (F).Act_Char = FDL (F).Last_Char then
         Read_Buffer (F);
      elsif FDL (F).End_Of_File then
         raise Ada.Text_IO.End_Error;
      end if;
      FDL (F).Act_Char := FDL (F).Act_Char + 1;
      C := FDL (F).Buffer (FDL (F).Act_Char);
   end Read;

   --
   -- Reads to start of next line on file F, or to end of file if no next
   -- line.  Skips to, and consumes next line mark.
   --
   procedure Read_Ln (F: in     File)
   is
      C : Character;
   begin
      while  not End_Of_File (F)  loop
         Read (F, C);
         exit when C = LF;
      end loop;
   end Read_Ln;

   --
   -- Reads a string of characters from file F.
   -- Leading blanks are skipped, and str is delimited by line mark.
   -- Line mark is not consumed.
   --
   procedure Read_String
     (F : in     File;
      S : in out String)
   is
      C : Character renames S (S'First);
      I : Integer;
   begin
      while  not End_Of_File (F)  loop       -- skip leading blanks
         Read (F, C);                        --
         exit when C /= ' ' or else          --
                   C /= Character'Val (9);   --
      end loop;                              --

      I := S'First + 1;                      -- Index to str
      while  not End_Of_File (F)  loop       -- read a string
         Read (F, S (I));                    --
         exit when S (I) = LF;               --
         I := I + 1;                         --
      end loop;                              --

      S (I..S'Last) := (others => Character'Val (0));
   end Read_String;

   --
   -- Reads a string of characters from file F.
   -- Leading blanks are not skipped, and str is terminated by line mark or
   -- control character, which is not consumed.
   --
   procedure Read_Line
     (F : in     File;
      S : in out String)
   is
      C : Character;
      I : Integer := S'First;    -- The Index inside string S
   begin
      loop                       -- read a string
         Read (F, C);            --
         S (I) := C;             --
         I := I + 1;             --
         exit when C = LF;       --
      end loop;                  --

      S (I - 1) := Character'Val (0);  -- ???

      while I <= S'Last  loop    -- ??? the tail of string "str"
         S (I) := ' ';           -- is filled by blanks
         I := I + 1;             --
      end loop;                  --
   end Read_Line;


   --
   -- Writes len bytes from buf to F at the current file position.
   --
   procedure Read_Bytes
     (F  : in     File;
      buf:    out String;
      len: in out CARDINAL)
   is
      Ch : Character;
      I  : Integer := 0;
   begin
      while (not End_Of_File (F)) and then (I < len) and then ((buf'First + I) <= buf'Last)  loop
         Read (F, Ch);
         buf (buf'First + I) := Ch;
         I := I + 1;
      end loop;
      len := I;
   end Read_Bytes;



   -------------------------------------------------------------------------
   -- The following routines provide a minimal set of output routines.    --
   -- Others may be introduced, but at least these should be implemented. --
   -------------------------------------------------------------------------

   --
   -- Writes a character C to file F.
   -- If C = FileIO.EOL, writes line mark appropriate to filing system.
   --
   procedure Write
     (F : in     File;
      C : in     Character)
   is
   begin
      if F = StdOut then
         Ada.Text_IO.Put (C);
      elsif F = StdErr then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, C);
      elsif FDL (F).File_Mode = Read_Mode then
         raise Ada.Text_IO.Mode_Error;
      elsif FDL (F).File_Mode = Not_Open then
         raise Ada.Text_IO.Status_Error;
      else
         if FDL (F).Last_Char = Buf_Len then
            Write_Flush (F);
         end if;
         FDL (F).Last_Char := FDL (F).Last_Char + 1;
         FDL (F).Buffer (FDL (F).Last_Char) := C;
      end if;
   end Write;

   --
   -- Skips to the start of the next line on file F.
   -- Writes line mark appropriate to filing system.
   --
   procedure Write_Ln (F: File)
   is
   begin
      Write (F, LF);
   end Write_Ln;

   --
   -- Writes entire string str to file F.
   --
   procedure Write_String
     (F : in   File;
      S : in   String)
   is
   begin
      for  I in S'Range  loop
         exit when S (I) = Character'Val (0);
         Write (F, S (I));
      end loop;
   end Write_String;

   --
   -- Writes text to file F.
   -- At most len characters are written.  Trailing spaces are introduced
   -- if necessary (thus providing left justification).
   --
   procedure Write_Text
     (F   : in   File;
      Text: in   String;
      Len : in   Integer)
   is
      The_Length : Natural;
   begin
      The_Length := Len;
      for  I in Text'Range  loop
         exit when The_Length = 0;
         Write (F, Text (I));
         The_Length := The_Length - 1;
      end loop;
      while  The_Length /= 0  loop
         Write (F, ' ');
         The_Length := The_Length - 1;
      end loop;
   end Write_Text;

   --
   -- Writes an Integer int into a field of wid characters width.
   -- If the number does not fit into wid characters, wid is expanded.
   -- If wid = 0, exactly one leading space is introduced.
   --
   procedure Write_Int
     (F     : in   File;
      Item  : in   Integer;
      Width : in   Natural)
   is
      The_Width   : Natural := Width;
      The_Sign    : Character;
      The_Value   : Integer;
      D           : Integer;
      L           : Natural;
      S           : String (1..25);
   begin
      if  Item < 0  then
         The_Sign  := '-';
         The_Value := - Item;
      else
         The_Sign  := ' ';
         The_Value := Item;
      end if;

      L := 0;
      loop
         D         := The_Value mod 10;
         The_Value := The_Value / 10;
         L         := L + 1;
         S (L)     := Character'Val (Character'Pos ('0') + D);
         exit when The_Value = 0;
      end loop;

      if  The_Width = 0  then
         Write (F,' ');
      end if;

      while  The_Width > (L + 1)  loop
         Write (F, ' ');
         The_Width := The_Width - 1;
      end loop;

      if  (The_Sign = '-') or else (The_Width > L)  then
         Write (F, The_Sign);
      end if;

      while  L > 0  loop
         Write (F, S (L));
         L := L - 1;
      end loop;
   end Write_Int;

   --
   -- Writes a CARDINAL card into a field of wid characters width.
   -- If the number does not fit into wid characters, wid is expanded.
   -- If wid = 0, exactly one leading space is introduced.
   --
   procedure Write_Card
     (F     : in   File;
      Item  : in   CARDINAL;
      Width : in   CARDINAL)
   is
      The_Value   : CARDINAL  := Item;
      The_Width   : Natural   := Width;
      L           : CARDINAL;
      D           : CARDINAL;
      S           : String (1..25);
   begin
      L := 0;
      loop
         D         := The_Value mod 10;
         The_Value := The_Value / 10;
         L         := L + 1;
         S (L)     := Character'Val (Character'Pos ('0') + D);
         exit when The_Value = 0;
      end loop;

      if  The_Width = 0  then
         Write (F, ' ');
      end if;

      while  The_Width > L  loop
         Write (F, ' ');
         The_Width := The_Width - 1;
      end loop;

      while  L > 0  loop
         Write (F, S (L));
         L := L - 1;
      end loop;

   end Write_Card;



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
   function SLENGTH (S: String) return CARDINAL
   is
      Result : CARDINAL;
   begin
      Result := 0;
      for  I in S'Range  loop
         exit when S (I) = Character'Val (0);
         Result := Result + 1;
      end loop;
      return Result;
   end SLENGTH;

   --
   -- Copies as much of source to destination as possible, truncating if too
   -- long, and nul terminating if shorter.
   -- Be careful - some libraries have the parameters reversed!
   --
   procedure Assign
     (source     : in     String;
      destination: in out String)
   is
      D : Integer := destination'First - source'First;
   begin
      if  (source'Last - source'First) = (destination'Last - destination'First)  then
         destination := source;
      elsif  (source'Last - source'First) < (destination'Last - destination'First)  then
         destination (destination'First .. (source'Last + D)) := source;
      else
         destination := source (source'First .. (destination'Last - D));
      end if;
   end Assign;

   --
   -- Extracts at most numberToExtract characters from source[startIndex]
   -- to destination.  If source is too short, fewer will be extracted, even
   -- zero perhaps
   --
   procedure Extract
     (source         : in     String;
      startIndex     : in     CARDINAL;
      numberToExtract: in     CARDINAL;
      destination    : in out String)
   is
   begin
      Ada.Text_IO.Put_Line ("procedure 'FileIO.Extract' not released yet!");
   end Extract;

   --
   -- Concatenates stringVal1 and stringVal2 to form destination.
   -- Nul terminated if concatenation is short enough, truncated if it is
   -- too long
   --
   procedure Concat
     (stringVal1 : in     String;
      stringVal2 : in     String;
      destination: in out String)
   is
      S1 : constant String := Ada.Strings.Fixed.Trim
                                (Source => stringVal1,
                                 Side   => Ada.Strings.Both);
      S2 : constant String := Ada.Strings.Fixed.Trim
                                (Source => stringVal2,
                                 Side   => Ada.Strings.Both);
   begin
      Ada.Strings.Fixed.Move
        (Source => S1 & S2,
         Target => destination);
   end Concat;

   --
   -- Returns -1, 0, 1 depending whether stringVal1 < = > stringVal2.
   -- This is not directly ISO compatible
   --
   function Compare
     (stringVal1: in     String;
      stringVal2: in     String)
     return Integer
   is
   begin
      Ada.Text_IO.Put_Line ("Compare 'FileIO.Compare' not released yet!");
      return 0;
   end Compare;



   --------------------------------------------------------------------------
   -- The following routineses present oversimplified Character and String --
   -- case manipulation. Really they are support for ASCII character set.  --
   --------------------------------------------------------------------------

   D : constant := Character'Pos ('a') - Character'Pos ('A');

   --
   -- Returns Character C in Low Case
   -- (added in Ada version)
   --
   function Lo_Case (C: Character) return Character
   is
   begin
      case  C  is
         when 'A'..'Z' =>
            return Character'Val (Character'Pos (C) + D);
         when others =>
            return C;
      end case;
   end Lo_Case;

   --
   -- Returns Character C in Hight Case
   -- (added in Ada version)
   --
   function Up_Case (C: Character) return Character
   is
   begin
      case  C  is
         when 'a'..'z' =>
            return Character'Val (Character'Pos (C) - D);
         when others =>
            return C;
      end case;
   end Up_Case;

   --
   -- Returns string S in Low Case
   -- (added in Ada version)
   --
   function Lo_Case (S: String) return String
   is
      Result : String := S;
   begin
      for  I in Result'Range  loop
         Result (I) := Lo_Case (Result (I));
      end loop;
      return Result;
   end Lo_Case;

   --
   -- Returns string S in Hight Case
   -- (added in Ada version)
   --
   function Up_Case (S: String) return String
   is
      Result : String := S;
   begin
      for  I in Result'Range  loop
         Result (I) := Up_Case (Result (I));
      end loop;
      return Result;
   end Up_Case;


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
   function ORDL (n: in     INT32) return CARDINAL
   is
   begin
      return CARDINAL (n);
   end ORDL;
   pragma Inline (ORDL);

   --
   -- Convert long integer n to corresponding short integer value.
   -- Potentially FileIO.INTL(n) = VAL(Integer, n)
   --
   function INTL (n: in     INT32) return Integer
   is
   begin
      return Integer (n);
   end INTL;
   pragma Inline (INTL);

   --
   -- Convert cardinal n to corresponding long integer value.
   -- Potentially FileIO.INT(n) = VAL(INT32, n)
   --
   function INT (n: in     CARDINAL) return INT32
   is
   begin
      return INT32 (n);
   end INT;
   pragma Inline (INT);

   --
   -- Close all files and halt execution.
   -- On some implementations Quit_Execution will be simply implemented as HALT
   --
   procedure Quit_Execution
   is
   begin
      POSIX.Process_Primitives.Exit_Process
        (POSIX.Process_Primitives.Normal_Exit);
   end Quit_Execution;




end FileIO;


