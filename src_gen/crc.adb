------------------------------------
--                                --
-- Main driver program generation --
--                                --
------------------------------------

with ada.text_io;

with CRA;
with CRS;
with CRT;
with FileIO;


package body CRC is


   subtype CARDINAL is FileIO.CARDINAL;

--VARIABLE's

   The_Err_Msgs_File :  FileIO.File;   -- output: error message texts 
   The_Frame_File    :  FileIO.File;   -- input:  compiler frame compiler.frm 
   The_Compiler_File :  FileIO.File;   -- output: generated compiler 


   procedure Put (C: in     Character)
   is
   begin
      FileIO.Write (The_Compiler_File, C);
   end Put;

   procedure Put_S (S: in     String)
   is
   begin
      for  I  in S'Range  loop
         if  S (I) = '$'  then
            FileIO.Write_Ln (The_Compiler_File);
         else
            FileIO.Write (The_Compiler_File, S (I));
         end if;
      end loop;
   end Put_S;


   -- 
   -- Emits the source code of a generated driver program
   -- using the frame file compiler.frm
   -- 
   procedure Write_Driver
   is

      The_Graph_Node    : CRT.Graph_Node;
      The_Symbol_Node   : CRT.Symbol_Node;
      The_Grammar_Name  : CRT.Name_Ptr;
      The_Left_Margin   : CARDINAL;
      The_Error_Message : String (1..256);

   begin
      CRT.Get_Graph_Node  (CRT.root, The_Graph_Node);                -- determine
      CRT.Get_Symbol_Node (The_Graph_Node.Index_1, The_Symbol_Node); -- Grammar
      The_Grammar_Name := The_Symbol_Node.Symbol_Name;               -- Name

      --
      -- opening The_Frame_File
      --
      declare
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & FileIO.FrmExt;
      begin
         FileIO.Open (The_Frame_File, The_File_Name, FALSE);
      end;

      if  not FileIO.Okay  then
         FileIO.Open   (The_Frame_File, "compiler.frm", FALSE);
         if  not FileIO.Okay  then
            FileIO.Search_File (The_Frame_File, "CRFRAMES", "compiler.frm", FALSE);
            if  not FileIO.Okay  then
              FileIO.Write_Ln       (FileIO.StdOut);
              FileIO.Write_String   (FileIO.StdOut, "'compiler.frm' not found.");
              FileIO.Write_String   (FileIO.StdOut, "Aborted.");
              FileIO.Quit_Execution;
            end if;
         end if;
      end if;

      The_Left_Margin := 0;

      --
      -- Read *.err
      --
      declare  -- opening The_Err_Msgs_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & FileIO.ErrExt;
      begin
         FileIO.Open (The_Err_Msgs_File, The_File_Name, FALSE);
         if  not FileIO.Okay  then
            FileIO.Write_Ln     (FileIO.StdOut);
            FileIO.Write_String (FileIO.StdOut, "Cannot find ");
            FileIO.Write_String (FileIO.StdOut,  The_File_Name);
            FileIO.Write_String (FileIO.StdOut, " - Aborted.");
            FileIO.Quit_Execution;
         end if;
      end;

      --
      -- Write *.adb (GNAT Ada body file)
      --
      declare  -- opening The_Compiler_File
         The_File_Name : constant String :=
            FileIO.Lo_Case (The_Grammar_Name.all) & FileIO.BodyExt;
      begin
         FileIO.Open (The_Compiler_File, The_File_Name, TRUE);
         if  not FileIO.Okay  then
            FileIO.Write_Ln     (FileIO.StdOut);
            FileIO.Write_String (FileIO.StdOut, "Cannot open ");
            FileIO.Write_String (FileIO.StdOut,  The_File_Name);
            FileIO.Write_String (FileIO.StdOut, " - Aborted.");
            FileIO.Quit_Execution;
         end if;
      end;

      -- with -->Parser
      CRA.Copy_Frame_Part ("-->Parser", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all & 'P');
      -- use  -->Parser
      CRA.Copy_Frame_Part ("-->Parser", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all & 'P');

      -- with -->Scanner
      CRA.Copy_Frame_Part ("-->Scanner", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all & 'S');
      -- use  -->Scanner
      CRA.Copy_Frame_Part ("-->Scanner", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all & 'S');

      -- procedure -->Grammar
      CRA.Copy_Frame_Part ("-->Grammar", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all);

      CRA.Copy_Frame_Part ("-->Errors", The_Left_Margin, The_Frame_File, The_Compiler_File);
      while  not FileIO.End_Of_File (The_Err_Msgs_File)  loop
         FileIO.Read_String  (The_Err_Msgs_File, The_Error_Message);
         FileIO.Write_String (The_Compiler_File, The_Error_Message);
         FileIO.Write_Ln     (The_Compiler_File);
         FileIO.Write_Text   (The_Compiler_File, " ", The_Left_Margin);
      end loop;

      -- -->Scanner.Error :=
      CRA.Copy_Frame_Part ("-->Scanner", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all & 'S');

      -- end -->Grammar
      CRA.Copy_Frame_Part ("-->Grammar", The_Left_Margin, The_Frame_File, The_Compiler_File);
      Put_S (The_Grammar_Name.all & ";$$$$$");


      FileIO.Close (The_Compiler_File);
      FileIO.Close (The_Err_Msgs_File);
      FileIO.Close (The_Frame_File);
   end Write_Driver;

end CRC;



