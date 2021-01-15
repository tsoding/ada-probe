with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Aids.Env is

   
    function Slurp(File_Path: String) return Typ is
        Result: Typ;
        File: File_Type;
        Line_Number: Integer := 1;
    begin
        Open(File => File,
             Mode => In_File,
             Name => File_Path);
      
        while not End_Of_File(File) loop
            declare
                Line: constant String := Get_Line(File);
                Index : Integer;
            begin
                Index := Ada.Strings.Fixed.Index(Line, "=");
                if Index /= 0 then
                    declare
                        Key : String renames Line(Line'First..Index-1);
                        Value : String renames Line(Index+1..Line'Last);
                    begin
                        -- Result is a tagged type, so can use object.operation notation
                        Result.Insert(Key, Value);
                        -- Insert will raise exception if Key already exists,
                        -- Include overwrites the entry:
                        -- Env_Hashed_Map.Include(Result, Key, Value);
                    end;
                else
                    raise Syntax_Error with (File_Path 
                                             & ":" 
                                             & Integer'Image(Line_Number)
                                             & ": Expected separator `=`");
                end if;

                Line_Number := Line_Number + 1;
            end;
        end loop;
        Close(File);
        return Result;
    end;

   function Extract(E : in Typ; Key: in String) return Unbounded_String is
   begin
      if not E.Contains(Key) then
         raise Env_Key_Not_Found with "key `" & Key & "` not found";
      end if;
      -- Ada Containers can be indexed like an array, even maps with Strings as key 
      -- ie, if My_Map("Foo") = Bar then
      return To_Unbounded_String( E(Key) );
   end;

   function Extract(E : in Typ; Key: in String) return Positive is
   begin
      if not E.Contains(Key) then
         raise Env_Key_Not_Found with "key `" & Key & "` not found";
      end if;

      -- Ada Containers can be indexed like an array
      return Positive'Value( E(Key) );
   end;
   
end Aids.Env;
