with Ada.Text_IO; use Ada.Text_IO;

package body Aids.Env is
    Function Find(Env   : in Typ; 
                  Key   : in String;
                  Found : in out Boolean
                 ) return String is
    Begin
        Found := Env.Contains( Key );
        Return (if not Found then "" else Env(Key));
    End Find;
    
    function Index_Of(Line: in String; X: in Character; Result: out Integer) return Boolean is
    begin
        for Index in Line'Range loop
            if Line(Index) = X then
                Result := Index;
                return True;
            end if;
        end loop;
        return False;
    end;

    function Slurp(File_Path: String) return Env_Hashed_Map.Map is
        Result: Env_Hashed_Map.Map;
        File: File_Type;
        Line_Number: Integer := 1;
    begin
        Open(File => File,
             Mode => In_File,
             Name => File_Path);
        while not End_Of_File(File) loop
            declare
                Line: String := Get_Line(File);
                Index : Integer;
            begin
                if Index_Of(Line, '=', Index) then
                    declare
                        Key   : String renames Line(Line'First..Index-1);
                        Value : String renames Line(Index+1..Line'Last);
                    begin
                        Env_Hashed_Map.Insert(Result, Key, Value);
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

    function Find(Env: in Typ; Key: in Unbounded_String; Value: out Unbounded_String) return Boolean is 
        Use Env_Hashed_Map;
        
        -- Renaming a function's return.
        C : Cursor  renames Find(Env, To_String(Key));
    begin
        -- Extended return example.
        Return Occupied : Constant Boolean := Has_Element(C) do
            if Occupied then
                Value := To_Unbounded_String( Env_Hashed_Map.Element(C) );
            end if;
        End return;
    end;
end Aids.Env;
