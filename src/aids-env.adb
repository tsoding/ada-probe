with Ada.Text_IO; use Ada.Text_IO;

package body Aids.Env is
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
                        Key : Unbounded_String := To_Unbounded_String(Line(Line'First..Index-1));
                        Value : Unbounded_String := To_Unbounded_String(Line(Index+1..Line'Last));
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
        C: Env_Hashed_Map.Cursor := Env_Hashed_Map.Find(Env, Key);
    begin
        if Env_Hashed_Map.Has_Element(C) then
            Value := Env_Hashed_Map.Element(C);
            return True;
        end if;
        return False;
    end;
end Aids.Env;
