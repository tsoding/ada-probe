with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;

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
        Index : Natural renames Ada.strings.Fixed.Index(
           Source  => Line,
           Pattern => (1 => X),
           From    => Line'First
          );
    begin
        Return Found : constant Boolean := Index in Positive do
            Result:= Index;
        End return;
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
                        Prev  : Natural  renames Positive'Pred( Index );
                        Next  : Positive renames Positive'Succ( index );
                        
                        -- Rename the Trim-function, and give it a default of
                        -- both sides; this handles a line like:
                        -- "   This_key      =     Some_Value "
                        Function Trim(Object : String;
                                      Sides  : Ada.Strings.Trim_End:= Ada.Strings.Both
                                     ) return String
                           renames Ada.Strings.Fixed.Trim;
                          
                        Key   : String renames Trim( Line(Line'First..Prev) );
                        Value : String renames Trim( Line(Next..Line'Last)  );
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
