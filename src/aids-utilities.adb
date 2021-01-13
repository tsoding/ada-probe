with
Ada.Unchecked_Conversion;

Package Body Aids.Utilities is

    Generic
        Type Source_Index   is (<>);
        Type Source_Element is limited private;
        Type Source_Array   is Array (Source_Index range <>) of Source_Element;

        Type Target_Index   is (<>);
        Type Target_Element is limited private;
        Type Target_Array   is Array (Target_Index range <>) of Target_Element;
    Function Convert_Array(Object : Source_Array) return Target_Array;

    Function Convert_Array(Object : Source_Array) return Target_Array is
        Pragma Assert( Target_Element'Size = Source_Element'Size,
                      "Target and Source element-sizes must match." );

        --- WARNING: NULL RANGE, this is incorrect. ---
        Subtype Target_Range is Target_Index range
          Target_Index'Last..Target_Index'First;


        Subtype Constrained_Source is Source_Array(Object'Range);
        Subtype Constrained_Target is Target_Array(Target_Range);

        Function Convert is new Ada.Unchecked_Conversion(
           Source => Constrained_Source,
           Target => Constrained_Target
          );
    Begin
        Return Item : Constant Target_Array := Convert( Object ) do
            Raise Program_Error with
              "I've forgotten how to grab the Target range via Source's range.";
            -- TODO: Fix this and make
        end return;
    End Convert_Array;


    function Chunk_Image(Chunk: Stream_Element_Array) return String
        renames Chunk_To_String;


    function String_To_Chunk(S: in String) return Stream_Element_Array is
        First : Constant Stream_Element_Offset:= Stream_Element_Offset(S'First);
        Last  : Constant Stream_Element_Offset:= Stream_Element_Offset(S'Last);
    begin
        Return Result: Stream_Element_Array(First..Last) do
            for Index in Result'Range loop
                Result(Index) :=
                  Stream_Element(Character'Pos(S(Positive(Index))));
            end loop;
        end return;
    end;


    function Chunk_To_String(C: in Stream_Element_Array) return String is
        Pragma Assert( Stream_Element'Size = Character'Size,
                      "Stream_Element and Character nust have equal sizes."
                     );

        Subtype Constrained_Array  is Stream_Element_Array(C'Range);
        Subtype Constrained_String is String(1..C'Length);

        Function Convert is new Ada.Unchecked_Conversion(
           Source => Constrained_Array,
           Target => Constrained_String
          );
    begin
        return Convert( C );
    end;

    Function Chunk_Debug(C: in Stream_Element_Array) return String is
        Pragma Assert( Stream_Element'Size = Character'Size,
                      "Stream_Element and Character nust have equal sizes."
                     );
         Pragma Assert( Character'Size = 8,
                      "Assuming 8-bit character."
                     );

        Type Nybble is range 16#0#..16#F#
          with Size => 4;

        Function Hex( Item : Nybble ) return Character is
          (case Item is
               when 16#0#..16#9# =>
                   Character'Val(Character'Pos('0') + Natural(Item)),
               when 16#A#..16#F# =>
                   Character'Val(Character'Pos('A') + Natural(Item-16#A#))
          );

        Type Char is record
            Hi, Lo : Nybble;
        end record
        with Pack, Size => 8;

        Function Convert is new Ada.Unchecked_Conversion(
           Source => Stream_Element,
           Target => Char
          );
    Begin
        Return Result : String(1..2*C'Length) do
            For Index in C'Range loop
                Declare
                    Nybbles      : Char renames Convert( C(Index) );
                    Result_Index : Constant Positive := 2*Natural(Index-C'First) + Result'First;
                    Hi           : Character renames Result(Result_Index);
                    Lo           : Character renames Result(Result_Index+1);
                Begin
                    Hi:= Hex( Nybbles.Hi );
                    Lo:= Hex( Nybbles.Lo );
                End;
            End loop;
        End return;
    End Chunk_Debug;


End Aids.Utilities;
