with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

-- Basic support for .env files in Ada everyone was waiting for
package Aids.Env is
    Syntax_Error : exception;

    package Env_Hashed_Map is new Ada.Containers.Hashed_Maps
        (Key_Type => Unbounded_String,
         Element_Type => Unbounded_String,
         Hash => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=");

    subtype Typ is Env_Hashed_Map.Map;

    function Slurp(File_Path: String) return Typ;
    function Find(Env: in Typ; 
                  Key: in Unbounded_String; 
                  Value: out Unbounded_String) return Boolean;

end Aids.Env;
