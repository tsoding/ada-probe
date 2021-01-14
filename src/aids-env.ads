with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.strings.Hash;

-- Basic support for .env files in Ada everyone was waiting for
package Aids.Env is
    Syntax_Error : exception;

    -- Indefinite hashed map can have unconstrained strinfs as keys or elements.
    package Env_Hashed_Map is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "="
        );

    subtype Typ is Env_Hashed_Map.Map;

    function Slurp(File_Path: String) return Typ;
    
    -- Retrieve a value given some key.
    function Find(Env   : in Typ; 
                  Key   : in Unbounded_String; 
                  Value : out Unbounded_String
                 ) return Boolean;
    function Find(Env   : in Typ; 
                  Key   : in String;
                  Found : in out Boolean
                 ) return String;
    

end Aids.Env;
