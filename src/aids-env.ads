-- String is an indefinite type, so you can avoid map of Unbounded_String
-- by using Indefinite_Hashed_Maps
with Ada.Containers.Indefinite_Hashed_Maps; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;

-- Basic support for .env files in Ada everyone was waiting for
package Aids.Env is

    type Typ is tagged private;
   
    Syntax_Error : exception;

    function Slurp(File_Path: String) return Typ;

   
    Env_Key_Not_Found : exception;

    -- overloading, no reason to have different names here
    function Extract(E : in Typ; Key: in String) return Unbounded_String;
    function Extract(E : in Typ; Key: in String) return Positive;

   
private
   
    -- private, because separation of concern / information hiding is a good thing
   
    package Env_Hashed_Map is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String,
         Element_Type => String,
         Hash => Ada.Strings.Hash_Case_Insensitive,
         Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);


    -- Ada has:
    -- - subtypes (compatible with the original, without type conversions, but range checked etc, renames/alias of type names ),
    -- - derived types (a new type, compatible (with type conversions) with the original)
    -- - extended types (types inheriting tagged types, OOP. Looks similar to derived types, but has a "with" extension)
    -- 
    -- The parent type of Typ is tagged, so this must be a type extension:
    type Typ is new Env_Hashed_Map.Map with null record;
   
   
   
end Aids.Env;

