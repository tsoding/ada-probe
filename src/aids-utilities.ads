with Ada.Streams;

Package Aids.Utilities is
    Use Ada.Streams;

    function Chunk_Image(Chunk: Stream_Element_Array) return String;

    function Chunk_To_String(C: in Stream_Element_Array) return String;
    function String_To_Chunk(S: in String) return Stream_Element_Array;

    -- Returns the stream as a hex-string.
    Function Chunk_Debug(C: in Stream_Element_Array) return String;
End Aids.Utilities;
