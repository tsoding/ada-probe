-- Personally, I'm not a fan of 'use <package>', but I guess that's what
-- maintaining >1MLOC codebase for a couple of decades will do to you...

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1; -- for CR and LF
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with AWS.Default;
with AWS.Net; use AWS;
with AWS.Net.SSL; use AWS.Net;

with Aids.Env;

procedure Freenode is
   
   
    type Irc_Credentials is record
        Host : Unbounded_String;
        Port : Positive;
        Nick : Unbounded_String;
        Pass : Unbounded_String;
        Channel : Unbounded_String;
    end record;

   
    procedure Print_Irc_Credentials(C: in Irc_Credentials) is
    begin
        Put_Line("Host: " & To_String(C.Host));
        Put_Line("Port: " & Positive'Image(C.Port));
        Put_Line("Nick: " & To_String(C.Nick));
        Put_Line("Pass: [REDACTED]");
        Put_Line("Channel: " & To_String(C.Channel));
    end;

    function Irc_Credentials_From_File(File_Path: String) return Irc_Credentials is
        E: Aids.Env.Typ := Aids.Env.Slurp(File_Path);
    begin
        -- E is a tagged type, so can use object.operation notation
        -- using aggregates will prevent missing assignment of member variables
        return Irc_Credentials'
            (Host => E.Extract("HOST"),
             Port => E.Extract("PORT`"),
             Nick => E.Extract("NICK"),
             Pass => E.Extract("PASS"),
             Channel => E.Extract("CHANNEL"));
    exception
        when Error : Aids.Env.Env_Key_Not_Found =>
            Put_Line(File_Path & ": " & Ada.Exceptions.Exception_Message(Error));
        raise; -- re-raise, to exit program 
    end;

    function Chunk_Image(Chunk: Stream_Element_Array) return String is
        Result : String(1..Integer(Chunk'Length));
        Index : Natural := 1;
    begin
        for I in Chunk'Range loop
            Result(Index) := Character'Val(Natural(Chunk(I)));
            Index := Index + 1;
        end loop;
        return Result;
    end;

    function String_To_Chunk(S: in String) return Stream_Element_Array is
        First: Stream_Element_Offset := Stream_Element_Offset(S'First);
        Last: Stream_Element_Offset := Stream_Element_Offset(S'Last);
        Result: Stream_Element_Array(First..Last);
    begin
        for Index in S'Range loop
            Result(Stream_Element_Offset(Index)) := 
                Stream_Element(Character'Pos(S(Index)));
        end loop;
        return Result;
    end;

    procedure Send_Line(Client: in out SSL.Socket_Type; Line: in String) is
    begin
        -- CR and LF is defined in Ada.Characters.Latin_1
        Client.Send(String_To_Chunk(Line & CR & LF));
    end;

    -- NOTE: stolen from https://github.com/AdaCore/aws/blob/master/regtests/0243_sshort/sshort.adb#L156
    procedure Secure_Connection(Credentials: Irc_Credentials) is
        Client: SSL.Socket_Type;
        Config: SSL.Config;
    begin
        Put_Line("Establishing secure (Kapp) connection to " 
                 & To_String(Credentials.Host)
                 & ":" 
                 & Integer'Image(Credentials.Port));
        SSL.Initialize(Config, "");
        Client.Set_Config(Config);
        Client.Connect(To_String(Credentials.Host), Credentials.Port);
        Send_Line(Client, "PASS oauth:" & To_String(Credentials.Pass));
        Send_Line(Client, "NICK " & To_String(Credentials.Nick));
        Send_Line(Client, "JOIN " & To_String(Credentials.Channel));
        Send_Line(Client, "PRIVMSG " & To_String(Credentials.Channel) & " :tsodinPog");
        
        -- Ada has actual infinite loops
        loop
            Put(Chunk_Image(Client.Receive));
        end loop;
    end;

    Not_Enough_Arguments: exception;

begin
    if Argument_Count < 1 then
        raise Not_Enough_Arguments;
    end if;

    Secure_Connection(Irc_Credentials_From_File(Argument(1)));
end Freenode;
