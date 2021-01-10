with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with AWS.Default;
with AWS.Net; use AWS;
with AWS.Net.SSL; use AWS.Net;

with Aids.Env; use Aids;

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
        E: Env.Typ := Env.Slurp(File_Path);
        Result: Irc_Credentials;

        Env_Key_Not_Found : exception;

        procedure Extract_String(Key: in Unbounded_String; Value: out Unbounded_String) is
        begin
            if not Env.Find(E, Key, Value) then
                raise Env_Key_Not_Found with (File_Path & ": key `" & To_String(Key) & "` not found");
            end if;
        end;

        procedure Extract_Positive(Key: in Unbounded_String; Value: out Positive) is
            S: Unbounded_String;
        begin
            if not Env.Find(E, Key, s) then
                raise Env_Key_Not_Found with (File_Path & ": key `" & To_String(Key) & "` not found");
            end if;

            Value := Positive'Value(To_String(S));
        end;
    begin
        Extract_String(To_Unbounded_String("HOST"), Result.Host);
        Extract_Positive(To_Unbounded_String("PORT"), Result.Port);
        Extract_String(To_Unbounded_String("NICK"), Result.Nick);
        Extract_String(To_Unbounded_String("PASS"), Result.Pass);
        Extract_String(To_Unbounded_String("CHANNEL"), Result.Channel);
        return Result;
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
        Client.Send(String_To_Chunk(Line & Character'Val(13) & Character'Val(10)));
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
        while true loop
            declare
                Chunk: Stream_Element_Array := Client.Receive;
            begin
                Put(Chunk_Image(Chunk));
            end;
        end loop;
    end;

    Not_Enough_Arguments: exception;

begin
    if Argument_Count < 1 then
        raise Not_Enough_Arguments;
    end if;

    declare
        Twitch: Irc_Credentials := Irc_Credentials_From_File(Argument(1));
    begin
        Secure_Connection(Twitch);
    end;
end;
