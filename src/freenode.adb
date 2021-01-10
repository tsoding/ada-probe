with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
    end record;

    Freenode : constant Irc_Credentials := 
        ( Host => To_Unbounded_String("chat.freenode.net"),
          Port => 6697,
          Nick => To_Unbounded_String(""),
          Pass => To_Unbounded_String("")
        );

    procedure Print_Irc_Credentials(C: in Irc_Credentials) is
    begin
        Put_Line("Host: " & To_String(C.Host));
        Put_Line("Port: " & Positive'Image(C.Port));
        Put_Line("Nick: " & To_String(C.Nick));
        Put_Line("Pass: [REDACTED]");
    end;

    function Irc_Credentials_From_File(File_Path: String) return Irc_Credentials is
        E: Env.Typ := Env.Slurp(File_Path);
        Result: Irc_Credentials;

        Unknown_Env_Key : exception;

        procedure Exctract_String(Key: in Unbounded_String; Value: out Unbounded_String) is
        begin
            if not Env.Find(E, Key, Value) then
                raise Unknown_Env_Key with (File_Path & ": unknown key `" & To_String(Key) & "`");
            end if;
        end;

        procedure Exctract_Positive(Key: in Unbounded_String; Value: out Positive) is
            S: Unbounded_String;
        begin
            if not Env.Find(E, Key, s) then
                raise Unknown_Env_Key with (File_Path & ": unknown key `" & To_String(Key) & "`");
            end if;

            Value := Positive'Value(To_String(S));
        end;
    begin
        Exctract_String(To_Unbounded_String("HOST"), Result.Host);
        Exctract_Positive(To_Unbounded_String("PORT"), Result.Port);
        Exctract_String(To_Unbounded_String("NICK"), Result.Nick);
        Exctract_String(To_Unbounded_String("PASS"), Result.Pass);
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
        while true loop
            declare
                Chunk: Stream_Element_Array := Client.Receive;
            begin
                Put(Chunk_Image(Chunk));
            end;
        end loop;
    end;

    Twitch: Irc_Credentials := Irc_Credentials_From_File("twitch.env");
begin
    --Secure_Connection(Freenode);
    Print_Irc_Credentials(Twitch);
end;
