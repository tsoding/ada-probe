with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Default;
with AWS.Net; use AWS;
with AWS.Net.SSL; use AWS.Net;

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

    Twitch : constant Irc_Credentials :=
        ( Host => To_Unbounded_String("irc.chat.twitch.tv"),
          Port => 6697,
          Nick => To_Unbounded_String("MrBotka"),
          Pass => To_Unbounded_String("12345")
        );

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
begin
    Secure_Connection(Freenode);
end;
