with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with AWS.Default;
with AWS.Net; use AWS;
with AWS.Net.SSL; use AWS.Net;

with Aids.Env; use Aids;
with Aids.Utilities; use Aids.Utilities;

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
        Put_Line("Port:"  & Positive'Image(C.Port));
        Put_Line("Nick: " & To_String(C.Nick));
        Put_Line("Pass: [REDACTED]");
        Put_Line("Channel: " & To_String(C.Channel));
    end;

    function Irc_Credentials_From_File(File_Path: String) return Irc_Credentials is
        
        -- Key extraction functions.
        Function Extract ( Key : String ) return String           with Inline;
        Function Extract ( Key : String ) return Unbounded_String with Inline;
        Function Extract ( Key : String ) return Positive         with Inline;
        
        E: Env.Typ := Env.Slurp(File_Path);
        Key_Not_Found : exception;

        -- Base Extraction; provides for the key-not-found exception.
        Function Extract ( Key : String ) return String is
        Begin
            Return E(Key);
        Exception
            when Constraint_Error => raise Key_Not_Found with 
                  (File_Path & ": key `" & Key & "` not found");
        End Extract;
        
        -- Extract & convert to an Unbounded_String.
        Function Extract ( Key : String ) return Unbounded_String is
            ( To_Unbounded_String( Source => Extract(Key) ) );
        
        -- Extract and convert to a Positive.
        Function Extract ( Key : String ) return Positive is
            Value : String renames Extract( Key );
        Begin
            Return Positive'Value( Value );
        Exception
            when Constraint_Error =>
                raise Constraint_Error with
                ''' & Value & "' could not be converted to a positive number.";
        End Extract;
        
    begin
        return Result : constant Irc_Credentials := (
           Nick    => Extract("NICK"),
           Pass    => Extract("PASS"),
           Channel => Extract("CHANNEL"),
           Host    => Extract("HOST"),
           Port    => Extract("PORT")
          );
    end;

    procedure Send_Line(Client: in out SSL.Socket_Type; Line: in String) is
        CRLF :  Constant String := (Character'Val(13), Character'Val(10));
    begin
        Client.Send(String_To_Chunk(Line & CRLF));
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
        Send_Line( Client, "PASS oauth:" & To_String(Credentials.Pass));
        Send_Line( Client, "NICK " & To_String(Credentials.Nick));
        Send_Line( Client, "JOIN " & To_String(Credentials.Channel));
        Send_Line( Client, "PRIVMSG " & To_String(Credentials.Channel) & " :tsodinPog");
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
    if Argument_Count not in Positive then
        raise Not_Enough_Arguments;
    end if;

    declare
        Twitch: Irc_Credentials := Irc_Credentials_From_File(Argument(1));
    begin
        Print_Irc_Credentials(Twitch);
        Secure_Connection(Twitch);
    end;
end;
