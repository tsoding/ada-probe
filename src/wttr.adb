with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with AWS.Response;
with AWS.Client;
with AWS.Messages;
with AWS.URL;
with AWS.Status;

procedure Wttr is
    Not_Enough_Arguments: exception;

    Response : AWS.Response.Data;
begin
    if Argument_Count < 1 then
        raise Not_Enough_Arguments;
    end if;
    declare
        URL : String := "https://wttr.in/" & AWS.URL.Encode(Argument(1)) & "?format=4";
    begin
        Put_Line("Requesting " & URL);
        Response := AWS.Client.Get(URL => URL);
        Put_Line(AWS.Messages.Image(AWS.Response.Status_Code(Response)));
        Put_Line(AWS.Response.Message_Body(Response));
    end;
end;
