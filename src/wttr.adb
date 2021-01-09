with Ada.Text_IO; use Ada.Text_IO;

with AWS.Response;
with AWS.Client;
with AWS.Messages;

procedure Wttr is
    Response : AWS.Response.Data;
begin
    Response := AWS.Client.Get(URL => "https://wttr.in/Novosibirsk?format=4");
    Put_Line("Response Status Code: " & AWS.Messages.Image(AWS.Response.Status_Code(Response)));
    Put_Line("Resonse Body: " & AWS.Response.Message_Body(Response));
end;
