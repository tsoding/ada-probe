with Ada.Text_IO; use Ada.Text_IO;
with AWS.Default;
with AWS.Server;

with Hello_World_CB;

procedure Main is
    WS : AWS.Server.HTTP;
begin
    Put_Line("Call me on port"
             & Positive'Image(AWS.Default.Server_Port)
             & ", I will stop in 60 seconds...");
    AWS.Server.Start(WS, "Hello, World",
                     Max_Connection => 1,
                     Callback => Hello_World_CB.HW_CB'Access);
    delay 60.0;
    AWS.Server.Shutdown(WS);
end;
