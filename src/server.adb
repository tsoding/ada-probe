with Ada.Text_IO; use Ada.Text_IO;
with AWS.Default;
with AWS.Server;

with Hello_World_CB;

procedure Server is
   WS : AWS.Server.HTTP;
   Port : Positive := AWS.Default.Server_Port;
begin
   Put_Line("Call me on port"
              & Positive'Image(Port)
              & ", I will stop if you press Q.");
   AWS.Server.Start(WS, "Hello, World",
                    Max_Connection => 1,
                    Port => Port,
                    Callback => Hello_World_CB.HW_CB'Access);
   AWS.Server.Wait(AWS.Server.Q_Key_Pressed);
   AWS.Server.Shutdown(WS);
end;
