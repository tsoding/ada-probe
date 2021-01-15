with Ada.Text_IO; use Ada.Text_IO;
with AWS.Default;
with AWS.Server;

with Hello_World_CB;

procedure Server is
   WS : AWS.Server.HTTP;
   Port : Positive := AWS.Default.Server_Port;

   -- Passing on an access to a locally declared entity is usually unsafe,
   -- because the pointer could leak out of its stack frame, which is bad..
   -- Ada, by default, won't allow such unsafe programming, and forces you
   -- to tell the compiler that you know what you're doing...
   -- Normally, you would use 'Unchecked_Access for that, except for subprograms...
   -- For subprograms you would have to use the GNAT-specific 'Unrestricted_Access.
   -- In Ada 2005, anonymous access to subprograms was added, which could have
   -- allowed a simple 'Access in your case. However, AWS.Server.Start clearly
   -- starts a task (which will execute the callback), meaning the task could continue
   -- (and thus more callbacks could be called) after leaving this scope...
   -- So you should also make sure to call Shutdown if an error occurs somewhere
   -- within this scope.
   --
   --     function Local_HW_CB(Request : AWS.Status.Data) return AWS.Response.Data is
   --     begin
   --        return AWS.Response.Build("text/html", "<p>Hello World</p>");
   --     end;

begin
   Put_Line("Call me on port"
              & Positive'Image(Port)
              & ", I will stop if you press Q.");
   AWS.Server.Start(WS, "Hello, World",
                    Max_Connection => 1,
                    Port => Port,
                    Callback => Hello_World_CB.HW_CB'Access);
                    -- As discussed above: Callback => Local_HW_CB'Unrestricted_Access);
   AWS.Server.Wait(AWS.Server.Q_Key_Pressed);
   AWS.Server.Shutdown(WS);


-- As discussed above:
--  exception
--     when others => -- handle all exceptions
--        AWS.Server.Shutdown(WS);
end;
