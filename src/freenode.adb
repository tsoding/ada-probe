with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Default;
with AWS.Net; use AWS;
with AWS.Net.SSL; use AWS.Net;

procedure Freenode is
   Secure : constant Boolean := False;
   Host   : constant String := "chat.freenode.net";
   Port   : constant Positive := (if Secure then 6697 else 6667);
begin
   declare
      Socket : Net.Socket_Type'Class := Net.Socket(Secure);
   begin
      Net.Connect(Socket, Host, Port);
      declare
         Chunk: Stream_Element_Array := Net.Receive(Socket);
         Output : String(1..Integer(Chunk'Length));
         Index : Natural := 1;
      begin
         for I in Chunk'Range loop
            Output(Index) := Character'Val(Natural(Chunk(I)));
            Index := Index + 1;
         end loop;
         Put_Line(Output);
      end;
      Net.Shutdown(Socket);
      Net.Free(Socket);
   end;
end;
