package body Hello_World_CB is
    function HW_CB(Request : AWS.Status.Data) return AWS.Response.Data is
    begin
        return AWS.Response.Build("text/html", "<p>Hello World</p>");
    end;
end Hello_World_CB;
