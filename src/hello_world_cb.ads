with AWS.Response;
with AWS.Status;

package Hello_World_CB is
   function HW_CB (Request: AWS.Status.Data) return AWS.Response.Data;
end Hello_World_CB;
