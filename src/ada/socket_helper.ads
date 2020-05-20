with Error_H;      use Error_H;
with Interfaces.C; use Interfaces.C;
with Ip;           use Ip;
with Socket_Types; use Socket_Types;
with Tcp_Type;     use Tcp_Type;

package Socket_Helper
   with SPARK_Mode
is

   -- procedure Get_Socket_From_Table
   --   (Index : in     Socket_Type_Index;
   --    Sock  :    out Socket)
   --   with
   --    Depends =>
   --       (Sock => Index),
   --    Post =>
   --       Sock /= null;

   procedure Get_Host_By_Name_H
     (Server_Name    :     char_array;
      Server_Ip_Addr : out IpAddr;
      Flags          :     unsigned;
      Error          : out Error_T)
     with
      Post =>
         (if Error = NO_ERROR then
            Is_Initialized_Ip (Server_Ip_Addr));

   procedure Get_Syn_Queue 
      (Sock      :     Socket;
       Syn_Queue : out Tcp_Syn_Queue_Item_Acc)
      with
         Global => (Input => Socket_Table),
         Post => Syn_Queue /= null;


end Socket_Helper;
