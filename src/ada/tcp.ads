---------------------------
-- TCP Types definitions --
---------------------------


with Net_Mem; use Net_Mem;
with Interfaces.C; use Interfaces.C;
with Compiler_Port; use Compiler_Port;
with Ip; use Ip;
with Error_H; use Error_H;
with Type_Def; use Type_Def;
with Tcp_Types; use Tcp_Types;

package Tcp is


   -------------------
   -- TCP functions --
   -------------------

   -- Ephemeral ports are used for dynamic port assignment
   Tcp_Dynamic_Port : Port;

   SOCKET_EPHEMERAL_PORT_MIN: Port := 49152;
   SOCKET_EPHEMERAL_PORT_MAX: Port := 65535;

   procedure Tcp_Init
      with
         Global => (
            Output => Tcp_Dynamic_Port
         ),
         Post => 
            (Tcp_Dynamic_Port = 0);

   procedure Tcp_Get_Dynamic_Port (
               P : out Port
   )
      with
         Global => (
            In_Out => Tcp_Dynamic_Port
         ),
         Depends => (
            P => Tcp_Dynamic_Port
         ),
         Post => (
            P <= SOCKET_EPHEMERAL_PORT_MAX and then
            P >= SOCKET_EPHEMERAL_PORT_MIN and then
            Tcp_Dynamic_Port <= SOCKET_EPHEMERAL_PORT_MAX and then
            Tcp_Dynamic_Port >= SOCKET_EPHEMERAL_PORT_MIN
         );

   -- procedure Tcp_Connect (
   --             Sock           :     Socket_Struct;
   --             Remote_Ip_Addr :     IpAddr;
   --             Remote_Port    :     Port;
   --             Error          : out Error_T
   -- );

   -- procedure Tcp_Listen (
   --             Sock    :     Socket_Struct;
   --             Backlog :     unsigned;
   --             Error   : out Error_T
   -- );

   procedure Tcp_Get_State (
               Sock  :     Socket;
               State : out Tcp_State
   )
   with
      Depends => (State => Sock),
      Post => State = Sock.State;

end Tcp;
