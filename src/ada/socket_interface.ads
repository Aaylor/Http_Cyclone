pragma Unevaluated_Use_Of_Old (Allow);
pragma Ada_2020;

with Interfaces.C; use Interfaces.C;
with Ip;           use Ip;
with Error_H;      use Error_H;
with Common_Type;  use Common_Type;
with Socket_Types; use Socket_Types;
with Net;          use Net;
with Tcp_Interface, Udp_Binding;
use Tcp_Interface, Udp_Binding;
with Tcp_Type;     use Tcp_Type;

package Socket_Interface with
   SPARK_Mode
is

   type Ttl_Type is mod 2**8;

   type Host_Resolver is mod 2 ** 6;

   HOST_NAME_RESOLVER_ANY   : Host_Resolver := 0;
   HOST_NAME_RESOLVER_DNS   : Host_Resolver := 1;
   HOST_NAME_RESOLVER_MDNS  : Host_Resolver := 2;
   HOST_NAME_RESOLVER_NBNS  : Host_Resolver := 4;
   HOST_NAME_RESOLVER_LLMNR : Host_Resolver := 8;
   HOST_TYPE_IPV4           : Host_Resolver := 16;
   HOST_TYPE_IPV6           : Host_Resolver := 32;

   procedure Get_Host_By_Name
     (Server_Name    :     char_array;
      Server_Ip_Addr : out IpAddr;
      Flags          :     Host_Resolver;
      Error          : out Error_T)
      with
        Depends =>
          (Server_Ip_Addr => (Server_Name, Flags),
           Error          => (Server_Name, Flags)),
        Post =>
          (if Error = NO_ERROR then 
             Is_Initialized_Ip(Server_Ip_Addr));

   procedure Socket_Open
     (Sock       : out Socket;
      S_Type     :     Socket_Type;
      S_Protocol :     Socket_Protocol)
      with
         Global =>
           (Input  =>  Net_Mutex,
            In_Out => (Tcp_Dynamic_Port, Socket_Table)),
         Depends =>
           (Sock             => (S_Type, Socket_Table),
            Socket_Table     => (S_Type, S_Protocol, Tcp_Dynamic_Port, Socket_Table),
            Tcp_Dynamic_Port => (S_Type, Tcp_Dynamic_Port),
            null             => Net_Mutex),
         Post =>
            (for all S in Socket_Table'Range =>
               (if S /= Sock then
                  Model_Socket_Table(S) =
                     Model_Socket_Table'Old(S))) and then
            (if Sock /= -1 then
               Socket_Table(Sock).S_Descriptor >= 0 and then
               Socket_Table(Sock).S_Type = S_Type and then
               not Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
               not Is_Initialized_Ip(Socket_Table(Sock).S_localIpAddr)),
         Contract_Cases =>
            (S_Type = SOCKET_TYPE_STREAM =>
               (if Sock /= -1 then
                  Socket_Table(Sock).S_Protocol = SOCKET_IP_PROTO_TCP and then
                  Socket_Table(Sock).S_Local_Port > 0),
             S_Type = SOCKET_TYPE_DGRAM =>
               (if Sock /= -1 then
                  Socket_Table(Sock).S_Protocol = SOCKET_IP_PROTO_UDP),
             others =>
               (if Sock /= -1 then
                  Socket_Table(Sock).S_Protocol = S_Protocol));

   procedure Socket_Set_Timeout
      (Sock    : Not_Null_Socket;
       Timeout : Systime)
      with
        Global =>
          (Input  => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>+ (Timeout, Sock),
           null         =>  Net_Mutex),
        Post =>
          (for all S in Socket_Table'Range =>
            Model_Socket_Table(S) = Model_Socket_Table'Old(S)); -- 'Update
             --(S_Timeout => timeout);

   procedure Socket_Set_Ttl
      (Sock : Not_Null_Socket;
       Ttl  : Ttl_Type)
      with
        Global =>
          (Input => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>+ (Ttl, Sock),
           null         =>  Net_Mutex),
        Post =>
          (for all S in Socket_Table'Range =>
            Model_Socket_Table(S) = Model_Socket_Table'Old(S));--'Update (
             -- S_TTL => unsigned_char (Ttl));

   procedure Socket_Set_Multicast_Ttl
      (Sock : Not_Null_Socket;
       Ttl  : Ttl_Type)
      with
        Global =>
          (Input => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>+ (Ttl, Sock),
           null         =>  Net_Mutex),
        Post =>
          (for all S in Socket_Table'Range =>
            Model_Socket_Table(S) = Model_Socket_Table'Old(S));--'Update (
              --S_Multicast_TTL => unsigned_char (Ttl));

   procedure Socket_Connect
      (Sock           : in     Not_Null_Socket;
       Remote_Ip_Addr : in     IpAddr;
       Remote_Port    : in     Port;
       Error          :    out Error_T)
      with
        Global =>
          (Input  => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          ((Socket_Table, Error) => (Socket_Table, Sock, Remote_Ip_Addr, Remote_Port),
           null =>  Net_Mutex),
        Pre =>
          Is_Initialized_Ip (Remote_Ip_Addr) and then
          (if Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM then
             Socket_Table(Sock).State = TCP_STATE_CLOSED),
        Post =>
          (for all S in Socket_Table'Range =>
             (if S /= Sock then
               Model_Socket_Table(S) = Model_Socket_Table'Old(S))),
        Contract_Cases => (
          Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM =>
               (if Error = NO_ERROR then
                  -- Socket_Table(Sock).S_Descriptor = Socket_Table(Sock).S_Descriptor'Old and then
                  Socket_Table(Sock).S_Type = Socket_Table(Sock).S_Type'Old and then
                  Socket_Table(Sock).S_Protocol = Socket_Table(Sock).S_Protocol'Old and then
                  Is_Initialized_Ip (Socket_Table(Sock).S_localIpAddr) and then
                  Socket_Table(Sock).S_Local_Port = Socket_Table(Sock).S_Local_Port'Old and then
                  Socket_Table(Sock).S_remoteIpAddr = Remote_Ip_Addr and then
                  Socket_Table(Sock).S_Remote_Port = Remote_Port and then
                  -- Socket_Table(Sock).S_Timeout = Socket_Table(Sock).S_Timeout'Old and then
                  -- Socket_Table(Sock).S_TTL = Socket_Table(Sock).S_TTL'Old and then
                  -- Socket_Table(Sock).S_Multicast_TTL = Socket_Table(Sock).S_Multicast_TTL'Old and then
                  -- Socket_Table(Sock).txBufferSize = Socket_Table(Sock).txBufferSize'Old and then
                  -- Socket_Table(Sock).rxBufferSize = Socket_Table(Sock).rxBufferSize'Old and then
                  Socket_Table(Sock).State = TCP_STATE_ESTABLISHED),

          Socket_Table(Sock).S_Type = SOCKET_TYPE_DGRAM =>
               Error = NO_ERROR and then
               Model(Sock) = Model(Sock)'Old'Update
                     (S_remoteIpAddr => Remote_Ip_Addr,
                      S_Remote_Port  => Remote_Port),

          Socket_Table(Sock).S_Type = SOCKET_TYPE_RAW_IP =>
             Error = NO_ERROR and then
             Model(Sock) = Model(Sock)'Old'Update
                (S_remoteIpAddr => Remote_Ip_Addr),

          others =>
             Model(Sock) = Model(Sock)'Old);

   procedure Socket_Send_To
      (Sock         : in     Not_Null_Socket;
       Dest_Ip_Addr : in     IpAddr;
       Dest_Port    : in     Port;
       Data         : in     char_array;
       Written      :    out Integer;
       Flags        : in     unsigned;
       Error        :    out Error_T)
      with
        Global =>
          (Input  => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          ((Error, Socket_Table, Written) =>
                  (Socket_Table, Sock, Data, Flags),
           null => (Net_Mutex, Dest_Port, Dest_Ip_Addr)),
        Pre  =>
          Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr),
        Post =>
          (for all S in Socket_Table'Range =>
             (if S /= Sock then
               Model_Socket_Table(S) = Model_Socket_Table'Old(S))) and then
          (if Error = NO_ERROR then
             Model(Sock) = Model(Sock)'Old and then
             Written > 0);

   procedure Socket_Send
      (Sock    : in     Not_Null_Socket;
       Data    : in     char_array;
       Written :    out Integer;
       Flags   : in     Socket_Flags;
       Error   :    out Error_T)
      with
        Global =>
          (Input  => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          ((Socket_Table, Written, Error) =>
                  (Socket_Table, Sock, Data, Flags),
           null    =>  Net_Mutex),
        Pre  =>
          Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr),
        Post =>
          (if Error = NO_ERROR then 
             Model(Sock) = Model(Sock)'Old and then
             Written > 0);

   procedure Socket_Receive_Ex
      (Sock         :     Not_Null_Socket;
       Src_Ip_Addr  : out IpAddr;
       Src_Port     : out Port;
       Dest_Ip_Addr : out IpAddr;
       Data         : out char_array;
       Received     : out unsigned;
       Flags        :     Socket_Flags;
       Error        : out Error_T)
      with
        Global =>
          (Input  => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>  (Sock, Data, Flags, Socket_Table),
           Data         =>  (Sock, Data, Flags, Socket_Table),
           Received     =>  (Sock, Data, Flags, Socket_Table),
           Src_Ip_Addr  =>  (Sock, Data, Flags, Socket_Table),
           Src_Port     =>  (Sock, Data, Flags, Socket_Table),
           Dest_Ip_Addr =>  (Sock, Data, Flags, Socket_Table),
           Error        =>  (Sock, Data, Flags, Socket_Table),
           null         =>  Net_Mutex),
        Pre =>
          Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
          Data'Last >= Data'First,
        Contract_Cases =>
          (Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM =>
               (if Error = NO_ERROR then
                  Model(Sock) = Model(Sock)'Old and then
                  Received > 0
                elsif Error = ERROR_END_OF_STREAM then
                  Model(Sock) = Model(Sock)'Old and then
                  Received = 0),
           others =>
               Error = ERROR_INVALID_SOCKET and then
               Model(Sock) = Model(Sock)'Old and then
               Received = 0);

   procedure Socket_Receive
      (Sock     :     Not_Null_Socket;
       Data     : out char_array;
       Received : out unsigned;
       Flags    :     Socket_Flags;
       Error    : out Error_T)
      with
        Global =>
          (Input => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>  (Sock, Data, Flags, Socket_Table),
           Data         =>  (Sock, Data, Flags, Socket_Table),
           Error        =>  (Sock, Data, Flags, Socket_Table),
           Received     =>  (Sock, Data, Flags, Socket_Table),
           null         =>  Net_Mutex),
        Pre =>
          Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
          Data'Last >= Data'First,
        Contract_Cases =>
          (Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM =>
             (if Error = NO_ERROR then
                Model(Sock) = Model(Sock)'Old and then
                Received > 0
             elsif Error = ERROR_END_OF_STREAM then
                Model(Sock) = Model(Sock)'Old and then
                Received = 0),
           others =>
             Error = ERROR_INVALID_SOCKET and then
             Model(Sock) = Model(Sock)'Old and then
             Received = 0);

   procedure Socket_Shutdown
      (Sock  :     Not_Null_Socket;
       How   :     Socket_Shutdown_Flags;
       Error : out Error_T)
      with
        Global =>
          (Input  => Net_Mutex,
           In_Out => Socket_Table),
        Depends =>
          ((Socket_Table, Error) => (Socket_Table, Sock, How),
           null  => Net_Mutex),
        Pre =>
          Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
          Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr),
        Post =>
          (if Error = NO_ERROR then
             Model(Sock) = Model(Sock)'Old);

   procedure Socket_Close
      (Sock : Not_Null_Socket)
      with
        Global  => (Input  => Net_Mutex,
                    In_Out => Socket_Table),
        Depends => (Socket_Table =>+ Sock,
                    null         => Net_Mutex),
        Post    => Socket_Table(Sock).S_Type = SOCKET_TYPE_UNUSED;

   procedure Socket_Set_Tx_Buffer_Size
      (Sock : Not_Null_Socket;
       Size : Tx_Buffer_Size)
      with
        Global =>
            (In_Out => Socket_Table),
        Depends =>
            (Socket_Table => (Size, Sock, Socket_Table)),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
               not Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
               Socket_Table(Sock).State = TCP_STATE_CLOSED,
        Post =>
          Model(Sock) = Model(Sock)'Old;--'Update
               --(S_Tx_Buffer_Size => Size);

   procedure Socket_Set_Rx_Buffer_Size
      (Sock : Not_Null_Socket;
       Size : Rx_Buffer_Size)
      with
        Global =>
            (In_Out => Socket_Table),
        Depends =>
            (Socket_Table =>+ (Size, Sock)),
        Pre =>
          Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
          not Is_Initialized_Ip (Socket_Table(Sock).S_remoteIpAddr) and then
          Socket_Table(Sock).State = TCP_STATE_CLOSED,
        Post =>
            Model(Sock) = Model(Sock)'Old;--'Update
               --(S_Rx_Buffer_Size => Size);

   procedure Socket_Bind
      (Sock          : Not_Null_Socket;
       Local_Ip_Addr : IpAddr;
       Local_Port    : Port)
      with
       Global =>
            (In_Out => Socket_Table),
       Depends =>
            (Socket_Table =>+ (Sock, Local_Ip_Addr, Local_Port)),
       Pre =>
         not Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
         not Is_Initialized_Ip(Socket_Table(Sock).S_localIpAddr) and then
         Is_Initialized_Ip(Local_Ip_Addr) and then
         (Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM or else
          Socket_Table(Sock).S_Type = SOCKET_TYPE_DGRAM),
       Post =>
         Model(Sock) = Model(Sock)'Old'Update
           (S_localIpAddr => Local_Ip_Addr,
            S_Local_Port  => Local_Port);

   procedure Socket_Listen
      (Sock    : Not_Null_Socket;
       Backlog : Natural)
       -- Error   :    out Error_T)
      with
        Global => 
            (Input => Net_Mutex,
             In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>+ (Sock, Backlog),
           null => Net_Mutex),
        Pre =>
          Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
          Is_Initialized_Ip(Socket_Table(Sock).S_localIpAddr) and then
          not Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
          Socket_Table(Sock).State = TCP_STATE_CLOSED,
        Post =>
          Model(Sock) = Model(Sock)'Old'Update
                  (S_State => TCP_STATE_LISTEN);

   procedure Socket_Accept
      (Sock           :     Not_Null_Socket;
       Client_Ip_Addr : out IpAddr;
       Client_Port    : out Port;
       Client_Socket  : out Socket)
      with
       Global =>
         (Input  => Net_Mutex,
          In_Out => (Tcp_Dynamic_Port, Socket_Table)),
       Depends =>
          (Socket_Table     => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Client_Ip_Addr   => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Client_Port      => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Client_Socket    => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Tcp_Dynamic_Port =>+ (Sock, Socket_Table),
           null             => Net_Mutex),
       Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
              Is_Initialized_Ip(Socket_Table(Sock).S_localIpAddr) and then
              not Is_Initialized_Ip(Socket_Table(Sock).S_remoteIpAddr) and then
              Socket_Table(Sock).State = TCP_STATE_LISTEN,
       Post => Model(Sock) = Model(Sock)'Old and then
               Is_Initialized_Ip(Client_Ip_Addr) and then
               Client_Port > 0 and then
               Client_Socket /= -1 and then
               Socket_Table(Client_Socket).S_Type = Socket_Table(Sock).S_Type and then
               Socket_Table(Client_Socket).S_Protocol = Socket_Table(Sock).S_Protocol and then
               Socket_Table(Client_Socket).S_Local_Port = Socket_Table(Sock).S_Local_Port and then
               Socket_Table(Client_Socket).S_localIpAddr = Socket_Table(Sock).S_localIpAddr and then
               Socket_Table(Client_Socket).S_remoteIpAddr = Client_Ip_Addr and then
               Socket_Table(Client_Socket).S_Remote_Port = Client_Port;

end Socket_Interface;
