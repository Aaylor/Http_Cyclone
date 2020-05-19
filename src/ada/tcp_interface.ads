pragma Ada_2020;
pragma Unevaluated_Use_Of_Old (Allow);

with Common_Type;  use Common_Type;
with Error_H;      use Error_H;
with Interfaces.C; use Interfaces.C;
with Ip;           use Ip;
with Net;          use Net;
with Socket_Types; use Socket_Types;
with Tcp_Type;     use Tcp_Type;

package Tcp_Interface
  with SPARK_Mode
is
    -- Ephemeral ports are used for dynamic port assignment
    Tcp_Dynamic_Port : Port;

    function Tcp_Init return Error_T
      with
        Import => True,
        Convention => C,
        External_Name => "tcpInit";

    procedure Tcp_Get_Dynamic_Port
      (P : out Port)
      with
        Global =>
          (In_Out => Tcp_Dynamic_Port),
        Depends =>
          (P                => Tcp_Dynamic_Port,
           Tcp_Dynamic_Port => Tcp_Dynamic_Port),
        Post =>
          P in SOCKET_EPHEMERAL_PORT_MIN .. SOCKET_EPHEMERAL_PORT_MAX and then
          Tcp_Dynamic_Port in SOCKET_EPHEMERAL_PORT_MIN .. SOCKET_EPHEMERAL_PORT_MAX;

    procedure Tcp_Connect
      (Sock           :        Not_Null_Socket;
       Remote_Ip_Addr :        IpAddr;
       Remote_Port    :        Port;
       Error          :    out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
          (Socket_Table  =>+ (Sock, Remote_Ip_Addr, Remote_Port),
           Error         =>  (Sock, Remote_Port, Remote_Ip_Addr, Socket_Table)),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
               Is_Initialized_Ip (Remote_Ip_Addr) and then
               -- @Clément : not sure this condition is wanted
               -- I don't know what happen if the connexion isn't closed.
               Socket_Table(Sock).State = TCP_STATE_CLOSED,
        Post =>
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
               Socket_Table(Sock).State = TCP_STATE_ESTABLISHED);

    procedure Tcp_Listen
      (Sock    : Not_Null_Socket;
       Backlog : Unsigned)
       -- Error   :    out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
          (Socket_Table =>+ (Sock, Backlog)),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
               Socket_Table(Sock).State = TCP_STATE_CLOSED,
        Post =>
          Model(Sock) = Model(Sock)'Old'Update
                  (S_State => TCP_STATE_LISTEN);

    procedure Tcp_Accept
      (Sock           : in     Not_Null_Socket;
       Client_Ip_Addr :    out IpAddr;
       Client_Port    :    out Port;
       Client_Socket  :    out Socket)
      with
        Global => 
          (Input  => Net_Mutex,
           In_Out => (Tcp_Dynamic_Port, Socket_Table)),
        Depends =>
          (Socket_Table     => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Client_Ip_Addr   => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Client_Port      => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Client_Socket    => (Sock, Tcp_Dynamic_Port, Socket_Table),
           Tcp_Dynamic_Port => (Socket_Table, Tcp_Dynamic_Port, Sock),
           null             => Net_Mutex),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
               -- Ensure the socket was previously placed in the listening state
               Socket_Table(Sock).State = TCP_STATE_LISTEN,
        Post =>
            (if Socket_Table(Sock).State = TCP_STATE_SYN_RECEIVED then
               (Model(Sock) = Model(Sock)'Old'Update
                        (S_State => TCP_STATE_SYN_RECEIVED) and then
               Is_Initialized_Ip (Client_Ip_Addr) and then
               Client_Port > 0 and then
               (if Client_Socket /= -1 then
                  Socket_Table(Client_Socket).S_Type = SOCKET_TYPE_STREAM and then
                  Socket_Table(Client_Socket).S_Protocol = SOCKET_IP_PROTO_TCP and then
                  Is_Initialized_Ip(Socket_Table(Client_Socket).S_localIpAddr) and then
                  Socket_Table(Client_Socket).S_Local_Port = Socket_Table(Sock).S_Local_Port and then
                  Socket_Table(Client_Socket).S_RemoteIpAddr = Client_Ip_Addr and then
                  Socket_Table(Client_Socket).S_Remote_Port = Client_Port)));
   
    procedure Tcp_Send
      (Sock    : Not_Null_Socket;
       Data    :     Char_Array;
       Written : out Integer;
       Flags   :     Unsigned;
       Error   : out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
          ((Socket_Table, Written, Error) => (Socket_Table, Sock, Data, Flags)),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM,
        Post =>
          Model(Sock) = Model(Sock)'Old and then
          (if Error = No_ERROR then Written > 0);

    procedure Tcp_Receive
      (Sock     :     Not_Null_Socket;
       Data     : out Char_Array;
       Received : out Unsigned;
       Flags    :     Unsigned;
       Error    : out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
          ((Socket_Table, Data, Error, Received) =>
                  (Socket_Table, Sock, Data, Flags)),
        Pre =>
          Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM and then
          Is_Initialized_Ip (Socket_Table(Sock).S_RemoteIpAddr) and then
          Data'Last >= Data'First,
        Post =>
          Model(Sock) = Model(Sock)'Old and then
          (if Error = NO_ERROR then
             Received > 0
           elsif Error = ERROR_END_OF_STREAM then
             Received = 0);

    procedure Tcp_Shutdown
      (Sock  :     Not_Null_Socket;
       How   :     Socket_Shutdown_Flags;
       Error : out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
          ((Socket_Table, Error) => (Socket_Table, Sock, How)),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM,
        Post =>
          Model(Sock) = Model(Sock)'Old;
   
    procedure Tcp_Abort
      (Sock  : in     Not_Null_Socket;
       Error :    out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends => ((Socket_Table, Error) => (Sock, Socket_Table)),
        Contract_Cases => -- @TODO
                -- It's True for all state except TCP_STATE_TIME_WAIT
         (Socket_Table(Sock).State = TCP_STATE_TIME_WAIT => 
               Model(Sock) = Model(Sock)'Old,
          others => 
                Socket_Table(Sock).S_Type = SOCKET_TYPE_UNUSED and then
                Socket_Table(Sock).State = TCP_STATE_CLOSED);

    procedure Tcp_Kill_Oldest_Connection
      (Sock : out Socket)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
            (Sock         => Socket_Table,
             Socket_Table =>+ null),
        Post =>
            (for all S in Socket_Table'Range =>
                (if S /= Sock then 
                  Model_Socket_Table(S) = 
                     Model_Socket_Table'Old(S))) and then
            (if Sock /= -1 then
              Socket_Table(Sock).S_Type = SOCKET_TYPE_UNUSED);

    procedure Tcp_Get_State
      (Sock  : in     Not_Null_Socket;
       State :    out Tcp_State)
      with
        Global  => (Input => (Net_Mutex, Socket_Table)),
        Depends =>
          (State => (Sock, Socket_Table),
           null  => Net_Mutex),
        Pre => Socket_Table(Sock).S_Type = SOCKET_TYPE_STREAM,
        Post =>
          State = Socket_Table(Sock).State and then
          Model(Sock) = Model(Sock)'Old;

end Tcp_Interface;
