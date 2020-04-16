with System;

package body Socket_interface 
with SPARK_Mode => Off
is

    procedure Get_Host_By_Name (
        Server_Name    : char_array; 
        Server_Ip_Addr : out IpAddr;
        Flags : Host_Resolver_Flags;
        Error : out Error_T)
    is
        F : Natural := 0;
    begin
        for I in Flags'Range loop
            F := F + Host_Resolver'Enum_Rep(Flags(I));
        end loop;
        Error := Error_T'Enum_Val(getHostByName(System.Null_Address, (Server_Name), Server_Ip_Addr, unsigned(F)));
    end;


    procedure Socket_Open (
        Sock:   out Socket_Struct;
        S_Type:     Socket_Type; 
        S_Protocol: Socket_Protocol)
    is 
    begin
        Sock := socketOpen(Socket_Type'Enum_Rep(S_Type), Socket_Protocol'Enum_Rep(S_Protocol));
    end Socket_Open;
    

    procedure Socket_Set_Timeout (
        Sock :    in out Socket_Struct;
        Timeout:  Systime;
        Error :   out Error_T)
    is
    begin
        Error := Error_T'Enum_Val(socketSetTimeout(Sock, Timeout));
    end Socket_Set_Timeout;

    procedure Socket_Connect (
        Sock: in out Socket_Struct;
        Remote_Ip_Addr : in IpAddr;
        Remote_Port : in Sock_Port;
        Error : out Error_T)
    is 
    begin
        Error := Error_T'Enum_Val(socketConnect (Sock, Remote_Ip_Addr, Remote_Port));
    end Socket_Connect;

    procedure Socket_Send (
        Sock: in Socket_Struct;
        Data: in char_array;
        Error : out Error_T)
    is
        Written : unsigned;
    begin
        Error := Error_T'Enum_Val(socketSend(Sock, Data, Data'Length, Written, 0));
    end Socket_Send;

    procedure Socket_Receive(
        Sock: Socket_Struct;
        Buf: out char_array;
        Error : out Error_T)
    is
        Received, Ret : unsigned;
    begin
        Ret := socketReceive(Sock, Buf, Buf'Length - 1, Received, 0);
        Error := Error_T'Enum_Val(Ret);
    end Socket_Receive;

    procedure Socket_Shutdown (
        Sock: Socket_Struct;
        Error : out Error_T)
    is
    begin
        Error := Error_T'Enum_Val(socketShutdown(Sock, 2));
    end Socket_Shutdown;

    procedure Socket_Close (Sock : in out Socket_Struct)
    is
    begin
        socketClose (Sock);
    end Socket_Close;

    procedure Socket_Set_Tx_Buffer_Size (
        Sock : in out Socket_Struct;
        Size :        Buffer_Size;
        Error:    out Error_T)
    is
    begin
        Error := Error_T'Enum_Val(socketSetTxBufferSize(Sock, unsigned_long(Size)));
    end Socket_Set_Tx_Buffer_Size;

    procedure Socket_Set_Rx_Buffer_Size (
        Sock : in out Socket_Struct;
        Size :        Buffer_Size;
        Error:    out Error_T)
    is
    begin
        Error := Error_T'Enum_Val(socketSetRxBufferSize(Sock, unsigned_long(Size)));
    end Socket_Set_Rx_Buffer_Size;


end Socket_interface;