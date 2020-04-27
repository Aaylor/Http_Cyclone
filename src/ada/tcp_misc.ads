with Type_Def; use Type_Def;
with Socket_Type; use Socket_Type;
with Tcp_Types; use Tcp_Types;
with Error_H; use Error_H;

package Tcp_Misc is

    ----------------------
    -- Tcp_Send_Segment --
    ----------------------

    procedure Tcp_Send_Segment (
        Sock         : in out Socket;
        Flags        : in     UInteger_8;
        Seq_Num      : in     UInteger_32;
        Ack_Num      : in     UInteger_32;
        Length       : in     Size;
        Add_To_Queue : in     Boolean;
        Error        :    out Error_T;
    );

    ----------------------
    -- Tcp_Change_State --
    ----------------------

    procedure Tcp_Change_State (
        Sock      : in out Socket;
        New_State :        Tcp_State
    )
    with
        Depends => (Sock =>+ (New_State));

    -- TODO : to finish (see TODO in code)
    procedure Tcp_Update_Events (
        Sock : in out Socket
    )
    with 
        Depends => (Sock => Sock);

end Tcp_Misc;