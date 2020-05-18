pragma Unevaluated_Use_Of_Old (Allow);

with Common_Type;  use Common_Type;
with Error_H;      use Error_H;
with Interfaces.C; use Interfaces.C;
with Socket_Types; use Socket_Types;
with Tcp_Type;     use Tcp_Type;

package Tcp_Misc_Binding with
   SPARK_Mode
is

   procedure Tcp_Change_State
      (Sock      : in Not_Null_Socket;
       New_State : in Tcp_State)
      with
        Global => (In_Out => Socket_Table),
        Depends => (Socket_Table =>+ (Sock, New_State)),
        Post => Model(Sock) = Model(Sock)'Old'Update
                              (S_State => New_State);

   procedure Tcp_Wait_For_Events
      (Sock       : in     Not_Null_Socket;
       Event_Mask : in     Socket_Event;
       Timeout    : in     Systime;
       Event      :    out Socket_Event)
      with
         Global => (In_Out => Socket_Table),
         Depends =>
           (Socket_Table =>+ (Sock, Event_Mask, Timeout),
            Event        => (Event_Mask, Timeout)),
         Pre  => Event_Mask /= 0,
         Post => Event = 0 or else
                 ((Event_Mask and Event) /= 0),
         Contract_Cases =>
           (Socket_Table(Sock).State = TCP_STATE_SYN_SENT =>
               (if Event = SOCKET_EVENT_CONNECTED then
                  Model(Sock) = Model(Sock)'Old'Update
                        (S_State => TCP_STATE_ESTABLISHED)
               elsif Event = SOCKET_EVENT_CLOSED then
                  -- Maybe here it's not the correct state return
                  -- @TODO investigate
                  Model(Sock) = Model(Sock)'Old'Update
                        (S_State => TCP_STATE_CLOSED)),
            others => True);

   procedure Tcp_Write_Tx_Buffer
      (Sock    :        Not_Null_Socket;
       Seq_Num :        unsigned;
       Data    :        char_array;
       Length  :        unsigned)
      with
        Import        => True,
        Convention    => C,
        External_Name => "tcpWriteTxBuffer";

   procedure Tcp_Delete_Control_Block
      (Sock : Not_Null_Socket)
      with
        Post => Model(Sock) = Model(Sock)'Old;

   procedure Tcp_Send_Segment
      (Sock         :     Not_Null_Socket;
       Flags        :     uint8;
       Seq_Num      :     unsigned;
       Ack_Num      :     unsigned;
       Length       :     unsigned_long;
       Add_To_Queue :     Bool;
       Error        : out Error_T)
      with
        Global => (In_Out => Socket_Table),
        Depends =>
            ((Socket_Table, Error) =>
                  (Socket_Table, Sock, Flags, Seq_Num, Ack_Num, Length, Add_To_Queue)
            ),
        Post =>
            (if Error = NO_ERROR then
               Model (Sock) = Model(Sock)'Old);

   procedure Tcp_Update_Events
      (Sock : Not_Null_Socket)
      with
         Import => True,
         Convention => C,
         External_Name => "tcpUpdateEvents",
         Global => null;

end Tcp_Misc_Binding;
