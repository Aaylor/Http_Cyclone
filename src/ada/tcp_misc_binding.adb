with System;

package body Tcp_Misc_Binding with
   SPARK_Mode => Off
is

   procedure Tcp_Delete_Control_Block 
      (Sock : Not_Null_Socket)
   is
      procedure tcpDeleteControlBlock
         (Sock : System.Address)
      with
        Import        => True,
        Convention    => C,
        External_Name => "tcpDeleteControlBlock";
   begin
      tcpDeleteControlBlock (Socket_Table(Sock)'Address);
   end Tcp_Delete_Control_Block;


   procedure Tcp_Change_State
      (Sock      : in Not_Null_Socket;
       New_State : in Tcp_State)
   is
      procedure tcpUpdateEvents (Sock : System.Address) with
         Import        => True,
         Convention    => C,
         External_Name => "tcpUpdateEvents";
   begin
      -- Enter CLOSED State?
      if New_State = TCP_STATE_CLOSED then
         -- Check previous state
         if Socket_Table(Sock).State = TCP_STATE_LAST_ACK
           or else Socket_Table(Sock).State = TCP_STATE_TIME_WAIT
         then
            -- The connection has been closed properly
            Socket_Table(Sock).closed_Flag := True;
         else
            -- the connection has been reset by the peer
            Socket_Table(Sock).reset_Flag := True;
         end if;
      end if;

      -- Enter the desired state
      Socket_Table(Sock).State := New_State;
      -- Update TCP related events
      tcpUpdateEvents (Socket_Table(Sock)'Address);
   end Tcp_Change_State;

   procedure Tcp_Wait_For_Events
      (Sock       : in     Not_Null_Socket;
       Event_Mask : in     Socket_Event;
       Timeout    : in     Systime;
       Event      :    out Socket_Event)
   is
      function tcpWaitForEvents
        (Sock      : System.Address;
         eventMask : unsigned;
         timeout   : Systime)
         return unsigned with
         Import        => True,
         Convention    => C,
         External_Name => "tcpWaitForEvents";
   begin
      Event := Socket_Event(tcpWaitForEvents (Socket_Table(Sock)'Address, unsigned(Event_Mask), Timeout));
   end Tcp_Wait_For_Events;

   procedure Tcp_Send_Segment
      (Sock         : in     Not_Null_Socket;
       Flags        :        uint8;
       Seq_Num      :        unsigned;
       Ack_Num      :        unsigned;
       Length       :        unsigned_long;
       Add_To_Queue :        Bool;
       Error        :    out Error_T)
   is
      function tcpSendSegment
         (Sock         : System.Address;
          Flags        : uint8;
          Seq_Num      : unsigned;
          Ack_Num      : unsigned;
          Length       : unsigned_long;
          Add_To_Queue : Bool) return unsigned
         with
            Import => True,
            Convention => C,
            External_Name => "tcpSendSegment";
   begin
      Error :=
         Error_T'Enum_Val(tcpSendSegment
            (Socket_Table(Sock)'Address, Flags, Seq_Num, Ack_Num, Length, Add_To_Queue));
   end Tcp_Send_Segment;

end Tcp_Misc_Binding;
