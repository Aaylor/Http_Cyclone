with Tcp_Types; use TCP_Types;
with Os_Port_FreeRTOS; use Os_Port_FreeRTOS;

package body Tcp_Misc is


    ----------------------
    -- Tcp_Change_State --
    ----------------------

    --@brief Update TCP FSM current state
    --@param in Sock: Handle referencing the socket
    --@param in New_State: New TCP state to switch to
    procedure Tcp_Change_State (
            Sock      : in out Socket;
            New_State :        Tcp_State)
    is
    begin
        -- Enter CLOSED state?
        if New_State = TCP_STATE_CLOSED then
            --Check previous state
            if Sock.State = TCP_STATE_LAST_ACK or
               Sock.State = TCP_STATE_TIME_WAIT then
               -- The connection has been closed properly
               Sock.closedFlag := 1; -- 1 for True. Find a better way to do that
            else
                -- The connection has been reset by the peer
                Sock.resetFlag := 1;
            end if;
        end if;

        -- Enter the desired state
        Sock.state := New_State;
        -- Update TCP related events
        Tcp_Update_Events (Sock);
    end Tcp_Change_State;

    



    -----------------------
    -- Tcp_Update_Events --
    -----------------------

    --@brief Update TCP related events
    --@param in Sock: Handle referencing the socket
    procedure Tcp_Update_Events (
        Sock : in out Socket)
    is
    begin
        -- Clear event flags
        Sock.S_Event_Flags := 0;
        
        -- Check current TCP state
        case Sock.State is
            -- ESTABLISHED or FIN-WAIT-1 state?
            when  TCP_STATE_ESTABLISHED
                | TCP_STATE_FIN_WAIT_1 => 
                Sock.S_Event_Flags := SOCKET_EVENT_CONNECTED'Enum_Rep;

            -- FIN-WAIT-2 state?
            when TCP_STATE_FIN_WAIT_2 =>
                Sock.S_Event_Flags := SOCKET_EVENT_CONNECTED'Enum_Rep()
                                    + SOCKET_EVENT_TX_SHUTDOWN'Enum_Rep();

            -- CLOSE-WAIT, LAST-ACK or CLOSING state?
            when  TCP_STATE_CLOSE_WAIT
                | TCP_STATE_LAST_ACK
                | TCP_STATE_CLOSING =>
                Sock.S_Event_Flags := SOCKET_EVENT_CONNECTED'Enum_Rep()
                                    + SOCKET_EVENT_RX_SHUTDOWN'Enum_Rep();
            
            -- TIME-WAIT or CLOSED state?
            when  TCP_STATE_TIME_WAIT
                | TCP_STATE_CLOSED =>
                Sock.S_Event_Flags := SOCKET_EVENT_CONNECTED'Enum_Rep()
                                    + SOCKET_EVENT_TX_SHUTDOWN'Enum_Rep()
                                    + SOCKET_EVENT_RX_SHUTDOWN'Enum_Rep();
        end case;

        -- Handle TX specific events
        if Sock.State = TCP_STATE_SYN_SENT
                or Sock.State = TCP_STATE_SYN_RECEIVED then

            -- Disallow write operations until the connection is established
            Sock.S_Event_Flags := Sock.S_Event_Flags
                                + SOCKET_EVENT_TX_DONE'Enum_Rep()
                                + SOCKET_EVENT_TX_ACKED'Enum_Rep();

        elsif Sock.State = TCP_STATE_ESTABLISHED
                or Sock.State = TCP_STATE_CLOSE_WAIT then

            -- Check whether the send buffer is full or not
            if (Sock.sndUser + Sock.sndNxt - Sock.sndUna) < Sock.txBufferSize then
                Sock.S_Event_Flags := Sock.S_Event_Flags
                                    + SOCKET_EVENT_TX_READY'Enum_Rep();
            end if;

            -- Check whether all the data in the send buffer has been transmitted
            if Sock.sndUser = 0 then
                -- All the pending data has been sent out
                Sock.S_Event_Flags := Sock.S_Event_Flags
                                    + SOCKET_EVENT_TX_DONE'Enum_Rep();
                -- Check whether an acknowledgment has been received
                if Sock.sndUna - Sock.sndNxt >= 0 then
                    Sock.S_Event_Flags := Sock.S_Event_Flags
                                        + SOCKET_EVENT_TX_ACKED'Enum_Rep();
                end if;
            end if;

        elsif Sock.State /= TCP_STATE_LISTEN then
            -- Unblock user task if the connection is being closed
            Sock.S_Event_Flags := SocK.S_Event_Flags
                                + SOCKET_EVENT_TX_READY'Enum_Rep()
                                + SOCKET_EVENT_TX_DONE'Enum_Rep()
                                + SOCKET_EVENT_TX_ACKED'Enum_Rep();
        end if;

        -- Handle RX specific events
        if Sock.State = TCP_STATE_ESTABLISHED
                or Sock.State = TCP_STATE_FIN_WAIT_1
                or Sock.State = TCP_STATE_FIN_WAIT_2 then
        
            -- Data is available for reading?
            if Sock.rcvUser > 0 then
                Sock.S_Event_Flags := Sock.S_Event_Flags
                                    + SOCKET_EVENT_RX_READY'Enum_Rep();
            end if;
        
        elsif Sock.State = TCP_STATE_LISTEN then
            -- If the socket is currently in the listen state, it will be marked
            -- as readable if an incoming connection request has been received
            if Sock.synQueue /= null then
                Sock.S_Event_Flags := Sock.S_Event_Flags
                                    + SOCKET_EVENT_RX_READY'Enum_Rep();
            end if;
        
        elsif Sock.State /= TCP_STATE_SYN_SENT
                or Sock.State /= TCP_STATE_SYN_RECEIVED then
            -- Readability can also indicate that a request to close
            -- the socket has been received from the peer
            Sock.S_Event_Flags := Sock.S_Event_Flags
                                + SOCKET_EVENT_RX_READY'Enum_Rep();
        end if;

        -- Check whether the socket is bound to a particular network interface
        --@TODO

        -- Mask unused events
        Sock.S_Event_Flags := Sock.S_Event_Flags and Sock.S_Event_Mask;

        -- Any Event to signal?
        if Sock.S_Event_Flags /= 0 then
            -- Unblock I/O operations currently in waiting state
            osSetEvent (Sock.S_event'Address);

            -- Set user event to signaled state if necessary
            if Sock.userEvent /= null then
                osSetEvent (Sock.userEvent);
            end if;
        end if;

    end Tcp_Update_Events;

end Tcp_Misc;