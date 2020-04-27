with Os_Port_FreeRTOS; use Os_Port_FreeRTOS;
with Interfaces.C; use Interfaces.C;
with Tcp_Types; use Tcp_Types;

package Socket_Type is

    type SackBlockArray is array (0 .. 3) of Tcp_Sack_Block;

    -----------------------
    -- Socket Definition --
    -----------------------

    type Socket_Struct is record
        S_Descriptor: Sock_Descriptor;
        S_Type: Sock_Type;
        S_Protocol: Sock_Protocol;
        S_Net_Interface: System.Address;
        S_localIpAddr: IpAddr;
        S_Local_Port: Port;
        S_remoteIpAddr: IpAddr;
        S_Remote_Port: Port;
        S_Timeout: Systime;
        S_TTL: unsigned_char;
        S_Multicast_TTL: unsigned_char;
        S_errnoCode: int;
        S_event: OsEvent;
        S_Event_Mask: unsigned;
        S_Event_Flags: unsigned;
        userEvent: System.Address;

        -- TCP specific variables
        State: Tcp_State;
        owned_Flag: Bool;
        closed_Flag: Bool;
        reset_Flag: Bool;
        
        smss: unsigned_short;
        rmss: unsigned_short;
        iss: unsigned_long;
        irs: unsigned_long;
        
        sndUna: unsigned_long;
        sndNxt: unsigned_long;
        sndUser: unsigned_short;
        sndWnd: unsigned_short;
        maxSndWnd: unsigned_short;
        sndWl1: unsigned_long;
        sndWl2: unsigned_long;
        
        rcvNxt: unsigned_long;
        rcvUser: unsigned_short;
        rcvWnd: unsigned_short;
        
        rttBusy: Bool;
        rttSeqNum: unsigned_long;
        rettStartTime: Systime;
        srtt: Systime;
        rttvar: Systime;
        rto: Systime;
        
        congestState: TCP_Congest_State;
        cwnd: unsigned_short;
        ssthresh: unsigned_short;
        dupAckCount: unsigned;
        n: unsigned;
        recover: unsigned_long;
        
        txBuffer: Tcp_Tx_Buffer;
        txBufferSize: unsigned_long;
        rxBuffer: Tcp_Rx_Buffer;
        rxBufferSize: unsigned_long;
        
        retransmitQueue: System.Address;
        retransmitTimer: Tcp_Timer;
        retransmitCount: unsigned;
        
        synQueue: System.Address;
        synQueueSize: unsigned;
        
        wndProbeCount: unsigned;
        wndProbeInterval: Systime;
        
        persistTimer: Tcp_Timer;
        overrideTimer: Tcp_Timer;
        finWait2Timer: Tcp_Timer;
        timeWaitTimer: Tcp_Timer;
        
        sackPermitted: Bool;
        sackBlock: SackBlockArray;
        sackBlockCount: unsigned;
        
        receiveQueue: System.Address;
    end record 
        with Convention => C;

    type Socket is access Socket_Struct;

end Socket_Type;