with Net_Mem; use Net_Mem;
with Type_Def; use Type_Def;
with Interfaces.C; use Interfaces.C;

package Tcp_Types is


    --------------------
    -- TCP FSM states --
    --------------------

    type TCP_State is (
        TCP_STATE_CLOSED,
        TCP_STATE_LISTEN,
        TCP_STATE_SYN_SENT,
        TCP_STATE_SYN_RECEIVED,
        TCP_STATE_ESTABLISHED,
        TCP_STATE_CLOSE_WAIT,
        TCP_STATE_LAST_ACK,
        TCP_STATE_FIN_WAIT_1,
        TCP_STATE_FIN_WAIT_2,
        TCP_STATE_CLOSING,
        TCP_STATE_TIME_WAIT
    );
    
    for TCP_State use (
        TCP_STATE_CLOSED       => 0,
        TCP_STATE_LISTEN       => 1,
        TCP_STATE_SYN_SENT     => 2,
        TCP_STATE_SYN_RECEIVED => 3,
        TCP_STATE_ESTABLISHED  => 4,
        TCP_STATE_CLOSE_WAIT   => 5,
        TCP_STATE_LAST_ACK     => 6,
        TCP_STATE_FIN_WAIT_1   => 7,
        TCP_STATE_FIN_WAIT_2   => 8,
        TCP_STATE_CLOSING      => 9,
        TCP_STATE_TIME_WAIT    => 10
    );

    ---------------------------
    -- TCP congestion states --
    ---------------------------
   
    type TCP_Congest_State is (
        TCP_CONGEST_STATE_IDLE,
        TCP_CONGEST_STATE_RECOVERY,
        TCP_CONGEST_STATE_LOSS_RECOVERY
    );

    for TCP_Congest_State use (
        TCP_CONGEST_STATE_IDLE          => 0,
        TCP_CONGEST_STATE_RECOVERY      => 1,
        TCP_CONGEST_STATE_LOSS_RECOVERY => 2
    );

    -----------------------
    -- TCP control flags --
    -----------------------

    type TCP_Flags is (
        TCP_FLAG_FIN,
        TCP_FLAG_SYN,
        TCP_FLAG_RST,
        TCP_FLAG_PSH,
        TCP_FLAG_ACK,
        TCP_FLAG_URG
    );

    for TCP_Flags use (
        TCP_FLAG_FIN => 1,
        TCP_FLAG_SYN => 2,
        TCP_FLAG_RST => 4,
        TCP_FLAG_PSH => 8,
        TCP_FLAG_ACK => 16,
        TCP_FLAG_URG => 32
    );

    ----------------------
    -- TCP option types --
    ----------------------

    type TCP_Option_Kind is (
        TCP_OPTION_END,
        TCP_OPTION_NOP,
        TCP_OPTION_MAX_SEGMENT_SIZE,
        TCP_OPTION_WINDOW_SCALE_FACTOR,
        TCP_OPTION_SACK_PERMITTED,
        TCP_OPTION_SACK,
        TCP_OPTION_TIMESTAMP
    );

    for TCP_Option_Kind use (
        TCP_OPTION_END                 => 0,
        TCP_OPTION_NOP                 => 1,
        TCP_OPTION_MAX_SEGMENT_SIZE    => 2,
        TCP_OPTION_WINDOW_SCALE_FACTOR => 3,
        TCP_OPTION_SACK_PERMITTED      => 4,
        TCP_OPTION_SACK                => 5,
        TCP_OPTION_TIMESTAMP           => 8
    );


    ----------------
    -- TCP header --
    ----------------

    type UInteger_32 is range 0 .. (2 ** 32 - 1);
    type UInteger_8 is range 0 .. (2 ** 8 - 1);
    type UInteger_16 is range 0 .. (2 ** 16 - 1);
    type Options_Array is array (Positive range <>) of UInteger_8; 

    type TCP_Header is record
        Src_Port : Port;
        Dest_Port : Port;
        Seq_Num : UInteger_32;
        Ack_Num : UInteger_32;
        Reserved1 : UInteger_8;
        Data_Offset : UInteger_8;
        Flags : UInteger_8;
        Reserved2 : UInteger_8;
        Window : UInteger_16;
        Checksum : UInteger_16;
        Urgent_Pointer : UInteger_16;
        -- Null terminating tab.
        Options : Options_Array (1 .. 128); -- Find a way to remove this use tab.
    end record;


    ---------------
    -- TCP timer --
    ---------------

    type TCP_Timer is record
        Running: Boolean;
        Start_Time : Systime;
        Interval : Systime;
    end record;


   
   -- TODO: use preprocessing instead of 14 to be coherent with
   -- the C code.
    type Chunk_Desc_Array is array(0 .. 14) of Chunk_Desc;

    type Tcp_Tx_Buffer is record
        chunkCount: unsigned;
        maxChunkCound: unsigned;
        chunk: Chunk_Desc_Array;
    end record
        with Convention => C;

    type Tcp_Rx_Buffer is record
        chunkCount: unsigned;
        maxChunkCound: unsigned;
        chunk: Chunk_Desc_Array;
    end record
        with Convention => C;

    type Tcp_Timer is record
        running: Bool;
        startTime: Systime;
        interval: Systime;
    end record
        with Convention => C;

    type TcpQueueItem is record
        length: unsigned;
    end record
        with Convention => C;
    
    type Tcp_Sack_Block is record
        leftEdge: unsigned_long;
        rightEdge: unsigned_long;
    end record
        with Convention => C;

end Tcp_Types;