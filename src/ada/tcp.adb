with System;
with Net; use Net;
with Os_Port_FreeRTOS; use Os_Port_FreeRTOS;

package body Tcp is

    procedure Tcp_Init 
    is
    begin
        -- Reset ephemeral port number
        Tcp_Dynamic_Port := 0;
    end Tcp_Init;



    procedure Tcp_Get_Dynamic_Port (
               P: out Port
    )
    is
        function netGetRand return unsigned
        with
            Import => True,
            Convention => C,
            External_Name => "netGetRand";

        -- Retrieve current port number
        P := Tcp_Dynamic_Port;
    begin
        -- Invalid port number?
        if P < SOCKET_EPHEMERAL_PORT_MIN or P > SOCKET_EPHEMERAL_PORT_MAX then
            P := SOCKET_EPHEMERAL_PORT_MIN + netGetRand mod (SOCKET_EPHEMERAL_PORT_MAX - SOCKET_EPHEMERAL_PORT_MIN + 1);
        end if;

        if P < SOCKET_EPHEMERAL_PORT_MAX then
            Tcp_Dynamic_Port := P + 1;
        else
            Tcp_Dynamic_Port := SOCKET_EPHEMERAL_PORT_MIN;
        end if;
    end Tcp_Get_Dynamic_Port;


    procedure Tcp_Get_State (
                Sock  :     Socket;
                State : out Tcp_State
    )
    is
    begin
        osAcquireMutex (Net_Mutex'Address);
        State := Sock.State;
        osReleaseMutex (Net_Mutex'Address);
    end Tcp_Get_State;

end Tcp;