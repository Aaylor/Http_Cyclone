with System; use System;

package Os_Port_FreeRTOS is

    -- This record type is consistant with the
    -- OsMutex type for freertos
    type Os_Mutex is record
        Handle: Address;
    end record
    with
      Convention => C;

    type OsEvent is record
        handle: System.Address;
    end record
      with Convention => C;

    -- Mutex management
    procedure osAcquireMutex (Mutex: System.Address)
    with
        Import => True,
        Convention => C,
        External_Name => "osAcquireMutex";

    procedure osReleaseMutex (Mutex: System.Address)
    with
        Import => True,
        Convention => C,
        External_Name => "osReleaseMutex";

    procedure osSetEvent (event: System.Address)
    with
        Import => True,
        Convention => C,
        External_Name => "osSetEvent";

end Os_Port_FreeRTOS;