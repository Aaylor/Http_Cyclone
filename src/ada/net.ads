with Os_Port_FreeRTOS; use Os_Port_FreeRTOS;

package Net is

    Net_Mutex : aliased constant Os_Mutex
    with
      Import => True,
      Convention => C,
      External_Name => "netMutex";

end Net;