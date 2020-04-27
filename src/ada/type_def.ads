with Os_Port_FreeRTOS; use Os_Port_FreeRTOS;
with Interfaces.C; use Interfaces.C;

package Type_Def is

    MAX_BYTES : constant := 128;

    type Port is range 0 .. (2 ** 16 - 1);
    type Sock_Descriptor is new unsigned;
    type Sock_Type is new unsigned;
    type Sock_Protocol is new unsigned;
    type uint8 is mod 2 ** 8;
    subtype Index is unsigned range 0 .. MAX_BYTES;
    type Block8 is array (Index range <>) of uint8;

    type Bool is new int;
    type Systime is new unsigned_long;

end Type_Def;