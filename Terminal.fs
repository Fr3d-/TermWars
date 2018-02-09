module Terminal

#nowarn "9"
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

type OS =
    | Linux
    | MacOS
    | Windows

let currentOS =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
        Linux
    else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
        MacOS
    else if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        Windows
    else
        raise (PlatformNotSupportedException ("Couldn't detect your OS"))

//#region DisableEcho

[<DllImport("Kernel32")>]
extern void* GetStdHandle(int nStdHandle)

[<DllImport("Kernel32")>]
extern bool GetConsoleMode(void* hConsoleHandle, int* lpMode)

[<DllImport("Kernel32")>]
extern bool SetConsoleMode(void* hConsoleHandle, int lpMode)

let private disableTerminalEchoWindows () =
    let INVALID_HANDLE_VALUE = nativeint -1
    let STD_OUTPUT_HANDLE = -11
    let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004
    let ENABLE_ECHO_INPUT = 0x0004
    let handle = GetStdHandle(STD_OUTPUT_HANDLE)
    if handle <> INVALID_HANDLE_VALUE then
        let mode = NativePtr.stackalloc<int> 1
        if GetConsoleMode(handle, mode) then
            let value = NativePtr.read mode
            let value = value &&& (~~~ENABLE_ECHO_INPUT)
            if SetConsoleMode (handle, value) |> not then
                failwith "Couldn't set console mode"
        else
            failwith "Couldn't get"
    else
        failwith "Couldn't get handle"

[<DllImport("libc")>]
extern int tcgetattr(int fd, void* termios_data);

[<DllImport("libc")>]
extern int tcsetattr(int fd, int optional_actions, void* termios_data);

// 4 * 4 + 20 * 4 + 2 * 4
[<Struct>]
[<StructLayout(LayoutKind.Sequential, Size = 104)>]  
type private Termios =
    //[<MarshalAs(UnmanagedType.ByValArray, SizeConst=128)>]
    [<MarshalAs(UnmanagedType.U4)>]
    val c_iflag : System.UInt32
    [<MarshalAs(UnmanagedType.U4)>]
    val c_oflag : System.UInt32
    [<MarshalAs(UnmanagedType.U4)>]
    val c_cflag : System.UInt32
    [<MarshalAs(UnmanagedType.U4)>]
    // 20 * 4
    val mutable c_lflag : System.UInt32
    [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 80)>]
    val c_cc : System.Byte []

    [<MarshalAs(UnmanagedType.U4)>]
    val c_ispeed : System.UInt32

    [<MarshalAs(UnmanagedType.U4)>]
    val c_ospeed : System.UInt32

let private disableTerminalEchoLinux () =
    let ECHO = 8u
    let TCSANOW = 0
    let TCSAFLUSH = 2

    let ptr = Marshal.AllocHGlobal(Marshal.SizeOf(typeof<Termios>))
    Marshal.StructureToPtr (Termios (), ptr, false)
    tcgetattr(0, ptr) |> ignore
    let mutable str : Termios = Marshal.PtrToStructure ptr
    str.c_lflag <- str.c_lflag &&& (~~~ECHO)

    //let pt2 = Marshal.AllocHGlobal(Marshal.SizeOf(typeof<Termios>))
    Marshal.StructureToPtr (str, ptr, true)
    tcsetattr (0, TCSAFLUSH, ptr) |> ignore

    Marshal.FreeHGlobal (ptr)

let disableTerminalEcho () =
    match currentOS with
    | Linux
    | MacOS -> disableTerminalEchoLinux ()
    | Windows -> disableTerminalEchoWindows ()

let draw x y (fg: System.ConsoleColor) (bg:System.ConsoleColor) (symbol: string option) =
    Console.SetCursorPosition (x, y)
    Console.BackgroundColor <- bg
    Console.ForegroundColor <- fg

    match symbol with
    | Some s ->
        printf "%s" s
    | None ->
        printf " "

    System.Console.SetCursorPosition (0, 24)

let flush () =
    while (Console.KeyAvailable) do
        Console.ReadKey(true) |> ignore