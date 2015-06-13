:- module fjnetm.
:- interface.

:- type fjnetm.socket.

:- type fjnetm.error ---> 
    ok
    ; general_failure
    ; alread_connected
    ; not_connected
    ; refused
    ; timed_out
    ; bad_permissions
    ; bad_address.

:- import_module io.
:- import_module maybe.
:- import_module bool.

:- pred fjnetm.create_socket(fjnetm.socket::uo, io::di, io::uo) is det.
:- pred fjnetm.destroy_socket(fjnetm.socket::in, io::di, io::uo) is det.

% fjnetm.connect_socket(Socket::in, Address::in, Port::in, MS_Timeout::in, Err::out, IO_in::di, IO_out::uo)
:- pred fjnetm.connect_socket(fjnetm.socket::in, string::in, int::in, int::in, fjnetm.error::out, io::di, io::uo) is det.
:- pred fjnetm.disconnect_socket(fjnetm.socket::in, fjnetm.error::out, io::di, io::uo) is det.

% fjnetm.listen_socket(Socket::in, Port::in, Err::out, IO_in::di, IO_out::uo)
:- pred fjnetm.listen_socket(fjnetm.socket::in, int::in, fjnetm.error::out, io::di, io::uo) is det.

% fjnetm.accept_socket(Socket::in, NewSocket::uo, IO_in::di, IO_out::uo)
:- pred fjnetm.accept_socket(fjnetm.socket::in, maybe(fjnetm.socket)::uo, io::di, io::uo) is det.

:- func fjnetm.read_pending(fjnetm.socket::in, io::di, io::uo) = (bool::uo) is det.

:- pred fjnetm.read(fjnetm.socket::in, string::uo, Err::out, io::di, io::uo) is det.
:- pred fjnetm.write(fjnetm.socket::in, string::in, Err::out, io::di, io::uo) is det.

:- implementation.

:- import_module bool.

% Validation for accepting sockets.
:- func socket_is_valid(fjnetm.socket::di, fjnetm.socket::uo, io::di, io::uo) = (bool::uo) is det.
:- func fjnetm.accept_socket(fjnetm.socket::in, io::di, io::uo) = (fjnetm.socket::uo) is det.

:- pragma foreign_decl("C", "#include <libfjnet/socket.h>").

:- pragma foreign_enum("C", fjnetm.error/0,
    [
        ok - "eSuccess",
        not_connected  - "eNotConnected",
        refused - "eRefused",
        timed_out - "eTimeout",
        general_failure - "eFailure",
        alread_connected - "eAlreadyConnected",
        bad_permissions - "ePermission",
        bad_address - "eBadAddress"
    ]
).

:- pragma foreign_type("C", fjnetm.socket, "struct WSocket *").

:- pragma foreign_proc("C", socket_is_valid(Socket::di, SocketOut::uo, IO_in::di, IO_out::uo) = (Bool::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Bool = ((Socket==NULL) || (State_Socket(Socket)!=0))?MR_NO:MR_YES;
        SocketOut = Socket;
    ").

:- pragma foreign_proc("C", fjnetm.accept_socket(Socket::in, IO_in::di, IO_out::uo) = (NewSocket::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        NewSocket = Accept_Socket(Socket);
    ").

fjnetm.accept_socket(Socket::in, NewSocket::uo, IO_in::di, IO_out::uo) :-
    NewSocketRaw = fjnetm.accept_socket(Socket, IO_in, IO_temp),
    Valid = socket_is_valid(NewSocketRaw, SocketRaw, IO_temp, IO_out),
    (
        Valid = yes,
        NewSocket = yes(SocketRaw)
    ;
        Valid = no,
        NewSocket = no
    ).

:- pragma foreign_proc("C", fjnetm.create_socket(Socket::uo, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Socket = Create_Socket();
    ").
    
:- pragma foreign_proc("C", fjnetm.destroy_socket(Socket::in, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Destroy_Socket(Socket);
    ").
    
:- pragma foreign_proc("C", fjnetm.connect_socket(Socket::in, Address::in, Port::in, MS_Timeout::in, Err::out, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Err = Connect_Socket(Socket, Address, Port, MS_Timeout);
    ").

:- pragma foreign_proc("C", fjnetm.disconnect_socket(Socket::in, Err::out, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Err = Disconnect_Socket(Socket);
    ").
    
:- pragma foreign_proc("C", fjnetm.listen_socket(Socket::in, Port::in, Err::out, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Err = Listen_Socket(Socket, Port);
    ").

    
:- pragma foreign_proc("C", fjnetm.read_pending(Socket::in, IO_in::di, IO_out::uo) = (Bool::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Bool = (Length_Socket(Socket)>0)?MR_YES:MR_NO;
    ").

:- pragma foreign_proc("C", fjnetm.read(Socket::in, String::uo, Err::out, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        {
            char *temp = NULL;
            Err = Read_Socket(Socket, &temp);
            String = temp;
        }
    ").


:- pragma foreign_proc("C", fjnetm.write(Socket::in, String::in, Err::out, IO_in::di, IO_out::uo),
    [will_not_call_mercury, promise_pure],
    "
        IO_out = IO_in;
        Err = Write_Socket(Socket, String);
    ").

%unsigned long Length_Socket(struct WSocket *aSocket);

%enum WSockErr Connect_Socket(struct WSocket *aSocket, const char *aTo,
%                             unsigned long aPortNum, long timeout);