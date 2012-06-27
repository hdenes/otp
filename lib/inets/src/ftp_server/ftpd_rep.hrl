%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTPD inner representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(ftpd_rep_hrl).
-define(ftpd_rep_hrl, true).

%% Control connection data
%%	username ~ actual username set by the user (using USER <loginname>)
%%	authed ~ indicates wether a proper password was given for the actual username
%%	control_sock ~ socket of the control connection
%%	pasv_pid ~ PID of the passive connection used by this control connection
%%		(if exists)
%%	curr_path ~ current directory path
-record(ctrl_conn_data, {username 		= none, 
						authed 			= false,
						control_socket 	= none,
						pasv_pid 		= none,
						curr_path 		= "/",
						chrootdir		= none,
						repr_type		= none,
						pwd_fun			= none,
						log_fun			= none,
						trace_fun		= none
						}).

%% Defines

-define(UTIL, ftpd_util).
-define(RESP(Comm, Msg), ?UTIL:response(Comm, Msg)).

%% Types

-type proplist()   :: proplists:proplist().
-type socket()     :: gen_tcp:socket().

-type reply()      :: {reply, ReplyCode :: integer(), Message :: string()}.
-type argschange() :: sameargs | {newargs, NewArgs :: proplist()}.

-type connitem()   :: ftpd:ftp_option() |
                      {control_socket, Socket :: socket()} |
                      {username, User :: string()} |
                      authed |
                      {repr_type, Params :: list()}.
-type connstate()  :: [connitem()].

-endif.


