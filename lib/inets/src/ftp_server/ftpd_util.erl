-module(ftpd_util).

-export([format_address/2, packet_to_tokens/1, check_repr_type/1,
         check_auth/2, response/2, get_file_info/2,
         logf/3, tracef/3,
         list2portip/1, eprtlist2portip/1,bin_to_upper/1]).

-include_lib("ftpd_rep.hrl").

%% Converts ip and port to "h1,h2,h3,h4,p1,p2" format
format_address({A1, A2, A3, A4}, Port) ->
	<<P1:8, P2:8>> = <<Port:16>>,
	lists:concat([A1,",",A2,",",A3,",",A4,",",P1,",",P2]).

bin_to_upper(T) ->
	<< <<if ((X =< 122) and (X >= 97)) -> X - 32; true -> X end>> || <<X:8>> <= T >>.

%% Separate command from message and convert to upper case, eg. "user someone" -> {"USER", ["someone"]}
%% OUTDATED -spec packet_to_tokens(Data :: string()) -> {Command :: string(), [Message :: string()]}.
packet_to_tokens(Data) ->
	TrimmedData = re:replace(Data, "\r\n", "",[{return,list}]),
	SplittedData = re:split(TrimmedData, " "),
	case SplittedData of
		[Command | Msg] -> {bin_to_upper(Command), Msg};
		_               -> io:format("Error: packet parse failed\n"), {"", []}
	end.
%% check for TYPE command arguments
check_repr_type([Type]) ->
	lists:member(Type, ["I"]);
check_repr_type(["L", Arg]) ->
	Arg == "8";
check_repr_type([_, _]) ->
	true;
check_repr_type(_) ->
	false.

%% Messages that require USER and PASS before
req_auth_messages() -> ["CWD", "PWD", "PASV"].

check_auth(Command, Args) ->
	case {lists:member(Command, req_auth_messages()), Args#ctrl_conn_data.authed } of
		{true, false} -> bad;
		_             -> ok
	end.

%% Construct tuple for response
-spec response(ReplyCode :: integer(), Message :: string()) -> reply().
response(ReplyCode, Message) -> {reply, ReplyCode, Message}.

%% Get file information
%% drwxrwsr-x   3 47688    60000        4096 Dec-9-2005 empty

get_file_info(FName,FullPath) ->
%% io:write(FullPath ++ FName),
{ok,{file_info,Size,Type,Access,
%%               {{2012,6,21},{17,20,49}},
			   AccTime,
               ModTime,
               {{CYr,CMn,CDa},{CH,CMin,CSec}},
               Mode,Links,MajorDev,
			MinorDev,INode,UID,GID}} 
		= file:read_file_info(FullPath ++ "/" ++ FName),
	TypeLetter =
	case Type of %% TODO: UNIX types needed
		device -> v;
		directory -> d;
		other -> o;
		regular -> '-';
		symlink -> s;
		_ -> u
	end,
	AccLetter =
	case Access of
		read -> 'r--';
		write -> '-w-';
		read_write -> 'rw-';
		_ -> '---'
	end,
	lists:concat([TypeLetter,AccLetter,AccLetter,AccLetter,
	" ", Links, " ", UID, " ", GID, " ", Size, " ", httpd_util:month(CMn), " ",
	CDa, " ", CYr, " ", FName]). %% CH, ":", CMin, " ", 

logf(ConnData, Event, Params) ->
	LogFun = ConnData#ctrl_conn_data.log_fun,
	LogFun(Event, Params).
tracef(ConnData, Event, Params) ->
	TraceFun = ConnData#ctrl_conn_data.trace_fun,
	TraceFun(Event, Params).


%%
%% Conversion between string list and IP/Port tuple
list2portip(Lst) when length(Lst) == 6 ->
	Fun = fun(A) -> {Res,More} = string:to_integer(A), Res end,	
	[A1,A2,A3,A4,P1,P2] = [ Fun(X) || X <- Lst ],
	case lists:member(error,[A1,A2,A3,A4,P1,P2]) of
		false ->
			<<Port:16>> = <<P1:8, P2:8>>,
			{{A1,A2,A3,A4},Port};
		true ->
			{error, bad_addr}				
	end;
list2portip(_) ->
	{error, bad_addr}.

eprtlist2portip([Tp,SAddr,SPort]) when ((Tp == "1") or (Tp == "2")) ->
	case {inet_parse:address(SAddr),string:to_integer(SPort)} of
		{{ok,IP},{Port,[]}} -> {IP,Port};
		Error				-> {error, bad_addr}
	end;

eprtlist2portip(_) ->
	{error, bad_addr}
.


