-module(ftpd_ctrl_conn).

-export([new_connection/3]).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-include_lib("inets/include/ftpd.hrl").
-include_lib("ftpd_rep.hrl").

%%
%% FTP control connection for handling commands
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Connect and process messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_connection(Sock, SupPid, Args) ->
	erlang:monitor(process, SupPid),
	io:format("---------------- CONNECTION START ----------------\n"),
	send_reply(Sock, 220, "Hello"),
	ConnData = construct_conn_data(Args, Sock),
	do_recv(Sock, ConnData).

construct_conn_data(Args, Sock) ->
	ErlTop = element(2,file:get_cwd()),
	#ctrl_conn_data{
		control_socket = Sock, 
		chrootdir      = proplists:get_value(chrootDir, Args, ErlTop), %% HINT chrootDir vs chrootdir
		pwd_fun        = proplists:get_value(pwd_fun,   Args, fun(_,_) -> not_authorized end),
		log_fun        = proplists:get_value(log_fun,   Args, fun(_,_) -> ok end),
		trace_fun      = proplists:get_value(trace_fun, Args, fun(_,_) -> ok end)
	}.

%% Control Connection - Wait for incoming messages
-spec do_recv(Sock :: socket(), Args :: connstate()) -> ok.
do_recv(Sock, Args) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
			{Command, Msg} = ?UTIL:packet_to_tokens(Data),
			io:format("[~p-Recv]: ~p - ~p\n", [self(), Command, Msg]),
			NewArgs = process_message(Sock, Command, Msg, Args),
			after_reply(Sock, Command, NewArgs);
        {error, closed} -> ok
    end.

process_message(Sock, Command, Msg, Args) ->
	case ?UTIL:check_auth(Command, Args) of
		ok ->
			{Reply, MaybeNewArgs} = handle_command(Command, Msg, Args),
			handle_reply(Sock, Reply),
			case MaybeNewArgs of
				{newargs, NewArgs} -> NewArgs;
				sameargs           -> Args
			end;
		bad ->
			handle_reply(Sock, ?RESP(530, "Please login with USER and PASS")),
			Args
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_command(Command :: string(), Message :: list(), Args :: connstate()) -> {reply(), argschange()}.
handle_command(<<"NOOP">>, _, _) ->
	{?RESP(200, "NOOP command successful"), sameargs};

handle_command(<<"QUIT">>, _, Args) ->
	User = Args#ctrl_conn_data.username,
	?UTIL:logf(Args, ?CONN_CLOSE, [User]),
	{?RESP(221, "Goodbye."), sameargs};

handle_command(<<"USER">>, [UserBin|_], Args) ->
	User = binary_to_list(UserBin),
	case Args#ctrl_conn_data.authed of
		true -> 
			{?RESP(503, "You are already logged in"), sameargs};
		false ->
			NewArgs = Args#ctrl_conn_data{ username = User },
			{?RESP(331, "Password required for " ++ User), {newargs, NewArgs}}
	end;

handle_command(<<"PASS">>, [PasswordBin|_], Args) ->
	Authed = Args#ctrl_conn_data.authed,
	User   = Args#ctrl_conn_data.username,
	Password = binary_to_list(PasswordBin),
	case {Authed, User} of
		{false, none} -> {?RESP(503, "Login with USER first"),     sameargs};
		{false, _} ->
			PwdFun = Args#ctrl_conn_data.pwd_fun,
			case PwdFun(User, Password) of
				authorized ->
					?UTIL:logf(Args, ?LOGIN_OK, [User]),
					NewArgs = Args#ctrl_conn_data{ authed = true },
					{?RESP(230, "Login ok - TODO: not sure"), {newargs, NewArgs}};
				not_authorized ->
					?UTIL:logf(Args, ?LOGIN_FAIL, [User]),
					NewArgs = Args#ctrl_conn_data{ username = none },
					{?RESP(530, "Login incorrect"), {newargs, NewArgs}}
			end;
		{true, _}     -> {?RESP(503, "You are already logged in"), sameargs}
	end;


handle_command(<<"TYPE">>, ParamsBin, Args) ->
	Params  = [ binary_to_list(E)  || E <- ParamsBin],	%% TEMP
	ParamsF = [ string:to_upper(E) || E <- Params],
	case ?UTIL:check_repr_type(ParamsF) of
		true ->
			NewArgs = Args#ctrl_conn_data{ repr_type = ParamsF },
			{?RESP(200, "TYPE set to " ++ hd(ParamsF)), {newargs, NewArgs}};
		false ->
			{?RESP(500, "'TYPE " ++ string:join(Params, " ") ++ "' not understood"), sameargs}
	end;

handle_command(<<"SIZE">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	FileName = string:join(Params, " "),
	CurDir = Args#ctrl_conn_data.curr_path,
	BaseDir = Args#ctrl_conn_data.chrootdir,
	FullPath = BaseDir ++ "/" ++ CurDir ++ FileName,
	case filelib:is_regular(FullPath) of
		true ->	Size = filelib:file_size(FullPath),
				{?RESP(213, integer_to_list(Size)), sameargs};
		false ->
				{?RESP(550, FileName ++ ": not a regular file"), sameargs}
	end;	


handle_command(<<"RETR">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	FileName = string:join(Params, " "),
	ftpd_data_conn:send_msg(retr, FileName, Args);

handle_command(<<"STOR">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	FullName = string:join(Params, " "),
	FileName = filename:basename(FullName) ++ filename:extension(FullName),
	ftpd_data_conn:send_msg(stor, {FileName, FullName, write}, Args);

handle_command(<<"APPE">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	FullName = string:join(Params, " "),
	FileName = filename:basename(FullName) ++ filename:extension(FullName),
	ftpd_data_conn:send_msg(stor, {FileName, FullName, append}, Args);

handle_command(<<"CWD">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	NewDir = string:join(Params, " "),
	CurDir = Args#ctrl_conn_data.curr_path,
	BaseDir = Args#ctrl_conn_data.chrootdir,
	case ftpd_dir:set_cwd(BaseDir, CurDir, NewDir) of
		{ok, ""} ->
			NewPath = "/",
			?UTIL:tracef(Args, ?CWD, [NewPath]),
			io:format("CWD new path: ~p", [NewPath]),
			NewArgs = Args#ctrl_conn_data{ curr_path = NewPath },
			{?RESP(250, "CWD command successful."), {newargs, NewArgs}};
		{ok, NewPath} ->
			?UTIL:tracef(Args, ?CWD, [NewPath -- "/"]),
			io:format("CWD new path: ~p", [NewPath]),
			NewArgs = Args#ctrl_conn_data{ curr_path = NewPath },
			{?RESP(250, "CWD command successful."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("CWD error: ~p", [Error]),
			{?RESP(550, NewDir ++ ": No such file or directory"), sameargs}
	end;

handle_command(<<"PWD">>, [], Args) ->
	{?RESP(257, "\"" ++ Args#ctrl_conn_data.curr_path ++ "\" is the current directory"), sameargs};

handle_command(<<"PWD">>, _, _) ->	% TODO: generalize
	{?RESP(501, "Invalid number of arguments"), sameargs};

handle_command(<<"STRU">>, [Type], _) ->
	case ?UTIL:bin_to_upper(Type) of
		<<"F">> -> {?RESP(200, "Structure set to F"), sameargs};
		_		-> {?RESP(504, "Unsupported structure type"), sameargs}
	end;

handle_command(<<"PASV">>, _, Args) ->
	{ok, Hostname} = inet:gethostname(),
	case inet:getaddr(Hostname, inet) of
		{ok, Address} ->
			ftpd_data_conn:reinit_passive_conn(Args#ctrl_conn_data.data_pid),
			{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(inet4),
			io:format("Passive mode start, port: ~p\n", [Port]),
			NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
			{?RESP(227, "Entering Passive Mode (" ++ ?UTIL:format_address(Address, Port) ++ ")."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("ERROR: inet:getaddr, ~p\n", [Error]),
			{?RESP(500, "PASV command failed"), sameargs}
	end;

handle_command(<<"PORT">>, [BinArg1], Args) ->
	Params = binary_to_list(BinArg1),
	IpPortParams = string:tokens(Params,","),

	case ?UTIL:list2portip(IpPortParams) of
		{error, Error} ->
			io:format("ERROR: ~p\n", [Error]),
			{?RESP(500, "PORT command failed (2)"), sameargs};
		{Addr, Port} ->	%% TODO NOT passpid
			ftpd_data_conn:reinit_active_conn(Args#ctrl_conn_data.data_pid),
			case ftpd_data_conn:start_active_mode(inet4, Addr, Port) of
				{ok, PasvPid} ->
					io:format("Active mode start, port: ~p\n", [Port]),
					NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
					{?RESP(200, "PORT command successful"), {newargs, NewArgs}};
				{error, Error} ->
					{?RESP(500, "PORT command failed (1)"), sameargs}					
			end
	end;

handle_command(<<"PORT">>, _, _) ->
	{?RESP(501, "Illegal PORT command"), sameargs};

handle_command(<<"EPSV">>, _, Args) ->
	ftpd_data_conn:reinit_passive_conn(Args#ctrl_conn_data.data_pid),
	{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(inet6),
	io:format("Passive mode start, port: ~p\n", [Port]),
	NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
	{?RESP(229, "Entering Extended Passive Mode (|||" ++ integer_to_list(Port) ++ "|)"), {newargs, NewArgs}};

%%
%%	format : EPRT<space><d><net-prt><d><net-addr><d><tcp-port><d>
%%
handle_command(<<"EPRT">>, [BinArg1], Args) ->
	Params = binary_to_list(BinArg1),
	IpPortParams = string:tokens(Params,"|"),
	case ?UTIL:eprtlist2portip(IpPortParams) of
		{error, Error} ->
			io:format("ERROR: ~p\n", [Error]),
			{?RESP(500, "EPRT command failed (2)"), sameargs};
		{Addr, Port} ->	%% TODO NOT passpid
			ftpd_data_conn:reinit_active_conn(Args#ctrl_conn_data.data_pid),
			case ftpd_data_conn:start_active_mode(inet6, Addr, Port) of
				{ok, PasvPid} ->
					io:format("Active mode start, port: ~p\n", [Port]),
					NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
					{?RESP(200, "EPRT command successful"), {newargs, NewArgs}};
				{error, Error} ->
					{?RESP(500, "EPRT command failed (1)"), sameargs}					
			end
	end;

handle_command(<<"EPRT">>, _, _) ->
	{?RESP(501, "Illegal PORT command"), sameargs};

handle_command(<<"LIST">>, ParamsBin, Args) -> %% TODO move to data_conn
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	DirToList = string:join(Params, " "),
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	case ftpd_dir:set_cwd(AbsPath, RelPath, DirToList) of
		{ok, NewPath} ->
			?UTIL:tracef(Args, ?LIST, [NewPath]),
			FullPath = AbsPath ++ "/" ++ RelPath ++ DirToList,
			io:format("LIST path: ~p\n", [FullPath]),
			{ok, FileNames} = file:list_dir(AbsPath ++ NewPath),
			ftpd_data_conn:send_msg(list, {lists:sort(FileNames), FullPath, lst}, Args);
		{error, Error} ->
			io:format("LIST error: ~p  | ~p | ~p | ~p | ~p\n", 
									[Error,Params,DirToList,AbsPath,RelPath]),
			{?RESP(550, "LIST fail TEMP TODO"), sameargs}
	end;

handle_command(<<"NLST">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	DirToList = string:join(Params, " "),
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	case ftpd_dir:set_cwd(AbsPath, RelPath, DirToList) of
		{ok, NewPath} ->
			io:format("NLST path ~p \n", [NewPath]),
			{ok, FileNames} = file:list_dir(AbsPath ++ NewPath),
			ftpd_data_conn:send_msg(list, {lists:sort(FileNames), "", nlst}, Args);
		{error, Error} ->
			{?RESP(550, "NLST fail TEMP TODO"), sameargs}			
	end;


handle_command(<<"REIN">>, [], Args) ->
	NewArgs = Args#ctrl_conn_data{ authed = false, username = none},
	{?RESP(200, "REIN command successful"), {newargs, NewArgs}};

handle_command(<<"MKD">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],
	Dir = string:join(Params, " "),
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	FullPath = AbsPath ++ RelPath ++ Dir,
	case file:make_dir(FullPath) of
		ok ->
			{?RESP(257, "\"" ++ RelPath ++ Dir ++ "\" directory created"), sameargs};
		{error, eexist} ->
			{?RESP(550, "Folder already exists"), sameargs};
		{error, _} ->
			{?RESP(550, "MKD command failed"), sameargs}
	end;

handle_command(<<"RMD">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],
	Dir = string:join(Params, " "),
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	FullPath = AbsPath ++ RelPath ++ Dir ++ "/",
	case file:del_dir(FullPath) of
		ok         -> {?RESP(250, "Folder deleted"),     sameargs};
		{error, _} -> {?RESP(550, "RMD command failed"), sameargs}
	end;

handle_command(<<"DELE">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],
	Dir = string:join(Params, " "),
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	FullPath = AbsPath ++ RelPath ++ Dir,
	case file:delete(FullPath) of
		ok         -> {?RESP(250, "File deleted"),        sameargs};
		{error, _} -> {?RESP(550, "DELE command failed"), sameargs}
	end;

handle_command(<<"RNFR">>, ParamsBin, Args) ->
	Params   = [ binary_to_list(E) || E <- ParamsBin],
	FromName = string:join(Params, " "),
	AbsPath  = Args#ctrl_conn_data.chrootdir,
	RelPath  = Args#ctrl_conn_data.curr_path,
	FullPath = AbsPath ++ RelPath ++ FromName,
	NewArgs = Args#ctrl_conn_data{ rename_from = FullPath },
	{?RESP(350, "Requested file action pending further information."), {newargs, NewArgs}};

handle_command(<<"RNTO">>, ParamsBin, Args) ->
	Params   = [ binary_to_list(E) || E <- ParamsBin],
	ToName = string:join(Params, " "),
	AbsPath  = Args#ctrl_conn_data.chrootdir,
	RelPath  = Args#ctrl_conn_data.curr_path,
	ToPath = AbsPath ++ RelPath ++ ToName,
	case Args#ctrl_conn_data.rename_from of
		none ->
			{?RESP(550, "RNTO command failed (1)"), sameargs};
		FromPath ->
			io:format("From: ~p || To: ~p", [FromPath, ToPath]),
			case file:rename(FromPath, ToPath) of
				ok ->
					NewArgs = Args#ctrl_conn_data{ rename_from = none },
					{?RESP(250, "RNTO ok"), {newargs, NewArgs}};
				_ ->
					{?RESP(550, "RNTO command failed (2)"), sameargs}
			end
	end;

handle_command(<<"">>, _, _) ->
	{?RESP(500, "Invalid command: try being more creative"), sameargs};

handle_command(Command, _, _) ->
	{?RESP(500, binary_to_list(Command) ++ " not implemented"), sameargs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Send response if needed
handle_reply(_, noreply) ->
	ok;
handle_reply(Sock, {reply, Code, Message}) ->
	send_reply(Sock, Code, Message).

%% Convert Code and Message to packet and send
send_reply(Sock, Code, Message) ->
	io:format("[~p-Send]: ~p - ~p\n", [self(), Code, Message]),
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	gen_tcp:send(Sock, Str).

%% Control behaviour after replying to a message
after_reply(Sock, <<"QUIT">>, _) ->
	io:format("---------------- CONNECTION CLOSE ----------------\n"),
	gen_tcp:close(Sock);
after_reply(Sock, _, Args) ->
	do_recv(Sock, Args).
