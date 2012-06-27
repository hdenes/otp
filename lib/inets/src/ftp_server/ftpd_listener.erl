-module(ftpd_listener).

-behaviour(gen_server).

-export([start_link/1, loop/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).


loop(LSock, SupPid, Args) ->
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> spawn_link(ftpd_ctrl_conn, new_connection, [Sock, SupPid, Args]),
					  loop(LSock, SupPid, Args);
		{_, _Res} -> err_tcp
	end.

start_link(Args) ->
	Reg_name = list_to_atom(lists:append("ftpd_listener_", integer_to_list(get_port(Args)))),
	gen_server:start_link({local, Reg_name}, ?MODULE, Args, []).

init(Args) ->
	process_flag(trap_exit, true),
	
	Port = get_port(Args), 

   	SupPid = proplists:get_value(sup_pid,Args),

	SockArgs =
		case proplists:lookup(bind_address, Args) of
			{bind_address, Addr={_,_,_,_,_,_,_,_}} ->
				[binary, {packet, 0}, {active, false}, {ip, Addr}, inet6];
			{bind_address, Addr} ->
				[binary, {packet, 0}, {active, false}, {ip, Addr}];
			none ->
				[binary, {packet, 0}, {active, false}]
			end,
	NewSockArgs =
		case proplists:lookup(fd, Args) of
			none -> SockArgs;
			FdProp -> 
				io:format("Fd connection"),
				[FdProp | SockArgs]
		end, 


   	 {ok, LSock} = gen_tcp:listen(Port, NewSockArgs),
    	spawn(?MODULE, loop, [LSock, SupPid, Args]), %% Args also contains SupPid!! TODO
	{ok, {Port, SupPid, LSock}}.


get_port(Args) ->
	case proplists:lookup(port,Args) of
		{port, Pt} -> Pt;
		none -> 21
	end.


handle_call(_Req, _From, State) -> {noreply, State}.
handle_cast(_Req, State) -> {noreply, State}.

%terminate(shutdown, State) removed, same body
%terminate({shutdown, _Reason}, State)
terminate(_Reason, State) -> 
	io:write("terminate: 3/n"),
	LSock = element(3, State),
	gen_tcp:close(LSock),
	ok.

handle_info(_Info, _State) ->
	{stop, normal}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
