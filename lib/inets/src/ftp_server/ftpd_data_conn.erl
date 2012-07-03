%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(ftpd_data_conn).

-export([start_passive_mode/1, start_active_mode/3,
         pasv_accept/1, actv_accept/1,
         reinit_data_conn/1, send_msg/3]).

-include_lib("inets/include/ftpd.hrl").
-include_lib("ftpd_rep.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Passive mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_active_mode(Ipv, Addr, Port) ->
	?LOG("Active mode begin, port: ~p\n", [Port]),
	SockArgs = [binary, {packet, 0}, {active, false}] ++ ipv_argl(Ipv),
	case gen_tcp:connect(Addr, Port, SockArgs) of
		{ok, Sock} -> {ok, spawn(?MODULE, actv_accept, [Sock])};
		Error      -> Error
	end.

start_passive_mode(Ipv) ->
	?LOG("Passive mode begin\n"),
	SockArgs    = [binary, {packet, 0}, {active, false}] ++ ipv_argl(Ipv),
	case gen_tcp:listen(0, SockArgs) of
		{ok, LSock} -> 
			Pid = spawn(?MODULE, pasv_accept, [LSock]),
			case inet:port(LSock) of
				{ok, Port} ->
					?LOG("[~p]: Passive mode started, port: ~p\n", [Pid, Port]),
					{ok, {Pid, Port}};
				Error -> Error
			end;
		Error -> Error
	end.

ipv_argl(inet4) -> [];
ipv_argl(inet6) -> [inet6].

reinit_data_conn(Args) ->
	case Args#ctrl_conn_data.data_pid of
		none    -> true;
		LastPid -> exit(LastPid, kill)
	end.

%% socket

send_msg(MsgType, Msg, Args) ->
	case Args#ctrl_conn_data.data_pid of
		none ->
			send_reply_on_ctrl(Args,500,"Data connection not established.");
		PasvPid ->
			send_reply_on_ctrl(Args,150,"Opening BINARY mode data connection"),
			PasvPid ! {MsgType, Msg, Args}
	end,
	{noreply, sameargs}.

actv_accept(DataSock) ->
	?LOG("ACTV accept start\n"),
	data_conn_main(DataSock).

pasv_accept(LSock) ->
	?LOG("PASV accept start\n"),
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> data_conn_main(Sock);
		_          -> err_tcp
	end.

data_conn_main(DataSock) ->
	?LOG("~p PASV send loop\n", [DataSock]),
	receive
		{list, {FileNames, Path, ListType}, Args} ->
			?LOG("~p PASV send LIST data\n", [Args#ctrl_conn_data.control_socket]),
			TempMsg =
				case ListType of
					lst  -> [?UTIL:get_file_info(FN, Path) || FN <- FileNames];
					nlst -> FileNames
				end,
			FormattedMsg = string:join(TempMsg, "\r\n"),
			?UTIL:send_message(DataSock, FormattedMsg),
			transfer_complete(Args);
		{retr, FileName, Args} ->
			AbsPath = Args#ctrl_conn_data.chrootdir,
			RelPath = Args#ctrl_conn_data.curr_path,
			FPath   = ftpd_dir:normalize_filepath(AbsPath,RelPath,FileName),
			case file:read_file(FPath) of
				{ok, Bin} ->
					BinT = ?UTIL:transformto(Bin,Args#ctrl_conn_data.repr_type),
					?UTIL:send_message(DataSock, BinT),
					TraceParams = [RelPath ++ "/" ++ FileName, FileName],
					?UTIL:tracef(Args, ?RETR, TraceParams), %% TODO 2nd param ??
					transfer_complete(Args);
				{error, Reason} ->
					RespStr = "Requested action not taken. File unavailable, "
					          "not found, not accessible",
					send_ctrl_response(Args, 550, RespStr),
					io:format("File error: ~p, ~p\n", [Reason, FPath])
			end;
		{stor, {FileName, FullClientName, Mode}, Args} ->
			AbsPath = Args#ctrl_conn_data.chrootdir,
			RelPath = Args#ctrl_conn_data.curr_path,
			FPath   = ftpd_dir:normalize_filepath(AbsPath,RelPath,FileName),
			Repr    = Args#ctrl_conn_data.repr_type,
			case receive_and_store(DataSock, FPath, Mode, Repr) of
				ok ->
					TraceParams = [RelPath ++ "/" ++ FileName, FullClientName],
					?UTIL:tracef(Args, ?STOR, TraceParams),
					transfer_complete(Args);
				{error, Reason} ->
					io:format("File receive error: ~p\n", [Reason]),
					RespStr = "Requested action not taken. File unavailable, "
					          "not found, not accessible",
					send_ctrl_response(Args, 550, RespStr)
			end
	end,
	?LOG("PASV send loop end\n"),
	gen_tcp:close(DataSock).

%% Receive binaries and store them in a file
receive_and_store(DataSock, FPath, Mode, ReprType) ->
	case file:open(FPath, [Mode, binary]) of
		{ok, Id} ->
			case {receive_and_write_chunks(DataSock, Id, ReprType), file:close(Id)} of
				{ok, ok} -> ok;
				_        -> {error, receive_fail}
			end;
		Error -> Error
	end.

receive_and_write_chunks(DataSock, DevId, ReprType) ->
	case gen_tcp:recv(DataSock, 0) of
		{ok, Data} ->
			file:write(DevId, ?UTIL:transformfrom(Data, ReprType)),
			receive_and_write_chunks(DataSock, DevId, ReprType);
		{error, closed} -> ok;
		{error, Reason} -> {error, Reason}
	end.

send_ctrl_response(Args, Command, Msg) ->
	case Args#ctrl_conn_data.control_socket of
		none     -> io:format("Error: no control socket\n"), ok;
		CtrlSock -> ?UTIL:send_reply(CtrlSock, Command, Msg)
	end.

transfer_complete(Args) ->
	send_reply_on_ctrl(Args, 226, "Transfer complete").

send_reply_on_ctrl(Args, Command, Msg) ->
	case Args#ctrl_conn_data.control_socket of
		none -> io:format("Error: Looking up control connection failed\n");
		Sock -> ?UTIL:send_reply(Sock, Command, Msg)
	end.
