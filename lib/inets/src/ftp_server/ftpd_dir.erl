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

-module(ftpd_dir).

-export([set_cwd/3, normalize_filepath/3]).

normalize_filepath(Root, Cwd, ReqFile) ->
	case hd(ReqFile) of
		$/ -> slash_correct(Root ++ ReqFile);
		_  -> slash_correct(Root ++ Cwd ++ "/" ++ ReqFile)
	end.

set_cwd(Root, Cwd, Req) ->
	case lists:prefix("./", Req) of
		true ->
			NewReq = slash_correct(dot_correct(lists:nthtail(1, Req))),
			cwd_fun(Root, Cwd, NewReq);
		false ->
			case lists:prefix("/", Req) of
				false ->
					NewReq = slash_correct(dot_correct(lists:append("/", Req))),
					cwd_fun(Root, Cwd, NewReq);
				true ->
					NewReq = slash_correct(dot_correct(Req)),
					cwd_fun(Root, "/", NewReq)
			end
	end.

slash_correct(Cwd) ->
	case string:str(Cwd, "//") of
		0     -> Cwd;
		Index ->
			Head = lists:sublist(Cwd, Index),
			Tail = lists:nthtail(Index+1, Cwd),
			slash_correct(lists:append(Head, Tail))
	end.

dot_correct(Cwd) ->
	case string:str(Cwd, "/./") of
		0     -> Cwd;
		Index ->
			Head = lists:sublist(Cwd, Index),
			Tail = lists:nthtail(Index+2, Cwd),
			dot_correct(lists:append(Head, Tail))
	end.

cwd_fun(Root, CwdAbsName, "") ->
%	io:format("CwdAbs: ~p\n", [CwdAbsName]),
	case lists:suffix("/", CwdAbsName) of
		true ->
			NewAbsName = lists:sublist(CwdAbsName, length(CwdAbsName)-1),
			cwd_fun(Root, NewAbsName, "");
		false ->
			NewAbsName = lists:concat(["/", CwdAbsName]),
			{ok, slash_correct(NewAbsName)}
	end;
%	io:format("SlashAbs: ~p\n", [slash_correct(NewAbsName)]),

cwd_fun(Root, CwdAbsName, Req) ->
%	io:format("~p\n", [Req]),
	{Index, Acc, NewReq} = cdd_fun(Req, 0),
	NewCwd = step_back(CwdAbsName, Acc),
	case step_forward(Root, NewCwd, Index, NewReq) of
		{ok, {NewAbsName, NextReq}} -> cwd_fun(Root, NewAbsName, NextReq);
		{ok, ReturnName} -> {ok, ReturnName};
		Error -> Error
	end.

step_forward(Root, CwdAbsName, 0, Req) ->
	NewAbsName = string:join([CwdAbsName, Req], ""),
	CorrectedAbsName = slash_correct(string:join([Root, NewAbsName], "")),
	case {file:read_link(CorrectedAbsName), filelib:is_dir(CorrectedAbsName)} of
		{{ok, NewPath}, _} -> 	NewReq = slash_correct(lists:concat([NewPath, "/", Req])),
					set_cwd(Root, CwdAbsName, NewReq);
					%{ok, {CwdAbsName, CorrectedNextReq}};
		{{error, _}, true} -> {ok, {NewAbsName, ""}};
		{{error, _}, false} -> {error, invalid_dir}
	end;

step_forward(Root, CwdAbsName, Index, Req) ->
	{CurrPwd, NextReq} = lists:split(Index-1, Req),
	NewAbsName = string:join([CwdAbsName, CurrPwd], ""),
	CorrectedAbsName = slash_correct(string:join([Root, NewAbsName], "")),
	case {file:read_link(CorrectedAbsName), filelib:is_dir(CorrectedAbsName)} of
		{{ok, NewPath}, _} -> CorrectedNextReq = slash_correct(string:join([NewPath, NextReq], "/")),
					set_cwd(Root, CwdAbsName, CorrectedNextReq);
					%{ok, {CwdAbsName, CorrectedNextReq}};
		{{error, _}, true} -> {ok, {NewAbsName, NextReq}};
		{{error, _}, false} -> {error, invalid_dir}
	end.

step_back(CwdAbsName, 0) ->
	CwdAbsName;
step_back("", _) ->
	"";
step_back(CwdAbsName, Acc) ->
	Pos = string:rchr(CwdAbsName, $/),
	NewCwd = lists:sublist(CwdAbsName, Pos-1),
	step_back(NewCwd, Acc-1).

cdd_fun(Req, Acc) ->
	case string:str(Req, "/..") of
		0 -> {0, Acc, Req};
		1 -> cdd_fun(lists:nthtail(3, Req), Acc+1);
		Index -> {Index, Acc, Req}
	end.

