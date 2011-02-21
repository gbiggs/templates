%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc .
%% @reference <a href=""></a>
%% @copyright 2011 Geoffrey Biggs
%% RT-Synthesis Research Group
%% Intelligent Systems Research Institute,
%% National Institute of Advanced Industrial Science and Technology (AIST),
%% Japan
%% All rights reserved.
%% Licensed under the Eclipse Public License -v 1.0 (EPL)
%% http://www.opensource.org/licenses/eclipse-1.0.txt
%% @version {@version}
%% @end
%%-----------------------------------------------------------------------------
-module().
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/0, stop_server/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the server.
%% @spec start_link() ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, SVR_NAME}, SVR_MODULE, [], []).


%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @spec stop_server() -> ok
%% @end
-spec(stop_server() -> ok).
%%-----------------------------------------------------------------------------
stop_server() ->
    gen_server:cast(SVR_NAME, stop).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init([]) -> {ok, State} | {stop, Reason}
%% where
%%      State = any()
%%      Reason = any()
%% @end
-spec(init([]) -> {ok, State::any()} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, SVR_STATE}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = any()
%%      Reply = any()
%%      NewState = any()
%% @end
-spec(handle_call(Request::any(), From::{pid(), Tag::any()}, State::any()) ->
    {reply, Reply::any(), NewState::any()}).
%%-----------------------------------------------------------------------------
handle_call(not_supported, _From, State) ->
    {reply, not_supported, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {noreply, NewState} | {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = any()
%%      NewState = any()
%% @end
-spec(handle_cast(Request::any(), State::any()) ->
    {noreply, NewState::any()} | {stop, normal, NewState::any()}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = timeout | any()
%%      State = any()
%%      NewState = any()
%% @end
-spec(handle_info(Info::timeout | any(), State::any()) ->
    {noreply, NewState::any()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = any()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::any()) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ok;
terminate(shutdown, _State) ->
    ok;
terminate({shutdown, Reason}, _State) ->
    ok;
terminate(Reason, _State) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = any()
%%      Extra = any()
%%      NewState = any()
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::any(),
        Extra::any()) -> {ok, NewState::any()}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test .
%% @spec _() -> {setup, fun(), fun(), fun()}
%% @end
-spec(_() ->
        {setup, fun(() -> {any(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
_() ->
    {setup,
        fun() ->
        end,
        fun(S) ->
        end,
        fun(S) ->
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test .
%% @spec _() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
_() ->
    [].


%%-----------------------------------------------------------------------------
%% @doc Test .
%% @spec () -> ok
%% @end
-spec(() -> ok).
%%-----------------------------------------------------------------------------
() ->

-endif. % TEST

