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
-behaviour(gen_fsm).


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
-export([start_link_fsm/0, start_fsm/0, stop_fsm/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).
% Introspection
-export([state/1]).
% States
-export([]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the state machine.
%% @spec start_link_fsm() -> {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link_fsm() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link_fsm() ->
    gen_fsm:start_link({local, FSM_NAME}, FSM_MODULE, [], []).


%%-----------------------------------------------------------------------------
%% @doc Start a state machine without linking.
%% @spec start_fsm() -> {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_fsm() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_fsm() ->
    gen_fsm:start({local, FSM_NAME}, FSM_MODULE, [], []).


%%-----------------------------------------------------------------------------
%% @doc Stop the state machine.
%% @spec stop_fsm() -> ok
%% @end
-spec(stop_fsm() -> ok).
%%-----------------------------------------------------------------------------
stop_fsm() ->
    gen_fsm:send_all_state_event(FSM_NAME, terminate).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise this state machine.
%% @spec init(Args) -> {ok, StateName, StateData}
%% where
%%      Args = any()
%%      StateName = atom()
%%      StateData = any()
%% @end
-spec(init(Args::any()) -> {ok, StateName::atom(), StateData::any()}).
%%-----------------------------------------------------------------------------
init(_) ->
    {ok, blag, any()}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event.
%% @spec handle_event(Event, StateName, StateData) -> Result
%% where
%%      Event = terminate
%%      StateName = atom()
%%      StateData = any()
%%      Result = {next_state, NextStateName, NewStateData}
%%      NextStateName = atom()
%%      NewStateData = any()
%% @end
-spec(handle_event(Event::terminate, StateName::atom(), StateData::any()) ->
    {next_state, NextStateName::atom(), NewStateData::any()}).
%%-----------------------------------------------------------------------------
handle_event(terminate, _StateName, StateData) ->
    {stop, normal, StateData}.
    %{next_state, none, StateData}.


%%-----------------------------------------------------------------------------
%% @doc Handle a synchronous event.
%% @spec handle_sync_event(Event, From, StateName, StateData -> Result
%% where
%%      Event = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      StateName = atom()
%%      StateData = any()
%%      Result = {reply, Reply, NextStateName, NewStateData} |
%%          {next_state, NextStateName, NewStateData}
%%      Reply = any()
%%      NextStateName = atom()
%%      NewStateData = any()
%% @end
-spec(handle_sync_event(Event::any(), From::{pid(), Tag::any()},
        StateName::atom(), StateData::any()) ->
    {reply, Reply::any(), NextStateName::atom(), NewStateData::any()} |
    {next_state, NextStateName::atom(), NewStateData::any()}).
%%-----------------------------------------------------------------------------
handle_sync_event(not_supported, _From, _StateName, StateData) ->
    {reply, none, none, StateData}.


%%-----------------------------------------------------------------------------
%% @doc Handle an info message.
%% @spec handle_info(Info, StateName, StateData) -> Result
%% where
%%      Info = any()
%%      StateName = atom()
%%      StateData = any()
%%      Result = {next_state, NextStateName, NewStateData}
%%      NextStateName = atom()
%%      NewStateData = any()
%% @end
-spec(handle_info(Info::any(), StateName::atom(), StateData::any()) ->
    {next_state, NextStateName::atom(), NewStateData::any()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, _StateName, StateData) ->
    {next_state, none, StateData}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, StateName, StateData) -> any()
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      StateName = atom()
%%      StateData = any()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        StateName::atom(), StateData::any()) -> any()).
%%-----------------------------------------------------------------------------
terminate(normal, _StateName, _StateData) ->
    ?LOG(rtl_info, "Shutting down normally.");
terminate(shutdown, _StateName, _StateData) ->
    ?LOG(rtl_info, "Shut down by supervisor.");
terminate({shutdown, Reason}, _StateName, _StateData) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]);
terminate(Reason, _StateName, _StateData) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, StateName, StateData, Extra) ->
%%      {ok, NextStateName, NewStateData}
%% where
%%      OldVsn = any() | {down, any()}
%%      StateName = atom()
%%      StateData = any()
%%      Extra = any()
%%      NextStateName = atom()
%%      NewStateData = any()
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, StateName::atom(),
        StateData::any(), Extra::any()) ->
    {ok, NextStateName::atom(), NewStateData::any()}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%-----------------------------------------------------------------------------
%% @doc State handler.
%% @spec (Event, StateData) -> Result
%% where
%%      Event = timeout | any()
%%      StateData = any()
%%      Result = {next_state, NextStateName, NewStateData}
%%      NextStateName = atom()
%%      NewStateData = any()
%% @end
-spec((Event::timeout | any(), StateData::any()) ->
    {next_state, NextStateName::atom(), NewStateData::any()}).
%%-----------------------------------------------------------------------------
(Event, StateData) ->
    .


%%-----------------------------------------------------------------------------
%% @doc State synchronous event handler.
%% @spec (Event, From, StateData) -> Result
%% where
%%      Event = timeout | any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      StateData = any()
%%      Result = {reply, Reply, NextStateName, NewStateData} |
%%          {next_state, NextStateName, NewStateData}
%%      Reply = any()
%%      NextStateName = atom()
%%      NewStateData = any()
%% @end
-spec((Event::timeout | any(), From::{pid(), Tag::any()}, StateData::any()) ->
    {reply, Reply::any(), NextStateName::atom(), NewStateData::any()} |
    {next_state, NextStateName::atom(), NewStateData::any()}).
%%-----------------------------------------------------------------------------
(Event, From, StateData) ->
    .


%%-----------------------------------------------------------------------------
%% @doc Get the current state.
%% @spec state(FSM) -> atom()
%% where
%%      FSM = pid()
%% @end
-spec(state(FSM::pid()) -> atom()).
%%-----------------------------------------------------------------------------
state(FSM) ->
    gen_fsm:sync_send_event(FSM, state).


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

