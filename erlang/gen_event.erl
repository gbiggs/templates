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
-behaviour(gen_event).


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
-export([]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialisation function.
%% @spec init(Args) -> {ok, State}
%% where
%%      Args = any()
%%      State = any()
%% @end
-spec(init(Args::any()) -> {ok, State::any()}).
%%-----------------------------------------------------------------------------
init(_) ->
    {ok, none}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event.
%% @spec handle_event(Event, State) -> {ok, NewState}
%% where
%%      Event = any()
%%      State = any()
%%      NewState = any()
%% @end
-spec(handle_event(Event::any(), State::any()) -> {ok, NewState::any()}).
%%-----------------------------------------------------------------------------
handle_event(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, State) -> {ok, NewState}
%% where
%%      Request = any()
%%      State = any()
%%      NewState = any()
%% @end
-spec(handle_call(Request::any(), State::any()) ->
    {ok, NewState::any()}).
%%-----------------------------------------------------------------------------
handle_call(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = any()
%%      State = any()
%%      NewState = any()
%% @end
-spec(handle_info(Info::any(), State::any()) ->
    {ok, NewState::any()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Arg, State) -> any()
%% where
%%      Arg = any() | {stop, Reason} | stop | remove_handler |
%%          {error, {'EXIT', Reason}} | {error, any()}
%%      Reason = any()
%%      State = any()
%% @end
-spec(terminate(Args::any() | {stop, Reason::any()} | stop | remove_handler |
        {error, {'EXIT', Reason::any()}} | {error, any()}, State::any()) ->
    any()).
%%-----------------------------------------------------------------------------
terminate(remove_handler, _State) ->
    ok;
terminate({stop, Reason}, _State) ->
    ok;
terminate(stop, _State) ->
    ok;
terminate({error, {'EXIT', Reason}}, _State) ->
    ok;
terminate({error, Reason}, _State) ->
    ok;
terminate(Args, _State) ->
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
-spec(code_change(OldVsn::any() | {down, any()}, State::any(), Extra::any()) ->
    {ok, NewState::any()}).
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

