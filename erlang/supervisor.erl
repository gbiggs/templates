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
-module(_sup).
-behaviour(supervisor).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1]).


%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type(restart_strategy() :: one_for_all | one_for_one | rest_for_one |
    simple_one_for_one).
-type(child_restart_spec() :: permanent | transient | temporary).
-type(child_shutdown_spec() :: brutal_kill | pos_integer() | infinity).
-type(child_type() :: worker | supervisor).
-type(child_modules() :: [atom()] | dynamic).
-type(child_spec() :: {Id::any(), StartFunc::{atom(), atom(), [any()]},
        Restart::child_restart_spec(), Shutdown::child_shutdown_spec(),
        Type::child_type(), Modules::child_modules()}).
-type(sup_init_res() :: {ok,
        {{restart_strategy(), MaxR::0 | pos_integer(),
                MaxT::0 | pos_integer()},
            [child_spec()]}} | ignore).


%%=============================================================================
%% External functions
%%=============================================================================


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Supervisor initialisation callback. See the supervisor documentation
%% for more details.
%% @spec init(Args) -> Result
%% @end
-spec(init(any()) -> sup_init_res()).
%%-----------------------------------------------------------------------------
init(_Args) ->
    {ok, {{rest_for_one, 1, 60}, child_spec()}}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for this supervisor.
%% @spec child_spec() -> ChildSpec
%% @end
-spec(child_spec() -> [child_spec()]).
%%-----------------------------------------------------------------------------
child_spec() ->
    [].


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test the child_spec is syntactically correct.
%% @spec child_spec_test() -> ok
%% @end
-spec(child_spec_test() -> ok).
%%-----------------------------------------------------------------------------
child_spec_test() ->
    ?assertEqual(ok, supervisor:check_childspecs(child_spec())).

-endif. % TEST

