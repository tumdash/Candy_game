-module(game).
-behaviour(gen_server).

-export([
    start_link/0,
    put_candies_to_bag/2,
    put_10red_10blue_to_bag/1,
    run_simulation/2,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

stop(Pid) ->
    gen_server:call(Pid, stop).

%simplest init
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%put candies list to bag
put_candies_to_bag(Pid, Candies) when is_list(Candies) ->
    ok = gen_server:call(Pid, {put_candies, Candies}).

%put candies list to bag
put_10red_10blue_to_bag(Pid) ->
    BlueList = [blue || _ <- lists:seq(1, 10)],
    RedList = [red || _ <- lists:seq(1, 10)],
    ok = gen_server:call(Pid, {put_candies, BlueList ++ RedList}).

%one run with rules
run(Pid) ->
    Token1 = gen_server:call(Pid, {take_random}),
    case Token1 of
        none -> %if no token in bag, return previuos taken
%            io:format("~nNo mo1re tokens in bag"),
            {eoc, gen_server:call(Pid, {taken_before})};
        _ ->
            Token2 = gen_server:call(Pid, {take_random}),
            case Token2 of
                none -> %if no token in bag, return previuos taken (Token1 in fact)
%                    io:format("~nNo mo2re tokens in bag. Having only ~p", [Token1]),
                    {eoc, gen_server:call(Pid, {taken_before})};
                _ ->
%                    io:format("~nTwo token taken from bag: ~p ~p", [Token1, Token2]),
                    case [Token1, Token2] of
                        [red, blue] ->
%                            io:format("~nApply rule 3"),
                            ok = gen_server:call(Pid, {put, red});
                        [blue, red] ->
%                            io:format("~nApply rule 3"),
                            ok = gen_server:call(Pid, {put, red});
                        [red, red] ->
%                            io:format("~nApply rule 2")
                             ok;
                        [blue, blue] ->
%                            io:format("~nApply rule 1"),
                            ok = gen_server:call(Pid, {put, blue}),
                            ok = gen_server:call(Pid, {put, blue})
                    end,
                    ok
            end
    end.

run_simulation(Pid, NumToSimulate)
        when NumToSimulate > 0 ->
    run_simulation(Pid, 1, NumToSimulate+1).

run_simulation(_Pid, Counter, Num)
        when Counter == Num ->
    not_ended;
run_simulation(Pid, Counter, Num)
        when Counter < Num ->
%    BagBefore = gen_server:call(Pid, {get_bag}),
    RunRes = run(Pid),
%    BagAfter = gen_server:call(Pid, {get_bag}),
%    io:format("~nSimulation run ~p: ~p -> ~p", [Counter, BagBefore, BagAfter]),

    case RunRes of
        {eoc, SimulationResult} ->
%            io:format("~nNo more tokens in bag. Abnormally finished"),
            SimulationResult;
        ok ->
            run_simulation(Pid, Counter+1, Num)
    end.

%internals of gen_server
init([]) ->
    {ok, {[], none}}.

handle_call({get_bag}, _, State={List, _}) ->
    {reply, List, State};

handle_call({put_candies, CandiesList}, _, _State) 
        when is_list(CandiesList)->
    {reply, ok, {CandiesList, none}};

handle_call({take_random}, _, State={[], _}) ->
    {reply, none, State};

handle_call({take_random}, _, {List,_}) ->
%generate random from 1 to len of List
    RandomNum = random:uniform(lists:flatlength(List)),
%take elem per number
    Token = lists:nth(RandomNum, List),
%throw out taken elem (ListLen should be less 10000)
    NewState = lists:sublist(List, RandomNum-1) ++
            lists:sublist(List, RandomNum+1, 10000),
    {reply, Token, {NewState, Token}};

handle_call({taken_before}, _, State={List, Token}) 
        when is_list(List) ->
    {reply, Token, State};

handle_call({put, Token}, _, {List, TokenTaken}) ->
    {reply, ok, {[Token|List], TokenTaken}};

handle_call(stop, _, State) ->
    {stop, normal, State}.

%never need here
handle_cast(_Request, State) -> {noreply, State}.

%never need here
handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
