-module(scenarios).

-export([
    test3rules/0,
    run_estimation/1
    ]).

test3rules() ->
    %create game
    {ok, P} = game:start_link(),
    %test rule 1
    ok = game:put_candies_to_bag(P, [blue, blue]),
    Res1 = game:run_simulation(P, 5),
    %test rule 2
    ok = game:put_candies_to_bag(P, [red, red]),
    Res2 = game:run_simulation(P, 5),
    %test rule 3
    ok = game:put_candies_to_bag(P, [red, blue]),
    Res3 = game:run_simulation(P, 5),
    io:format("~nResults (not ended, red, red): ~p ~p ~p", [Res1, Res2, Res3]).

run_estimation(Limit) when is_number(Limit) ->
    %create game
    {ok, P} = game:start_link(),

    run_estimation_calc(1, Limit+1, P, 0, 0, 0),

    game:stop(P).

run_estimation_calc(Counter, Num, _P, Est1, Est2, Est3)
        when Counter == Num ->
    io:format("~n Exiting on step ~p of ~p with final results", [Counter, Num-1]),
    io:format("~n ~p ~p ~p", [Est1, Est2, Est3]);
run_estimation_calc(Counter, Num, P, Est1, Est2, Est3)
        when Counter < Num ->
    ok = game:put_10red_10blue_to_bag(P),
    Result = game:run_simulation(P, 100),

    case Result of
        not_ended ->
            NEst1 = Est1+1, NEst2 = Est2, NEst3 = Est3;
        blue ->
            NEst1 = Est1, NEst2 = Est2+1, NEst3 = Est3;
        red ->
            NEst1 = Est1, NEst2 = Est2, NEst3 = Est3+1
    end,

    io:format("~n After ~p simulations: ~p ~p ~p", [Counter, NEst1/Counter, NEst2/Counter, NEst3/Counter]),
    run_estimation_calc(Counter+1, Num, P, NEst1, NEst2, NEst3).
