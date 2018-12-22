:- consult(lamps2).
:- consult(csv).


selection_strategy_op([
                        leftmost,
                        min,
                        max,
                        ff,
                        anti_first_fail,
                        occurrence,
                        ffc,
                        max_regret
                    ]).

value_order_op([
                    up,
                    down
    ]).

branching_op([
                    step,
                    enum,
                    bisect,
                    median,
                    middle
    ]).



testAll(Board) :-

    selection_strategy_op(Selection_op),
    value_order_op(Order_op),
    branching_op(Branching_op),

    findall(Options-Time, (
                    member(S, Selection_op), 
                    member(O, Order_op), 
                    member(B, Branching_op),
                    Options = [S, O, B],
                    solve(Board, _, _, _, _, [time_out(5000, success) | Options], Time),
                    write(Options), write(' - '), write(Time), nl
                    ),
                List),
                

    open_test_csv(Stream),
    forall(member(Options-Time, List), save_test_csv(Stream, Options, Time)),
    close(Stream).