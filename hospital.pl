/* Hospital Transfer System 
   COU4303 Assignment
   Includes: DFS, BFS, A*, GUI, and menu system
   Run: ?- [COU4303_Group34]. then ?- gui. or ?- run.
*/

:-set_prolog_flag(answer_write_options,[max_depth(0)]).
:-use_module(library(lists)).
:-use_module(library(apply)).
:-use_module(library(pairs)).

% XPCE GUI stuff 
:- dynamic xpce_ok/0.
:- catch(use_module(library(pce)), _, true).
:- (current_predicate(send/2) -> assertz(xpce_ok) ; true).

/* Hospital data - name, facilities, x, y coordinates */
hospital(nawaloka_colombo, [advanced_surgery, cardiac, neurology, dialysis], 0, 0).
hospital(national_kandy, [advanced_surgery, orthopedic, oncology, psychiatry], 80, 80).
hospital(teaching_rathnapura, [advanced_surgery, cardiac, dialysis], -70, -30).
hospital(asiri_galle, [advanced_surgery, oncology, orthopedic], -90, -90).
hospital(kalmunai_ampara, [advanced_surgery, neurology, psychiatry], 200, -50).
hospital(venus_jaffna, [cardiac, orthopedic, neurology],10,380).
hospital(central_badulla, [oncology, dialysis, psychiatry],160,-40).


/* Road connections between hospitals */
road(nawaloka_colombo, national_kandy, 115).
road(nawaloka_colombo, teaching_rathnapura, 90).
road(nawaloka_colombo, asiri_galle, 120).
road(nawaloka_colombo, central_badulla, 225).
road(nawaloka_colombo, kalmunai_ampara, 350).
road(nawaloka_colombo, venus_jaffna, 395).

road(national_kandy, teaching_rathnapura, 90).
road(national_kandy, central_badulla, 70).
road(national_kandy, kalmunai_ampara, 180).
road(national_kandy, venus_jaffna, 315).

road(teaching_rathnapura, asiri_galle, 75).
road(teaching_rathnapura, central_badulla, 140).

road(miracle_kurunegala, national_kandy, 60).
road(miracle_kurunegala, nawaloka_colombo, 100).

road(asiri_galle, central_badulla, 200).

road(central_badulla, kalmunai_ampara, 160).
road(central_badulla, teaching_rathnapura, 150).

road(kalmunai_ampara, venus_jaffna, 420).

/* Traffic levels - affects travel cost */
traffic(nawaloka_colombo, national_kandy, high).
traffic(nawaloka_colombo, teaching_rathnapura, medium).
traffic(nawaloka_colombo, asiri_galle, high).
traffic(nawaloka_colombo, central_badulla, medium).
traffic(nawaloka_colombo, kalmunai_ampara, high).
traffic(nawaloka_colombo, venus_jaffna, high).
traffic(national_kandy, central_badulla, medium).
traffic(national_kandy, kalmunai_ampara, medium).
traffic(national_kandy, venus_jaffna, high).
traffic(teaching_rathnapura, asiri_galle, low).
traffic(teaching_rathnapura, central_badulla, medium).
traffic(asiri_galle, central_badulla, medium).
traffic(central_badulla, kalmunai_ampara, medium).
traffic(kalmunai_ampara, venus_jaffna, low).

/* Blocked roads (none by default) */
:- dynamic blocked/2.
blocked:- fail.

/* Helper functions */
edge(A,B,D):-road(A,B,D).
edge(A,B,D):-road(B,A,D).

is_road_blocked(A,B):- blocked(A,B) ; blocked(B,A).

traffic_multiplier(low, 1.0).
traffic_multiplier(medium, 1.4).
traffic_multiplier(high, 1.9).

get_edge_cost(A,B,Cost) :-
    edge(A,B,Base),
    \+ is_road_blocked(A,B),
    ( traffic(A,B,Level) -> true
    ; traffic(B,A,Level) -> true
    ; Level = low
    ),
    traffic_multiplier(Level,Multiplier),
    Cost is Base*Multiplier.

get_neighbors(Node, Next, Cost) :-
    get_edge_cost(Node, Next, Cost).

distance(X1,Y1,X2,Y2,Dist) :-
    DX is X2-X1, 
    DY is Y2-Y1,
    Dist is sqrt(DX*DX + DY*DY).

get_heuristic(From, Goal, H) :-
    hospital(From, _, X1, Y1),
    hospital(Goal, _, X2, Y2),
    distance(X1,Y1,X2,Y2,H).

hospital_has_facility(H, Need) :-
    hospital(H, Facilities, _, _),
    member(Need, Facilities).

get_all_hospitals(Hs) :-
    findall(H, hospital(H,_,_,_), Hs).

/* DFS Implementation */
dfs_path(Start, Goal, Path, Cost, Expanded) :-
    dfs_search(Start, Goal, [Start], Rev, 0, Cost, 0, Expanded),
    reverse(Rev, Path).

dfs_search(Goal, Goal, Visited, Visited, Cost, Cost, Exp, Exp).
dfs_search(Node, Goal, Visited, PathOut, CostAcc, CostOut, ExpIn, ExpOut) :-
    get_neighbors(Node, Next, C),
    \+ member(Next, Visited),
    Exp1 is ExpIn + 1,
    Cost1 is CostAcc + C,
    dfs_search(Next, Goal, [Next|Visited], PathOut, Cost1, CostOut, Exp1, ExpOut).

/* BFS Implementation */
bfs_path(Start, Goal, Path, Cost, Expanded) :-
    bfs_search([[Start]], Goal, PathRev, 0, Expanded),
    calculate_path_cost(PathRev, Cost),
    reverse(PathRev, Path).

bfs_search([[Goal|Rest]|_], Goal, [Goal|Rest], Exp, Exp).
bfs_search([CurrPath|Queue], Goal, Path, ExpIn, ExpOut) :-
    CurrPath = [Node|_],
    findall([N|CurrPath],
            (get_neighbors(Node, N, _), \+ member(N, CurrPath)),
            NextPaths),
    length(NextPaths, K),
    Exp1 is ExpIn + K,
    append(Queue, NextPaths, NewQ),
    bfs_search(NewQ, Goal, Path, Exp1, ExpOut).

calculate_path_cost([_], 0).
calculate_path_cost([A,B|T], Cost) :-
    get_neighbors(A,B,C1),
    calculate_path_cost([B|T], C2),
    Cost is C1 + C2.

/* A* Implementation */
astar_path(Start, Goal, Path, Cost, Expanded) :-
    get_heuristic(Start, Goal, H0),
    Open0 = [f(H0,0,Start,[Start])],
    astar_search(Goal, Open0, [], PathRev, Cost, 0, Expanded),
    reverse(PathRev, Path).

astar_search(Goal, Open, _, Path, Cost, Exp, Exp) :-
    pick_best(Open, f(_,G,Goal,Path), _), 
    Cost = G.

astar_search(Goal, Open, Closed, Path, Cost, ExpIn, ExpOut) :-
    pick_best(Open, f(_,G,Node,Pth), OpenRest),
    ( member(Node, Closed) ->
        astar_search(Goal, OpenRest, Closed, Path, Cost, ExpIn, ExpOut)
    ;
        findall(f(Fn, Gn, N, [N|Pth]),
                ( get_neighbors(Node, N, C),
                  \+ member(N, Pth),
                  Gn is G + C,
                  get_heuristic(N, Goal, Hn),
                  Fn is Gn + Hn ),
                Children),
        length(Children, K),
        Exp1 is ExpIn + K,
        merge_open_list(OpenRest, Children, OpenNext),
        astar_search(Goal, OpenNext, [Node|Closed], Path, Cost, Exp1, ExpOut)
    ).

% A* helper functions
pick_best([X|Xs], Best, Rest) :- pick_best_helper(Xs, X, Best, Rest).
pick_best_helper([], Best, Best, []).
pick_best_helper([f(F,G,N,P)|Xs], f(Fb,Gb,Nb,Pb), Best, [Drop|Rest]) :-
    ( F < Fb -> Pick = f(F,G,N,P), Drop = f(Fb,Gb,Nb,Pb)
    ; Pick = f(Fb,Gb,Nb,Pb), Drop = f(F,G,N,P) ),
    pick_best_helper(Xs, Pick, Best, Rest0), 
    Rest = [Drop|Rest0].

merge_open_list(Open, Children, Merged) :- 
    append(Open, Children, Tmp), 
    remove_worse(Tmp, Merged).

remove_worse(List, Pruned) :-
    findall(Node-FGNP, (member(f(F,G,N,P), List), FGNP=(F,G,N,P), Node=N), Pairs),
    keysort(Pairs, Sorted),
    pick_best_per_node(Sorted, BestPairs),
    findall(f(F,G,N,P), member(_-(F,G,N,P), BestPairs), Pruned).

pick_best_per_node([], []).
pick_best_per_node([K-(F,G,N,P)|Rest], [K-(F,G,N,P)|Out]) :-
    take_same_node(K, Rest, Same, Rem),
    min_by_f_value([F-G-N-P|Same], F-G-N-P), 
    pick_best_per_node(Rem, Out).

take_same_node(_, [], [], []).
take_same_node(K, [K-(F,G,N,P)|T], [F-G-N-P|Same], Rem) :- !, take_same_node(K, T, Same, Rem).
take_same_node(_, Rest, [], Rest).

min_by_f_value([F-G-N-P], F-G-N-P).
min_by_f_value([F1-G1-N1-P1, F2-G2-N2-P2|T], Min) :-
    (F1 =< F2 -> Head = F1-G1-N1-P1 ; Head = F2-G2-N2-P2),
    min_by_f_value([Head|T], Min).

/* Find destination hospitals */
find_nearest_by_air(Start, Need, Dest) :-
    findall(H, (hospital(H,_,_,_), H \= Start, hospital_has_facility(H,Need)), Candidates),
    hospital(Start,_,Xs,Ys),
    map_list_to_pairs([H,Dist]>>(hospital(H,_,Xh,Yh), distance(Xs,Ys,Xh,Yh,Dist)), Candidates, Pairs),
    keysort(Pairs, [ _-Dest | _ ]).

find_best_by_astar(Start, Need, Dest, BestCost) :-
    findall(H, (hospital(H,_,_,_), H \= Start, hospital_has_facility(H,Need)), Candidates),
    findall(Cost-H,
        ( member(H, Candidates),
          ( astar_path(Start,H,_Path,Cost,_Exp) ->
                true
          ; bfs_path(Start,H,_Path2,Cost,_Exp2) ->
                true
          ; Cost = 1.0Inf )
        ),
        Costs),
    keysort(Costs, [BestCost-Dest | _]).

/* Compare all algorithms */
compare_all(Start, Dest) :-
    ( bfs_path(Start, Dest, PB, CB, EB) -> true ; PB=[], CB=inf, EB=0 ),
    ( dfs_path(Start, Dest, PD, CD, ED) -> true ; PD=[], CD=inf, ED=0 ),
    ( astar_path(Start, Dest, PA, CA, EA) 
        -> true
        ; bfs_path(Start, Dest, PA, CA, EA)   % fallback if A* fails
    ),
    format('\n=== Comparison ~w -> ~w ===~n', [Start, Dest]),
    format('BFS : Path=~w~n      Cost=~2f, Expanded=~w~n', [PB, CB, EB]),
    format('DFS : Path=~w~n      Cost=~2f, Expanded=~w~n', [PD, CD, ED]),
    format('A*  : Path=~w~n      Cost=~2f, Expanded=~w  (usually best)~n~n', [PA, CA, EA]).

do_transfer(Start, Facility, nearest_air) :-
    (hospital_has_facility(Start, Facility) ->
        format("Start hospital ~w already has ~w facility. No transfer needed.~n",
               [Start, Facility]);
        find_nearest_by_air(Start, Facility, Dest),
        format('Destination (nearest-air): ~w~n', [Dest]), 
        compare_all(Start, Dest)).

do_transfer(Start, Facility, best_astar) :-
    (hospital_has_facility(Start, Facility) ->
        format("Start hospital ~w already has ~w facility. No transfer needed.~n",
               [Start, Facility]);
        find_best_by_astar(Start, Facility, Dest, BestCost),
        format('Destination (best A*): ~w (Cost ~2f)~n', [Dest, BestCost]), 
        compare_all(Start, Dest)).


/* Main menu system */
run :-
    nl, writeln('=== Hospital Transfer System ==='),
    writeln('1) List hospitals'),
    writeln('2) Compare BFS/DFS/A* to a chosen destination'),
    writeln('3) Transfer (nearest-by-air)'),
    writeln('4) Transfer (best-by-A*)'),
    writeln('5) Toggle a blocked road'),
    writeln('6) Exit'),
    write('Enter choice: '), read(Choice), handle_choice(Choice), !, run.
run :- writeln('Goodbye.').

handle_choice(1) :- get_all_hospitals(Hs), forall(member(H,Hs), show_hospital_info(H)).
handle_choice(2) :- ask_for_hospital('Start hospital', S), ask_for_hospital('Destination hospital', D), compare_all(S,D).
handle_choice(3) :- ask_for_hospital('Start hospital', S), ask_for_facility(F), do_transfer(S, F, nearest_air).
handle_choice(4) :- ask_for_hospital('Start hospital', S), ask_for_facility(F), do_transfer(S, F, best_astar).
handle_choice(5) :- ask_for_hospital('Road endpoint A', A), ask_for_hospital('Road endpoint B', B), toggle_road_block(A,B).
handle_choice(6) :- !, fail.
handle_choice(_) :- writeln('Invalid choice.').

show_hospital_info(H) :- 
    hospital(H,Fs,X,Y), 
    format('~w @ (~w,~w) -> Facilities: ~w~n', [H,X,Y,Fs]).

ask_for_hospital(Prompt, H) :- 
    get_all_hospitals(Hs), 
    format('~w from: ~w~n> ', [Prompt,Hs]), 
    read(H),
    ( member(H,Hs) -> true ; writeln('Invalid. Try again.'), ask_for_hospital(Prompt,H)).

ask_for_facility(F) :- 
    writeln('Facility needed (advanced_surgery | cardiac | orthopedic | oncology | dialysis | psychiatry | neurology):'),
    write('> '), 
    read(F),
    ( member(F,[advanced_surgery, cardiac, orthopedic, oncology, dialysis, psychiatry, neurology]) -> true ; writeln('Invalid. Try again.'), ask_for_facility(F)).

toggle_road_block(A,B) :-
    ( is_road_blocked(A,B) -> 
        retractall(blocked(A,B)), 
        retractall(blocked(B,A)), 
        format('Unblocked ~w <-> ~w~n',[A,B])
    ; edge(A,B,_) -> 
        assertz(blocked(A,B)), 
        format('Blocked ~w <-> ~w~n',[A,B])
    ; writeln('There is no such road.')
    ).

/* GUI System */
gui :- ( xpce_ok -> start_gui ; writeln('XPCE GUI not available. Use run/0 instead.'), fail).

:- dynamic gui_window/1.

start_gui :-
    ( gui_window(W) -> send(W, destroy), retractall(gui_window(_)) ; true ),
    new(W, dialog('Hospital Transfer System')), 
    assertz(gui_window(W)),
    get_all_hospitals(Hs),
    
    % Start Hospital dropdown
    send(W, append, new(LStart, label(start_label, 'Start Hospital:'))),
    send(LStart, font, font(helvetica, bold, 16)),
    send(W, append, new(Start, menu(start_hospital, cycle))),
    send(Start, append, '-- Select --'),
    fill_menu(Start, Hs),

    % Facility dropdown
    send(W, append, new(LFac, label(fac_label, 'Facility:'))),
    send(LFac, font, font(helvetica, bold, 16)),
    send(W, append, new(Fac, menu(facility, cycle))),
    send(Fac, append, '-- Select --'),
    fill_menu(Fac, [advanced_surgery, cardiac, orthopedic, oncology, dialysis, psychiatry, neurology]),

    % Algorithm dropdown
    send(W, append, new(LAlg, label(alg_label, 'Algorithm:'))),
    send(LAlg, font, font(helvetica, bold, 16)),
    send(W, append, new(Alg, menu(algorithm, cycle))),
    send(Alg, append, '-- Select --'),
    fill_menu(Alg, [nearest_air,best_astar]), 
    
    send(W, append, new(Cmp, button(compare, message(@prolog, gui_compare, Start?selection, Fac?selection, Alg?selection)))), 
    send(Cmp, font, font(helvetica, bold, 14)), 
    send(Cmp, size, size(120, 80)),
    send(W, append, new(Out, text)), 
    send(Out, name, result), 
    send(Out, size, size(80,10)), 
    send(Out, font, font(screen, bold, 14)), 
    send(W, open_centered).

fill_menu(Menu, Items) :- forall(member(I,Items), send(Menu, append, I)).

gui_compare(Start, Facility, Strategy) :-
    % check if user selected all options
    ( Start == '-- Select --' ; Facility == '-- Select --' ; Strategy == '-- Select --' ) ->
        send(@display, inform, 'Please select all options before comparing.')
    ;
    (
        ( Strategy == nearest_air ->
            ( hospital_has_facility(Start, Facility) ->
                % Start hospital already has facility
                with_output_to(string(S),
                    format("Start hospital ~w already has ~w facility. No transfer needed.~n",
                           [Start, Facility]))
            ;
                % Need transfer
                find_nearest_by_air(Start,Facility,Dest),
                with_output_to(string(S),
                    ( format('Dest (nearest-air): ~w~n',[Dest]),
                      compare_all(Start,Dest)))
            )
        ; Strategy == best_astar ->
            ( hospital_has_facility(Start, Facility) ->
                % Start hospital already has facility
                with_output_to(string(S),
                    format("Start hospital ~w already has ~w facility. No transfer needed.~n",
                           [Start, Facility]))
            ;
                % Need transfer
                ( find_best_by_astar(Start,Facility,Dest,Best) ->
                    with_output_to(string(S),
                        ( format('Dest (best A*): ~w (~2f)~n',[Dest,Best]),
                          compare_all(Start,Dest)))
                ; S = "No feasible destination by A*."
                )
            )
        ),
        gui_window(W),
        get(W, member, result, Out),
        send(Out, clear),
        send(Out, append, S)
    ).