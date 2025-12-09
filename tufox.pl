% TuFox text adventure game with AI rabbits and planner-driven detective

:- encoding(utf8).
:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- dynamic location/2.
:- dynamic alive/1.
:- dynamic role/2.
:- dynamic task/6.
:- dynamic cooldown/3.
:- dynamic inspected/1.
:- dynamic body/2.
:- dynamic next_meeting/1.
:- dynamic round_counter/1.
:- dynamic revealed_fox/1.
:- dynamic vote/2.
:- dynamic alias/2.
:- dynamic trust/3.
:- dynamic log_entry/4.
:- dynamic spoken_log/3.
:- dynamic history_statement/4.
:- dynamic show_rabbits_on_map/1.
:- dynamic show_tasks_on_map/1.
:- dynamic meeting_pending/0.
:- dynamic action_log/1.
:- dynamic use_external_planner/1.
:- dynamic show_planned_actions/1.

rooms([
    'Tower','Library','Armory','Observatory',
    'Hall','Dining Room','Kitchen','Storage',
    'Study','Throne Room','Bathroom','Bedroom',
    'Chapel','Dungeon','Wine Cellar','Balcony'
]).

rooms_grid([
    ['Tower','Library','Armory','Observatory'],
    ['Hall','Dining Room','Kitchen','Storage'],
    ['Study','Throne Room','Bathroom','Bedroom'],
    ['Chapel','Dungeon','Wine Cellar','Balcony']
]).

% task(TaskId, Room, NeededRounds, RemainingRounds, Status, Occupant)
task_specs([
    spec(collect_food,4),
    spec(fix_wiring,5),
    spec(clean_vent,4),
    spec(fix_chandelier,4),
    spec('organize scrolls',3),
    spec('polish armor',4),
    spec('sweep hall',3),
    spec('check telescope',4)
]).

assign_tasks_to_rooms :-
    task_specs(Specs),
    rooms(Rooms),
    random_permutation(Rooms, Shuffled),
    length(Specs, Count),
    take(Count, Shuffled, SelectedRooms),
    assign_spec_to_room(Specs, SelectedRooms).

assign_spec_to_room([], []).
assign_spec_to_room([spec(Task,Need)|Specs], [Room|Rooms]) :-
    assertz(task(Task,Room,Need,Need,available,none)),
    assign_spec_to_room(Specs, Rooms).

take(0, _, []).
take(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N-1,
    take(N1, T, Rest).

characters([player,bunny1,bunny2,bunny3,bunny4,detective]).
role(player,fox).
role(bunny1,rabbit).
role(bunny2,rabbit).
role(bunny3,rabbit).
role(bunny4,rabbit).
role(detective,detective).

start :-
    reset_world,
    format('\nWelcome to TuFox!\n', []),
    write('You are the fox. Eliminate rabbits until only one remains.'),nl,
    write('Rabbits win if tasks finish or the fox dies.'),nl,
    print_help,
    look,
    game_loop.

print_help :-
    nl,
    write('Commands:'),nl,
    write('  move(Direction).         % move up/down/left/right on the map'),nl,
    write('  look.                    % describe current room'),nl,
    write('  status.                  % show game status'),nl,
    write('  kill(Target).            % eliminate a rabbit in this room (cooldown 3)'),nl,
    write('  wait.                    % end your action'),nl,
    write('  show_rabbits.            % toggle rabbit visibility on map'),nl,
    write('  show_tasks.              % toggle task visibility on map'),nl,
    write('  use_planner.             % toggle external PDDL planner for detective'),nl,
    write('  show_pddl.               % toggle visibility of PDDL planned actions'),nl,
    nl.

reset_world :-
    retractall(location(_,_)),
    retractall(alive(_)),
    retractall(task(_,_,_,_,_,_)),
    retractall(cooldown(_,_,_)),
    retractall(inspected(_)),
    retractall(body(_,_)),
    retractall(next_meeting(_)),
    retractall(round_counter(_)),
    retractall(revealed_fox(_)),
    retractall(vote(_,_)),
    retractall(alias(_,_)),
    retractall(trust(_,_,_)),
    retractall(log_entry(_,_,_,_)),
    retractall(spoken_log(_,_,_)),
    retractall(history_statement(_,_,_,_)),
    retractall(show_rabbits_on_map(_)),
    assertz(show_rabbits_on_map(true)),
    retractall(show_tasks_on_map(_)),
    assertz(show_tasks_on_map(true)),
    retractall(meeting_pending),
    retractall(action_log(_)),
    retractall(use_external_planner(_)),
    assertz(use_external_planner(true)), % Default to external planner
    retractall(show_planned_actions(_)),
    assertz(show_planned_actions(false)), % Default to hidden
    assign_tasks_to_rooms,
    forall(characters(Cs), (forall(member(C,Cs), assertz(alive(C))))),
    assign_initial_locations,
    initialize_trust,
    assertz(cooldown(player,kill,0)),
    assertz(cooldown(detective,inspect,2)),
    assertz(next_meeting(4)),
    assertz(round_counter(0)).

initialize_trust :-
    characters(Chars),
    forall((member(A, Chars), member(B, Chars), A \= B), assertz(trust(A,B,100))).

assign_initial_locations :-
    rooms(Rooms),
    % 1. Assign Player
    random_member(PlayerRoom, Rooms),
    assertz(location(player, PlayerRoom)),
    
    % 2. Assign Rabbits (avoid Player)
    findall(Bunny, role(Bunny, rabbit), Bunnies),
    subtract(Rooms, [PlayerRoom], RoomsForBunnies),
    random_permutation(RoomsForBunnies, ShuffledForBunnies),
    length(Bunnies, NumBunnies),
    take(NumBunnies, ShuffledForBunnies, BunnyRooms),
    maplist(assert_location, Bunnies, BunnyRooms),
    
    % 3. Assign Detective (avoid Player and Rabbits)
    append([PlayerRoom], BunnyRooms, OccupiedRooms),
    subtract(Rooms, OccupiedRooms, FreeRooms),
    (FreeRooms \= [] ->
        random_member(DetectiveRoom, FreeRooms),
        assertz(location(detective, DetectiveRoom))
    ;   % Fallback if no free rooms (unlikely with 16 rooms)
        random_member(DetectiveRoom, Rooms),
        assertz(location(detective, DetectiveRoom))
    ).

assert_location(Char, Room) :- assertz(location(Char, Room)).

assign_bunny_locations(Rooms, Bunnies) :-
    random_permutation(Rooms, Shuffled),
    assign_unique_rooms(Shuffled, Bunnies).

assign_unique_rooms(_, []).
assign_unique_rooms([], Bunnies) :- % Fallback if more bunnies than rooms
    rooms(All),
    assign_bunny_locations(All, Bunnies).
assign_unique_rooms([Room|RestRooms], [Bunny|RestBunnies]) :-
    assertz(location(Bunny, Room)),
    assign_unique_rooms(RestRooms, RestBunnies).

look :-
    % location(player,Room),
    % format('You are in the ~w.~n', [Room]),
    % print_connected(Room),
    % print_room_state(Room),
    display_map.

print_connected(Room) :-
    findall(Dest, path(Room, Dest), Ds),
    list_to_set(Ds, Unique),
    format('Connected rooms: ~w~n', [Unique]).

print_room_state(Room) :-
    findall(C, (location(C,Room), alive(C), C \= player), Others),
    display_names(Others, VisibleOthers),
    (VisibleOthers = [] -> write('No other characters here.\n')
    ; format('Others here: ~w~n', [VisibleOthers])),
    (body(Room,V) -> (visible_name(V,VisibleV), format('There is a body here: ~w~n',[VisibleV])) ; true),
    findall(T, (task(T,Room,_,Remaining,Status,_), member(Status,[available,in_progress]), Remaining>0), Tasks),
    (Tasks = [] -> true ; format('Active tasks here: ~w~n',[Tasks]) ).

status :-
    round_counter(R),
    format('Round ~w.~n', [R]),
    alive_rabbits(AliveRabbits),
    display_names(AliveRabbits, VisibleRabbits),
    format('Alive rabbits: ~w~n', [VisibleRabbits]),
    findall(D, (role(D,rabbit), \+ alive(D)), DeadRabbits),
    display_names(DeadRabbits, VisibleDead),
    (VisibleDead \= [] -> format('Dead rabbits: ~w~n', [VisibleDead]) ; true),
    (alive(player) -> write('You are alive.\n'); write('You are dead.\n')),
    list_tasks_status,
    display_map,
    show_cooldowns.

list_tasks_status :-
    findall(desc(T,Room,Status,Total,Remaining,Occupant), task(T,Room,Total,Remaining,Status,Occupant), Descs),
    forall(member(desc(T,Room,S,Total,R,O), Descs), (
        visible_name(O, VisibleO),
        (alive(O) -> OccStr = VisibleO ; format(string(OccStr), "~w (was dead)", [VisibleO])),
        Done is Total - R,
        format('Task ~w in ~w: ~w (~w/~w rounds done, occupant ~w)~n', [T,Room,S,Done,Total,OccStr])
    )).

show_cooldowns :-
    forall(member(Skill, [kill]), (
        cooldown(player, Skill, V), format('Cooldown ~w: ~w~n',[Skill,V])
    )).

move(Direction) :-
    alive(player),
    location(player,From),
    (   adjacent_room(From, Direction, To)
    ->  retractall(location(player,_)), % Ensure only one location
        assertz(location(player,To)),
        format('You moved to ~w.~n',[To]),
        check_bodies(To),
        player_done
    ;   valid_direction(Direction)
    ->  write('Cannot move in that direction from here.'),nl, player_turn
    ;   write('Unknown direction. Use up/down/left/right.'),nl, player_turn
    ).

valid_direction(up).
valid_direction(down).
valid_direction(left).
valid_direction(right).

adjacent_room(Room, Direction, Adjacent) :-
    rooms_grid(Grid),
    locate_room(Grid, Room, Row, Col),
    direction_delta(Direction, DRow, DCol),
    Row1 is Row + DRow,
    Col1 is Col + DCol,
    within_grid(Grid, Row1, Col1),
    nth1(Row1, Grid, RowList),
    nth1(Col1, RowList, Adjacent).

direction_delta(up, -1, 0).
direction_delta(down, 1, 0).
direction_delta(left, 0, -1).
direction_delta(right, 0, 1).

% adjacency derived from the grid layout
path(Room, Adjacent) :-
    adjacent_room(Room, _, Adjacent).

within_grid(Grid, Row, Col) :-
    Row > 0,
    Col > 0,
    length(Grid, RowCount),
    Row =< RowCount,
    nth1(1, Grid, FirstRow),
    length(FirstRow, ColCount),
    Col =< ColCount.

locate_room(Grid, Room, Row, Col) :-
    nth1(Row, Grid, RowList),
    nth1(Col, RowList, Room), !.

wait :-
    alive(player),
    write('You wait.'),nl,
    player_done.

show_rabbits :-
    show_rabbits_on_map(Current),
    (Current == true -> New = false ; New = true),
    retract(show_rabbits_on_map(Current)),
    assertz(show_rabbits_on_map(New)),
    format('Show rabbits on map: ~w~n', [New]),
    player_turn.

show_tasks :-
    show_tasks_on_map(Current),
    (Current == true -> New = false ; New = true),
    retract(show_tasks_on_map(Current)),
    assertz(show_tasks_on_map(New)),
    format('Show tasks on map: ~w~n', [New]),
    player_turn.

use_planner :-
    use_external_planner(Current),
    (Current == true -> New = false ; New = true),
    retract(use_external_planner(Current)),
    assertz(use_external_planner(New)),
    format('External planner enabled: ~w~n', [New]),
    player_turn.

show_pddl :-
    show_planned_actions(Current),
    (Current == true -> New = false ; New = true),
    retract(show_planned_actions(Current)),
    assertz(show_planned_actions(New)),
    format('Show PDDL actions: ~w~n', [New]),
    player_turn.

kill(Target) :-
    alive(player),
    cooldown(player,kill,CD),
    (CD > 0 -> format('Kill skill cooling down (~w).~n',[CD]), player_turn
    ; resolve_target(Target, Resolved),
      location(player,Room), location(Resolved,Room), alive(Resolved), Resolved \= player 
    ->  (Resolved == detective 
        ->  write('You cannot kill the detective!'), nl, player_turn
        ;   findall(C, (location(C,Room), alive(C)), Present),
            length(Present, RoomCount),
            findall(A, alive(A), AllAlive),
            length(AllAlive, TotalAlive),
            % If total alive > 3 (Player + >2 others), witnesses prevent killing.
            % If total alive <= 3 (e.g. Player + Detective + 1 Rabbit), killing is allowed even with witnesses.
            (TotalAlive > 3, RoomCount > 2
            ->  write('Too many witnesses! You cannot kill when others are watching.'), nl, player_turn
            ;   retract(alive(Resolved)),
                release_tasks_for(Resolved),
                assertz(body(Room,Resolved)),
                retract(cooldown(player,kill,_)),
                assertz(cooldown(player,kill,3)),
                visible_name(Resolved, Visible),
                format('You eliminated ~w!~n',[Visible]),
                player_done
            )
        )
    ; write('No valid target here.'),nl, player_turn).

player_done :-
    ai_turns,
    game_loop.

player_turn :-
    (alive(player) -> true ; write('You are dead. Watching the chaos...'),nl, player_done),
    read_command(Command),
    (Command == quit -> halt ; (catch(call(Command), Err, (print_message(error, Err), player_turn)))).

read_command(Command) :-
    prompt('|: ', ''),
    read_line_to_string(user_input, Raw),
    normalize_space(string(Trimmed), Raw),
    (Trimmed == "" -> read_command(Command)
    ; ensure_period(Trimmed, WithPeriod),
      (   catch(read_term_from_atom(WithPeriod, Command0, [variable_names(Vars)]), error(syntax_error(_),_), fail)
      ->  bind_variable_names(Vars),
          Command = Command0
      ;   write('Could not parse that command. Try syntax like look. or kill(bunny1).'),nl,
          read_command(Command)
      )
    ).

bind_variable_names([]).
bind_variable_names([Name=Var|Rest]) :-
    ( var(Var) -> Var = Name ; true ),
    bind_variable_names(Rest).

ensure_period(Str, Str) :-
    sub_string(Str, _, 1, 0, '.'), !.
ensure_period(Str, WithPeriod) :-
    string_concat(Str, '.', WithPeriod).

visible_name(player, you) :- !.
visible_name(none, none) :- !.
visible_name(Char, Char).

display_names(Chars, Names) :-
    maplist(visible_name, Chars, Names).

resolve_target(Target, Target).

progress_task(Task,Room,Actor) :-
    retract(task(Task,Room,Need,Remaining,in_progress,Actor)),
    NewR is Remaining - 1,
    (NewR =< 0 -> (
        assertz(task(Task,Room,Need,0,complete,none)),
        format(string(Msg), 'Task ~w completed!~n',[Task]),
        assertz(action_log(Msg))
    ) ; 
        assertz(task(Task,Room,Need,NewR,in_progress,Actor)),
        visible_name(Actor, VName),
        Done is Need - NewR,
        format(string(Msg), '~w is busy working on ~w (~w/~w).~n', [VName, Task, Done, Need]),
        assertz(action_log(Msg))
    ).

check_bodies(Room) :-
    (   body(Room,_)
    ->  write('You spot a body here! Meeting will be called at the end of the round.'),nl,
        (meeting_pending -> true ; assertz(meeting_pending))
    ;   true
    ).

game_loop :-
    (check_victory -> true ;
        (alive(player) -> player_turn ; (ai_turns, game_loop))
    ).

check_victory :-
    (\+ alive(player) -> rabbits_win, true
    ; alive_rabbits(List), length(List,L), (L =< 1 -> fox_win, true ;
        tasks_remaining(Rem), (Rem =< 0 -> rabbits_win, true ; fail))).

alive_rabbits(List) :-
    findall(R, (alive(R), R \= player), List).

fox_win :-
    write('You have reduced the rabbits. Fox wins!'),nl.

rabbits_win :-
    write('Rabbits completed objectives. Rabbits win!'),nl.

tasks_remaining(Rem) :-
    findall(T, (task(T,_,_,R,Status,_), Status \= complete, R > 0), Ts),
    length(Ts, Rem).

% AI logic
ai_turns :-
    alive_rabbits(Rs),
    forall(member(AI, Rs), ai_act(AI)),
    (record_round_logs -> true ; write('DEBUG: record_round_logs failed'), nl, fail),
    (tick_world -> true ; write('DEBUG: tick_world failed'), nl, fail),
    (check_meeting_trigger -> true ; write('DEBUG: check_meeting_trigger failed'), nl, fail).

check_meeting_trigger :-
    (   meeting_pending
    ->  resolve_meeting('Body found'), retractall(meeting_pending)
    ;   round_counter(R), next_meeting(NM), R >= NM
    ->  resolve_meeting('Time up')
    ;   true
    ).

record_round_logs :-
    round_counter(R0),
    R is R0 + 1,
    log_order(Order),
    forall(member(Char, Order), (alive(Char) -> log_character_state(Char, R) ; true)).

log_order(Order) :-
    characters(All),
    exclude(=(player), All, NonPlayer),
    append(NonPlayer, [player], Order).

log_character_state(Char, Round) :-
    location(Char, Room),
    findall(O, (location(O, Room), alive(O)), Others0),
    sort(Others0, Others),
    update_room_logs(Round, Room, Others),
    retractall(log_entry(Char, Round, Room, _)),
    assertz(log_entry(Char, Round, Room, Others)).

update_room_logs(Round, Room, Others) :-
    forall(log_entry(Other, Round, Room, _), (
        retract(log_entry(Other, Round, Room, _)),
        assertz(log_entry(Other, Round, Room, Others))
    )).

ai_act(AI) :- % dispatch for every AI agent
    ai_act_logic(AI).

ai_act_logic(AI) :-
    \+ alive(AI), !.

ai_act_logic(detective) :-
    location(detective,Room),
    ( body(Room,_) ->
        (meeting_pending -> true ; assertz(meeting_pending), assertz(action_log('Detective found a body! Meeting pending...\n')))
    ; ( cooldown(detective,inspect,CD),
        CD =:= 0,
        findall(T, (location(T,Room), alive(T), T \= detective, \+ inspected(T)), Targets),
        Targets \= [] ->
            Targets = [Target|_],
            inspect_identity(Target)
      ; % Check if external planner is enabled
        ( use_external_planner(true)
        ->  find_lowest_trust_target(detective, Target),
            generate_pddl_problem(Target),
            ( run_pyperplan(Plan), Plan = [Action|_]
            ->  write('Detective is using PDDL planner...'), nl,
                (show_planned_actions(true) -> format('Planned actions: ~w~n', [Plan]) ; true),
                apply_action(detective, Action)
            ;   % Fallback if planner fails: DO NOTHING (or log error)
                write('PDDL planner failed. Detective waits.'), nl,
                (exists_file('plan.txt') -> 
                    open('plan.txt', read, S), 
                    read_string(S, _, Content), 
                    close(S), 
                    format('DEBUG: plan.txt content:\n~s\n', [Content])
                ;   write('DEBUG: plan.txt does not exist.\n')
                )
            )
        ;   % Internal logic: find lowest trust target
            find_lowest_trust_target(detective, Target),
            location(Target, TargetRoom),
            move_ai_toward(detective, TargetRoom)
        )
      )
    ).

ai_act_logic(AI) :-
    location(AI,Room),
    (body(Room,_) ->
        visible_name(AI, VName),
        (meeting_pending -> true ; assertz(meeting_pending), format(string(Msg), '~w found a body! Meeting pending...~n', [VName]), assertz(action_log(Msg)))
    ; attempt_task(AI)).

find_lowest_trust_target(Detective, Target) :-
    findall(score(T,S), (
        alive(T), 
        T \= Detective, 
        \+ inspected(T),  % Only consider uninspected targets
        trust(Detective, T, S)
    ), Scores),
    format('DEBUG: Trust Scores for Detective: ~w~n', [Scores]), % DEBUG
    (   Scores = []
    ->  % If everyone is inspected or no trust scores, fallback to random uninspected or just random
        findall(T, (alive(T), T \= Detective, \+ inspected(T)), Uninspected),
        (Uninspected \= [] -> random_member(Target, Uninspected)
        ; random_member(Target, [player,bunny1,bunny2,bunny3,bunny4]), alive(Target))
    ;   min_member(score(_,MinS), Scores),
        include(matches_score(MinS), Scores, Lowest),
        % If multiple targets have same lowest trust, pick the closest one
        maplist(add_distance_to_score(Detective), Lowest, WithDist),
        keysort(WithDist, [_-score(Target,_)|_])
    ).

add_distance_to_score(Detective, score(T,S), Dist-score(T,S)) :-
    location(Detective, DetRoom),
    location(T, TargetRoom),
    shortest_distance(DetRoom, TargetRoom, Dist).

inspect_identity(Target) :-
    role(Target, Role),
    assertz(inspected(Target)),
    % Increase trust significantly after inspection so detective ignores them later
    trust(detective, Target, OldTrust),
    NewTrust is OldTrust + 100,
    retract(trust(detective, Target, OldTrust)),
    assertz(trust(detective, Target, NewTrust)),
    
    retract(cooldown(detective,inspect,_)),
    assertz(cooldown(detective,inspect,2)),
    (Role == fox -> 
        assertz(revealed_fox(Target)),
        visible_name(Target, VisibleTarget),
        format('Detective inspects ~w and reveals they are the FOX!~n', [VisibleTarget]),
        rabbits_win,
        halt
    ; true),
    visible_name(Target, VisibleTarget),
    format(string(Msg), 'An inspection reveals ~w is ~w.~n',[VisibleTarget,Role]),
    assertz(action_log(Msg)).

attempt_task(AI) :-
    (choose_task(AI, TargetTask, TargetRoom) ->
        location(AI,Room),
        (Room == TargetRoom ->
            (task(TargetTask,Room,_,_,available,none) ->
                retract(task(TargetTask,Room,N,R,available,none)),
                assertz(task(TargetTask,Room,N,R,in_progress,AI)),
                % Immediately progress the task in the same turn
                progress_task(TargetTask,Room,AI)
            ; progress_task_if_owner(AI,TargetTask,Room)
            )
        ; move_ai_toward(AI,TargetRoom)
        )
    ; wander_randomly(AI)).

wander_randomly(AI) :-
    location(AI, CurrentRoom),
    findall(Adj, adjacent_room(CurrentRoom, _, Adj), Adjs),
    random_member(NextRoom, Adjs),
    move_ai_toward(AI, NextRoom).

progress_task_if_owner(AI,Task,Room) :-
    (task(Task,Room,_,_,in_progress,AI) -> progress_task(Task,Room,AI) ; true).

choose_task(AI, Task, Room) :-
    location(AI,Current),
    findall(dist(D,Task0,Room0),
        ( task(Task0,Room0,_,_,Status,Occupant),
          Status \= complete,
          preferred_task(AI, Status, Occupant),
          shortest_distance(Current, Room0, D)
        ),
        Distances),
    Distances \= [],
    closest_tasks(Distances, Closest),
    random_member(dist(_, Task, Room), Closest).

preferred_task(_, available, _).
preferred_task(AI, in_progress, Occupant) :- Occupant == AI.

closest_tasks(Distances, Closest) :-
    findall(D, member(dist(D,_,_), Distances), Ds),
    min_list(Ds, Min),
    include(matches_distance(Min), Distances, Closest).

matches_distance(Min, dist(D,_,_)) :- D =:= Min.

shortest_distance(Room, Room, 0) :- !.
shortest_distance(Start, Goal, Dist) :-
    bfs_queue([(Start,0)], [Start], Goal, Dist).

bfs_queue([(Node,D)|_], _, Goal, D) :- Node == Goal, !.
bfs_queue([(Node,D)|Rest], Visited, Goal, Dist) :-
    findall((Next,D1), (path(Node,Next), \+ member(Next,Visited), D1 is D+1), Nexts),
    findall(Next, member((Next,_), Nexts), NextRooms),
    append(Rest, Nexts, Queue),
    append(Visited, NextRooms, NewVisited),
    bfs_queue(Queue, NewVisited, Goal, Dist).

move_ai_toward(AI,TargetRoom) :-
    location(AI,Room),
    (   Room == TargetRoom
    ->  true
    ;   next_step(Room,TargetRoom,Next),
        retract(location(AI,Room)),
        assertz(location(AI,Next)),
        visible_name(AI, VisibleAI),
        format(string(Msg), '~w moves to ~w.~n',[VisibleAI,Next]),
        assertz(action_log(Msg))
    ;   true
    ).

next_step(Start,Goal,Next) :-
    shortest_path_nodes(Start, Goal, Path),
    Path = [Start,Next|_].

shortest_path_nodes(Start, Goal, Path) :-
    bfs_path([(Start,[Start])], [], Goal, RevPath),
    reverse(RevPath, Path).

bfs_path([(Node,Path)|_], _, Goal, Path) :- Node == Goal, !.
bfs_path([(Node,Path)|Rest], Visited, Goal, ResultPath) :-
    findall((Next,[Next|Path]),
        ( path(Node,Next), \+ member(Next,Visited), \+ member(Next,Path)),
        Nexts),
    append(Rest, Nexts, Queue),
    append(Visited, [Node], NewVisited),
    bfs_path(Queue, NewVisited, Goal, ResultPath).

resolve_meeting(Reason) :-
    format('--- Meeting called (~w) ---~n', [Reason]),
    findall(V, body(_,V), Victims),
    clear_bodies,
    discussion_phase,
    validate_statements,
    apply_advanced_trust_rules(Victims),
    run_votes,
    update_meeting_timer,
    scramble_locations,
    print_trust_scores,
    display_map, % Show map after scramble
    clear_bodies,
    !.

apply_advanced_trust_rules(Victims) :-
    write('--- Analyzing meeting history ---'), nl,
    apply_isolation_penalty,
    apply_victim_witness_bonus(Victims).

apply_isolation_penalty :-
    findall(AI, (role(AI, rabbit), alive(AI)), AIs),
    forall(member(AI, AIs), check_isolation(AI)).

check_isolation(AI) :-
    get_last_two_statements(AI, stmt(_,_,[]), stmt(_,_,[])),
    !,
    visible_name(AI, VName),
    format('~w has been alone for 2 consecutive meetings. Trust decreases by 10.~n', [VName]),
    adjust_trust_global(AI, -10).
check_isolation(_).

apply_victim_witness_bonus(Victims) :-
    forall(member(V, Victims), check_victim_history(V)).

check_victim_history(Victim) :-
    get_last_two_statements(Victim, stmt(_,_,Others1), stmt(_,_,Others2)),
    intersection(Others1, Others2, Common),
    Common \= [],
    !,
    visible_name(Victim, VName),
    display_names(Common, VCommon),
    format('Victim ~w repeatedly met: ~w. Trust increases by 10.~n', [VName, VCommon]),
    forall(member(W, Common), adjust_trust_global(W, 10)).
check_victim_history(_).

get_last_two_statements(AI, S1, S2) :-
    findall(R-stmt(R,Room,Others), history_statement(AI, R, Room, Others), Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, [_-S1, _-S2 | _]).

adjust_trust_global(Target, Delta) :-
    forall((alive(Observer), Observer \= Target, Observer \= player), (
        trust(Observer, Target, Old),
        New is max(0, Old + Delta),
        retract(trust(Observer, Target, Old)),
        assertz(trust(Observer, Target, New))
    )).

decrease_player_trust :-
    write('Suspicion grows... Trust in player decreases by 5.'), nl,
    forall((alive(AI), AI \= player), (
        trust(AI, player, Old),
        New is max(0, Old - 5),
        retract(trust(AI, player, Old)),
        assertz(trust(AI, player, New))
    )).

print_trust_scores :-
    write('--- Trust Matrix (Row trusts Column) ---'), nl,
    findall(C, alive(C), Chars),
    % Header row
    write('           '), % Padding for row labels
    forall(member(C, Chars), (
        visible_name(C, Name),
        format('~|~w~t~10+', [Name])
    )),
    nl,
    % Data rows
    forall(member(Observer, Chars), (
        visible_name(Observer, ObsName),
        format('~|~w~t~10+|', [ObsName]),
        forall(member(Target, Chars), (
            (Observer == Target -> write('   -      ')
            ; trust(Observer, Target, Score) -> format('~|~w~t~10+', [Score])
            ; write('   ?      ')
            )
        )),
        nl
    )),
    nl.

resolve_meeting :- resolve_meeting('Unknown').

scramble_locations :-
    write('Characters scatter to new locations...'), nl,
    rooms(Rooms),
    location(player, PlayerRoom),
    
    % 1. Scramble Rabbits
    findall(R, (role(R, rabbit), alive(R)), Rabbits),
    forall(member(R, Rabbits), retractall(location(R, _))),
    assign_bunny_locations(Rooms, Rabbits),
    
    % 2. Scramble Detective (avoid Player)
    retractall(location(detective, _)),
    subtract(Rooms, [PlayerRoom], AvailableForDet),
    random_member(DetRoom, AvailableForDet),
    assertz(location(detective, DetRoom)).

discussion_phase :-
    write('--- Discussion phase ---'),nl,
    findall(Char, alive(Char), Speakers),
    forall(member(Char, Speakers), speak_from_log(Char)).

speak_from_log(player) :-
    findall(entry(R,Room,Others), (log_entry(player,R,Room,Others), \+ spoken_log(player,R,Room)), Entries),
    exclude(conflicts_with_history, Entries, SafeEntries),
    (SafeEntries = [] -> write('You stay silent to avoid conflicts.'),nl
    ; present_log_choices(SafeEntries, Choice),
      (   Choice == 0
      ->  write('You chose to remain silent.'), nl
      ;   nth1(Choice, SafeEntries, entry(R,Room,Others))
      ->  register_statement(player, R, Room, Others)
      ;   write('Invalid choice. Remaining silent.'), nl
      )
    ).
speak_from_log(Char) :-
    findall(entry(R,Room,Others), (log_entry(Char,R,Room,Others), \+ spoken_log(Char,R,Room)), Entries),
    (Entries = [] -> true
    ; ( include(has_many_witnesses, Entries, ImportantEntries), ImportantEntries \= []
      -> random_member(entry(R,Room,Others), ImportantEntries)
      ;  random_member(entry(R,Room,Others), Entries)
      ),
      register_statement(Char, R, Room, Others)
    ).

has_many_witnesses(entry(_,_,Others)) :-
    length(Others, L),
    L >= 2.

present_log_choices(Entries, Choice) :-
    write('Choose a log entry to state (enter number followed by dot, 0 to stay silent):'), nl,
    print_log_options(Entries, 1),
    read(Choice).

print_log_options([], _).
print_log_options([entry(Round, Room, Others)|Rest], Index) :-
    format_log_snapshot(Round, Room, Others, Text),
    format('~w. ~w~n', [Index, Text]),
    Next is Index + 1,
    print_log_options(Rest, Next).

conflicts_with_history(entry(R,Room,Others)) :-
    history_statement(_, R, Room, PrevOthers),
    PrevOthers \= Others.

register_statement(Char, Round, Room, Others) :-
    assertz(spoken_log(Char, Round, Room)),
    assertz(history_statement(Char, Round, Room, Others)),
    format_statement(Char, Round, Room, Others, Text),
    format('~w~n', [Text]).

format_statement(Char, Round, Room, Others, Text) :-
    visible_name(Char, VisibleChar),
    display_names(Others, VisibleOthers),
    atomic_list_concat(VisibleOthers, ',', OthersText),
    format(string(Text), '~w was in ~w at round ~w, seeing: ~w.', [VisibleChar, Room, Round, OthersText]).

validate_statements :-
    findall(stmt(Speaker,R,Room,Others), history_statement(Speaker,R,Room,Others), Statements),
    forall((alive(AI), AI \= player), validate_against_logs(AI, Statements)).

validate_against_logs(AI, Statements) :-
    visible_name(AI, VisibleAI),
    format('~w is validating statements:~n', [VisibleAI]),
    forall(member(stmt(Speaker,R,Room,Others), Statements), adjust_trust(AI, Speaker, R, Room, Others)),
    nl.

adjust_trust(AI, Speaker, _, _, _) :- AI == Speaker, !.
adjust_trust(AI, Speaker, Round, Room, Others) :-
    trust(AI, Speaker, Current),
    ( log_entry(AI, Round, Room, Logged) ->
        ( Logged == Others -> Delta = 10, Outcome = match
        ; Delta = -10, Outcome = conflict
        )
    ; Delta = 0, Outcome = missing, Logged = none
    ),
    New is max(0, Current + Delta),
    report_trust_evaluation(AI, Speaker, Round, Room, Others, Logged, Outcome, Current, New),
    apply_trust_delta(AI, Speaker, Delta, Current, New).

apply_trust_delta(_, _, 0, _, _) :- !.
apply_trust_delta(AI, Target, _, Old, New) :-
    retract(trust(AI, Target, Old)),
    assertz(trust(AI, Target, New)).

report_trust_evaluation(AI, Speaker, Round, Room, StatedOthers, Logged, Outcome, Current, New) :-
    (Outcome \= missing ->
        format_statement(Speaker, Round, Room, StatedOthers, StatementText),
        visible_name(AI, VisibleAI),
        visible_name(Speaker, VisibleSpeaker),
        format_log_snapshot(Round, Room, Logged, LoggedText),
        trust_outcome_text(Outcome, DeltaText),
        format('  Statement by ~w: ~w~n', [VisibleSpeaker, StatementText]),
        format('  Log of ~w: ~w~n', [VisibleAI, LoggedText]),
        format('  Evaluation: ~w (Trust ~w -> ~w)~n', [DeltaText, Current, New])
    ; true).

format_log_snapshot(_, _, none, 'No corresponding log').
format_log_snapshot(Round, Room, Others, Text) :-
    display_names(Others, VisibleOthers),
    atomic_list_concat(VisibleOthers, ',', OthersText),
    format(string(Text), 'Saw ~w in ~w at round ~w', [OthersText, Room, Round]).

trust_outcome_text(match, 'Match (+10 trust)').
trust_outcome_text(conflict, 'Conflict (-10 trust)').
trust_outcome_text(missing, 'No record (No change)').

run_votes :-
    retractall(vote(_,_)),
    (alive(player) -> player_vote ; true),
    ai_votes,
    print_vote_matrix,
    tally_votes.

print_vote_matrix :-
    write('--- Vote Matrix (Row votes for Column) ---'), nl,
    findall(C, alive(C), Chars),
    % Header row
    write('           '),
    forall(member(C, Chars), (
        visible_name(C, Name),
        format('~|~w~t~10+', [Name])
    )),
    format('~|~w~t~10+', ['Abstain']),
    nl,
    % Data rows
    forall(member(Voter, Chars), (
        visible_name(Voter, VName),
        format('~|~w~t~10+|', [VName]),
        (vote(Voter, Target) -> true ; Target = unknown),
        forall(member(Candidate, Chars), (
            (Target == Candidate -> write('    X     ')
            ; write('    -     ')
            )
        )),
        (Target == abstain -> write('    X     ') ; write('    -     ')),
        nl
    )),
    nl.

player_vote :-
    write('Cast your vote (atom ending with period). alive characters: '),
    alive_rabbits(Rs), display_names(Rs, Visible), write(Visible),nl,
    read(V),
    (   (V = vote(T) -> Target = T ; Target = V), % Support vote(X) or just X
        alive(Target), Target \= player 
    ->  assertz(vote(player,Target)) 
    ;   write('Abstain.'),nl, assertz(vote(player, abstain))
    ).

ai_votes :-
    forall((alive(AI), AI \= player), ai_single_vote(AI)).

ai_single_vote(AI) :-
    alive_targets_for_vote(AI, Candidates),
    (AI == detective ->
        (revealed_fox(Fox), alive(Fox) -> record_vote(AI, Fox)
        ; select_vote_by_trust(AI, Candidates, Vote), record_vote(AI, Vote))
    ; role(AI, rabbit) -> rabbit_vote(AI, Candidates)
    ; select_vote_by_trust(AI, Candidates, Vote), record_vote(AI, Vote)
    ).

rabbit_vote(AI, Candidates) :-
    select_vote_by_trust(AI, Candidates, Vote),
    record_vote(AI, Vote).

unique_lowest_trust_target(AI, Candidates, Vote) :-
    findall(score(T,Score), (member(T, Candidates), (trust(AI, T, Score) -> true ; Score = 100)), Scores),
    Scores \= [],
    findall(Sc, member(score(_,Sc), Scores), AllScores),
    min_list(AllScores, Min),
    include(matches_score(Min), Scores, LowestScores),
    findall(T, member(score(T,_), LowestScores), LowestTargets),
    list_to_set(LowestTargets, UniqueLowest),
    UniqueLowest = [Vote].

matches_score(Min, score(_,Score)) :- Score =:= Min.

record_vote(AI, abstain) :-
    assertz(vote(AI, abstain)).

record_vote(AI, Vote) :-
    Vote \= abstain,
    assertz(vote(AI, Vote)).

alive_targets_for_vote(AI, Candidates) :-
    findall(T, (alive(T), T \= AI), Candidates).

random_vote(Candidates, Vote) :-
    Candidates \= [],
    random_member(Vote, Candidates).

select_vote_by_trust(AI, Candidates, Vote) :-
    findall(score(T,Score), (member(T, Candidates), trust(AI, T, Score)), AllScores),
    % Filter out highly trusted targets (e.g. > 150)
    include(low_trust_target, AllScores, Scores),
    (   Scores = []
    ->  Vote = abstain % If everyone is trusted, abstain
    ;   findall(S, member(score(_,S), Scores), Values),
        max_list(Values, Max),
        min_list(Values, Min),
        Diff is Max - Min,
        (   Diff =< 15
        ->  Vote = abstain
        ;   weighted_vote(Scores, Vote)
        )
    ).

low_trust_target(score(_, Score)) :- Score < 150.

weighted_vote(Scores, Vote) :-
    findall(S, member(score(_,S), Scores), Values),
    max_list(Values, Max),
    maplist(calc_weight(Max), Scores, Weighted),
    foldl(sum_weight, Weighted, 0, TotalWeight),
    random(0.0, TotalWeight, R),
    pick_weighted(Weighted, R, Vote).

calc_weight(Max, score(T,S), w(T,W)) :-
    W is Max - S + 10. % Lower score -> Higher weight

sum_weight(w(_,W), Acc, NewAcc) :- NewAcc is Acc + W.

pick_weighted([w(T,_)|_], R, T) :- R =< 0, !.
pick_weighted([w(T,W)|Rest], R, Vote) :-
    R1 is R - W,
    (R1 =< 0 -> Vote = T ; pick_weighted(Rest, R1, Vote)).

tally_votes :-
    findall(Target, vote(_,Target), Targets),
    count_targets(Targets,Counts),
    (Counts = [] -> write('No votes.'),nl ;
        keysort(Counts,Sorted), reverse(Sorted, [Count-Target|_]),
        vote_threshold(Threshold),
    (   Count >= Threshold, Target \= abstain
    ->  eliminate(Target)
    ;   Target == abstain
    ->  format('Vote result: Abstain (~w votes). No one ejected.~n', [Count])
    ;   format('Vote failed. Requires at least ~w votes.~n', [Threshold])
    )).

vote_threshold(Threshold) :-
    findall(Char, alive(Char), Alive),
    length(Alive, AliveCount),
    Threshold is (AliveCount + 1) // 2.

count_targets([],[]).
count_targets([H|T], Counts) :-
    count_targets(T,Partial),
    (select(N-H,Partial,Rest) -> N1 is N+1, Counts = [N1-H|Rest]
    ; Counts = [1-H|Partial]).

eliminate(Target) :-
    alive(Target),
    retract(alive(Target)),
    release_tasks_for(Target),
    visible_name(Target, VisibleTarget),
    (   Target == player
    ->  format('~w is ejected!~n',[VisibleTarget]),
        rabbits_win,
        halt
    ;   location(Target,_),
        format('~w is ejected!~n',[VisibleTarget])
    ).

update_meeting_timer :-
    retractall(next_meeting(_)),
    round_counter(R),
    NM is R + 4,
    retractall(next_meeting(_)),
    assertz(next_meeting(NM)),
    retractall(vote(_,_)),
    !.

clear_bodies :-
    retractall(body(_,_)).

% world tick: cooldown reductions and task progress persistence

tick_world :-
    decrement_cooldowns,
    display_map,
    print_action_logs,
    round_counter(R),
    R1 is R+1,
    retract(round_counter(_)),
    assertz(round_counter(R1)),
    print_round(R1).

print_action_logs :-
    write('Activity Log:'), nl,
    forall(action_log(Msg), format('  ~w', [Msg])),
    retractall(action_log(_)),
    nl.

print_round(R) :-
    format('--- Round ~w ---~n', [R]).

% Map rendering helpers
display_map :-
    nl,
    write('Map:'),nl,
    rooms_grid(Rows),
    maplist(maplist(cell_display), Rows, CellRows),
    findall(Len, (
        member(Row, CellRows),
        member(Cell, Row),
        member(Line, Cell),
        string_length(Line, Len)
    ), Lengths),
    max_list(Lengths, MaxCellLen0),
    MaxCellLen is max(12, MaxCellLen0),
    % render each row with separators for consistent alignment
    forall(member(RowCells, CellRows), (
        length(RowCells, Count),
        row_separator(Count, MaxCellLen, Sep),
        write(Sep), nl,
        render_row(RowCells, MaxCellLen, Lines),
        forall(member(Line, Lines), (write(Line), nl))
    )),
    CellRows = [FirstRow|_],
    length(FirstRow, FirstCount),
    row_separator(FirstCount, MaxCellLen, FinalSep),
    write(FinalSep), nl,
    nl.

row_separator(CellCount, CellWidth, Separator) :-
    ChunkWidth is CellWidth + 2,
    repeat_char(ChunkWidth, '-', Chunk),
    length(Chunks, CellCount),
    maplist(=(Chunk), Chunks),
    atomic_list_concat(Chunks, '+', Body),
    atomic_list_concat(['+', Body, '+'], Separator).

render_row(Cells, Width, Lines) :-
    maplist(length, Cells, Heights),
    max_list(Heights, MaxHeight),
    numlist(1, MaxHeight, Indexes),
    maplist(row_line(Cells, Width), Indexes, Lines).

row_line(Cells, Width, Index, Line) :-
    maplist(get_cell_line(Index), Cells, Texts),
    maplist(pad_cell(Width), Texts, Padded),
    atomic_list_concat(Padded, '|', Body),
    atomic_list_concat(['|', Body, '|'], Line).

get_cell_line(Index, CellLines, Line) :-
    (nth1(Index, CellLines, Line) -> true ; Line = "").

pad_cell(Width, Text, Padded) :-
    string_length(Text, Len),
    Pad is Width - Len,
    repeat_char(Pad, ' ', Spaces),
    atomic_list_concat([' ', Text, Spaces, ' '], Padded).

repeat_char(N, Char, String) :-
    length(Chars, N),
    maplist(=(Char), Chars),
    atomics_to_string(Chars, '', String).

cell_display(Room, Lines) :-
    room_label(Room, Label),
    player_hint(Room, PH),
    task_hint(Room, TH),
    include(\=(""), [PH, TH], Hints),
    (   Hints = []
    ->  RoomLine = Label
    ;   atomics_to_string([Label|Hints], ' ', RoomLine)
    ),
    
    (   show_tasks_on_map(true)
    ->  room_tasks_line(Room, TaskLine), Tasks = [TaskLine]
    ;   Tasks = []
    ),

    room_characters_line(Room, CharLine),
    (   CharLine == ""
    ->  Chars = []
    ;   Chars = [CharLine]
    ),
    
    append([ [RoomLine], Tasks, Chars ], Lines).

room_characters_line(Room, Line) :-
    show_rabbits_on_map(ShowRabbits),
    findall(S, (
        location(B,Room), alive(B),
        (   role(B, detective)
        ->  S = "Det"
        ;   ShowRabbits == true, role(B, rabbit)
        ->  visible_name(B,N),
            (atom_concat(bunny,R,N) -> atom_concat('R',R,S) ; S=N)
        )
    ), L),
    (L=[] -> Line="" ; atomics_to_string(L,',',S), format(string(Line),"Chars: ~w",[S])).

room_label(Room, Label) :- atom_string(Room, Label).

player_hint(Room, "(You)") :- location(player, Room), !.
player_hint(_, "").

task_hint(_, "").

room_tasks_line(Room, Line) :-
    findall(Display,
        ( task(TaskName,Room,Total,Rem,Status,_),
          (Status == complete -> Label = " (done)"
          ; Done is Total - Rem,
            format(string(Label), " (~w/~w)", [Done, Total])),
          format(string(Display), "~w~w", [TaskName, Label])
        ),
        Tasks),
    (   Tasks = []
    ->  Line = ""
    ;   atomics_to_string(Tasks, ', ', TaskText),
        atomic_list_concat(['Task:', TaskText], ' ', Line)
    ).

task_status_label(complete, " (done)") :- !.
task_status_label(_, "").

decrement_cooldowns :-
    forall(cooldown(Char,Skill,CD), (
        New is max(0, CD-1),
        retract(cooldown(Char,Skill,CD)),
        assertz(cooldown(Char,Skill,New))
    )),
    % Clean up tasks if owner is dead, but do NOT auto-progress tasks here
    % (Tasks are progressing in ai_act_logic -> attempt_task)
    forall(task(T,R,N,Rem,in_progress,Occ), (
        (   alive(Occ)
        ->  true
        ;   retract(task(T,R,N,Rem,in_progress,Occ)),
            assertz(task(T,R,N,Rem,available,none))
        )
    )).

release_tasks_for(Actor) :-
    forall(task(T,R,N,Rem,in_progress,Actor), (
        retract(task(T,R,N,Rem,in_progress,Actor)),
        assertz(task(T,R,N,Rem,available,none))
    )).

% Planner integration (fallback plan if planner not available)

execute_plan_step(detective) :-
    plan_for_detective(Plan),
    (Plan = [Action|_] -> apply_action(detective, Action) ; true).

plan_for_detective(Plan) :-
    default_plan(Plan).

default_plan([
    move(detective,'Hall'),
    move(detective,'Kitchen'),
    inspect(player)
]).

apply_action(_,move(detective,Room)) :-
    move_ai_toward(detective,Room).
apply_action(_,inspect(Target)) :-
    inspect_identity(Target).

run_pyperplan(Plan) :-
    % Clean up old solution file
    (exists_file('adversary_problem.pddl.soln') -> delete_file('adversary_problem.pddl.soln') ; true),
    % Use GBF search with FF heuristic for speed (suboptimal but fast)
    % pyperplan writes solution to <problem_file>.soln
    catch(shell('python3 -m pyperplan -s gbf -H hff adversary_domain.pddl adversary_problem.pddl > plan.txt'),_,fail),
    (exists_file('adversary_problem.pddl.soln') -> read_plan_file('adversary_problem.pddl.soln',Plan) ; fail).

% Dynamic PDDL Problem Generator
generate_pddl_problem(Target) :-
    open('adversary_problem.pddl', write, Stream),
    write(Stream, '(define (problem tufox-instance)\n'),
    write(Stream, '  (:domain tufox)\n'),
    write(Stream, '  (:objects\n'),
    write(Stream, '    detective player bunny1 bunny2 bunny3 bunny4 - agent\n'),
    rooms(Rooms),
    forall(member(R, Rooms), (
        normalize_room_name(R, RName),
        format(Stream, '    ~w', [RName])
    )),
    write(Stream, ' - room\n  )\n'),
    
    write(Stream, '  (:init\n'),
    write(Stream, '    (alive detective)\n'),
    write(Stream, '    (is-detective detective)\n'),
    
    % Write state for all alive agents
    forall((alive(A), A \= detective), (
        format(Stream, '    (alive ~w)\n', [A]),
        location(A, Loc),
        normalize_room_name(Loc, LocName),
        format('DEBUG: PDDL Gen - ~w at ~w (~w)~n', [A, Loc, LocName]), % DEBUG
        format(Stream, '    (at ~w ~w)\n', [A, LocName]),
        (inspected(A) -> true ; format(Stream, '    (not-inspected ~w)\n', [A]))
    )),
    
    location(detective, DetRoom),
    normalize_room_name(DetRoom, DetRoomName),
    format('DEBUG: PDDL Gen - Detective at ~w (~w)~n', [DetRoom, DetRoomName]), % DEBUG
    format(Stream, '    (at detective ~w)\n', [DetRoomName]),
    
    % Explicitly iterate over all rooms to generate connections
    forall(member(R, Rooms), (
        forall(path(R, Adj), (
            normalize_room_name(R, RN),
            normalize_room_name(Adj, AN),
            format(Stream, '    (connected ~w ~w)\n', [RN, AN])
        ))
    )),
    write(Stream, '  )\n'),
    
    format(Stream, '  (:goal (inspected ~w))\n', [Target]),
    write(Stream, ')\n'),
    close(Stream).

normalize_room_name(Atom, Normalized) :-
    atom_string(Atom, Str),
    string_lower(Str, Lower),
    split_string(Lower, " ", "_", Parts),
    atomic_list_concat(Parts, '_', Normalized).

read_plan_file(File, Plan) :-
    open(File,read,Stream),
    read_lines(Stream, Lines),
    close(Stream),
    % Use convlist to filter out lines that fail to parse (e.g. logs or empty lines)
    convlist(parse_action, Lines, Plan).

read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [L|Ls]) :-
    read_line_to_codes(Stream, Codes), atom_codes(A,Codes), atom_string(A,S), normalize_space(string(L),S),
    read_lines(Stream, Ls).

parse_action(Line, Action) :-
    % Remove parentheses if present
    (sub_string(Line, 0, 1, _, "(") -> 
        sub_string(Line, 1, _, 1, Content) 
    ; Content = Line),
    % Use split_string to handle multiple spaces robustly
    split_string(Content, " ", " ", StringTokens),
    maplist(atom_string, Tokens, StringTokens),
    (   parse_tokens(Tokens, Action)
    ->  true
    ;   format('DEBUG: Failed to parse action line: "~w" (Tokens: ~w)~n', [Line, Tokens]), fail
    ).

parse_tokens(['move', 'detective', _, ToRoomAtom], move(detective, Room)) :-
    room_from_plan_atom(ToRoomAtom, Room).
parse_tokens(['inspect', 'detective', TargetAtom, _], inspect(Target)) :-
    atom_string(Target, TargetAtom).

room_from_plan_atom(RoomAtom, Room) :-
    atom_string(RoomAtom, PlanString),
    normalize_token(PlanString, NormalizedPlan),
    rooms(Rooms),
    member(Room, Rooms),
    atom_string(Room, RoomString),
    normalize_token(RoomString, NormalizedPlan), !.

normalize_token(Str, Normalized) :-
    string_lower(Str, Lower),
    split_string(Lower, " _", " _", Parts),
    atomic_list_concat(Parts, '', Normalized).