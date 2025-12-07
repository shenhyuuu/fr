% TuFox text adventure game with AI rabbits and planner-driven detective

:- encoding(utf8).
:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(pyperplan_runner).

:- dynamic location/2.
:- dynamic alive/1.
:- dynamic role/2.
:- dynamic task/6.
:- dynamic cooldown/3.
:- dynamic inspected/1.
:- dynamic body/2.
%:- dynamic next_meeting/1.
:- dynamic round_counter/1.
%:- dynamic revealed_fox/1.
%:- dynamic vote/2.
:- dynamic alias/2.
%:- dynamic trust/3.
:- dynamic log_entry/4.
:- dynamic spoken_log/3.
:- dynamic history_statement/4.

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
    spec(collect_food,6),
    spec(fix_wiring,7),
    spec(clean_vent,6),
    spec(fix_chandelier,5),
    spec('Organize Ancient Scrolls',4)
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
    show_action_feedback,
    game_loop.

print_help :-
    nl,
    write('Commands:'),nl,
    write('  move(Direction).         % move up/down/left/right on the map'),nl,
    write('  look.                    % describe current room'),nl,
    write('  status.                  % show game status'),nl,
    write('  kill(Target).            % eliminate a rabbit in this room (cooldown 3)'),nl,
    write('  wait.                    % end your action'),nl,
    nl.

reset_world :-
    retractall(location(_,_)),
    retractall(alive(_)),
    retractall(task(_,_,_,_,_,_)),
    retractall(cooldown(_,_,_)),
    retractall(inspected(_)),
    retractall(body(_,_)),
    retractall(round_counter(_)),
    retractall(alias(_,_)),
    retractall(log_entry(_,_,_,_)),
    retractall(spoken_log(_,_,_)),
    retractall(history_statement(_,_,_,_)),
    assign_tasks_to_rooms,
    forall(characters(Cs), (forall(member(C,Cs), assertz(alive(C))))),
    assign_aliases,
    assign_initial_locations,
    assertz(cooldown(player,kill,0)),
    assertz(cooldown(detective,inspect,2)),
    assertz(round_counter(0)).

% initialize_trust :-
%     characters(Chars),
%     forall((member(A, Chars), member(B, Chars), A \= B), assertz(trust(A,B,100))).

assign_aliases :-
    characters(Chars),
    exclude(=(player), Chars, NonPlayer),
    length(NonPlayer, Count),
    numlist(1, Count, Numbers),
    maplist(number_alias, Numbers, Aliases),
    random_permutation(Aliases, Shuffled),
    pair_aliases(NonPlayer, Shuffled).

number_alias(N, AliasAtom) :-
    atomic_list_concat([rabbit, N], AliasAtom).

pair_aliases([], []).
pair_aliases([C|Cs], [A|As]) :-
    assertz(alias(C,A)),
    pair_aliases(Cs, As).

assign_initial_locations :-
    rooms(Rooms),
    random_member(PlayerRoom, Rooms),
    assertz(location(player, PlayerRoom)),
    repeat,
        random_member(DetectiveRoom, Rooms),
        DetectiveRoom \= PlayerRoom,
    !,
    assertz(location(detective, DetectiveRoom)),
    findall(Bunny, role(Bunny, rabbit), Bunnies),
    assign_bunny_locations(Rooms, Bunnies).

assign_bunny_locations(_, []) :- !.
assign_bunny_locations(Rooms, [Bunny|Rest]) :-
    random_member(Room, Rooms),
    assertz(location(Bunny, Room)),
    assign_bunny_locations(Rooms, Rest).

look :-
    location(player,Room),
    format('You are in the ~w.~n', [Room]),
    print_connected(Room),
    print_room_state(Room),
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
    (alive(player) -> write('You are alive.\n'); write('You are dead.\n')),
    list_tasks_status,
    display_map,
    show_cooldowns.

list_tasks_status :-
    findall(desc(T,Room,Status,Remaining,Occupant), task(T,Room,_,Remaining,Status,Occupant), Descs),
    forall(member(desc(T,Room,S,R,O), Descs), (
        visible_name(O, VisibleO),
        format('Task ~w in ~w: ~w (~w rounds left, occupant ~w)~n', [T,Room,S,R,VisibleO])
    )).

show_cooldowns :-
    forall(member(Skill, [kill]), (
        cooldown(player, Skill, V), format('Cooldown ~w: ~w~n',[Skill,V])
    )).

move(Direction) :-
    alive(player),
    location(player,From),
    (   adjacent_room(From, Direction, To)
    ->  retract(location(player,From)),
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

kill(Target) :-
    alive(player),
    cooldown(player,kill,CD),
    (CD > 0 -> format('Kill skill cooling down (~w).~n',[CD]), player_turn
    ; resolve_target(Target, Resolved),
      location(player,Room), location(Resolved,Room), alive(Resolved), Resolved \= player ->
        retract(alive(Resolved)),
        release_tasks_for(Resolved),
        assertz(body(Room,Resolved)),
        retract(cooldown(player,kill,_)),
        assertz(cooldown(player,kill,3)),
        visible_name(Resolved, Visible),
        format('You eliminated ~w!~n',[Visible]),
        player_done
    ; write('No valid target here.'),nl, player_turn).

player_done :-
    show_action_feedback,
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
visible_name(Char, Name) :-
    (alias(Char, Alias) -> Name = Alias ; Name = Char).

display_names(Chars, Names) :-
    maplist(visible_name, Chars, Names).

show_action_feedback :-
    print_other_characters_here,
    display_map.

print_other_characters_here :-
    location(player, Room),
    findall(C, (location(C,Room), alive(C), C \= player), Others),
    display_names(Others, VisibleOthers),
    (   VisibleOthers = []
    ->  write('No other characters here.'), nl
    ;   format('Other characters here: ~w~n', [VisibleOthers])
    ).

resolve_target(Input, Target) :-
    (alias(Target, Input) -> true ; Target = Input).

progress_task(Task,Room,Actor) :-
    retract(task(Task,Room,Need,Remaining,in_progress,Actor)),
    NewR is Remaining - 1,
    (NewR =< 0 -> (
        assertz(task(Task,Room,Need,0,complete,none)),
        format('Task ~w completed!~n',[Task])
    ) ; assertz(task(Task,Room,Need,NewR,in_progress,Actor))).

check_bodies(_Room) :-
    % Meeting mechanics are disabled; bodies no longer trigger discussions.
    true.

game_loop :-
    (check_victory -> true ;
        (alive(player) -> player_turn ; (ai_turns, game_loop))
    ).

check_victory :-
    ( inspected(player) -> rabbits_win, true
    ; \+ alive(player) -> rabbits_win, true
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
    record_round_logs,
    tick_world.

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
    ( cooldown(detective,inspect,CD),
      CD =:= 0,
      findall(T, (location(T,Room), alive(T), T \= detective, \+ inspected(T)), Targets),
      Targets \= [] ->
        Targets = [Target|_],
        inspect_identity(Target)
    ; execute_plan_step(detective)
    ).

ai_act_logic(AI) :-
    location(AI,_Room),
    attempt_task(AI).

inspect_identity(Target) :-
    role(Target, Role),
    assertz(inspected(Target)),
    retract(cooldown(detective,inspect,_)),
    assertz(cooldown(detective,inspect,2)),
    % Meeting mechanics disabled; revealing a fox no longer triggers a meeting.
    true,
    visible_name(Target, VisibleTarget),
    format('An inspection reveals ~w is ~w.~n',[VisibleTarget,Role]),
    (Target == player -> rabbits_win, halt ; true).

attempt_task(AI) :-
    (choose_task(AI, TargetTask, TargetRoom) ->
        location(AI,Room),
        (Room == TargetRoom ->
            (task(TargetTask,Room,_,_,available,none) ->
                retract(task(TargetTask,Room,N,R,available,none)),
                assertz(task(TargetTask,Room,N,R,in_progress,AI))
            ; progress_task_if_owner(AI,TargetTask,Room)
            )
        ; move_ai_toward(AI,TargetRoom)
        )
    ; true).

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
        format('~w moves to ~w.~n',[VisibleAI,Next])
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

/* Meeting, voting, and trust mechanics are temporarily disabled.

resolve_meeting :-
    write('--- Meeting called ---'),nl,
    clear_bodies,
    discussion_phase,
    validate_statements,
    run_votes,
    update_meeting_timer,
    clear_bodies,
    !.

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
      ->  write('你选择保持沉默。'), nl
      ;   nth1(Choice, SafeEntries, entry(R,Room,Others))
      ->  register_statement(player, R, Room, Others)
      ;   write('无效选择，保持沉默。'), nl
      )
    ).
speak_from_log(Char) :-
    findall(entry(R,Room,Others), (log_entry(Char,R,Room,Others), \+ spoken_log(Char,R,Room)), Entries),
    (Entries = [] -> true
    ; random_member(entry(R,Room,Others), Entries),
      register_statement(Char, R, Room, Others)
    ).

present_log_choices(Entries, Choice) :-
    write('请选择一条日志发言 (输入编号后跟句点，0保持沉默):'), nl,
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
    format(string(Text), '~w在第~w轮在~w，该地方有~w。', [VisibleChar, Round, Room, OthersText]).

validate_statements :-
    findall(stmt(Speaker,R,Room,Others), history_statement(Speaker,R,Room,Others), Statements),
    forall((alive(AI), AI \= player), validate_against_logs(AI, Statements)).

validate_against_logs(AI, Statements) :-
    visible_name(AI, VisibleAI),
    format('~w正在校验发言:~n', [VisibleAI]),
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
    format_statement(Speaker, Round, Room, StatedOthers, StatementText),
    visible_name(AI, VisibleAI),
    visible_name(Speaker, VisibleSpeaker),
    format_log_snapshot(Round, Room, Logged, LoggedText),
    trust_outcome_text(Outcome, DeltaText),
    format('  ~w的发言: ~w~n', [VisibleSpeaker, StatementText]),
    format('  ~w的日志: ~w~n', [VisibleAI, LoggedText]),
    format('  评估: ~w (信任 ~w -> ~w)~n', [DeltaText, Current, New]).

format_log_snapshot(_, _, none, '无对应日志').
format_log_snapshot(Round, Room, Others, Text) :-
    display_names(Others, VisibleOthers),
    atomic_list_concat(VisibleOthers, ',', OthersText),
    format(string(Text), '第~w轮在~w看到~w', [Round, Room, OthersText]).

trust_outcome_text(match, '记录匹配，信任+10').
trust_outcome_text(conflict, '记录冲突，信任-10').
trust_outcome_text(missing, '未记录，信任不变').

run_votes :-
    retractall(vote(_,_)),
    (alive(player) -> player_vote ; true),
    ai_votes,
    tally_votes.

player_vote :-
    write('Cast your vote (atom ending with period). alive characters: '),
    alive_rabbits(Rs), display_names(Rs, Visible), write(Visible),nl,
    read(V),
    (resolve_target(V, Target), alive(Target), Target \= player -> assertz(vote(player,Target)) ; write('Abstain.'),nl).

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
    (   unique_lowest_trust_target(AI, Candidates, Vote)
    ->  record_vote(AI, Vote)
    ;   visible_name(AI, VisibleAI),
        format('~w弃权。~n', [VisibleAI])
    ).

unique_lowest_trust_target(AI, Candidates, Vote) :-
    findall(score(T,Score), (member(T, Candidates), (trust(AI, T, Score) -> true ; Score = 100)), Scores),
    Scores \= [],
    findall(Sc, member(score(_,Sc), Scores), AllScores),
    min_list(AllScores, Min),
    include(matches_score(Min), Scores, LowestScores),
    findall(T, member(score(T,_), LowestScores), LowestTargets),
    list_to_set(LowestTargets, UniqueLowest),
    UniqueLowest = [Vote].

record_vote(AI, Vote) :-
    assertz(vote(AI, Vote)),
    visible_name(AI, VisibleAI),
    visible_name(Vote, VisibleVote),
    format('~w votes for ~w.~n',[VisibleAI,VisibleVote]).

alive_targets_for_vote(AI, Candidates) :-
    findall(T, (alive(T), T \= AI), Candidates).

random_vote(Candidates, Vote) :-
    Candidates \= [],
    random_member(Vote, Candidates).

select_vote_by_trust(AI, Candidates, Vote) :-
    findall(score(T,Score), (member(T, Candidates), trust(AI, T, Score)), Scores),
    (   Scores = []
    ->  random_vote(Candidates, Vote)
    ;   findall(Sc, member(score(_,Sc), Scores), AllScores),
        min_list(AllScores, Min),
        include(matches_score(Min), Scores, Lowest),
        random_member(score(Vote,_), Lowest)
    ).

matches_score(Min, score(_,Score)) :- Score =:= Min.

tally_votes :-
    findall(Target, vote(_,Target), Targets),
    count_targets(Targets,Counts),
    (Counts = [] -> write('No votes.'),nl ;
        keysort(Counts,Sorted), reverse(Sorted, [Count-Target|_]),
        vote_threshold(Threshold),
        (   Count >= Threshold
        ->  eliminate(Target)
        ;   format('投票未通过，需要至少~w票。~n', [Threshold])
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
    NM is R + 3,
    retractall(next_meeting(_)),
    assertz(next_meeting(NM)),
    retractall(vote(_,_)),
    !.

clear_bodies :-
    retractall(body(_,_)).
*/

% world tick: cooldown reductions and task progress persistence

tick_world :-
    decrement_cooldowns,
    round_counter(R),
    R1 is R+1,
    retract(round_counter(_)),
    assertz(round_counter(R1)),
    print_round(R1).

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
    Cells = [FirstCell|_],
    length(FirstCell, CellHeight),
    numlist(1, CellHeight, Indexes),
    maplist(row_line(Cells, Width), Indexes, Lines).

row_line(Cells, Width, Index, Line) :-
    maplist(nth1(Index), Cells, Texts),
    maplist(pad_cell(Width), Texts, Padded),
    atomic_list_concat(Padded, '|', Body),
    atomic_list_concat(['|', Body, '|'], Line).

pad_cell(Width, Text, Padded) :-
    string_length(Text, Len),
    Pad is Width - Len,
    repeat_char(Pad, ' ', Spaces),
    atomic_list_concat([' ', Text, Spaces, ' '], Padded).

repeat_char(N, Char, String) :-
    length(Chars, N),
    maplist(=(Char), Chars),
    atomics_to_string(Chars, '', String).

cell_display(Room, [RoomLine, TaskLine]) :-
    room_label(Room, Label),
    player_hint(Room, PH),
    task_hint(Room, TH),
    include(\=(""), [PH, TH], Hints),
    (   Hints = []
    ->  RoomLine = Label
    ;   atomics_to_string([Label|Hints], ' ', RoomLine)
    ),
    room_tasks_line(Room, TaskLine).

room_label(Room, Label) :- atom_string(Room, Label).

player_hint(Room, "(You are here)") :- location(player, Room), !.
player_hint(_, "").

task_hint(Room, "(Finished)") :- task(_,Room,_,_,complete,_), !.
task_hint(_, "").

room_tasks_line(Room, Line) :-
    findall(Display,
        ( task(TaskName,Room,_,_,Status,_),
          task_status_label(Status, StatusLabel),
          format(string(Display), "~w~w", [TaskName, StatusLabel])
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
    forall(task(T,R,N,Rem,in_progress,Occ), (
        (   alive(Occ)
        ->  progress_task(T,R,Occ)
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
    ( build_detective_plan(Plan) -> true ; default_plan(Plan)).

default_plan([
    move(detective,'Hall'),
    move(detective,'Kitchen'),
    inspect(player)
]).

apply_action(_, move(detective,Room)) :-
    move_ai_toward(detective,Room).
apply_action(_, move(_,_,Room)) :-
    move_ai_toward(detective,Room).
apply_action(_, inspect(Target)) :-
    inspect_identity(Target).
apply_action(_, inspect(_,Target,_)) :-
    inspect_identity(Target).
apply_action(_, none).

build_detective_plan(Plan) :-
    generate_detective_problem(ProblemFile),
    absolute_file_name('adversary_domain.pddl', DomainFile, [access(read)]),
    pyperplan_executable(Exe),
    catch(run_pyperplan_soln(Exe, DomainFile, ProblemFile, RawActions), _, fail),
    maplist(convert_plan_action, RawActions, Converted),
    exclude(=(none), Converted, Plan),
    Plan \= [],
    cleanup_plan_artifacts(ProblemFile).
build_detective_plan(_) :-
    cleanup_plan_artifacts(_),
    fail.

pyperplan_executable(Exe) :-
    (current_prolog_flag(windows, true) -> Exe = 'pyperplan.exe' ; Exe = 'python3').

convert_plan_action(move(_,_,To), move(detective,Room)) :-
    resolve_room_token(To, Room), !.
convert_plan_action(move(_,To), move(detective,Room)) :-
    resolve_room_token(To, Room), !.
convert_plan_action(inspect(_,Target,_), inspect(Agent)) :-
    resolve_agent_token(Target, Agent), !.
convert_plan_action(inspect(Target), inspect(Agent)) :-
    resolve_agent_token(Target, Agent), !.
convert_plan_action(_, none).

resolve_room_token(Token, Room) :-
    normalize_token_atom(Token, Normalized),
    rooms(Rooms),
    member(Room, Rooms),
    atom_string(Room, RoomStr),
    normalize_token(RoomStr, Normalized), !.

resolve_agent_token(Token, Agent) :-
    normalize_token_atom(Token, Normalized),
    characters(Agents),
    member(Agent, Agents),
    atom_string(Agent, AgentStr),
    normalize_token(AgentStr, Normalized), !.

normalize_token_atom(Token, Normalized) :-
    atom_string(Token, Str),
    normalize_token(Str, Normalized).

normalize_token(Str, Normalized) :-
    string_lower(Str, Lower),
    split_string(Lower, " _", " _", Parts),
    atomic_list_concat(Parts, '', Normalized).

generate_detective_problem(ProblemFile) :-
    setup_call_cleanup(
        tmp_file_stream(text, ProblemFile, Stream),
        (
            gather_agents(Agents),
            gather_rooms(Rooms),
            findall(conn(A,B), path(A,B), Connections0),
            sort(Connections0, Connections),
            detective_init(DetectiveAt),
            agents_init(Agents, AgentInit),
            inspected_init(InspectedFacts),
            goal_inspections(Agents, Goals),
            format(Stream, "(define (problem tufox-instance)~n  (:domain tufox)~n  (:objects~n", []),
            write_agent_objects(Stream, Agents),
            write_room_objects(Stream, Rooms),
            format(Stream, "  )~n  (:init~n", []),
            write_alive(Stream, Agents),
            write_at(Stream, DetectiveAt, AgentInit),
            write_connections(Stream, Connections),
            write_inspected(Stream, InspectedFacts),
            format(Stream, "    (fox player)~n  )~n", []),
            write_goals(Stream, Goals),
            format(Stream, ")~n", [])
        ),
        close(Stream)
    ).

cleanup_plan_artifacts(ProblemFile) :-
    (var(ProblemFile) -> true ; (exists_file(ProblemFile) -> delete_file(ProblemFile) ; true)),
    (var(ProblemFile) -> SolnFile = _ ; problem_soln_file(ProblemFile, SolnFile)),
    (nonvar(SolnFile), exists_file(SolnFile) -> delete_file(SolnFile) ; true).

gather_agents(Agents) :-
    findall(A, (alive(A), A \= detective), Others),
    append([detective], Others, Agents).

gather_rooms(Rooms) :-
    rooms(Raw),
    maplist(pddl_atom, Raw, Rooms).

detective_init(at(detective, Room)) :-
    location(detective, Room).

agents_init([], []).
agents_init([detective|Rest], Facts) :-
    agents_init(Rest, Facts).
agents_init([Agent|Rest], [at(Agent, Room)|Facts]) :-
    location(Agent, Room),
    agents_init(Rest, Facts).

inspected_init(InspectedFacts) :-
    findall(inspected(A), inspected(A), InspectedFacts).

goal_inspections(Agents, Goals) :-
    exclude(=(detective), Agents, Targets),
    maplist(goal_inspected, Targets, Goals).

goal_inspected(Agent, inspected(Agent)).

write_agent_objects(Stream, Agents) :-
    maplist(pddl_atom, Agents, Tokens),
    atomic_list_concat(Tokens, ' ', Line),
    format(Stream, "    ~w - agent~n", [Line]).

write_room_objects(Stream, Rooms) :-
    atomic_list_concat(Rooms, ' ', Line),
    format(Stream, "    ~w - room~n", [Line]).

write_alive(Stream, Agents) :-
    forall(member(A, Agents), (pddl_atom(A, Token), format(Stream, "    (alive ~w)~n", [Token]))).

write_at(Stream, at(Detective, DetRoom), AgentInit) :-
    pddl_atom(Detective, DTok),
    pddl_atom(DetRoom, DRTok),
    format(Stream, "    (at ~w ~w)~n", [DTok, DRTok]),
    forall(member(at(Agent, Room), AgentInit), (
        pddl_atom(Agent, ATok),
        pddl_atom(Room, RTok),
        format(Stream, "    (at ~w ~w)~n", [ATok, RTok])
    )).

write_connections(Stream, Connections) :-
    forall(member(conn(A,B), Connections), (
        pddl_atom(A, ATok),
        pddl_atom(B, BTok),
        format(Stream, "    (connected ~w ~w)~n", [ATok, BTok])
    )).

write_inspected(Stream, Facts) :-
    forall(member(inspected(A), Facts), (
        pddl_atom(A, Tok),
        format(Stream, "    (inspected ~w)~n", [Tok])
    )).

write_goals(Stream, Goals) :-
    format(Stream, "  (:goal (and~n", []),
    forall(member(inspected(A), Goals), (
        pddl_atom(A, Tok),
        format(Stream, "    (inspected ~w)~n", [Tok])
    )),
    format(Stream, "  ))~n", []).

problem_soln_file(ProblemFile, SolnFile) :-
    atom_string(ProblemFile, PStr),
    string_concat(PStr, '.soln', SStr),
    atom_string(SolnFile, SStr).

pddl_atom(Source, Token) :-
    atom(Source),
    atom_string(Source, Str),
    normalize_token(Str, Lower),
    split_string(Lower, " ", " ", Parts),
    atomic_list_concat(Parts, '_', Token).
