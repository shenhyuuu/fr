% TuFox text adventure game with AI rabbits and planner-driven detective

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
    spec(fix_chandelier,3),
    spec('Organize Ancient Scrolls',2)
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
    format('\nWelcome to TuFox!\n'),
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
    assign_tasks_to_rooms,
    forall(characters(Cs), (forall(member(C,Cs), assertz(alive(C))))),
    assign_aliases,
    assign_initial_locations,
    assertz(cooldown(player,kill,0)),
    assertz(cooldown(detective,inspect,2)),
    assertz(next_meeting(3)),
    assertz(round_counter(0)).

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

resolve_target(Input, Target) :-
    (alias(Target, Input) -> true ; Target = Input).

progress_task(Task,Room,Actor) :-
    retract(task(Task,Room,Need,Remaining,in_progress,Actor)),
    NewR is Remaining - 1,
    (NewR =< 0 -> (
        assertz(task(Task,Room,Need,0,complete,none)),
        format('Task ~w completed!~n',[Task])
    ) ; assertz(task(Task,Room,Need,NewR,in_progress,Actor))).

check_bodies(Room) :-
    (   body(Room,_)
    ->  write('You spot a body here! A meeting will be triggered.'),nl,
        resolve_meeting
    ;   true
    ).

game_loop :-
    (check_victory -> true ;
        round_counter(R),
        next_meeting(NM),
        (R >= NM -> resolve_meeting ; true),
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
    tick_world.

ai_act(AI) :- % dispatch for every AI agent
    ai_act_logic(AI).

ai_act_logic(AI) :-
    \+ alive(AI), !.

ai_act_logic(detective) :-
    location(detective,Room),
    ( body(Room,_) ->
        resolve_meeting
    ; ( cooldown(detective,inspect,CD),
        CD =:= 0,
        findall(T, (location(T,Room), alive(T), T \= detective, \+ inspected(T)), Targets),
        Targets \= [] ->
            Targets = [Target|_],
            inspect_identity(Target)
      ; execute_plan_step(detective)
      )
    ).

ai_act_logic(AI) :-
    location(AI,Room),
    (body(Room,_) ->
        resolve_meeting
    ; attempt_task(AI)).

inspect_identity(Target) :-
    role(Target, Role),
    assertz(inspected(Target)),
    retract(cooldown(detective,inspect,_)),
    assertz(cooldown(detective,inspect,2)),
    (Role == fox -> assertz(revealed_fox(Target)), resolve_meeting ; true),
    visible_name(Target, VisibleTarget),
    format('An inspection reveals ~w is ~w.~n',[VisibleTarget,Role]).

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

resolve_meeting :-
    write('--- Meeting called ---'),nl,
    clear_bodies,
    run_votes,
    update_meeting_timer,
    clear_bodies,
    !.

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
        (revealed_fox(Fox), alive(Fox) -> Vote = Fox
        ; random_vote(Candidates, Vote))
    ; random_vote(Candidates, Vote)
    ),
    assertz(vote(AI, Vote)),
    visible_name(AI, VisibleAI),
    visible_name(Vote, VisibleVote),
    format('~w votes for ~w.~n',[VisibleAI,VisibleVote]).

alive_targets_for_vote(AI, Candidates) :-
    findall(T, (alive(T), T \= AI), Candidates).

random_vote(Candidates, Vote) :-
    Candidates \= [],
    random_member(Vote, Candidates).

tally_votes :-
    findall(Target, vote(_,Target), Targets),
    count_targets(Targets,Counts),
    (Counts = [] -> write('No votes.'),nl ;
        keysort(Counts,Sorted), reverse(Sorted, [_-Target|_]),
        eliminate(Target)).

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
    (run_pyperplan(Plan) -> true ; default_plan(Plan)).

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
    catch(shell('python3 -m pyperplan adversary_domain.pddl adversary_problem.pddl > plan.txt'),_,fail),
    (exists_file('plan.txt') -> read_plan_file('plan.txt',Plan) ; fail).

read_plan_file(File, Plan) :-
    open(File,read,Stream),
    read_lines(Stream, Lines),
    close(Stream),
    maplist(parse_action, Lines, Plan).

read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [L|Ls]) :-
    read_line_to_codes(Stream, Codes), atom_codes(A,Codes), atom_string(A,S), normalize_space(string(L),S),
    read_lines(Stream, Ls).

parse_action(Line, move(detective,Room)) :-
    sub_atom(Line,_,_,_, 'move'),
    atomic_list_concat(['(', 'move', detective, RoomAtom, ')' ], ' ', Line),
    room_from_plan_atom(RoomAtom, Room).
parse_action(_, inspect(player)).

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
