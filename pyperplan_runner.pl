% Pyperplan runner that captures planner output into a .soln file and parses
% it back into Prolog terms like move/3 or inspect/3.

:- module(pyperplan_runner,
          [ run_pyperplan_soln/4        % +Exe,+DomainPDDL,+ProblemPDDL,-Actions
          , read_plan_file/2            % +SolnFile,-Actions
          ]).

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(pcre)).

%% run_pyperplan_soln(+Exe, +DomainPDDL, +ProblemPDDL, -Actions:list) is det.
%  Calls pyperplan and captures stdout into ProblemPDDL+'.soln'.
%  After the process exits, reads and parses that .soln file.
run_pyperplan_soln(Exe, Domain, Problem, Actions) :-
    soln_path_(Problem, SolnFile),
    pyperplan_args(Exe, Domain, Problem, Args),
    setup_call_cleanup(
        process_create(Exe, Args,
                       [ stdout(file(SolnFile)),
                         stderr(pipe(Err)),
                         process(PID)
                       ]),
        read_string(Err, _, Stderr),
        close(Err)
    ),
    process_wait(PID, exit(Status)),
    ( Status = exit(0)
    -> true
    ;  format(user_error, "~s", [Stderr]),
       throw(error(pyperplan_failed(Status), _))
    ),
    ( exists_file(SolnFile)
    -> read_plan_file(SolnFile, Actions)
    ;  throw(error(existence_error(file, SolnFile), _))
    ).

pyperplan_args(Exe, Domain, Problem, ['-m','pyperplan',Domain,Problem]) :-
    memberchk(Exe, ['python3', python3]), !.
pyperplan_args(_Exe, Domain, Problem, [Domain, Problem]).

%% read_plan_file(+SolnFile, -Actions:list) is det.
%  Reads a pyperplan .soln file and parses action lines like:
%     "0: (MOVE DETECTIVE HALL BALCONY) [1]"
%  into Prolog terms:
%     move(detective,hall,balcony).
read_plan_file(SolnFile, Actions) :-
    ( exists_file(SolnFile)
    -> setup_call_cleanup(
           open(SolnFile, read, In, [encoding(utf8)]),
           read_string(In, _, Content),
           close(In)
       ),
       parse_plan_text_(Content, Actions)
    ;  throw(error(existence_error(file, SolnFile), _))
    ).

% --- helpers ---------------------------------------------------------------

soln_path_(Problem, SolnFile) :-
    atom_string(Problem, PStr),
    string_concat(PStr, '.soln', SStr),
    atom_string(SolnFile, SStr).

parse_plan_text_(Text, Actions) :-
    split_string(Text, "\n", "\r", Lines0),
    include(has_paren_, Lines0, Lines),
    maplist(line_to_action_term_, Lines, Actions).

has_paren_(Line) :- sub_string(Line, _, _, _, "(").

% Extract the first "(...)" group and convert to a functor:
% "(move detective hall balcony)" -> move(detective,hall,balcony)
line_to_action_term_(Line, Term) :-
    ( re_matchsub("\\(([^\\)]+)\\)", Line, Dict, [])
    -> Str = Dict.1,
       string_lower(Str, Low),
       split_string(Low, " ", " \t", Parts),
       maplist(atom_string, Atoms, Parts),
       Atoms = [Name|Args],
       Term =.. [Name|Args]
    ;  atom_string(Term, Line)   % fallback: leave line as atom
    ).

