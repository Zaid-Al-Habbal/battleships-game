% battleship.pl

% Dynamic predicates for board and ships
:- dynamic cell/3.      % cell(Row, Col, State) - State: guess_ship, guess_water, empty
:- dynamic solution_cell/3. % solution_cell(Row, Col, State) - State: ship, water
:- dynamic ship/4.      % ship(Type, Row, Col, Orientation)
:- dynamic row_clue/2.  % row_clue(Row, Count)
:- dynamic col_clue/2.  % col_clue(Col, Count)
:- dynamic hint_cell/3. % hint_cell(Row, Col, State) - State: ship, water
:- dynamic ship_display/4. % ship_display(Row, Col, Type, Symbol) - for visual representation

% Ship types and sizes
ship_type(battleship, 4).
ship_type(cruiser, 3).
ship_type(destroyer, 2).
ship_type(submarine, 1).

% Ship symbols for different orientations and positions
% Battleship (4 cells)
ship_symbol(battleship, horizontal, 1, '◄').  % Left end
ship_symbol(battleship, horizontal, 2, '■').  % Middle
ship_symbol(battleship, horizontal, 3, '■').  % Middle
ship_symbol(battleship, horizontal, 4, '►').  % Right end

ship_symbol(battleship, vertical, 1, '▲').    % Top end
ship_symbol(battleship, vertical, 2, '■').    % Middle
ship_symbol(battleship, vertical, 3, '■').    % Middle
ship_symbol(battleship, vertical, 4, '▼').    % Bottom end

% Cruiser (3 cells)
ship_symbol(cruiser, horizontal, 1, '◄').     % Left end
ship_symbol(cruiser, horizontal, 2, '■').     % Middle
ship_symbol(cruiser, horizontal, 3, '►').     % Right end

ship_symbol(cruiser, vertical, 1, '▲').       % Top end
ship_symbol(cruiser, vertical, 2, '■').       % Middle
ship_symbol(cruiser, vertical, 3, '▼').       % Bottom end

% Destroyer (2 cells)
ship_symbol(destroyer, horizontal, 1, '◄').   % Left end
ship_symbol(destroyer, horizontal, 2, '►').   % Right end

ship_symbol(destroyer, vertical, 1, '▲').     % Top end
ship_symbol(destroyer, vertical, 2, '▼').     % Bottom end

% Submarine (1 cell)
ship_symbol(submarine, horizontal, 1, '●').   % Single cell
ship_symbol(submarine, vertical, 1, '●').     % Single cell

% Fleet for each board size
fleet(6, [cruiser-1, destroyer-2, submarine-2]).
fleet(8, [battleship-1, cruiser-2, destroyer-3, submarine-4]).
fleet(10, [battleship-1, cruiser-2, destroyer-3, submarine-4]).

% Start the game
start_battleship :-
    get_time(Time), X is floor(Time), set_random(seed(X)),
    write('Choose board size (6, 8, 10): '), read(Size),
    (fleet(Size, Fleet) -> true ; (write('Invalid size.'), nl, fail)),
    init_board(Size),
    place_fleet(Fleet, Size),
    compute_clues(Size),
    add_random_hints(Size, 3),
    init_guess_board(Size),
    print_partial_board(Size),
    print_clues(Size),
    play(Size).

% Restart the game with the same board size
restart_game(Size) :-
    fleet(Size, Fleet),
    get_time(Time), X is floor(Time), set_random(seed(X)),
    init_board(Size),
    place_fleet(Fleet, Size),
    compute_clues(Size),
    add_random_hints(Size, 3),
    init_guess_board(Size),
    write('Game restarted!'), nl,
    print_partial_board(Size),
    print_clues(Size),
    play(Size).

% Initialize the board with empty cells
init_board(Size) :-
    retractall(cell(_,_,_)),
    retractall(solution_cell(_,_,_)),
    retractall(hint_cell(_,_,_)),
    retractall(ship_display(_,_,_,_)),
    forall(between(1, Size, Row),
        forall(between(1, Size, Col),
            (assertz(cell(Row, Col, empty)), assertz(solution_cell(Row, Col, water)))
        )
    ).

% Helper: create an empty board as a list of (Row,Col,water)
empty_board(Size, Board) :-
    findall((Row,Col,water), (between(1,Size,Row), between(1,Size,Col)), Board).

% Main entry: place the fleet and commit the solution
place_fleet(Fleet, Size) :-
    empty_board(Size, Board),
    place_fleet_bt(Fleet, Size, Board, FinalBoard, [], Ships),
    retractall(solution_cell(_,_,_)),
    retractall(ship(_,_,_,_)),
    retractall(ship_display(_,_,_,_)),
    commit_solution(FinalBoard),
    commit_ships(Ships),
    create_ship_display(Ships).

% Create visual display for ships
create_ship_display([]).
create_ship_display([(Type, Row, Col, Orientation)|Rest]) :-
    ship_type(Type, Len),
    ship_cells(Row, Col, Orientation, Len, Cells),
    create_ship_symbols(Cells, Type, Orientation, 1),
    create_ship_display(Rest).

create_ship_symbols([], _, _, _).
create_ship_symbols([(R, C)|Rest], Type, Orientation, Position) :-
    ship_symbol(Type, Orientation, Position, Symbol),
    assertz(ship_display(R, C, Type, Symbol)),
    NextPos is Position + 1,
    create_ship_symbols(Rest, Type, Orientation, NextPos).

% Recursive fleet placement
place_fleet_bt([], _, Board, Board, Ships, Ships).
place_fleet_bt([Type-N|Rest], Size, BoardIn, BoardOut, ShipsIn, ShipsOut) :-
    place_n_ships_bt(Type, N, Size, BoardIn, BoardMid, ShipsIn, ShipsMid),
    place_fleet_bt(Rest, Size, BoardMid, BoardOut, ShipsMid, ShipsOut).

place_n_ships_bt(_, 0, _, Board, Board, Ships, Ships).
place_n_ships_bt(Type, N, Size, BoardIn, BoardOut, ShipsIn, ShipsOut) :-
    ship_type(Type, Len),
    try_place_ship_bt(Type, Len, Size, BoardIn, BoardMid, ShipsIn, ShipsMid),
    N1 is N-1,
    place_n_ships_bt(Type, N1, Size, BoardMid, BoardOut, ShipsMid, ShipsOut).

try_place_ship_bt(Type, Len, Size, BoardIn, BoardOut, ShipsIn, [(Type,Row,Col,Orient)|ShipsIn]) :-
    % Try extensive random placement with better bounds
    attempt_random_placement(Len, Size, BoardIn, BoardOut, Row, Col, Orient, 200).

try_place_ship_bt(Type, Len, Size, BoardIn, BoardOut, ShipsIn, [(Type,Row,Col,Orient)|ShipsIn]) :-
    % Fall back to systematic search if random fails
    (Orient = horizontal ; Orient = vertical),
    between(1, Size, Row),
    between(1, Size, Col),
    ship_cells(Row, Col, Orient, Len, Cells),
    valid_ship_placement(Cells, Size, BoardIn),
    mark_ship_cells(Cells, BoardIn, BoardOut),
    !.

% Helper predicate for random placement attempts
attempt_random_placement(Len, Size, BoardIn, BoardOut, Row, Col, Orient, MaxAttempts) :-
    between(1, MaxAttempts, _),
    random_between(0, 1, OrientChoice),
    (OrientChoice = 0 -> Orient = horizontal ; Orient = vertical),
    
    % Calculate valid bounds based on orientation and ship length
    (Orient = horizontal -> 
        (MaxCol is Size - Len + 1,
         MaxCol > 0,
         random_between(1, MaxCol, Col),
         random_between(1, Size, Row))
    ;
        (MaxRow is Size - Len + 1,
         MaxRow > 0,
         random_between(1, MaxRow, Row),
         random_between(1, Size, Col))
    ),
    
    ship_cells(Row, Col, Orient, Len, Cells),
    valid_ship_placement(Cells, Size, BoardIn),
    mark_ship_cells(Cells, BoardIn, BoardOut),
    !.

valid_ship_placement([], _, _).
valid_ship_placement([(R,C)|Rest], Size, Board) :-
    R >= 1, R =< Size, C >= 1, C =< Size,
    member((R,C,water), Board),
    no_adjacent_ship_bt(R, C, Size, Board),
    valid_ship_placement(Rest, Size, Board).

no_adjacent_ship_bt(Row, Col, Size, Board) :-
    forall(between(-1, 1, DR),
        forall(between(-1, 1, DC),
            (   (DR = 0, DC = 0) -> true
            ;   NR is Row+DR, NC is Col+DC,
                (   (NR < 1 ; NR > Size ; NC < 1 ; NC > Size) -> true
                ;   \+ member((NR,NC,ship), Board)
                )
            )
        )
    ).

mark_ship_cells([], Board, Board).
mark_ship_cells([(R,C)|Rest], BoardIn, BoardOut) :-
    select((R,C,water), BoardIn, BoardMid),
    mark_ship_cells(Rest, [(R,C,ship)|BoardMid], BoardOut).

commit_solution([]).
commit_solution([(R,C,water)|Rest]) :-
    assertz(solution_cell(R, C, water)),
    commit_solution(Rest).
commit_solution([(R,C,ship)|Rest]) :-
    assertz(solution_cell(R, C, ship)),
    commit_solution(Rest).

commit_ships([]).
commit_ships([(Type,Row,Col,Orient)|Rest]) :-
    assertz(ship(Type, Row, Col, Orient)),
    commit_ships(Rest).

% Compute clues for each row and column
compute_clues(Size) :-
    retractall(row_clue(_,_)), retractall(col_clue(_,_)),
    forall(between(1, Size, I), (
        findall(_, solution_cell(I, _, ship), RowShips),
        length(RowShips, RowCount),
        assertz(row_clue(I, RowCount)),
        findall(_, solution_cell(_, I, ship), ColShips),
        length(ColShips, ColCount),
        assertz(col_clue(I, ColCount))
    )).

% Initialize the guess board (all unknown)
init_guess_board(Size) :-
    forall(between(1, Size, Row),
        forall(between(1, Size, Col),
            (retractall(cell(Row, Col, guess_ship)), retractall(cell(Row, Col, guess_water)), true)
        )
    ).

% Print the partial board (hide ships, show guesses)
print_partial_board(Size) :-
    nl, write('Your Board:'), nl,
    write('     '),  % Extra space to account for double-digit rows
    forall(between(1, Size, Col), (
        (Col < 10 -> format('~w ', [Col]) ; format('~w', [Col]))
    )),
    nl,
    forall(between(1, Size, Row), (
        (Row < 10 -> format('~w  | ', [Row]) ; format('~w | ', [Row])),
        forall(between(1, Size, Col), (
            (hint_cell(Row, Col, ship) -> 
                (ship_display(Row, Col, _, Symbol) -> write(Symbol) ; write('O')), write(' ') ;
             hint_cell(Row, Col, water) -> write('~ ') ;
             cell(Row, Col, guess_ship) -> write('O ') ;
             cell(Row, Col, guess_water) -> write('~ ') ;
             write('. '))
        )),
        row_clue(Row, RC), format('| ~w', [RC]),
        nl
    )),
    write('     '),  % Extra space to match the header
    forall(between(1, Size, Col), (
        col_clue(Col, CC), 
        (Col < 10 -> format('~w ', [CC]) ; format('~w', [CC]))
    )),
    nl, nl.

% Print clues (now integrated into board, so this can be a no-op or just print a note)
print_clues(_Size) :- true.

% User play loop
play(Size) :-
    (game_won(Size) ->
        write('Congratulations! You solved the puzzle!'), nl,
        print_solution(Size)
    ;
        write('Enter your move as three inputs, each followed by a period (e.g., 2. 3. ship. or 4. 5. water.).'), nl,
        write('Type solution. for any input to print the solution and exit.'), nl,
        write('Type restart. for any input to restart the game.'), nl,
        write('Example: To mark row 2, column 3 as a ship, enter:'), nl,
        write('  2. 3. ship.'), nl,
        write('Or to mark row 4, column 5 as water, enter:'), nl,
        write('  4. 5. water.'), nl,
        write('To fill an entire column with water, enter:'), nl,
        Temp is Size+1,
        format('  ~w. C. water.  (where C is the column number)~n', [Temp]),
        write('To fill an entire row with water, enter:'), nl,
        format('  R. ~w. water.  (where R is the row number)~n', [Temp]),
        write('To exit the game, enter exit. for any input.'), nl,
        read(Row), (Row == exit -> write('Exiting game. Goodbye!'), nl ;
                      Row == solution -> print_solution(Size) ;
                      Row == restart -> restart_game(Size) ;
        read(Col), (Col == exit -> write('Exiting game. Goodbye!'), nl ;
                      Col == solution -> print_solution(Size) ;
                      Col == restart -> restart_game(Size) ;
        read(State), (State == exit -> write('Exiting game. Goodbye!'), nl ;
                        State == solution -> print_solution(Size) ;
                        State == restart -> restart_game(Size) ;
        (Row =:= Size+1, integer(Col), Col >= 1, Col =< Size, State == water) ->
            fill_column_with_water(Size, Col),
            print_partial_board(Size),
            print_clues(Size),
            play(Size)
        ; (Col =:= Size+1, integer(Row), Row >= 1, Row =< Size, State == water) ->
            fill_row_with_water(Size, Row),
            print_partial_board(Size),
            print_clues(Size),
            play(Size)
        ; (valid_move(Row, Col, State, Size) ->
            make_move(Row, Col, State),
            print_partial_board(Size),
            print_clues(Size),
            play(Size)
        ;
            write('Invalid move. Try again.'), nl,
            print_partial_board(Size),
            print_clues(Size),
            play(Size)
        ))))).

% Fill all non-hint, unmarked cells in a column with water
fill_column_with_water(Size, Col) :-
    forall(between(1, Size, Row),
        (   \+ hint_cell(Row, Col, _),
            cell(Row, Col, empty)
        ->  retract(cell(Row, Col, empty)),
            assertz(cell(Row, Col, guess_water))
        ;   true
        )
    ).

% Fill all non-hint, unmarked cells in a row with water
fill_row_with_water(Size, Row) :-
    forall(between(1, Size, Col),
        (   \+ hint_cell(Row, Col, _),
            cell(Row, Col, empty)
        ->  retract(cell(Row, Col, empty)),
            assertz(cell(Row, Col, guess_water))
        ;   true
        )
    ).

% Check if the move is valid
valid_move(Row, Col, State, Size) :-
    integer(Row), integer(Col), Row >= 1, Row =< Size, Col >= 1, Col =< Size,
    (State = ship ; State = water),
    \+ hint_cell(Row, Col, _).

% Make a move (guess ship or water)
make_move(Row, Col, ship) :-
    (retract(cell(Row, Col, _)) ; true),
    assertz(cell(Row, Col, guess_ship)).
make_move(Row, Col, water) :-
    (retract(cell(Row, Col, _)) ; true),
    assertz(cell(Row, Col, guess_water)).

% Check if the game is won (all guesses match the real ship layout)
game_won(Size) :-
    forall(between(1, Size, Row),
        forall(between(1, Size, Col),
            (hint_cell(Row, Col, ship) -> true
            ; hint_cell(Row, Col, water) -> true
            ; solution_cell(Row, Col, ship) -> cell(Row, Col, guess_ship)
            ; solution_cell(Row, Col, water) -> cell(Row, Col, guess_water))
        )
    ).

% Print the solution board with ship shapes
print_solution(Size) :-
    nl, write('Solution:'), nl,
    write('     '), forall(between(1, Size, Col), ((Col < 10 -> format('~w ', [Col]) ; format('~w', [Col])))), nl,
    forall(between(1, Size, Row), (
        (Row < 10 -> format('~w  | ', [Row]) ; format('~w | ', [Row])),
        forall(between(1, Size, Col), (
            (solution_cell(Row, Col, ship) -> 
                (ship_display(Row, Col, _, Symbol) -> write(Symbol) ; write('O')), write(' ') 
            ; write('~ '))
        )),
        nl
    )), nl,
    print_ship_legend.

% Print legend showing ship types and their symbols
print_ship_legend :-
    nl, write('Ship Legend:'), nl,
    write('Battleship (4): '), 
    write('Horizontal: ◄■■► | Vertical: ▲■■▼'), nl,
    write('Cruiser (3):    '), 
    write('Horizontal: ◄■► | Vertical: ▲■▼'), nl,
    write('Destroyer (2):  '), 
    write('Horizontal: ◄►  | Vertical: ▲▼'), nl,
    write('Submarine (1):  '), 
    write('●'), nl, nl.

% Add N random hints based on the solution
add_random_hints(Size, N) :-
    findall((Row, Col), (
        between(1, Size, Row),
        between(1, Size, Col),
        row_clue(Row, RC), RC =\= 0,
        col_clue(Col, CC), CC =\= 0
    ), AllCells),
    length(AllCells, L),
    N1 is min(N, L),
    random_permutation(AllCells, Shuffled),
    add_hints_from_list(Shuffled, N1).

add_hints_from_list(_, 0) :- !.
add_hints_from_list([], _) :- !.
add_hints_from_list([(Row, Col)|Rest], N) :-
    (   \+ hint_cell(Row, Col, _)
    ->  solution_cell(Row, Col, State),
        (State = ship ; State = water),
        assertz(hint_cell(Row, Col, State)),
        (State = ship -> assertz(cell(Row, Col, guess_ship)) ; assertz(cell(Row, Col, guess_water))),
        N1 is N-1,
        add_hints_from_list(Rest, N1)
    ;   add_hints_from_list(Rest, N)
    ).

% Get all cells occupied by a ship
ship_cells(Row, Col, horizontal, Len, Cells) :-
    EndCol is Col + Len - 1,
    findall((Row, C), between(Col, EndCol, C), Cells).
ship_cells(Row, Col, vertical, Len, Cells) :-
    EndRow is Row + Len - 1,
    findall((R, Col), between(Row, EndRow, R), Cells).

% --- End of file ---