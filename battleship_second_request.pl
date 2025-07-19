%------------------------------ init ------------------------------

ship_form(submarine).
ship_form(start).
ship_form(middle).
ship_form(end).

ship_type(submarine).
ship_type(destroyer).
ship_type(cruiser).
ship_type(battleship).

type_segment(submarine, 1).
type_segment(destroyer, 1).
type_segment(destroyer, 2).
type_segment(cruiser, 1).
type_segment(cruiser, 2).
type_segment(cruiser, 3).
type_segment(battleship, 1).
type_segment(battleship, 2).
type_segment(battleship, 3).
type_segment(battleship, 4).

ship_length(submarine, 1).
ship_length(destroyer, 2).
ship_length(cruiser, 3).
ship_length(battleship, 4).

ship_segment_form(submarine, 1, submarine).
ship_segment_form(destroyer, 1, start).
ship_segment_form(destroyer, 2, end).
ship_segment_form(cruiser, 1, start).
ship_segment_form(cruiser, 2, middle).
ship_segment_form(cruiser, 3, end).
ship_segment_form(battleship, 1, start).
ship_segment_form(battleship, 2, middle).
ship_segment_form(battleship, 3, middle).
ship_segment_form(battleship, 4, end).

symbol_value(submarine, none, '●').
symbol_value(start, horizontal, '◀').
symbol_value(start, vertical, '▲').
symbol_value(middle, _, '■').
symbol_value(end, horizontal, '▶').
symbol_value(end, vertical, '▼').

orientation(none).
orientation(horizontal).
orientation(vertical).

%------------------------------ dynamics ------------------------------

:- dynamic(board_size/2).
:- dynamic(number_of_ships_in_row/2).
:- dynamic(number_of_ships_in_col/2).
:- dynamic(ship/5).
:- dynamic(hint_ship/4).
:- dynamic(water/2).
:- dynamic(ship_count/2).

%------------------------------ validations ------------------------------

validate_type_orientation(submarine, none):- !.
validate_type_orientation(destroyer, horizontal):- !.
validate_type_orientation(destroyer, vertical):- !.
validate_type_orientation(cruiser, horizontal):- !.
validate_type_orientation(cruiser, vertical):- !.
validate_type_orientation(battleship, horizontal):- !.
validate_type_orientation(battleship, vertical):- !.

validate_form_orientation(submarine, none):- !.
validate_form_orientation(start, horizontal):- !.
validate_form_orientation(start, vertical):- !.
validate_form_orientation(middle, none):- !.
validate_form_orientation(middle, horizontal):- !.
validate_form_orientation(middle, vertical):- !.
validate_form_orientation(end, horizontal):- !.
validate_form_orientation(end, vertical):- !.

validate_board_size(Row, Col):- Row > 0, Col > 0.
validate_row(Row):- board_size(MaxRow, _), Row > 0, Row =< MaxRow.
validate_col(Col):- board_size(_, MaxCol), Col > 0, Col =< MaxCol.

validate_ship(Type, Segment, Orientation):- ship_type(Type), type_segment(Type, Segment), orientation(Orientation), validate_type_orientation(Type, Orientation).

validate_hint_ship(Form, Orientation):- ship_form(Form), orientation(Orientation), validate_form_orientation(Form, Orientation).

%------------------------------ setters & getters ------------------------------

set_board_size(Row, Col):- validate_board_size(Row, Col), retractall(board_size(_, _)), assert(board_size(Row, Col)).

set_number_of_ships_in_row(Row, Number):- retractall(number_of_ships_in_row(Row, _)), assert(number_of_ships_in_row(Row, Number)).

set_number_of_ships_in_col(Col, Number):- retractall(number_of_ships_in_col(Col, _)), assert(number_of_ships_in_col(Col, Number)).

set_ship(Row, Col, Type, Segment, Orientation):- validate_row(Row), validate_col(Col), validate_ship(Type, Segment, Orientation), retractall(water(Row, Col)), retractall(ship(Row, Col, _, _, _)), assert(ship(Row, Col, Type, Segment, Orientation)).
get_ship(Row, Col, Type, Segment, Orientation):- validate_row(Row), validate_col(Col), ship(Row, Col, Type, Segment, Orientation).
list_ships:- listing(ship).

set_hint_ship(Row, Col, Form, Orientation):- validate_row(Row), validate_col(Col), validate_hint_ship(Form, Orientation), retractall(water(Row, Col)), retractall(hint_ship(Row, Col, _, _)), assert(hint_ship(Row, Col, Form, Orientation)).
list_hint_ships:- listing(hint_ship).

set_water(Row, Col):- validate_row(Row), validate_col(Col), retractall(ship(Row, Col, _, _, _)), assert(water(Row, Col)).
list_waters:- listing(water).

set_ship_count(Type, Count):- ship_type(Type), retractall(ship_count(Type, _)), assert(ship_count(Type, Count)).
list_ship_counts:- listing(ship_count).

%------------------------------ check_left ------------------------------

check_left(Row, Col, Type, Segment, horizontal):-
    Col > 1,
	Segment > 1,
	!,
    PreviousCol is Col - 1,
    PreviousSegment is Segment - 1,
    ship(Row, PreviousCol, Type, PreviousSegment, horizontal).

check_left(Row, Col, _, _, _):-
    (
        Col > 1
    ->
        PreviousCol is Col - 1,
        \+ ship(Row, PreviousCol, _, _, _)
	;
        true
    ).
	
%------------------------------ check_up ------------------------------
	
check_up(Row, Col, Type, Segment, vertical):-
	Row > 1,
    Segment > 1,
	!,
    PreviousRow is Row - 1,
    PreviousSegment is Segment - 1,
    ship(PreviousRow, Col, Type, PreviousSegment, vertical).

check_up(Row, Col, _, _, _):-
    (
        Row > 1
    ->
        PreviousRow is Row - 1,
        \+ ship(PreviousRow, Col, _, _, _)
	;
        true
    ).
	
%------------------------------ check_right ------------------------------

check_right(Row, Col, Type, Segment, horizontal):-
	board_size(_, MaxCol),
	ship_length(Type, Length),
	Col < MaxCol,
    Segment < Length,
	!,
    NextCol is Col + 1,
    NextSegment is Segment + 1,
    ship(Row, NextCol, Type, NextSegment, horizontal).

check_right(Row, Col, _, _, _):-
    board_size(_, MaxCol),
    (
        Col < MaxCol
    ->
        NextCol is Col + 1,
        \+ ship(Row, NextCol, _, _, _)
	;
        true
    ).

%------------------------------ check_down ------------------------------
	
check_down(Row, Col, Type, Segment, vertical):-
	board_size(MaxRow, _),
	ship_length(Type, Length),
	Row < MaxRow,
    Segment < Length,
	!,
    NextRow is Row + 1,
    NextSegment is Segment + 1,
    ship(NextRow, Col, Type, NextSegment, vertical).

check_down(Row, Col, _, _, _):-
    board_size(MaxRow, _),
    (
        Row < MaxRow
    ->
        NextRow is Row + 1,
        \+ ship(NextRow, Col, _, _, _)
	;
        true
    ).
	
%------------------------------ check_left_upper ------------------------------

check_left_upper(Row, Col):-
	(
		Row > 1, Col > 1
	->
		PreviousRow is Row - 1,
		PreviousCol is Col - 1,
		\+ ship(PreviousRow, PreviousCol, _, _, _)
	;
		true
	).
	
%------------------------------ check_left_bottom ------------------------------
	
check_left_bottom(Row, Col):-
	board_size(MaxRow, _),
	(
		Row < MaxRow, Col > 1
	->
		NextRow is Row + 1,
		PreviousCol is Col - 1,
		\+ ship(NextRow, PreviousCol, _, _, _)
	;
		true
	).

%------------------------------ check_right_upper ------------------------------
	
check_right_upper(Row, Col):-
	board_size(_, MaxCol),
	(
		Row > 1, Col < MaxCol
	->
		PreviousRow is Row - 1,
		NextCol is Col + 1,
		\+ ship(PreviousRow, NextCol, _, _, _)
	;
		true
	).
	
%------------------------------ check_right_bottom ------------------------------
	
check_right_bottom(Row, Col):-
	board_size(MaxRow, MaxCol),
	(
		Row < MaxRow, Col < MaxCol
	->
		NextRow is Row + 1,
		NextCol is Col + 1,
		\+ ship(NextRow, NextCol, _, _, _)
	;
		true
	).

%------------------------------ checks ------------------------------

check_left_up_right_down(Row, Col, Type, Segment, Orientation):-
	check_left(Row, Col, Type, Segment, Orientation),
	check_up(Row, Col, Type, Segment, Orientation),
	check_right(Row, Col, Type, Segment, Orientation),
	check_down(Row, Col, Type, Segment, Orientation).
	
check_diagonal(Row, Col):-
	check_left_upper(Row, Col),
	check_left_bottom(Row, Col),
	check_right_upper(Row, Col),
	check_right_bottom(Row, Col).
	
check_row(Row):-
	findall(Col, ship(Row, Col, _, _, _), L1),
	findall(Col, hint_ship(Row, Col, _, _), L2),
	union(L1, L2, L),
	length(L, N),
	number_of_ships_in_row(Row, Number),
	N = Number.

check_col(Col):-
	findall(Row, ship(Row, Col, _, _, _), L1),
	findall(Row, hint_ship(Row, Col, _, _), L2),
	union(L1, L2, L),
	length(L, N),
	number_of_ships_in_col(Col, Number),
	N = Number.

%------------------------------ rules ------------------------------

check_rule_1:- forall(ship(Row, Col, Type, Segment, Orientation), check_left_up_right_down(Row, Col, Type, Segment, Orientation)).

check_rule_2:- forall(ship(Row, Col, _, _, _), check_diagonal(Row, Col)).

check_rule_3:- board_size(MaxRow, _), forall(between(1, MaxRow, Row), check_row(Row)).

check_rule_4:- board_size(_, MaxCol), forall(between(1, MaxCol, Col), check_col(Col)).

check_rules:-
	check_rule_1,
	check_rule_2,
	check_rule_3,
	check_rule_4.

%------------------------------ fill_row_col_water ------------------------------

fill_water(Row, Col):-
	\+ hint_ship(Row, Col, _, _),
	\+ ship(Row, Col, _, _, _),
	set_water(Row, Col),
	fail.

fill_row_water(Row):-
	check_row(Row),
	board_size(_, MaxCol),
	forall(between(1, MaxCol, Col), \+ fill_water(Row, Col)),
	fail.

fill_col_water(Col):-
	check_col(Col),
	board_size(MaxRow, _),
	forall(between(1, MaxRow, Row), \+ fill_water(Row, Col)),
	fail.

check_clue:-
	board_size(MaxRow, MaxCol),
	forall(between(1, MaxRow, Row), \+ fill_row_water(Row)),
	forall(between(1, MaxCol, Col), \+ fill_col_water(Col)).

%------------------------------ fill_ship_around_water ------------------------------

fill_ship_left_water(Row, Col):-
	Col > 1,
	PreviousCol is Col - 1,
	fill_water(Row, PreviousCol),
	fail.

fill_ship_up_water(Row, Col):-
	Row > 1,
	PreviousRow is Row - 1,
	fill_water(PreviousRow, Col),
	fail.

fill_ship_right_water(Row, Col):-
	board_size(_, MaxCol),
	Col < MaxCol,
	NextCol is Col + 1,
	fill_water(Row, NextCol),
	fail.

fill_ship_down_water(Row, Col):-
	board_size(MaxRow, _),
	Row < MaxRow,
	NextRow is Row + 1,
	fill_water(NextRow, Col),
	fail.

fill_ship_left_upper_water(Row, Col):-
	Row > 1, Col > 1,
	PreviousRow is Row - 1,
	PreviousCol is Col - 1,
	fill_water(PreviousRow, PreviousCol),
	fail.

fill_ship_left_bottom_water(Row, Col):-
	board_size(MaxRow, _),
	Row < MaxRow, Col > 1,
	NextRow is Row + 1,
	PreviousCol is Col - 1,
	fill_water(NextRow, PreviousCol),
	fail.

fill_ship_right_upper_water(Row, Col):-
	board_size(_, MaxCol),
	Row > 1, Col < MaxCol,
	PreviousRow is Row - 1,
	NextCol is Col + 1,
	fill_water(PreviousRow, NextCol),
	fail.

fill_ship_right_bottom_water(Row, Col):-
	board_size(MaxRow, MaxCol),
	Row < MaxRow, Col < MaxCol,
	NextRow is Row + 1,
	NextCol is Col + 1,
	fill_water(NextRow, NextCol),
	fail.

fill_ship_around_water(Row, Col, _, _):-
	\+ fill_ship_left_upper_water(Row, Col),
	\+ fill_ship_left_bottom_water(Row, Col),
	\+ fill_ship_right_upper_water(Row, Col),
	\+ fill_ship_right_bottom_water(Row, Col),
	fail.

fill_ship_around_water(Row, Col, submarine, _):-
	!,
	\+ fill_ship_left_water(Row, Col),
	\+ fill_ship_up_water(Row, Col),
	\+ fill_ship_right_water(Row, Col),
	\+ fill_ship_down_water(Row, Col),
	fail.

fill_ship_around_water(Row, Col, start, horizontal):-
	!,
	\+ fill_ship_left_water(Row, Col),
	\+ fill_ship_up_water(Row, Col),
	\+ fill_ship_down_water(Row, Col),
	fail.

fill_ship_around_water(Row, Col, start, vertical):-
	!,
	\+ fill_ship_left_water(Row, Col),
	\+ fill_ship_up_water(Row, Col),
	\+ fill_ship_right_water(Row, Col),
	fail.

fill_ship_around_water(Row, Col, end, horizontal):-
	!,
	\+ fill_ship_up_water(Row, Col),
	\+ fill_ship_right_water(Row, Col),
	\+ fill_ship_down_water(Row, Col),
	fail.

fill_ship_around_water(Row, Col, end, vertical):-
	!,
	\+ fill_ship_left_water(Row, Col),
	\+ fill_ship_right_water(Row, Col),
	\+ fill_ship_down_water(Row, Col),
	fail.

helper_fill_ship_around_water(Row, Col, Type, Segment, Orientation):-
	ship_segment_form(Type, Segment, Form),
	fill_ship_around_water(Row, Col, Form, Orientation).

check_ship_around:-
	forall(ship(Row, Col, Type, Segment, Orientation), \+ helper_fill_ship_around_water(Row, Col, Type, Segment, Orientation)),
	forall(hint_ship(Row, Col, Form, Orientation), \+ fill_ship_around_water(Row, Col, Form, Orientation)).

%------------------------------ fill_ship ------------------------------



%------------------------------ solve ------------------------------

solve:-
	check_clue,
	print_board,
	check_ship_around.

%------------------------------ prints ------------------------------

print_row_clue(Row):- number_of_ships_in_row(Row, Clue), write(Clue).

print_col_clue(Col):- number_of_ships_in_col(Col, Clue), write(Clue), write(' ').

print_cell(Row, Col):-
	board_size(_, MaxCol),
	(
		ship(Row, Col, Type, Segment, Orientation)
	->
		ship_segment_form(Type, Segment, Form),
		symbol_value(Form, Orientation, Symbol),
		write(Symbol)
	;
		hint_ship(Row, Col, Form, Orientation)
	->
		symbol_value(Form, Orientation, Symbol),
		write(Symbol)
	;
		water(Row, Col)
	->
		write('~')
	;
		write('.')
	),
	write(' '),
	(
		Col = MaxCol
	->
		write('| '),
		print_row_clue(Row),
		writeln('')
	;
		true
	).

print_board:-
	board_size(MaxRow, MaxCol),
	forall(between(1, MaxRow, Row),
		forall(between(1, MaxCol, Col),
			print_cell(Row, Col))),
	forall(between(1, MaxCol, Col),
		write('- ')),
	writeln(''),
	forall(between(1, MaxCol, Col),
		print_col_clue(Col)),
	writeln(''),
	writeln('').
	
%------------------------------ clear ------------------------------

clear:-
	retractall(board_size(_, _)),
	retractall(ship(_, _, _, _, _)),
	retractall(hint_ship(_, _, _, _)),
	retractall(ship_count(_, _)),
	retractall(water(_, _)),
	retractall(number_of_ships_in_row(_, _)),
	retractall(number_of_ships_in_col(_, _)).
	
%------------------------------ testing ------------------------------

test_1:-
	clear,
	
	set_board_size(6, 6),
	
	set_number_of_ships_in_row(1, 4),
	set_number_of_ships_in_row(2, 0),
	set_number_of_ships_in_row(3, 2),
	set_number_of_ships_in_row(4, 1),
	set_number_of_ships_in_row(5, 2),
	set_number_of_ships_in_row(6, 1),
	
	set_number_of_ships_in_col(1, 1),
	set_number_of_ships_in_col(2, 0),
	set_number_of_ships_in_col(3, 4),
	set_number_of_ships_in_col(4, 0),
	set_number_of_ships_in_col(5, 3),
	set_number_of_ships_in_col(6, 2),
	
	set_ship(1, 1, submarine, 1, none),
	set_ship(1, 3, submarine, 1, none),
	set_ship(1, 5, destroyer, 1, horizontal),
	set_ship(1, 6, destroyer, 2, horizontal),
	set_ship(3, 5, destroyer, 1, horizontal),
	set_ship(3, 6, destroyer, 2, horizontal),
	set_ship(4, 3, cruiser, 1, vertical),
	set_ship(5, 5, submarine, 1, none),
	set_ship(5, 3, cruiser, 2, vertical),
	set_ship(6, 3, cruiser, 3, vertical),
	
	print_board,
	
	check_rules.
	
test_2:-
	clear,
	
	set_board_size(6, 6),
	
	set_number_of_ships_in_row(1, 4),
	set_number_of_ships_in_row(2, 0),
	set_number_of_ships_in_row(3, 2),
	set_number_of_ships_in_row(4, 1),
	set_number_of_ships_in_row(5, 2),
	set_number_of_ships_in_row(6, 1),
	
	set_number_of_ships_in_col(1, 1),
	set_number_of_ships_in_col(2, 1),
	set_number_of_ships_in_col(3, 3),
	set_number_of_ships_in_col(4, 0),
	set_number_of_ships_in_col(5, 3),
	set_number_of_ships_in_col(6, 2),
	
	set_ship(1, 1, destroyer, 1, horizontal),
	set_ship(1, 2, destroyer, 2, horizontal),
	set_ship(1, 5, destroyer, 1, horizontal),
	set_ship(1, 6, destroyer, 2, horizontal),
	set_ship(3, 5, destroyer, 1, horizontal),
	set_ship(3, 6, destroyer, 2, horizontal),
	set_ship(4, 3, cruiser, 1, vertical),
	set_ship(5, 5, submarine, 1, none),	
	set_ship(5, 3, cruiser, 2, vertical),
	set_ship(6, 3, cruiser, 3, vertical),
	
	print_board,
	
	check_rules.
	
test_3:-
	clear,
	
	set_board_size(6, 6),
	
	set_number_of_ships_in_row(1, 4),
	set_number_of_ships_in_row(2, 0),
	set_number_of_ships_in_row(3, 2),
	set_number_of_ships_in_row(4, 1),
	set_number_of_ships_in_row(5, 2),
	set_number_of_ships_in_row(6, 2),
	
	set_number_of_ships_in_col(1, 1),
	set_number_of_ships_in_col(2, 0),
	set_number_of_ships_in_col(3, 4),
	set_number_of_ships_in_col(4, 0),
	set_number_of_ships_in_col(5, 2),
	set_number_of_ships_in_col(6, 4),
	
	set_ship(1, 1, submarine, 1, none),
	set_ship(1, 3, submarine, 1, none),
	set_ship(1, 5, destroyer, 1, horizontal),
	set_ship(1, 6, destroyer, 2, horizontal),
	set_ship(3, 5, destroyer, 1, horizontal),
	set_ship(3, 6, destroyer, 2, horizontal),
	set_ship(4, 3, cruiser, 1, vertical),
	set_ship(5, 3, cruiser, 2, vertical),
	set_ship(6, 3, cruiser, 3, vertical),
	set_ship(5, 6, destroyer, 1, vertical),
	set_ship(6, 6, destroyer, 2, vertical),
	
	print_board,
	
	check_rules.
	
test_4:-
	clear,
	
	set_board_size(6, 6),
	
	set_number_of_ships_in_row(1, 4),
	set_number_of_ships_in_row(2, 0),
	set_number_of_ships_in_row(3, 2),
	set_number_of_ships_in_row(4, 1),
	set_number_of_ships_in_row(5, 1),
	set_number_of_ships_in_row(6, 3),
	
	set_number_of_ships_in_col(1, 1),
	set_number_of_ships_in_col(2, 0),
	set_number_of_ships_in_col(3, 4),
	set_number_of_ships_in_col(4, 0),
	set_number_of_ships_in_col(5, 3),
	set_number_of_ships_in_col(6, 3),
	
	set_ship(1, 1, submarine, 1, none),
	set_ship(1, 3, submarine, 1, none),
	set_ship(1, 5, destroyer, 1, horizontal),
	set_ship(1, 6, destroyer, 2, horizontal),
	set_ship(3, 5, destroyer, 1, horizontal),
	set_ship(3, 6, destroyer, 2, horizontal),
	set_ship(4, 3, cruiser, 1, vertical),
	set_ship(5, 3, cruiser, 2, vertical),
	set_ship(6, 3, cruiser, 3, vertical),
	set_ship(6, 5, destroyer, 1, horizontal),
	set_ship(6, 6, destroyer, 2, horizontal),
	
	print_board,
	
	check_rules.
	
test_5:-
	clear,
  
	set_board_size(6, 6),
  
	set_number_of_ships_in_row(1, 2),
	set_number_of_ships_in_row(2, 2),
	set_number_of_ships_in_row(3, 2),
	set_number_of_ships_in_row(4, 2),
	set_number_of_ships_in_row(5, 1),
	set_number_of_ships_in_row(6, 1),
  
	set_number_of_ships_in_col(1, 2),
	set_number_of_ships_in_col(2, 2),
	set_number_of_ships_in_col(3, 0),
	set_number_of_ships_in_col(4, 3),
	set_number_of_ships_in_col(5, 0),
	set_number_of_ships_in_col(6, 3),

	set_ship(1, 2, destroyer, 1, vertical),
	set_ship(2, 2, destroyer, 2, vertical),
	set_ship(1, 4, submarine, 1, none),
	set_ship(3, 4, submarine, 1, none),
	set_ship(4, 3, submarine, 1, none),
	set_ship(6, 4, submarine, 1, none),
    set_ship(4, 1, destroyer, 1, vertical),
	set_ship(5, 1, destroyer, 2, vertical),
    set_ship(2, 6, cruiser, 1, vertical),
	set_ship(3, 6, cruiser, 2, vertical),
	set_ship(4, 6, cruiser, 3, vertical),
	
	print_board,
	
	check_rules.

test_6:-
	clear,
  
	set_board_size(6, 6),
  
	set_number_of_ships_in_row(1, 2),
	set_number_of_ships_in_row(2, 2),
	set_number_of_ships_in_row(3, 2),
	set_number_of_ships_in_row(4, 2),
	set_number_of_ships_in_row(5, 1),
	set_number_of_ships_in_row(6, 1),
  
	set_number_of_ships_in_col(1, 2),
	set_number_of_ships_in_col(2, 2),
	set_number_of_ships_in_col(3, 0),
	set_number_of_ships_in_col(4, 3),
	set_number_of_ships_in_col(5, 0),
	set_number_of_ships_in_col(6, 3),

	set_ship_count(submarine, 3),
	set_ship_count(destroyer, 2),
	set_ship_count(cruiser, 1),
	set_ship_count(battleship, 0),

	set_hint_ship(2, 2, end, vertical),
    set_hint_ship(2, 6, start, vertical),
	
	print_board,
	
	solve,

	print_board.

test_7:-
	clear,
  
  	set_board_size(8, 8),
  
  	set_number_of_ships_in_row(1, 5),
	set_number_of_ships_in_row(2, 1),
	set_number_of_ships_in_row(3, 3),
	set_number_of_ships_in_row(4, 1),
	set_number_of_ships_in_row(5, 2),
	set_number_of_ships_in_row(6, 2),
	set_number_of_ships_in_row(7, 2),
	set_number_of_ships_in_row(8, 1),
  
	set_number_of_ships_in_col(1, 3),
	set_number_of_ships_in_col(2, 2),
	set_number_of_ships_in_col(3, 2),
	set_number_of_ships_in_col(4, 3),
	set_number_of_ships_in_col(5, 2),
	set_number_of_ships_in_col(6, 2),
	set_number_of_ships_in_col(7, 2),
	set_number_of_ships_in_col(8, 1),

	set_hint_ship(1, 8, end, horizontal),
	set_hint_ship(3, 4, middle, none),
	set_hint_ship(7, 2, submarine, none),

	print_board,

	solve,

	print_board.