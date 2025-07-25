% Grid dimensions: 6 rows x 6 columns
grid_size(6, 6).

% Number of ship cells in each row
row_counts([2, 2, 2, 2, 1, 1]).

% Number of ship cells in each column
col_counts([2, 2, 0, 3, 0, 3]).

% Fleet composition: ship_length(Number of ships)
fleet(3, 1).
fleet(2, 2).
fleet(1, 2).

% Pre-filled cells: pre_filled(Row, Col, Content)
% Content can be 'ship' or 'water'
pre_filled(2, 2, ship).
pre_filled(5, 4, water).

% Corrected example solution (no contact between ships)
cell(1,1, water). cell(1,2, head_v). cell(1,3, water). cell(1,4, circle). cell(1,5, water).  cell(1,6, water).
cell(2,1, water).  cell(2,2, tail_v).  cell(2,3, water). cell(2,4, water). cell(2,5, water). cell(2,6, head_v).
cell(3,1, water). cell(3,2, water). cell(3,3, water). cell(3,4, circle). cell(3,5, water). cell(3,6, body).
cell(4,1, head_v). cell(4,2, water).  cell(4,3, water).  cell(4,4, water).  cell(4,5, water). cell(4,6, tail_v).
cell(5,1, tail_v). cell(5,2, water). cell(5,3, water). cell(5,4, water). cell(5,5, water). cell(5,6, water).
cell(6,1, water). cell(6,2, water). cell(6,3, water).  cell(6,4, circle). cell(6,5, water). cell(6,6, water).



% Print the entire grid with row and column counts
print_grid :-
    grid_size(Rows, Cols),
    row_counts(RowCounts),
    col_counts(ColCounts),
    print_col_headers(1, Cols),
    print_row_separator(Cols),
    print_rows(1, Rows, Cols, RowCounts),
    print_row_separator(Cols),
    print_col_counts(ColCounts, 1).

% Print column numbers on top
print_col_headers(Col, MaxCol) :-
    write('   '), print_col_nums(Col, MaxCol), nl.

print_col_nums(Col, MaxCol) :-
    Col > MaxCol, !.
print_col_nums(Col, MaxCol) :-
    format(' ~w ', [Col]),
    Next is Col + 1,
    print_col_nums(Next, MaxCol).

% Print separator line
print_row_separator(Cols) :-
    write('   +'),
    forall(between(1, Cols, _), write('---')),
    write('+'), nl.

% Print each row with its row count on the right
print_rows(Row, MaxRow, _, _) :- Row > MaxRow, !.
print_rows(Row, MaxRow, Cols, RowCounts) :-
    format('~w |', [Row]),
    print_row(Row, 1, Cols),
    nth1(Row, RowCounts, Count),
    format('| ~w~n', [Count]),
    NextRow is Row + 1,
    print_rows(NextRow, MaxRow, Cols, RowCounts).

% Print each cell in a row with proper symbol
print_row(_, Col, MaxCol) :- Col > MaxCol, !.
print_row(Row, Col, MaxCol) :-
    cell(Row, Col, Content),
    cell_symbol(Content, Symbol),
    format(' ~w ', [Symbol]),
    NextCol is Col + 1,
    print_row(Row, NextCol, MaxCol).

% Map cell content to its printable symbol
cell_symbol(water,  '~').
cell_symbol(circle, '●').
cell_symbol(head_h, '▶').
cell_symbol(tail_h, '◀').
cell_symbol(head_v, '▲').
cell_symbol(tail_v, '▼').
cell_symbol(body,   '▮').
cell_symbol(_,      '?').  % fallback for unknown values


% Print column counts at the bottom
print_col_counts(Counts, Index) :-
    Index =:= 1, write('   '), !,
    print_col_count_list(Counts, 1).

print_col_count_list([], _):- nl.
print_col_count_list([H|T], I) :-
    format(' ~w ', [H]),
    I2 is I + 1,
    print_col_count_list(T, I2).


% True if the cell type is a ship part
is_ship_part(circle).
is_ship_part(head_h).
is_ship_part(tail_h).
is_ship_part(head_v).
is_ship_part(tail_v).
is_ship_part(body).



% Check that the number of ship cells in each row matches row_counts
check_row_counts :-
    grid_size(Rows, _),
    row_counts(RowCounts),
    check_each_row(1, Rows, RowCounts).

check_each_row(Row, MaxRow, _) :- Row > MaxRow, !.
check_each_row(Row, MaxRow, RowCounts) :-
    findall(1,
            (cell(Row, _, Part), is_ship_part(Part)),
            ShipCells),
    length(ShipCells, Count),
    nth1(Row, RowCounts, ExpectedCount),
    Count =:= ExpectedCount,
    NextRow is Row + 1,
    check_each_row(NextRow, MaxRow, RowCounts).



% Check that the number of ship cells in each column matches col_counts
check_col_counts :-
    grid_size(_, Cols),
    col_counts(ColCounts),
    check_each_col(1, Cols, ColCounts).

check_each_col(Col, MaxCol, _) :- Col > MaxCol, !.
check_each_col(Col, MaxCol, ColCounts) :-
    findall(1,
            (cell(_, Col, Part), is_ship_part(Part)),
            ShipCells),
    length(ShipCells, Count),
    nth1(Col, ColCounts, ExpectedCount),
    Count =:= ExpectedCount,
    NextCol is Col + 1,
    check_each_col(NextCol, MaxCol, ColCounts).
