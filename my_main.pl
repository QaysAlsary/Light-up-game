
size(8, 8).

wall(1, 6). wall(2, 2). wall(2, 3). wall(3, 7).
wall(4, 1). wall(4, 5). wall(5, 4). wall(5, 8).
wall(6, 2). wall(7, 6). wall(7, 7). wall(8, 3).

wall_num(1, 6, 1). wall_num(2, 2, 3). wall_num(3, 7, 0). wall_num(5, 4, 4).
wall_num(5, 8, 0). wall_num(6, 2, 2). wall_num(7, 6, 1).

% Non Valid
/* light(1, 2). light(1, 5). light(1, 8).
light(2, 1). light(2, 4).
light(3, 2).
light(4, 4). light(4, 6).
light(5, 3). light(5, 5).
light(6, 1). light(6, 4).
light(7, 2). light(7, 8).
light(8, 1). light(8, 6).*/

% Valid
light(1, 2). light(1, 7).
light(2, 1). light(2, 8).
light(3, 2).
light(4, 4). light(4, 6).
light(5, 3). light(5, 5).
light(6, 1). light(6, 4).
light(7, 2). light(7, 8).
light(8, 6).

cell(X, Y):- size(N, M), 1 =< X, X =< N, 1 =< Y, Y =< M.

loop_rows(I):- size(N, _), between(1, N, I).
loop_cols(J):- size(_, M), between(1, M, J).
loop_grid(I, J):- loop_rows(I), loop_cols(J).

display_cell(I, J):- wall_num(I, J, X), write(X), write(' '), !.
display_cell(I, J):- wall(I, J), write('W '), !.
display_cell(I, J):- light(I, J), write('L '), !.
display_cell(_, _):- write('* '), !.
display_row(I):- forall(loop_cols(J), display_cell(I, J)).
display_grid:- forall(loop_rows(I), (display_row(I), nl , nl)).

neighbor(X, Y, NX, Y):- NX is X - 1, cell(NX, Y).
neighbor(X, Y, NX, Y):- NX is X + 1, cell(NX, Y).
neighbor(X, Y, X, NY):- NY is Y - 1, cell(X, NY).
neighbor(X, Y, X, NY):- NY is Y + 1, cell(X, NY).


remove_head([], []).
remove_head([_ | Tail], Tail).

concatenate(L1, L2, R):- append(L1, L2, R).

left_extent(X, Y, []):- not(cell(X, Y)); wall(X, Y).
left_extent(X, Y, [(X, Y) | Tail]):- cell(X, Y), not(wall(X, Y)), Y2 is Y - 1, left_extent(X, Y2, Tail).
% ?- left_extent(1, 4, R).
% R = [(1, 4),  (1, 3),  (1, 2),  (1, 1)] ;
% ?- left_extent(4, 4, R).
% R = [(4, 4),  (4, 3),  (4, 2)] ;

right_extent(X, Y, []):- not(cell(X, Y)); wall(X, Y).
right_extent(X, Y, [(X, Y) | Tail]):- cell(X, Y), not(wall(X, Y)), Y2 is Y + 1, right_extent(X, Y2, Tail).
% ?- right_extent(3, 2, R).
% R = [(3, 2),  (3, 3),  (3, 4),  (3, 5),  (3, 6)] ;

row_extent(X, Y, Res):-
    left_extent(X, Y, Left),
    right_extent(X, Y, Right),
    remove_head(Right, Right2),
    concatenate(Left, Right2, Res).
% ?- row_extent(5, 6, Res).
% Res = [(5, 6),  (5, 5),  (5, 7)] ;

up_extent(X, Y, []):- not(cell(X, Y)); wall(X, Y).
up_extent(X, Y, [(X, Y) | Tail]):- cell(X, Y), not(wall(X, Y)), X2 is X - 1, up_extent(X2, Y, Tail).
% ?- up_extent(7, 3, Res).
% Res = [(7, 3),  (6, 3),  (5, 3),  (4, 3),  (3, 3)] ;

down_extent(X, Y, []):- not(cell(X, Y)); wall(X, Y).
down_extent(X, Y, [(X, Y) | Tail]):- cell(X, Y), not(wall(X, Y)), X2 is X + 1, down_extent(X2, Y, Tail).
% ?- down_extent(1, 5, Res).
% Res = [(1, 5),  (2, 5),  (3, 5)] ;

col_extent(X, Y, Res):-
    up_extent(X, Y, Up),
    down_extent(X, Y, Down),
    remove_head(Down, Down2),
    concatenate(Up, Down2, Res).
% ?- col_extent(4, 3, Res).
% Res = [(4, 3),  (3, 3),  (5, 3),  (6, 3),  (7, 3)] ;

cell_extent(X, Y, Res):-
    row_extent(X, Y, Row),
    col_extent(X, Y, Col),
    remove_head(Col, Col2),
    concatenate(Row, Col2, Res).
% ?- cell_extent(5, 7, Res).
% Res = [(5, 7),  (5, 6),  (5, 5),  (4, 7),  (6, 7)] ;

count_lights([], 0).
count_lights([(X, Y) | Tail], Lights):-
    light(X, Y),
    count_lights(Tail, Lights2),
    Lights is Lights2 + 1.

count_lights([(X, Y) | Tail], Lights):-
    not(light(X, Y)),
    count_lights(Tail, Lights2),
    Lights is Lights2.
% ?- count_lights([(5, 7),  (5, 6),  (5, 5),  (4, 7),  (6, 7)], X).
% X = 1 ;

count_cell_lights(X, Y, Lights):-
    cell_extent(X, Y, Extent),
    count_lights(Extent, Lights).
% ?- count_cell_lights(2, 4, X).
% X = 2 ;

lighted_cell(X, Y):- count_cell_lights(X, Y, Lights), Lights > 0.
% ?- lighted_cell(1, 1).
% true ;

light_neighbor(X, Y, X2, Y2):- neighbor(X, Y, X2, Y2), light(X2, Y2).
% ?- light_neighbor(2, 2, X, Y).
% X = 1,
% Y = 2 ;
% X = 3,
% Y = 2 ;
% X = 2,
% Y = 1 ;

light_neighbors_count(X, Y, Count):-
    aggregate_all(count, light_neighbor(X, Y, _, _), Count).
% ?- light_neighbors_count(2, 2, Count).
% Count = 3.

valid_light_neighbors_count(X, Y):-
    wall_num(X, Y, Count),
    light_neighbors_count(X, Y, Count).
% ?- valid_light_neighbors_count(2, 2).
% true.

all_cells_lighted:-
    forall(
        (
            loop_grid(X, Y),
            not(wall(X, Y))
        ),
        lighted_cell(X, Y)
    ).
% ?- all_cells_lighted().
% true.

no_double_light:-
    forall(
        (
            loop_grid(X, Y),
            light(X, Y)
        ),
        count_cell_lights(X, Y, 1)
    ).
% ?- no_double_light().
% true.

light_count_correct:-
    forall(
        (
            loop_grid(X, Y),
            wall_num(X, Y, _)
        ),
        valid_light_neighbors_count(X, Y)
    ).
% ?- light_count_correct().
% true.

solved() :-
    all_cells_lighted,
    no_double_light,
    light_count_correct.
% ?- solved().
% true.




replay :- retractall(light(_,_)) ,
           display_grid.


















