-module(day11).
-compile([export_all]).

part1() ->
    part1("input.txt").

empty_matrix({Rows, Cols}) ->
    lists:foldl(fun(R, M) -> array:set(R, array:new({size, Cols}), M) end,
                array:new({size, Rows}), lists:seq(0, Rows-1)).

part1(Filename) ->
    Fh = myio:open_file(Filename),
    {Matrix, Size} = build_matrix(Fh),
    occupied(
      extract_seats(
        p1_iterate_until(Matrix, Size,
                         p1_iterate(Matrix, empty_matrix(Size),
                                    {0, 0}, Size)))).

extract_seats(Matrix) ->
    lists:append(array:foldl(fun(_Idx, Row, Acc) -> [array:to_list(Row)|Acc] end,
                             [], Matrix)).

p1_iterate_until(Matrix, _Size, Matrix) ->
    Matrix;
p1_iterate_until(_Old, Size, New) ->
    p1_iterate_until(New, Size, p1_iterate(New, empty_matrix(Size), {0, 0}, Size)).

set_cell({Row, Col}, Value, Matrix) ->
    array:set(Row, array:set(Col, Value, array:get(Row, Matrix)),
              Matrix).

get_cell({Row, Col}, _Matrix, {RowCount, ColCount}) when Row >= RowCount;
                                                         Col >= ColCount;
                                                         Row < 0;
                                                         Col < 0 ->
    undefined;
get_cell({Row, Col}, Matrix, _Size) ->
    array:get(Col, array:get(Row, Matrix)).

neighbors({Row, Col}, Matrix, Size) ->
    [get_cell({Row-1, Col-1}, Matrix, Size),
     get_cell({Row-1, Col}, Matrix, Size),
     get_cell({Row-1, Col+1}, Matrix, Size),
     get_cell({Row, Col-1}, Matrix, Size),
     get_cell({Row, Col+1}, Matrix, Size),
     get_cell({Row+1, Col-1}, Matrix, Size),
     get_cell({Row+1, Col}, Matrix, Size),
     get_cell({Row+1, Col+1}, Matrix, Size)].

occupied(Seats) ->
    lists:foldl(fun($#, Tally) -> Tally+1;
                   (_, Tally) ->  Tally
                end, 0, Seats).

next_cell_phase($., _Cell, _Matrix, _Size) ->
    $.;
next_cell_phase(C, Cell, Matrix, Size) ->
    next_seat_phase(C, occupied(neighbors(Cell, Matrix, Size))).

next_seat_phase($L, 0) ->
    $#;
next_seat_phase($#, N) when N > 3 ->
    $L;
next_seat_phase(Seat, _) ->
    Seat.

p1_iterate(_Matrix, Next, {Rows, _Col}, {Rows, _Cols}) ->
    Next;
p1_iterate(Matrix, Next, {Row, Cols}, {_Xm, Cols}=Size) ->
    p1_iterate(Matrix, Next, {Row+1, 0}, Size);
p1_iterate(Matrix, Next, {Row, Col}=Idx, Size) ->
    p1_iterate(Matrix, set_cell(Idx,
                                next_cell_phase(get_cell(Idx, Matrix, Size),
                                                Idx,
                                                Matrix,
                                                Size),
                                Next),
               {Row, Col+1}, Size).

p1_iterate([H|T], Next) ->
    p1_iterate(T, [H|Next]).

build_matrix(Fh) ->
    build_matrix(myio:next_line(Fh), Fh, {array:new(), {0, 0}}).

build_matrix(eof, _Fh, {Matrix, {Rows, Cols}}) ->
    {Matrix, {Rows, Cols}};
build_matrix(Line, Fh, {Matrix, {Rows, _Cols}}) ->
    build_matrix(myio:next_line(Fh), Fh,
                 {array:set(Rows, array:from_list(Line), Matrix),
                  {Rows+1, length(Line)}}).
