open! Core_kernel

module type S = sig

    type item [@@deriving compare, sexp, yojson]
    val size : float
    val is_equal : item -> item -> bool

end

module Vertical
(S : S)
= struct

    type dragging =
        {
            offset_x : float;
            offset_y : float;
            delta_x : float;
            delta_y : float;
            initial : int;
            current : int;
            target : S.item;
        }
        [@@deriving sexp, compare, yojson]

    type issue =
        | No_item_at of int
        [@@deriving sexp, compare, yojson]

    type t =
        | Waiting
        | Issue of issue
        | Dragging of dragging
        [@@deriving sexp, compare, yojson]

    let get_index_from_position ~items ~position =
        Int.of_float (Float.round_down (position /. S.size)) |>
        (Int.max 0) |>
        (Int.min (List.length items))

    let get_position_from_index ~index =
        (float_of_int index) *. S.size

    let get_offset_from_update ~(move_from: int) ~(move_to: int) ~(index: int) : float =
        if move_to >= move_from then
            (if index < move_to && index >= move_from then (S.size *. -1.0) else 0.)
        else if move_to < move_from then
            (if index >= move_to && index < move_from then (S.size) else 0.)
        else 0.

    let move_to ~items ~item ~(move_to: int) =
        if (List.length items) <= move_to
        then
            List.filter items ~f:(fun i -> not (S.is_equal i item)) @ [ item ] 
        else
            List.foldi items ~init:[] ~f:(fun i prev next ->
                match S.is_equal next item with
                | true when i = move_to -> 
                    item :: prev
                | true -> 
                    prev
                | false when i = move_to ->
                    next :: item :: prev
                | false ->
                    next :: prev
            ) |> List.rev

    (* update t *)

    let empty =
        Waiting

    let start_index ~items ~index ~offset_x ~offset_y =
        let position = get_position_from_index ~index in
        match List.nth items index with
        | Some item ->
            Dragging {
                offset_x = offset_x;
                offset_y = offset_y;
                delta_x = offset_x;
                delta_y = offset_y +. position;
                initial = index;
                current = index;
                target = item;
            }
        | None ->
            Issue (No_item_at index)

    let start ~items ~x ~y =
        let index = get_index_from_position ~position:y ~items in
        let position = get_position_from_index ~index in
        match List.nth items index with
        | Some item ->
            Dragging {
                offset_x = x;
                offset_y = (y -. position);
                delta_x = x;
                delta_y = y;
                initial = index;
                current = index;
                target = item;
            }
        | None ->
            Issue (No_item_at index)

    let get_new_items t ~items =
        match t with
        | Waiting -> 
            items
        | Issue _ -> 
            items
        | Dragging d ->
            move_to ~items ~item:d.target ~move_to:d.current

    let drag t ~items ~x ~y =
        match t with
        | Waiting -> Waiting
        | Issue i -> Issue i
        | Dragging d ->
            let index = get_index_from_position ~position:y ~items in
            let index = if index > d.initial then index + 1 else index in
            Dragging {
                d with
                delta_x = x;
                delta_y = y;
                current = index;
            }

    let cancel t =
        match t with
        | Waiting -> Waiting
        | Issue i -> Issue i
        | Dragging _ -> Waiting

    let release t =
        match t with
        | Waiting -> Waiting
        | Issue i -> Issue i
        | Dragging _ -> Waiting

end

let%test_module _ = (module struct

    module Test_int = struct

        type item = int [@@deriving compare, sexp, yojson]

        let size = 30.0

        let is_equal i0 i1 = i0 = i1

    end

    module Drag_test = Vertical(Test_int)

    let%test_unit "can get an index from a position" = 
        let ind = Drag_test.get_index_from_position ~items:[0;1;2;3;4] ~position:50. in
        [%test_result: int] (ind) ~expect:1;
        let ind = Drag_test.get_index_from_position ~items:[0;1;2;3;4] ~position:(-10.) in
        [%test_result: int] (ind) ~expect:0;
        let ind = Drag_test.get_index_from_position ~items:[0;1;2;3;4] ~position:160. in
        [%test_result: int] (ind) ~expect:5;
        let ind = Drag_test.get_index_from_position ~items:[0;1;2;3;4] ~position:30. in
        [%test_result: int] (ind) ~expect:1;
        ()

    let%test_unit "can get a position from an index" =
        let pos = Drag_test.get_position_from_index ~index:0 in
        [%test_result: float] (pos) ~expect:0.0;
        let pos = Drag_test.get_position_from_index ~index:1 in
        [%test_result: float] (pos) ~expect:30.0;
        ()

    let%test_unit "can get an offset from an update" =
        let offset = Drag_test.get_offset_from_update ~move_from:0 ~move_to:3 ~index:1 in
        [%test_result: float] (offset) ~expect:(-30.0);
        let offset = Drag_test.get_offset_from_update ~move_from:0 ~move_to:3 ~index:4 in
        [%test_result: float] (offset) ~expect:(0.0);
        let offset = Drag_test.get_offset_from_update ~move_from:2 ~move_to:3 ~index:0 in
        [%test_result: float] (offset) ~expect:(0.0);
        let offset = Drag_test.get_offset_from_update ~move_from:3 ~move_to:0 ~index:1 in
        [%test_result: float] (offset) ~expect:(30.0);
        ()

    let%test_unit "can move an item in a list" =
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:0 ~move_to:3 in
        [%test_result: int list] (new_items) ~expect:[1;2;0;3;4;5;];
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:1 ~move_to:1 in
        [%test_result: int list] (new_items) ~expect:[0;1;2;3;4;5;];
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:5 ~move_to:1 in
        [%test_result: int list] (new_items) ~expect:[0;5;1;2;3;4;];
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:5 ~move_to:4 in
        [%test_result: int list] (new_items) ~expect:[0;1;2;3;5;4;];
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:5 ~move_to:5 in
        [%test_result: int list] (new_items) ~expect:[0;1;2;3;4;5;];
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:2 ~move_to:3 in
        [%test_result: int list] (new_items) ~expect:[0;1;2;3;4;5;];
        let new_items = Drag_test.move_to ~items:[0;1;2;3;4;5] ~item:2 ~move_to:4 in
        [%test_result: int list] (new_items) ~expect:[0;1;3;2;4;5;];
        ()
end)
