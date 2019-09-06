open! Core_kernel

module type S = Orderable_list_abs.S

module Vertical
(S : S)
= struct

    module Abs = Orderable_list_abs.Vertical(S)

    open Abs

    open Js_of_ocaml
    open Virtual_dom
    open Virtual_dom.Tyxml.Html

    let rec get_offset (t : Dom_html.element Js.t Js.opt) =
        let (x,y) = (Js.Opt.get (Js.Opt.map t (fun t -> get_offset t##.offsetParent)) (fun _ -> (0., 0.))) in
        let ox = float_of_int (Js.Opt.get (Js.Opt.map t (fun t -> t##.offsetLeft)) ( (fun _ -> 0))) in
        let oy = float_of_int (Js.Opt.get (Js.Opt.map t (fun t -> t##.offsetTop)) ( (fun _ -> 0))) in
        (x +. ox, y +. oy)

    let render t ~items ~z ~render_item ~update_items ~update_self =

        let items_and_offsets = match t with
            | Dragging d -> 
                let deltas = List.mapi items ~f:(fun i c -> 
                    if S.is_equal c d.target 
                    then
                    (
                        c,
                        0.,
                        0.
                    )
                    else
                    (
                        c,
                        0.,
                        get_offset_from_update ~move_from:d.initial ~move_to:d.current ~index:i
                    ))
                in
                deltas
            | _ -> 
                List.map items ~f:(fun c -> (c, 0., 0.))
        in

        let render_cell i (item, dx, dy) =
            match t with
            | Waiting
            | Issue _ ->
                div 
                ~a:[
                    a_style (Printf.sprintf "margin: 1px; position:relative; display:block; height:%2fpx; transform:translate(%2fpx, %2fpx); transition: transform 0.0s; display: flex; flex-direction: row;" (S.size -. 2.) dx dy);
                ]
                [
                    div ~a:[ 
                        a_style (Printf.sprintf "display:block; width: 40px; height: %2fpx; background-color:#EEE;" (S.size -. 2.0));
                        a_onmousedown (fun e ->
                            (* let tar : Dom_html.element Js.t Js.opt = e##.target in *)
                            let (ox, oy) = get_offset e##.currentTarget in
                            let px = float_of_int (Js.Optdef.get (e##.pageX) (fun _ -> 0)) in
                            let py = float_of_int (Js.Optdef.get (e##.pageY) (fun _ -> 0)) in
                            let dx = px -. ox in
                            let dy = py -. oy in
                            update_self (fun _ -> start_index ~items ~index:i ~offset_x:dx ~offset_y:dy);
                            Vdom.Event.Prevent_default
                        )
                    ] [];
                    div [ render_item item ];
                ]
            | Dragging d when S.is_equal item d.target ->
                div 
                ~a:[
                    a_style (Printf.sprintf "margin: 1px; position:relative; z-index:%d; display:block; height:%2fpx; transform:translate(%2fpx, %2fpx); transition: transform 0.1s; display: flex; flex-direction: row;" z (S.size -. 2.) dx dy);
                ]
                [
                    div ~a:[ a_style (Printf.sprintf "pointer-events: none; display:block; width: 40px; height: %2fpx;" (S.size -. 2.0)) ] [];
                    div [];
                ]
            | Dragging _ ->
                div 
                ~a:[
                    a_style (Printf.sprintf "margin: 1px; position:relative; z-index:%d; display:block; height:%2fpx; transform:translate(%2fpx, %2fpx); transition: transform 0.1s; display: flex; flex-direction: row;" z (S.size -. 2.) dx dy);
                ]
                [
                    div ~a:[ a_style (Printf.sprintf "pointer-events: none; display:block; width: 40px; height: %2fpx; background-color:#EEE;" (S.size -. 2.0)) ] [];
                    div [ render_item item ];
                ]
        in

        let height = S.size *. (float_of_int (List.length items)) in

        (*
        let ondown e = 
            (* let tar : Dom_html.element Js.t Js.opt = e##.target in *)
            let ox = float_of_int (Js.Opt.get (Js.Opt.map (e##.currentTarget) (fun t -> t##.offsetLeft)) ( (fun _ -> 0))) in
            let oy = float_of_int (Js.Opt.get (Js.Opt.map (e##.currentTarget) (fun t -> t##.offsetTop)) ( (fun _ -> 0))) in
            let px = float_of_int (Js.Optdef.get (e##.pageX) (fun _ -> 0)) in
            let py = float_of_int (Js.Optdef.get (e##.pageY) (fun _ -> 0)) in
            let dx = px -. ox in
            let dy = py -. oy in
            update_self (fun _ -> start ~items ~x:dx ~y:dy);
            Vdom.Event.Prevent_default
        in
        *)
        match t with
        | Dragging d ->

            let target = 
                let dx = d.delta_x -. d.offset_x in
                let dy = d.delta_y -. d.offset_y in
                [ (
                    div
                    ~a:[
                        a_style (Printf.sprintf "position:absolute; z-index:%d; left:0; top:0; display: block; height:%2fpx; transform:translate(%2fpx, %2fpx); transition: transform 0.0s; display: flex; flex-direction: row;" (z + 1) S.size dx dy);
                    ]
                    [
                        div ~a:[ a_style (Printf.sprintf "pointer-events: none; display:block; width: 40px; height: %2fpx; background-color:#EEE;" S.size) ] [];
                        div [ render_item d.target ];
                    ]
                ) ]
            in
            let onmove e =
                (* let tar : Dom_html.element Js.t Js.opt = e##.target in *)
                let (ox, oy) = get_offset e##.currentTarget in
                let px = float_of_int (Js.Optdef.get (e##.pageX) (fun _ -> 0)) in
                let py = float_of_int (Js.Optdef.get (e##.pageY) (fun _ -> 0)) in
                let dx = px -. ox in
                let dy = py -. oy in
                update_self (fun t -> drag t ~items ~x:dx ~y:dy);
                Vdom.Event.Prevent_default
            in
            let onup e =
                (* let tar : Dom_html.element Js.t Js.opt = e##.target in *)
                let (ox, oy) = get_offset e##.currentTarget in
                let px = float_of_int (Js.Optdef.get (e##.pageX) (fun _ -> 0)) in
                let py = float_of_int (Js.Optdef.get (e##.pageY) (fun _ -> 0)) in
                let dx = px -. ox in
                let dy = py -. oy in
                let new_t = drag t ~items ~x:dx ~y:dy in
                let items = get_new_items new_t ~items in
                update_self (fun t -> release t);
                update_items (get_new_items new_t ~items);
                Vdom.Event.Prevent_default
            in
            (*
            let onout _ =
                update_self (fun t -> cancel t);
                Vdom.Event.Prevent_default
            in
            *)
            div 
            ~a:[
                a_style (Printf.sprintf "position: relative; height:%2fpx; display:flex; flex-direction:column; cursor:pointer;" height);
                a_onmousemove onmove;
                a_onmouseup onup;
                (* a_onmouseout onout; *)
            ]
            (target @ (List.mapi items_and_offsets ~f:render_cell))
        | _ ->
            div 
            ~a:[ a_style (Printf.sprintf "position: relative; height:%2fpx; display:flex; flex-direction:column; cursor:pointer;" height); ]
            (List.mapi items_and_offsets ~f:render_cell)

    
end
