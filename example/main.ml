open Core_kernel

module Some_list = struct

    type item = int [@@deriving compare, sexp, yojson]

    let size = 20.0

    let is_equal (l0:item) (l1:item) =
        l0 = l1

end

module Layer_list = Orderable_list_js.Vertical(Some_list)

module Render = struct
    open Js_of_ocaml
    open Virtual_dom
    open Virtual_dom.Tyxml.Html

    let render_item i =
        div 
            ~a:[ a_style "background-color: white; padding-left: 10px; padding-right: 10px;" ]
            [ txt (Printf.sprintf "Int : %d" i) ]

    let render_list ~domain_data ~ui_data ~update_items ~update_self =
        let view = Layer_list.render 
            ui_data 
            ~items:domain_data 
            ~z:1
            ~render_item:render_item
            ~update_items
            ~update_self
        in
        view
    
    let render ~domain_data ~ui_data ~update_items ~update_self =
        body [
            render_list ~domain_data ~ui_data ~update_items ~update_self
        ]

    let main = 

        (* initial render *)

        let domain_data = [ 1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20; ] in
        let ui_data = Layer_list.Abs.empty in
        let update_items _ = () in
        let update_self _ = () in

        let tyxml_elt = render ~domain_data ~ui_data ~update_items ~update_self in
        let vdom_elt = Virtual_dom.Tyxml.Html.toelt tyxml_elt in
        let dom_elt = Vdom.Node.to_dom vdom_elt in

        Dom_html.document##.body := dom_elt;

        (* ui controller loop *)

        let domain_data = ref domain_data in
        let ui_data = ref ui_data in
        let prev_node = ref vdom_elt in
        let elt = ref dom_elt in

        let rec loop _ = 

            let tyxml_elt = 
                render 
                ~domain_data:!domain_data
                ~ui_data:!ui_data
                ~update_items:(fun items -> domain_data := items; loop ())
                ~update_self:(fun thunk -> ui_data := thunk !ui_data; loop ())
            in
            let vdom_elt = Virtual_dom.Tyxml.Html.toelt tyxml_elt in
            let new_node = vdom_elt in

            let patch = Vdom.Node.Patch.create ~previous:!prev_node ~current:new_node in
            let new_elt = Vdom.Node.Patch.apply patch !elt in
            elt := new_elt;
            prev_node := new_node;
            
            ()
        in

        loop ()

end


