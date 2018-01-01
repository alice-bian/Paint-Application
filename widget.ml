(** A library of widgets for building GUIs. *)
(************)
(** Widgets *)
(************)

(** A widget is an object that provides three services:
    - it can repaint itself (given an appropriate graphics context)
    - it can handle events
    - it knows its dimensions
*)
type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

(************************)
(** {1 Layout Widgets } *)
(************************)

(** The simplest widget just occupies space *)
let space (p: Gctx.dimension) : widget =
  { repaint = (fun _ -> ());
    handle = (fun _ _ -> ());
    size = (fun _ -> p);
  }

(** Adds a one-pixel border to an existing widget. *)
let border (w: widget) : widget =
  { repaint = (fun (g: Gctx.gctx) ->
        let (width, height) = w.size () in
        let x = width + 3 in    (* not + 4 because we start at 0 *)
        let y = height + 3 in
        Gctx.draw_line g (0,0) (x,0);
        Gctx.draw_line g (0,0) (0, y);
        Gctx.draw_line g (x,0) (x, y);
        Gctx.draw_line g (0, y) (x, y);
        let g = Gctx.translate g (2,2) in
        w.repaint g);

    handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
            w.handle (Gctx.translate g (2,2)) e);

    size = (fun () ->
            let (width, height) = w.size () in
            width + 4, height + 4);
  }

(* Determines whether a given event is within a region of a widget whose*)
(* upper-left hand corner is (0,0) with width w and height h.           *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally.  They
    are aligned at their top edge. *)
let hpair (w1:widget) (w2:widget) : widget = {
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (fst (w1.size ()), 0) in
      w2.repaint g);
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (fst (w1.size ()), 0)) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
  size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (x1 + x2, max y1 y2))
}

(** The vpair widget lays out two widgets vertically.  They
    are aligned at their left edge. *)
let vpair (w1: widget) (w2: widget) : widget = {
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (0, snd (w1.size ())) in
      w2.repaint g);
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (0, snd (w1.size ()))) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
  size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (max x1 x2, y1 + y2))
}

(* Note: the OCaml List module has a function fold_right (List.fold_right).
   Note the order of the arguments (which is different
   from previous homeworks).
   Also, you will see that there is a fold_left function, you
   may want to think about what this does, and how it's different
   from the fold you're used to.
*)

(* Takes a pair layout function (such as hpair or vpair) and a list of     *)
(* widgets as arguments, and uses the pair layout function to arrange the  *)
(* wigdets in order.                                                       *)
let list_layout
    (pair: widget -> widget -> widget)
    (ws: widget list) : widget =
  List.fold_right pair ws (space (0, 0))

(* Creates horizontal widget list. *)
let hlist (ws: widget list) : widget = 
  list_layout hpair ws

(* Creates vertical widget list. *)
let vlist (ws: widget list) : widget = 
  list_layout vpair ws


(*****************************)
(** {1    Label Widgets    } *)
(*****************************)

(* Throughout the paint program, we will find the need to associate some value
   with a widget, and also to provide a way to update that value. The mechanism
   for this is called the "value_controller", which is generic to accomodate
   values of different types.

  Because both the widget and the controller share the same, mutable value,
  the constructor must create both together. For label widgets, the value
  we're dealing with is of type string. *)

(** A record of functions that allows us to read and write the string
    associated with a label. *)
type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }
 
(** Construct a label widget and its controller. *)
let label (s: string) : widget * label_controller =
  let r = { contents = s } in
  { repaint = (fun (g: Gctx.gctx) ->
            Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },
  {
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }

(*****************************************)
(** {1   Event Listeners and Notifiers } *)
(*****************************************)

(** An event listener processes events as they "flow" through the widget
    hierarchy. *)

type event_listener = Gctx.gctx -> Gctx.event -> unit

(* Below we define two special forms of event_listeners. *)

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()


(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
      begin match Gctx.event_type e with
        | Gctx.KeyPress key -> action key
        | _ -> ()
      end

(** A notifier_controller is associated with a notifier widget.
    It allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

(** A notifier widget is a widget "wrapper" that doesn't take up any
    extra screen space -- it extends an existing widget with the
    ability to react to events. It maintains a list of of "listeners"
    that eavesdrop on the events propagated through the notifier
    widget.

    When an event comes in to the notifier, it is passed to each
    event_listener in turn, and then passed to the child widget. *)
let notifier (w: widget) : widget * notifier_controller =
  let listeners = { contents = [] } in
  { repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
          List.iter (fun h -> h g e) listeners.contents;
          w.handle g e);
    size = w.size
  },
  { add_event_listener =
      fun (newl: event_listener) ->
          listeners.contents <- newl :: listeners.contents
  }

(*****************************************)
(** {1   Button                        } *)
(*****************************************)

(** A button has a string, which can be controlled by the
    corresponding value_controller, and an event listener, which can be
    controlled by the notifier_controller to add listeners (e.g. a
    mouseclick_listener) that will perform an action when the button is
    pressed. *)
let button (s: string)
    : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)

(*****************************************)
(** {1   Canvas                        } *)
(*****************************************)

(** A Canvas is a bordered widget with a notifier. New event listeners
    can be added by the notifier_controller.
    The repaint method of a canvas is a parameter of the widget constructor. *)
let canvas (dim: Gctx.dimension) (paint : Gctx.gctx -> unit)
    : widget * notifier_controller =
  let w =
    { repaint = paint;
      handle = (fun _ _ -> ());
      size = (fun _ -> dim) }
  in
  notifier (border w)

(*****************************************)
(** {1   Checkbox                      } *)
(*****************************************)

(** A checkbox is a controller for a value associated with a widget.

    This controller can read and write the value. It also allows
    change listeners to be registered by the application. These listeners are
    run whenever this value is set. *)
type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  set_value           : 'a -> unit
}

(** This generic function takes a value of type 'a and returns a value controller *)
(*  for it. *)
let make_control (v: 'a) : 'a value_controller = 
  let val_cont = { contents = [] } in 
    (* list of functions that will be performed on x *)
  
  let val_on = ref v in (* value that functions are acting on *)
  {
    add_change_listener = (fun x -> (* adding function into controller *)
      val_cont.contents <- ([x] @ val_cont.contents));
    get_value = (fun () -> !val_on);
    set_value = (fun x -> val_on.contents <- x;
                 List.fold_right (fun f _ -> (f x)) val_cont.contents ());
  }


(* Implements generic Checkbox widget. First argument bool indicates the  *)
(* initial state of the checkbox. Second argument string indicates the    *)
(* string for the label of the checkbox. Note that the bool               *)
(* value_controller, not the widget, is what actually tracks whether the  *)
(* checkbox is checked. When Checkbox is toggled, an "X" is drawn in the  *)
(* box.                                                                   *)
let checkbox (init: bool) (s: string)
    : widget * bool value_controller =
  let c = make_control init in
  let w = { repaint = (fun (g: Gctx.gctx) ->
                         Gctx.draw_line g (0, 0) (0, 20);
                         Gctx.draw_line g (0, 20) (20, 20);
                         Gctx.draw_line g (20, 20) (20, 0);
                         Gctx.draw_line g (20, 0) (0, 0);
                         Gctx.draw_string g (25, 0) s;
                         if c.get_value ()
                         then (Gctx.draw_line g (0, 0) (20, 20);
                               Gctx.draw_line g (20, 0) (0, 20))
                         else ());
            handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
                        if event_within g e (20, 20) && 
                           Gctx.event_type e = Gctx.MouseDown
                        then c.set_value (not (c.get_value())) 
                        else () );
            size = (fun () ->
                       (fst (Gctx.text_size s) + 25), 
                       (snd (Gctx.text_size s)));
           } in
  (w, c)

(*****************************************)
(** {1   Additional widgets            } *)
(*****************************************)

(* Implements generic slider widget. First argument int indicates the   *)
(* initial state of the slider. Second argument string indicates the    *)
(* string for the label of the checkbox. This function creates a 10 by  *)
(* 100 pixel box that fills up only as a user drags his or her mouse    *)
(* across the box. The amount of the box filled up corresponds to the   *)
(* current value within a range, such as a pen stroke's thickness.      *)
let slider (init: int) (s: string)
    : widget * int value_controller =
  let c = make_control init in
  let w = { repaint = (fun (g: Gctx.gctx) ->
                         Gctx.draw_line g (0, 0) (0, 10);
                         Gctx.draw_line g (0, 10) (100, 10);
                         Gctx.draw_line g (100, 10) (100, 0);
                         Gctx.draw_line g (100, 0) (0, 0);
                         Gctx.draw_string g (105, 0) s;
                         Gctx.fill_rect g (0, 10) (c.get_value(), 10));
            handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
                        if event_within g e (100, 10) &&
                           Gctx.event_type e = Gctx.MouseDrag
                        then let (x, y) = Gctx.event_pos e g in
                             c.set_value x
                        else ());
            size = (fun () -> (100, 10));
           } in
  (w, c)



