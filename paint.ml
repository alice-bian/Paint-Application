(** Paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES and PROGRAM STATE    *)
(******************************************)

(** The paint program uses the mutable record (called [state] below)
to store its state.  *)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). 
    Available shapes include a line, point, and ellipse. The Point shape 
    allows users to draw a series of points at once by dragging their mouse 
    while holding down the button. After mouse release, all drawn points 
    are added to the deque of shapes. The Ellipse shape allows users to draw
    an ellipse by dragging their mouse while holding down the button. After 
    mouse release, ellipse is added to the deque of shapes. *)
type shape = 
  | Line of color * thickness * point * point
  | Points of Gctx.color * point list
  | Ellipse of Gctx.color * Gctx.thickness * point * point

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    This program includes the following modes:

    - LineStartMode means the paint program is waiting for the user to make
      the first click to start a line.

    - LineEndMode means that the paint program is waiting for the user's
      second click. The point associated with this mode stores the location of
      the user's first mouse click.
    
    - PaintMode constructor distinguishes when user wants to draw lines versus 
      points.
      
    - EllipseStartMode means the paint program is waiting for the user to make
      the first click to start an ellipse.
    
    - EllipseEndMode means the paint program is waiting for the user's 
      mouse release. The point associated with this mode stores the location of
      the user's original mouse click. *)
      
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
  least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;
  
  (** Keeps track of shape currently being previewed. *)
  mutable preview : shape option;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;
  
  (** The currently selected pen thickness. *)
  mutable thickness : thickness;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  preview = None;
  mode = LineStartMode;
  color = black;
  thickness = thin;
}



(** This function creates a graphics context with the appropriate
    pen color and thickness.
*)
let with_params (g: gctx) (c: color) (t: thickness) : gctx =
  let g = with_color g c in
  let g = with_thickness g t in
  g

(*********************************)
(** PAINT CANVAS REPAINTING      *)
(*********************************)
(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas. In addition, the repaint function:
    
    - Draws the previewed item last.
    
    - When shape matches with Points (c, ps), it calls the Gctx.draw_points
      function and draws each point in ps on the canvas. Also sets color and
      thickness in the graphics context.
    
    - When shape matches with Ellipse (c, (x1, y1), (x2, y2)), it uses the 
      "bounding box" method to draw an ellipse within the rectangle formed 
      by opposite corners (x1, y1) and (x2, y2). Calls on Gctx.draw_ellipse 
      function and draws final ellipse on the canvas. Also sets color and 
      thickness in the graphics context.
    *)
    
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line (c, t, p1, p2) -> draw_line (with_params g c t) p1 p2
      | Points (c, ps) -> draw_points (with_params g c thin) ps
      | Ellipse (c, t, (x1, y1), (x2, y2)) -> 
          let rx = abs(x2 - x1)/2 in
          let ry = abs(y2 - y1)/2 in
          draw_ellipse (with_params g c t) (abs(x1 + x2)/2, abs(y1 + y2)/2) rx ry
    end in
  Deque.iterate draw_shape paint.shapes;
  
  begin match paint.preview with
    | Some x -> begin match x with
                | Line (c, t, p1, p2) -> draw_line (with_params g c t) p1 p2
                | Points (c, ps) -> draw_points (with_params g c thin) ps
                | Ellipse (c, t, (x1, y1), (x2, y2)) -> 
                    let rx = abs(x2 - x1)/2 in
                    let ry = abs(y2 - y1)/2 in
                    draw_ellipse (with_params g c t) (abs(x1 + x2)/2, abs(y1 + y2)/2) rx ry
                end
    | None -> ()
  end

(** Create the actual paint_canvas widget and its associated
notifier_controller. *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur 
    in the canvas region. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
    (* This case occurs when the mouse has been clicked in the canvas, but *)
    (* before the button has been released. How we process the event       *)
    (* depends on the current mode of the paint canvas.                    *)
        (begin match paint.mode with 
            | LineStartMode ->
                (* The paint_canvas was waiting for the first click of a line,   *)
                (* so change it to LineEndMode, recording the starting point of  *)
                (* the line.                                                     *)
                paint.mode <- LineEndMode p
            | LineEndMode p1 ->
                (* Nothing happens in this case because the line now ends when a *)
                (* user releases the mouse button. We are not waiting for a      *)
                (* second mouse click to create the line because the line is     *)
                (* created in the MouseUp case (see below).                      *)
                ()
            | PointMode -> let ps = [p] in
                             paint.preview <- 
                               Some (Points (paint.color, ps))
                (* Creates and stores a new Points (c, ps) in paint.preview.     *)
                (* Points (c, t, ps) stores current color c of the graphics      *)
                (* context and creates a list of points ps which momentarily     *)
                (* only contains a single point at the current location of the   *)
                (* Gctx.MouseDown event.                                         *)
            | EllipseStartMode -> 
                (* The paint_canvas was waiting for the click of an ellipse, so  *)
                (* change it to EllipseEndMode, recording the starting point of  *)
                (* the ellipse.                                                  *)
                paint.mode <- EllipseEndMode p
            | EllipseEndMode p1 -> ()
                (* Nothing happens in this case because the ellipse ends when a  *)
                (* user releases the mouse button.                               *)
          end)
    | MouseDrag ->
    (* In this case, the mouse has been clicked, and it's being dragged    *)
    (* with the button down. Sets paint.preview to some Line with two      *)
    (* points (where the mouse was first clicked and where it is now) when *)
    (* user drags the mouse and is in LineEndMode.                         *)
    (* If in PointMode, updates preview to include that point as part of   *)
    (* existing Points constructor.                                        *)
    (* Note that it is not possible to be in EllipseStartMode based on our *)
    (* implementation of MouseDown (thus, we return unit in that case). If *)
    (* in EllipseEndMode, updates preview to make that ellipse a part of   *)
    (* the existing Ellipse constructor.                                   *)
        (begin match paint.mode with 
            | LineStartMode -> ()
            | LineEndMode p1 ->
                paint.preview <- 
                  Some (Line (paint.color, paint.thickness, p1, p))
            | PointMode -> let points =
                             begin match paint.preview with
                             | Some (Points (_, ps)) -> p :: ps
                             | _ -> []
                             end in
                           paint.preview <- Some (Points (paint.color, points))
            | EllipseStartMode -> ()
            | EllipseEndMode p1 ->
                paint.preview <- 
                  Some (Ellipse (paint.color, paint.thickness, p1, p))
          end)

    | MouseUp ->
    (* In this case there was a mouse button release event.                *)
    (* First, removes line preview. A line is then drawn from the initial  *)
    (* position to the point of release, and it is added to paint.shapes.  *)
    (* Also resets paint.mode back to LineStartMode so that user can draw  *)
    (* another line.                                                       *)
    (* Extract list of points from paint.preview (if present) and then     *)
    (* clears preview. If list of points is nonempty, then adds a Points   *)
    (* shape into deque.                                                   *)
    (* Extract ellipse from paint.preview (if present) during              *)
    (* EllipseEndMode, and then clears preview. Adds Ellipse shape into    *)
    (* deque.                                                              *)
        (begin match paint.mode with 
            | LineStartMode -> ()
            | LineEndMode p1 ->
                paint.preview <- None;
                Deque.insert_tail 
                  (Line (paint.color, paint.thickness, p1, p)) paint.shapes;
                paint.mode <- LineStartMode
            | PointMode -> begin match paint.preview with
                           | Some (Points (_, ps)) -> 
                               if ps = [] then () 
                               else Deque.insert_tail 
                                 (Points (paint.color, ps)) paint.shapes;
                               paint.preview <- None
                           | _ -> ()
                           end
            | EllipseStartMode -> ()
            | EllipseEndMode p1 -> 
                paint.preview <- None;
                Deque.insert_tail 
                  (Ellipse (paint.color, paint.thickness, p1, p)) paint.shapes;
                paint.mode <- EllipseStartMode
          end)
	 
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over *) 
    (* the canvas without pushing any buttons) and the KeyPress event (where *)
    (* the user typed a key when the mouse was over the canvas).             *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action

(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(**
This part of the program creates the other widgets for the
paint program -- the buttons, color selectors, etc., and
lays them out in the top - level window.

*)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(**
This function runs when the Undo button is clicked. It simply removes the last 
shape from the shapes deque. Resets the paint.preview shape to None. If paint.mode
is LineEndMode, then resets it to LineStartMode to prevent strange behaviors when 
user starts drawing a line, drags mouse off canvas, and clicks Undo button.*)
let undo () : unit =
  paint.preview <- None;
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes);
  begin match paint.mode with
  | LineStartMode -> ()
  | LineEndMode _ -> paint.mode <- LineStartMode
  | PointMode -> ()
  | EllipseStartMode -> ()
  | EllipseEndMode _ -> paint.mode <- EllipseStartMode
  end
;; nc_undo.add_event_listener (mouseclick_listener undo)

(** The Quit button, with associated functionality. *)
let w_quit, lc_quit, nc_quit = button "Quit"
;; nc_quit.add_event_listener (mouseclick_listener (fun () -> exit 0))

(** A spacer widget *)
let spacer : widget = space (10,10)

(** Create the Point button, with associated functionality. *)
let (w_point, lc_point, nc_point) = button "Point"
;; nc_point.add_event_listener 
     (mouseclick_listener (fun () -> paint.mode <- PointMode))

(** Create the Line button, with associated functionality. *)
let (w_line, lc_line, nc_line) = button "Line"
;; nc_line.add_event_listener 
     (mouseclick_listener (fun () -> paint.mode <- LineStartMode))

(** Create the Ellipse button, with associated functionality. *)
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"
;; nc_ellipse.add_event_listener 
     (mouseclick_listener (fun () -> paint.mode <- EllipseStartMode))

(* CHECKBOX FOR THICKNESS *)
(* Sets the paint thickness to 5 pixels if user checks the checkbox. *)
(* Otherwise, sets the paint thickness to the default 1 pixel.       *)
let (w_checkbox, vc_checkbox) = checkbox false "Thick lines"
;; vc_checkbox.add_change_listener (fun x -> 
     if x then paint.thickness <- thick else paint.thickness <- thin)

(* SLIDER FOR THICKNESS *)
(* This slider is only activated when the thickness Checkbox is    *)
(* checked. Note that the stroke value set to paint.thickness is   *)
(* scaled such that the paint strokes are within a reasonable and  *)
(* aesthetic range of thickness.                                   *)
let (w_slider, vc_slider) = slider 1 "Thickness Slider"
;; vc_slider.add_change_listener (fun x -> if vc_checkbox.get_value() 
                                           then let stroke = {t = x/5} in
                                                    paint.thickness <- stroke
                                           else ())

(* The mode toolbar uses Widget.hlist. Includes Point, Line, and Ellipse *)
(* buttons, as well as the thickness Checkbox and thickness Slider.      *)
let mode_toolbar : widget = 
  hlist [border w_point; spacer; 
         border w_line; spacer;  
         border w_ellipse; spacer; 
         border w_checkbox; spacer;
         border w_slider]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
    : widget * notifier_controller =
  let repaint_square (gc:gctx) =
	 let c = get_color () in
    fill_rect (with_color gc c) (0, width-1) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
   color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
   with. They are also installed with a mouseclick listener
   that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
    paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
let color_toolbar : widget =
  hlist [color_indicator; spacer; (color_button black); spacer;
         (color_button white); spacer; (color_button red); spacer;
         (color_button green); spacer; (color_button blue); spacer;
         (color_button blue); spacer; (color_button yellow); spacer;
         (color_button cyan); spacer; (color_button magenta); spacer;
         border w_undo; spacer; border w_quit]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
let paint_widget =
  vlist [paint_canvas; spacer; mode_toolbar; spacer; color_toolbar]


(**************************************)
(** Start the application             *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
