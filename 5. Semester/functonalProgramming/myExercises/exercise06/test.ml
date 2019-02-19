
type quadtree_node = NoPoint 
                   | Point of int * int
                   | QNode of quadtree_node (* bottom left *)
                            * quadtree_node (* top left *)
                            * quadtree_node (* bottom right *)
                            * quadtree_node (* top right *)
type quadtree = { width:int; height:int; root:quadtree_node }
let print_quadtree filename qtree =
  let file = open_out filename in 
  let rec impl (x1, y1, x2, y2) = function NoPoint -> ()
    | Point (x,y) -> Printf.fprintf file "<circle cx=\"%d\" cy=\"%d\" r=\"1\" fill=\"black\"/>\n" x (qtree.height - y)
    | QNode (nn, np, pn, pp) -> 
      let xmid = (x1 + x2) / 2 in 
      let ymid = (y1 + y2) / 2 in 
      Printf.fprintf file "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n" 
        x1 (qtree.height-ymid) x2 (qtree.height-ymid);
      Printf.fprintf file "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n" 
        xmid (qtree.height -y1) xmid (qtree.height-y2);
      impl (x1, y1, xmid, ymid) nn; 
      impl (x1, ymid, xmid, y2) np; 
      impl (xmid, y1, x2, ymid) pn; 
      impl (xmid, ymid, x2, y2) pp
  in
  Printf.fprintf file "<?xml version=\"1.0\" standalone=\"no\"?>\n
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n
    <svg viewBox = \"0 0 %d %d\">\n
    <rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"white\"/>\n" qtree.width qtree.height
    qtree.width qtree.height;
  impl (0, 0, qtree.width, qtree.height) qtree.root;
  Printf.fprintf file "</svg>";
  close_out file;;

let x = Point (3,3);;
let z = { width=16; height=16; root=x };;
let y = "aaa";;
print_quadtree y z;;

let where p k = match p,k with 
                      | (a,b),(x,y,x',y') -> if (a < ((x+x')/2))
                        then (
                            if (b < ((y+y')/2))
                            then 1
                            else 2 )
                        else (
                            if (b < ((y+y')/2))
                            then 3
                            else 4 );;

let rec insert_help p q k = match p,k with 
                      | (a,b),(x,y,x',y') -> match q with
                          | NoPoint -> Point (a,b)
                          | Point (l,m) -> (if (a,b) = (l,m)
                                            then Point (l,m)
                                            else match (where (l,m) k),(where (a,b) k) with
                                                | 1,1 -> QNode ((insert_help p q (x,y,((x+x')/2),((y+y')/2))),NoPoint,NoPoint,NoPoint)
                                                | 1,2 -> QNode ((Point (l,m)),Point (a,b),NoPoint,NoPoint)
                                                | 1,3 -> QNode ((Point (l,m)),NoPoint,Point (a,b),NoPoint)
                                                | 1,4 -> QNode ((Point (l,m)),NoPoint,NoPoint,Point (a,b))
                                                | 2,1 -> QNode (Point (a,b),(Point (l,m)),NoPoint,NoPoint)
                                                | 2,2 -> QNode (NoPoint,(insert_help p q (x,((y+y')/2),((x+x')/2),y')),NoPoint,NoPoint)
                                                | 2,3 -> QNode (NoPoint,(Point (l,m)),Point (a,b),NoPoint)
                                                | 2,4 -> QNode (NoPoint,(Point (l,m)),NoPoint,Point (a,b))
                                                | 3,1 -> QNode ((Point (a,b)),NoPoint,(Point (l,m)),NoPoint)
                                                | 3,2 -> QNode (NoPoint,Point (a,b),(Point (l,m)),NoPoint)
                                                | 3,3 -> QNode (NoPoint,NoPoint,(insert_help p q (((x+x')/2),y,x',((y+y')/2))),NoPoint)
                                                | 3,4 -> QNode (NoPoint,NoPoint,(Point (l,m)),Point (a,b))
                                                | 4,1 -> QNode (Point (a,b),NoPoint,NoPoint,(Point (l,m)))
                                                | 4,2 -> QNode (NoPoint,Point (a,b),NoPoint,(Point (l,m)))
                                                | 4,3 -> QNode (NoPoint,NoPoint,Point (a,b),(Point (l,m)))
                                                | _,_ -> QNode (NoPoint,NoPoint,NoPoint,(insert_help p q (((x+x')/2),((y+y')/2),x',y'))) )
                          | QNode (uL,oL,uR,oR) -> match where (a,b) k with
                                                | 1 -> QNode ((insert_help p uL (x,y,((x+x')/2),((y+y')/2))),oL,uR,oR)
                                                | 2 -> QNode (uL,(insert_help p oL (x,((y+y')/2),((x+x')/2),y')),uR,oR)
                                                | 3 -> QNode (uL,oL,(insert_help p uR (((x+x')/2),y,x',((y+y')/2))),oR)
                                                | _ -> QNode (uL,oL,uR,(insert_help p oR (((x+x')/2),((y+y')/2),x',y')));;

let insert p qt = {width = qt.width; height=qt.height; root=(insert_help p (qt.root) (0,0,qt.width,qt.height))};;

let a66_t = { width=16; height=16; root=NoPoint };;
let insert_points = List.fold_left (fun t p -> insert p t) a66_t;;

print_endline "hiiiiier ";


(insert_points [8,2;8,12]).root
let x = QNode (NoPoint, NoPoint, Point (8,2), Point (8, 12));;
(insert_points [8,8;0,0;8,8]).root
let x = QNode (Point (0,0), NoPoint, NoPoint, Point (8, 8));;

(*
(insert_points [5,5]).root;;
let x = Point (5,5);;
(insert_points [5,5;5,5]).root;;
let x = Point (5,5);;
(insert_points [4,4;12,12]).root
let x = QNode (Point (4,4), NoPoint, NoPoint, Point (12,12));;
(insert_points [4,4;4,12;12,12]).root
let x = QNode (Point (4,4), Point (4, 12), NoPoint, Point (12, 12));;
(insert_points [6,6;2,2]).root
let x = QNode (QNode (Point (2,2), NoPoint, NoPoint, Point (6,6)), NoPoint, NoPoint, NoPoint);;
(insert_points [2,14;6,11;11,2;14,6]).root
let x = QNode (NoPoint, QNode (NoPoint, Point (2,14), Point (6, 11), NoPoint), QNode (Point (11,2), NoPoint, NoPoint, Point(14,6)), NoPoint);;

*)