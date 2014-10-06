open Graphics;;
open Complex;;

print_endline "Left side of box:"
let left = read_float();;

print_endline "Bottom of box:"
let bottom = read_float();;

print_endline "Width of box:"
let width = read_float();;

let right = left   +. width;;
let top   = bottom +. width *. 2.0 /. 3.0;; 

open_graph " 1200x800";;

let thresh = 2.0 ;;
let two = {re=2.0 ; im = 0.0}
let max_iter = 255

let rec approx_diverge c value iter = 
  if (norm2 value) > thresh || iter >= max_iter then iter
  else approx_diverge c (add (pow value two) c) (iter+1);;

for x = 0 to 1199 do
  let real = left +. ((right-.left) *. float_of_int(x) /. 1200.0) in
  for y = 0 to 799 do
    let imag = bottom +. ((top-.bottom) *. float_of_int(y) /. 800.0) in
    let c = {re= real;im= imag} in

    let num = approx_diverge c c 0 in
 
    let color256 = int_of_float( 255.0 *.float_of_int(num) /. float_of_int(max_iter)) in
    let hexcolor = 16777216 - color256 * (1 + 256 + 65536) in
    Graphics.set_color hexcolor;
    Graphics.plot x y ;

  done
done;;

read_line();;
