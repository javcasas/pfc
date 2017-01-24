open Mat;;

let v1=new vector4 [|1.0;3.0;2.0;4.0|];;
let v2=new vector [|4.0;2.0;3.0;1.0|];;

let suma=v1#suma v2;;

print_float suma#longitud;;
