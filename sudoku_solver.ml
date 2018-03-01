exception Dpll_error;;

(*
 * 同じ3*3ブロックに属するマスのリスト
 *)
let block =
  [[ 0; 1; 2; 9;10;11;18;19;20];
   [ 3; 4; 5;12;13;14;21;22;23];
   [ 6; 7; 8;15;16;17;24;25;26];
   [27;28;29;36;37;38;45;46;47];
   [30;31;32;39;40;41;48;49;50];
   [33;34;35;42;43;44;51;52;53];
   [54;55;56;63;64;65;72;73;74];
   [57;58;59;66;67;68;75;76;77];
   [60;61;62;69;70;71;78;79;80];
  ];;

(*
 * lsリストの中からnを抜いたリストを返す
 *)
let remove n ls = List.filter (fun x -> n <> x) ls;;


(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 確定しているマスのインデックス
 * num_decided : 確定しているマスの数字
 *
 * 確定マスと同じ行のマスから、確定された数字を候補から除外する。
 *)

let rec check_width num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> []
  | x::tl ->
     if n/9=index/9 && n<>index then 
       begin
         match x with
         | []  ->    raise Dpll_error
         | [y] ->    if (y=num_decided)
                     then raise Dpll_error
                     else x::(check_width num_decided n (index+1) tl)
         | y::tl2 -> [remove num_decided x]@(check_width num_decided n (index+1) tl)
         | _ -> failwith "error c w in"
       end
     else x::(check_width num_decided n (index+1) tl)
  | _ -> failwith "error c w out";;


(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 確定しているマスのインデックス
 * num_decided : 確定しているマスの数字
 *
 * 確定マスと同じ列のマスから、確定された数字を候補から除外する。
 *)
       
let rec check_height num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> []
  | x::tl ->
     if n mod 9=index mod 9  && n<>index then 
       begin
         match x with
         | []  ->    raise Dpll_error
         | [y] ->    if (y=num_decided)
                     then raise Dpll_error
                     else x::(check_height num_decided n (index+1) tl)
         | y::tl2 -> [remove num_decided x]@(check_height num_decided n (index+1) tl)
         | _ -> failwith "error c h in"
       end
     else x::(check_height num_decided n (index+1) tl)
  | _ -> failwith "error c h out";;


(*
 * インデックスnのマスが属するブロックリストを返す。
 * 例 : block_search 4 = [3;4;5;12;13;14;21;22;23]; 
 *)
let block_search n = List.(hd (filter (fun l -> exists (fun x -> x = n) l) block));;


(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 確定しているマスのインデックス
 * num_decided : 確定しているマスの数字
 *
 * 確定マスと属するブロック内のマスから、確定された数字を候補から除外する。
 *)

let rec check_block num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> []
  | x::tl ->
     if (List.exists (fun z -> z = index) (block_search n)) && n<>index  then 
       begin
         match x with
         | []  ->    raise Dpll_error
         | [y] ->    if (y=num_decided)
                     then raise Dpll_error
                     else x::(check_block num_decided n (index+1) tl)
         | y::tl2 -> [remove num_decided x]@(check_block num_decided n (index+1) tl)
         | _ -> failwith "error c b in"
       end
     else x::(check_block num_decided n (index+1) tl)
  | _ -> failwith "error c b out";;


(*
 * 与えられた数独候補リストの全要素が、数字が一つだけ格納されたリスト、
 * つまり、入る数字が確定されたマスであるときtrue, そうでないときfalseを返す。
 *)
let rec check_complete sudoku_ls =
  match sudoku_ls with
  | []  -> true
  | x::tl ->
     begin
       match x with
       | [y] -> check_complete tl
       | y::tl2 -> false
       | _ -> failwith "error c c"
     end
  | _ -> failwith "error c c";;



(*
 * 数独の形(9*9の数字行列)で表示する。
 * もし、確定していないマスの場合は0と表示する
 *)
let rec print_board n sudoku_ls =
  if n mod 27 = 0 then Printf.printf"\n";
  if n > 80 then ()
  else 
    match sudoku_ls with
    | []  -> ()
    | x::tl ->
       begin
         match x with
         | [y] ->
            if n mod 9 = 8 then
              let _ = (Printf.printf"%d\n" List.(hd (hd sudoku_ls))) in (print_board (n+1) tl)
                                                                         
            else if n mod 3 = 2 then
              let _ = (Printf.printf"%d   " List.(hd (hd sudoku_ls))) in (print_board (n+1) tl)
            else 
              let _ = (Printf.printf"%d " List.(hd (hd sudoku_ls))) in (print_board (n+1) tl)
         | y::tl2 ->
            if n mod 9 = 8 then
              let _ = (Printf.printf"0\n") in (print_board (n+1) tl)
            else
              let _ = (Printf.printf"0 ") in (print_board (n+1) tl)
       | _ -> failwith "error pb"
       end
    | _ -> failwith "error pb!";;


(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 検証しているマスのインデックス
 * num_decided : 検証している数字
 *
 * 検証している数字が、同行のマスで候補としているのが、
 * 検証しているマスのみの場合true、そうでない場合falseを返す
 *)
let rec verify_width num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> true
  | x::tl ->
     if (n/9=index/9) && (n <> index) then
       begin
         if (List.exists (fun z -> z = num_decided) x)
         then false 
         else verify_width num_decided n (index+1) tl
       end
     else verify_width num_decided n (index+1) tl
  | _ -> failwith "error v w";;


(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 検証しているマスのインデックス
 * num_decided : 検証している数字
 *
 * 検証している数字が、同列のマスで候補としているのが、
 * 検証しているマスのみの場合true、そうでない場合falseを返す
 *)
let rec verify_height num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> true
  | x::tl ->
     if (n mod 9=index mod 9) && (n <> index) then
       begin
         if (List.exists (fun z -> z = num_decided) x)
         then false 
         else verify_height num_decided n (index+1) tl
       end
     else verify_height num_decided n (index+1) tl
  | _ -> failwith "error v h";;



(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 検証しているマスのインデックス
 * num_decided : 検証している数字
 *
 * 検証している数字が、属しているブロックのマスで候補としているのが、
 * 検証しているマスのみの場合true、そうでない場合falseを返す
 *)
let rec verify_block num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> true
  | x::tl ->
     if (List.exists (fun z -> z = index) (block_search n)) && (n <> index) then
       begin
         if (List.exists (fun z -> z = num_decided) x)
         then false 
         else verify_block num_decided n (index+1) tl
       end
     else verify_block num_decided n (index+1) tl
  | _ -> failwith "error v b";;


(*
 * sudoku_ls   : 現在の数独候補リスト
 * index       : 現在参照しているマスのインデックス
 * n           : 確定しているマスのインデックス
 * num_decided : 確定しているマスの数字
 *
 * 検証してtrueが返ってきた場合、そのマスをその数字で確定する。
 *)
let rec change_decided_mass num_decided n index sudoku_ls =
  match sudoku_ls with
  | []    -> []
  | x::tl ->
     if n = index then
       num_decided::tl
     else x::(change_decided_mass num_decided n (index+1) tl)
  | _ -> failwith "error cdm";;



(*
 * sudoku_ls : 現在の数独候補リスト
 * mass_ls   : 検証しているマスの入る数字の候補リスト  
 * n         : 検証しているマスのインデックス
 *
 * 検証マスの候補リストから一つずつ検証していく
 *)
let rec check_board n mass_ls sudoku_ls=
  match mass_ls with
  | [] -> sudoku_ls
  | ls_hd::tl ->
     if  (verify_width ls_hd n 0 sudoku_ls) || (verify_height ls_hd n 0 sudoku_ls) || (verify_block ls_hd n 0 sudoku_ls)
     then change_decided_mass [ls_hd] n 0 sudoku_ls
     else check_board n tl sudoku_ls
       
  | _ -> failwith "error c b";;



(*
 * sudoku_ls     : 数独候補リストの前から(n-1)番目以降
 * sudoku_origin : 元々の数独候補リスト  
 * n             : 見ているマスのインデックス
 *
 * 候補が複数あるマスを適当に数字を確定させた数独候補リストと、
 * 仮確定する前の候補から選んだ数字を候補からのぞいた数独候補リストの組を返す。
 *)
let rec dpll n sudoku_ls sudoku_origin =
  match sudoku_ls with
  | []    -> ([],[])
  | x::tl ->
     begin
       match x with
       | [y] ->    dpll (n+1) tl sudoku_origin
       | y::tl2 -> 
                   ((change_decided_mass [y] n 0 sudoku_origin),
                        (change_decided_mass tl2 n 0 sudoku_origin))
       | _ -> failwith "error dpll in"
     end
  | _ -> Printf.printf"%d\n\n" n;
         print_board 0 sudoku_origin;
         print_string "\n";failwith "error dpll out";;
     
                        


(*
 * sudoku_ls     : 現在の数独候補リスト
 * n             : 参照しているマスのインデックス
 * sudoku_before : n = 0 時点での数独候補リスト
 * stack         : マスを仮確定した際に、仮確定マスの数独候補リストを格納するスタック
 *
 * 参照しているマスによって動作を変更する。
 * マスに入る数字が確定しているときは、そのマスの行・列・ブロックに属するマスから、
 * 確定している数字を候補から除去する。
 * 
 * マスに入る数字が確定していない場合は、そのマスの候補数字を検証して、
 * 同じブロックに属するマスの候補にない場合、数字を確定する。
 * 
 * 上記の方法ではsudoku_lsに変更がなくなった場合、マス一つを指定して、
 * そのマスの候補から一つを仮確定して、続ける。
 * その際、仮確定する前にstackに、指定したマスの候補リストから、
 * 仮確定した数字を候補から外したリスト仮確定する前の数独候補リストをストックしておく。
 * もし、仮確定が失敗したとわかったとき、この数独候補リストから再開することができる。
 *)

let rec check n sudoku_ls sudoku_before stack=
  if (check_complete sudoku_ls)
  then print_board 0 sudoku_ls
  else
    if n > 80 then
      if sudoku_ls = sudoku_before then
        (
          
          let (sudoku_dpll, sudoku_stack) = (dpll 0 sudoku_ls sudoku_ls) in
          if sudoku_dpll = []
          then
            match stack with
            | x::tl -> check 0 x x tl
            | _     -> failwith "error stack"
          else
            (check 0 sudoku_dpll sudoku_dpll (sudoku_stack::stack);)
         
        )
      else check 0 sudoku_ls sudoku_ls stack
    else
      
      let mass_ls = (List.nth sudoku_ls n) in
      begin 
        if (List.length mass_ls) = 1
        then let num_decided = (List.hd mass_ls) in
             let sudoku_ls2 =
               try (check_width  num_decided n 0 sudoku_ls)
               with Dpll_error -> []
             in
             let sudoku_ls3 =
               try (check_height num_decided n 0 sudoku_ls2)
               with Dpll_error -> []
             in
             let sudoku_ls4 =
               try (check_block  num_decided n 0 sudoku_ls3)
               with Dpll_error -> []
             in
             if sudoku_ls4 <> []
             then check (n+1) sudoku_ls4 sudoku_before stack
             else
               match stack with
               | x::tl -> check 0 x x tl
               | _     -> failwith "error mass stack"
                        
        else let sudoku_ls2 = (check_board n mass_ls sudoku_ls) in
             if sudoku_ls = sudoku_ls2
             then check (n+1) sudoku_ls2 sudoku_before stack
             else check n sudoku_ls2 sudoku_before stack
      end;;


(* 入力値 *)
(*
let sudoku =
  [  0;0;6; 0;0;0; 0;0;1
    ;0;7;0; 0;6;0; 0;5;0
    ;8;0;0; 1;0;3; 2;0;0
    
    ;0;0;5; 0;4;0; 8;0;0
    ;0;4;0; 7;0;2; 0;9;0
    ;0;0;8; 0;1;0; 7;0;0
    
    ;0;0;1; 2;0;5; 0;0;3
    ;0;6;0; 0;7;0; 0;8;0
    ;2;0;0; 0;0;0; 4;0;0];;
 *)

(*
let sudoku = 
  [  0;2;0; 0;0;0; 0;0;9
    ;4;0;1; 0;0;5; 0;0;3
    ;0;9;0; 0;6;0; 5;0;0
    
    ;2;5;0; 7;0;0; 0;0;0
    ;0;7;9; 0;0;0; 0;3;0
    ;0;0;0; 0;9;0; 0;0;4
    
    ;8;6;0; 5;7;0; 0;0;0
    ;1;3;0; 0;0;8; 0;9;0
    ;0;0;0; 0;0;0; 0;0;5];;
 *)


let sudoku = 
  [  0;0;0; 0;0;0; 0;0;0
    ;0;0;0; 0;0;1; 0;8;0
    ;6;4;0; 0;0;0; 7;0;0
    
    ;0;0;0; 2;0;3; 0;0;0
    ;0;0;1; 8;0;5; 0;0;0
    ;9;0;0; 0;0;0; 4;0;2
    
    ;0;0;0; 0;0;9; 3;5;0
    ;7;0;0; 0;6;0; 0;0;0
    ;0;0;0; 0;2;0; 0;0;0];;

(*
let sudoku = 
  [  0;3;0; 0;9;0; 0;1;0
    ;1;0;9; 0;8;0; 0;0;6
    ;6;0;0; 7;0;0; 0;0;0
    
    ;0;0;1; 0;0;0; 8;0;0
    ;2;0;6; 0;0;8; 0;0;0
    ;0;0;0; 0;0;3; 0;4;0
    
    ;0;0;7; 6;0;1; 0;0;8
    ;0;0;2; 0;0;0; 0;3;4
    ;0;0;0; 0;0;0; 9;0;0];;
 *) 

(*
 * 入力として与えられた数独を、
 * すでにわかっているマスはその数字のみが入ったリスト、
 * 
わかっていないマスは0~9が格納されたリストに変えた数独候補リストを作成する。
 *)
let sudoku_list =
  (List.map (fun x -> if x = 0 then [1;2;3;4;5;6;7;8;9] else [x]) sudoku);;


check 0 sudoku_list sudoku_list [];;
