(*
 * 盤面
 *  0  1  2  3  4  5  6  7  8
 *  9 10 11 12 13 14 15 16 17
 * 18 19 20 21 22 23 24 25 26
 * 27 28 29 30 31 32 33 34 35
 * 
 *)



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
     if n/9=index/9 then 
       begin
         match x with
         | [y] ->    x::(check_width num_decided n (index+1) tl)
         | y::tl2 -> [remove num_decided x]@(check_width num_decided n (index+1) tl)
         | _ -> failwith "error"
       end
     else x::(check_width num_decided n (index+1) tl)
  | _ -> failwith "error";;


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
     if n mod 9=index mod 9 then 
       begin
         match x with
         | [y] ->    x::(check_height num_decided n (index+1) tl)
         | y::tl2 -> [remove num_decided x]@(check_height num_decided n (index+1) tl)
         | _ -> failwith "error"
       end
     else x::(check_height num_decided n (index+1) tl)
  | _ -> failwith "error";;


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
     if (List.exists (fun z -> z = index) (block_search n)) then 
       begin
         match x with
         | [y] ->    x::(check_block num_decided n (index+1) tl)
         | y::tl2 -> [remove num_decided x]@(check_block num_decided n (index+1) tl)
         | _ -> failwith "error"
       end
     else x::(check_block num_decided n (index+1) tl)
  | _ -> failwith "error";;


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
       | _ -> failwith "error"
     end
  | _ -> failwith "error";;



(*
 * 数独の形(9*9の数字行列)で表示する。
 * もし、確定していないマスの場合は0と表示する
 *)
let rec print_board n sudoku_ls =
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
                                                                         
            else
              let _ = (Printf.printf"%d " List.(hd (hd sudoku_ls))) in (print_board (n+1) tl)
         | y::tl2 ->
            if n mod 9 = 8 then
              let _ = (Printf.printf"0\n") in (print_board (n+1) tl)
            else
              let _ = (Printf.printf"0 ") in (print_board (n+1) tl)
       | _ -> failwith "error"
       end
    | _ -> failwith "error";;


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
  | _ -> failwith "error";;


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
  | _ -> failwith "error";;



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
  | _ -> failwith "error";;


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
       [num_decided]::tl
     else x::(change_decided_mass num_decided n (index+1) tl)
  | _ -> failwith "error";;


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
     then change_decided_mass ls_hd n 0 sudoku_ls
     else check_board n tl sudoku_ls
       
  | _ -> failwith "error";;



(*
 * sudoku_ls : 現在の数独候補リスト
 * n         : 参照しているマスのインデックス
 *
 * 参照しているマスによって動作を変更する。
 * マスに入る数字が確定しているときは、そのマスの行・列・ブロックに属するマスから、
 * 確定している数字を候補から除去する。
 * マスに入る数字が確定していない場合は、そのマスの候補数字を検証して、
 * 同じブロックに属するマスの候補にない場合、数字を確定する。
 *)
let rec check n sudoku_ls =
  if (check_complete sudoku_ls)
  then print_board 0 sudoku_ls
  else if n > 80 then
    check 0 sudoku_ls
  else
    let mass_ls = (List.nth sudoku_ls n) in
    begin 
      if (List.length mass_ls) = 1
      then let num_decided = (List.hd mass_ls) in
           let sudoku_ls2 = (check_width  num_decided n 0  sudoku_ls) in
           let sudoku_ls3 = (check_height num_decided n 0 sudoku_ls2) in
           let sudoku_ls4 = (check_block  num_decided n 0 sudoku_ls3) in
           check (n+1) sudoku_ls4
           
      else let sudoku_ls2 = (check_board n mass_ls sudoku_ls) in
           if sudoku_ls = sudoku_ls2
           then check (n+1) sudoku_ls2
           else check n sudoku_ls2
    end;;

(* 入力値 *)
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
 * 入力として与えられた数独を、
 * すでにわかっているマスはその数字のみが入ったリスト、
 * 
わかっていないマスは0~9が格納されたリストに変えたint list list(数独候補リスト) を作成する。
 *)
let sudoku_list =
  (List.map (fun x -> if x = 0 then [1;2;3;4;5;6;7;8;9] else [x]) sudoku);;


let _ = (check 0 sudoku_list);;
