(* Курс по Теории Типов, ИТМО, осень 2022/2023 учебного года *)
(* Реализация двоичных списков на Окамле и демонстрация их работы *)

type 'a binlist = Nil | Zero of ('a * 'a) binlist | One of 'a * (('a * 'a) binlist);;

(* Специальный синтаксис для указания типа функции при полиморфной рекурсии ранга >= 2 *)
let rec inc: type a.a binlist * a -> a binlist = fun (list,elem) ->
    match (list,elem) with
        (Nil,elem) -> One (elem, Nil)
      | (Zero tl,elem) -> One (elem, tl)
      | (One (hd,tl),elem) -> Zero (inc (tl,(elem,hd)) );;

let rec to_string: type a.a binlist * (a -> string) -> string = fun (list,printer) ->
    let double_printer (l,r) = "(" ^ printer l ^ "," ^ printer r ^ ")" in
    match list with
        Nil -> "[]"
      | Zero tl -> "() :: " ^ to_string (tl, double_printer)
      | One (hd,tl) -> printer hd ^ " :: " ^ to_string (tl, double_printer);;

let rec len: type a.a binlist -> int = fun list ->
    match list with
        Nil -> 0
      | Zero tl -> 2 * len tl
      | One (_,tl) -> 2 * len tl + 1;;

let rec mklst i n = if i >= n then Nil else inc (mklst (i+1) n,i);;

let rec as_rev_binary n = if n = 0 then "" else string_of_int (n mod 2) ^ as_rev_binary (n / 2);;

let lst = [Nil; inc (Nil, 5); inc (inc (Nil, 4), 12); mklst 0 0b101110];;
List.iter (fun l -> Printf.printf "%s => %s\n" (as_rev_binary (len l)) (to_string (l, string_of_int))) lst;;
