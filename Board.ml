(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Board.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: tvallee <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/07 15:20:41 by tvallee           #+#    #+#             *)
(*   Updated: 2015/11/08 20:10:49 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Nested =
struct

    (* ************************ Declaration type ******************************** *)
        type t = char list
        type player = char

    (* ************************** Declaration public function ******************* *)

    let create : t= ['-';'-';'-';'-';'-';'-';'-';'-';'-']

    let getline (tab: t) (line: int) =
        let rec inner lst line = match line with
            | 0 -> (match lst with
                    | a::(b::(c::tail)) -> a::(b::[c])
                    | _ -> [])
            | _ -> (match lst with
                    | a::(b::(c::tail)) -> inner tail (line - 1)
                    | _ -> [] )
        in inner tab line

    let isWon (board: t) (p: char)=
        let check a b c=
                List.nth board a = p
            &&  List.nth board b = p
            &&  List.nth board c = p
        in
        let rec check_c n=
            if n = 3 then
                false 
            else if check n (n + 3) (n + 6) then
                true
            else
                check_c (n + 1)
        in
        let rec check_l n =
            if n = 3 then
                false
            else if check n (n + 1) (n + 2) then
                true
            else
                check_c (n + 1)
        in
        if p = 'O' && List.nth board 0 = '/' then true
        else if p = 'X' && List.nth board 0 = '\\' then true
        else
            (check 0 4 8) || (check 2 4 6) || (check_c 0) || (check_l 0)

    let isFree (board: t) (x: int) (y: int) =
        if (isWon board 'O' || isWon board 'X') then false
        else
        if (List.nth board ((3 * y) + x)) <> '-' then
            false
        else
            true
    let rec _replace lst i newelem= match lst with
    | head::tail -> if i = 0 then newelem::tail else head::(_replace tail (i - 1) newelem)
    | [] -> []

    let rec terminated board= match board with
    | h::t -> if h = 'O' || h = 'X' then terminated t else false
    | [] -> true

    let setBox (board: t) (x: int) (y: int) (newval: char) =
        let coord = (3 * y) + x in
        let newb = _replace board coord newval
        in
        if (isWon newb 'O' || terminated newb) then
            ['/'; '-'; '\\'; '|' ; ' '; '|'; '\\'; '-'; '/']
        else if (isWon newb 'X' || terminated newb) then
            ['\\'; ' '; '/'; ' ' ; 'X'; ' '; '/'; ' '; '\\']
        else newb


    let rec isFull (board: t) = match board with
        |a::[] when a <> '-' -> false
        |a::b when a = '-' -> true
        |a::b -> isFull b
        | _ -> false

end

(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

type t = Nested.t list

let create () =
    let rec loop i= match i with
    | 0 -> []
    | _ -> Nested.create::(loop (i - 1))
    in
    loop 9

let _get t x y= List.nth t ((x / 3) + ((y / 3) * 3))

let rec _replace lst i newelem= match lst with
| head::tail -> if i = 0 then newelem::tail else head::(_replace tail (i - 1) newelem)
| [] -> []

(******************************************************************************)

let rec _printLine b1 b2 b3=
    let bubu lst= match lst with
    | c1::(c2::(c3::tail)) ->
            print_char c1; print_char ' '; print_char c2; print_char ' ';
            print_char c3
    | _ -> ()
    in
    let _bail l1 l2 l3 i=
        bubu (Nested.getline l1 i);
        print_string " | ";
        bubu (Nested.getline l2 i);
        print_string " | ";
        bubu (Nested.getline l3 i);
        print_newline ()
    in
    let rec loop i= match i with
    | 3 -> ()
    | _ -> _bail b1 b2 b3 i ; loop (i + 1)
    in loop 0

let rec print (board : t)= match board with
| b1::(b2::(b3::tail)) -> _printLine b1 b2 b3;
                          print_endline "---------------------";
                          print tail
| _ -> ()

(******************************************************************************)

let isFree (board : t) (x : int) (y : int)=
    Nested.isFree (_get board x y) (x mod 3) (y mod 3)

let hasWon (board : t) (p : char)=
    let check a b c=
            Nested.isWon (List.nth board a) p
        &&  Nested.isWon (List.nth board b) p
        &&  Nested.isWon (List.nth board c) p
    in
    let rec check_c n=
        if n = 3 then false else if check n (n + 3) (n + 6) then true else
            check_c (n + 1)
    in
    let rec check_l n =
        if n = 3 then false else if check n (n + 1) (n + 2) then true else
            check_c (n + 1)
    in
    (check 0 4 8) || (check 2 4 6) || (check_c 0) || (check_l 0)

let setBox (board : t) (x : int) (y : int) (p : char): t=
    let newn= Nested.setBox (_get board x y) (x mod 3) (y mod 3) p
    in
    if Nested.isWon newn p then begin
        print_char p;
        print_string " wins grid ";
        print_int ((x / 3) + ((y / 3) * 3));
        print_endline "!"; end;
    _replace board ((x / 3) + ((y / 3) * 3)) newn
