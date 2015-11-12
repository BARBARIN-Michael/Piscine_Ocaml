(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   win_failFunction.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *) (*   Created: 2015/11/08 11:08:19 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/08 14:58:12 by tvallee          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_num (c: char) =
    (c > '0' && c <= '9')

let splitcoord str =
    if (String.length str <> 3 ||
        (not (is_num (str.[0]) && is_num (str.[2]) && (str.[1] = ' ')))) then
        (-1, -1)
    else
        ((int_of_char str.[0] - (int_of_char '0') - 1),
        (int_of_char str.[2] - (int_of_char '0') - 1))


let game_over board=
    if Board.hasWon board 'O' then begin print_endline "O wins the game !";
                                              true end
    else if Board.hasWon board 'X' then begin print_endline "X wins the game !";
                                              true end
    else false


let next_p p= if p = 'O' then 'X' else 'O'

let print_status s player=
    if s then (print_char player; print_endline "'s turn to play.")

let valid_input board x y player=
    if x = (-1) then begin print_endline "Incorrect format"; false end
    else if (x < 0 || (y < 0) || not (Board.isFree board x y))
    then begin print_endline "Illegal move"; false end
    else true

(******************************************************************************)
(******************************************************************************)

let rec main board player should_print=

    (if should_print then begin print_newline (); Board.print board;
    print_newline () end);

    if not (game_over board) then begin

        print_status should_print player;
        let (y, x) = splitcoord (read_line ())
        in
        if valid_input board x y player then
                if player = 'O' then main (Board.setBox board x y player) 'X' true
                                else main (Board.setBox board x y player) 'O' true
        else
            main board player false;
    end


let () =
    main (Board.create ()) 'O' true
