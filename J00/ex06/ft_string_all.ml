(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 20:48:04 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/02 21:41:22 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_digit c =
    c >= '0' && c <= '9'

let ft_string_all is_digit str =
    let rec parse l str =
        if (l >= 0) then begin
            if (is_digit (String.get str l) = true) then
                parse (l - 1) str
            else
                false
        end
        else
            true
    in
        parse (String.length str - 1) str

let () =
    print_endline (string_of_bool(ft_string_all (is_digit) "0123456789"));
    print_endline (string_of_bool(ft_string_all (is_digit) "9876543210"));
    print_endline (string_of_bool(ft_string_all (is_digit) "0123a56789"));
    print_endline (string_of_bool(ft_string_all (is_digit) "Je te dit "))
