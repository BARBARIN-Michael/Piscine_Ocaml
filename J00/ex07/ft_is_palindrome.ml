(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 21:42:16 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/02 21:58:39 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let ft_is_palindrome str =
    let rec parse_pal str l_start l_end =
        if (l_end <= 0) then
            true
        else if (l_start <= l_end) then begin
            if ((String.get str l_start) <> (String.get str l_end)) then
                false
            else
                parse_pal str (l_start + 1) (l_end - 1)
        end
        else 
            true
    in
        parse_pal str 0 (String.length str - 1)

let () =
    print_endline (string_of_bool(ft_is_palindrome "radar"));
    print_endline (string_of_bool(ft_is_palindrome "toto"));
    print_endline (string_of_bool(ft_is_palindrome "madam"));
    print_endline (string_of_bool(ft_is_palindrome "car"));
    print_endline (string_of_bool(ft_is_palindrome ""))

