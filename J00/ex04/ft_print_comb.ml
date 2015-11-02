(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 17:15:50 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/02 19:22:13 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb =
    let rec ft_parse a b c =
        print_int a; print_int b; print_int c;
        if (a == 7 && b == 8 && c == 9) then ()
        else
            print_string ", ";
        if c < 9 then ft_parse a b (c + 1)
        else if b < 8 && c <= 9 then
            ft_parse a (b + 1) (b + 2)
        else if a < 7 && b <= 8 && c <= 9 then
            ft_parse (a + 1)  (a + 2) (a + 3)

    in
        ft_parse 0 1 2;
        print_char '\n'

let () = ft_print_comb
