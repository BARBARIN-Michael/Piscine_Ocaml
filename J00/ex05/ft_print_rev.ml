(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 20:22:50 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/02 20:43:16 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
    let rec parse_str str l =
        if (l >= 0) then begin
            print_char (String.get str l);
            parse_str str (l - 1)
        end
    in
        parse_str str (String.length str - 1);
        print_char '\n'

let () =
    ft_print_rev ("Hello world !");
    ft_print_rev ("Je dit des conneries pour tester le programme ...")
