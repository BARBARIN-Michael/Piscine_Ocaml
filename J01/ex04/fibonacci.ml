(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mbarbari <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 13:17:09 by mbarbari          #+#    #+#             *)
(*   Updated: 2015/11/03 13:23:16 by mbarbari         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec fibonacci n =
    if (n < 0) then
        -1
    else if (n = 0) then
        0
    else if (n = 1) then
        1
    else
        fibonacci (n - 2) + fibonacci (n - 1)

let () =
    print_int (fibonacci (-1));
    print_char '\n';
    print_int (fibonacci 1);
    print_char '\n';
    print_int (fibonacci 3);
    print_char '\n';
    print_int (fibonacci 6)
