(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mbarbari <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 12:03:34 by mbarbari          #+#    #+#             *)
(*   Updated: 2015/11/03 13:43:47 by mbarbari         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ackermann m n =
    if (m < 0 || n < 0) then
        -1
    else if (m = 0) then
        (n + 1)
    else if (m > 0 && n = 0) then
        ackermann (m - 1) 1
    else
        ackermann (m -1) (ackermann m (n - 1))

let () =
    print_int (ackermann 2 3);
    print_char '\n';
    print_int (ackermann 0 0);
    print_char '\n';
    print_int (ackermann (-1) 7);
    print_char '\n';
    print_int (ackermann 4 1)
