(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 22:08:06 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/02 22:46:40 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_rot_n n str =
    let rot_n car =
        if ((car >= 'a' && car <= 'z') || (car >= 'A' && car <= 'Z')) then begin
                if (car <= 'Z' && (char_of_int(int_of_char(car) + (n mod 26)) > 'Z')) then
                    (char_of_int(int_of_char(car) + (n mod 26) - 26)) 
                else if (car <= 'z' && (char_of_int(int_of_char(car) + (n mod 26)) > 'z')) then
                    (char_of_int(int_of_char(car) + (n mod 26) - 26)) 
                else
                    (char_of_int(int_of_char(car) + (n mod 26)))
        end
        else
            car
    in
        String.map rot_n str


let () =
    print_endline(ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
    print_endline(ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
    print_endline(ft_rot_n 42 "0123456789");
    print_endline(ft_rot_n 2 "OI2EAS67B9");
    print_endline(ft_rot_n 0 "Damned !");
    print_endline(ft_rot_n 42 "");
    print_endline(ft_rot_n 1 "NBzlk qnbjr !")


