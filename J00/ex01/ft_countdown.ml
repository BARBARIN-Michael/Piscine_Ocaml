(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mbarbari <mbarbari@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 11:40:11 by mbarbari          #+#    #+#             *)
(*   Updated: 2015/11/02 12:04:18 by mbarbari         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x =
	if x <= 0
	then begin 
		print_int 0;
		print_char '\n';
	end
	else begin
		print_int x;
		print_char '\n';
		ft_countdown (x - 1)
	end

let main () =
	ft_countdown (5);
	ft_countdown (0);
	ft_countdown (-5)

let () = main ()


