exception Type_error;;

let type_list = [] ;;


let rec typeof delta = 
		function
		         Ast.Const(Ast.CBool(b))   -> Ast.Tbool
			|Ast.Const(Ast.CFloat(f))  -> Ast.Tfloat
			|Ast.Const(Ast.CInt(n))    -> Ast.Tint
			|Ast.Const(Ast.CString(s)) -> Ast.Tstring
			|Ast.Aexpr(ae)             -> typeof_aexpr delta ae
			|Ast.Call(x,l_expr)        -> 
				begin
					match List.assoc x delta with (*estrae dall'ambiente il tipo di x che Ã¨ il tipo funzione*)

					Ast.Tfunction(tl,t,p) ->
						List.iter2 (fun e t -> if typeof delta e = t then ()
						else if typeof delta e = Ast.Tint && t = Ast.Tfloat then ()
						else
							raise Type_error) l_expr tl (*confronta se il tipo dei parametri passati a x sono dello stesso tipo della lista di parametri del tipo funzione *); t 					
					|_ -> raise Type_error
				end

			|Ast.Create(t,expr)        -> 
				if typeof delta expr = Ast.Tint then 
					Ast.Tarray(t)
				else
					raise Type_error
			
			|Ast.Cast(t,e)             ->
				if typeof delta e = Ast.Tfloat then
					Ast.Tint
				else
					raise Type_error

			|Ast.Neg(e)                ->
				if typeof delta e = Ast.Tbool then
					Ast.Tbool
				else 
					raise Type_error
 					
			|Ast.Pos(e)                ->
				if typeof delta e = Ast.Tint then
					Ast.Tint 
				else if typeof delta e = Ast.Tfloat then Ast.Tfloat 
				else 
					raise Type_error

			|Ast.Not(e)                ->
				if typeof delta e = Ast.Tint then
					Ast.Tint 
				else if typeof delta e = Ast.Tfloat then Ast.Tfloat 
				else 
					raise Type_error
			
			|Ast.Plus(e1,e2)           -> prom delta e1 e2
			|Ast.Minus(e1,e2)          -> prom delta e1 e2	
			|Ast.Times(e1,e2)          -> prom delta e1 e2
			|Ast.Divide(e1,e2)         -> prom delta e1 e2	
			
			|Ast.Rest(e1,e2)           ->
				if (typeof delta e1 = Ast.Tint && typeof delta e2 = Ast.Tint) then
					typeof delta e1 
				else
					raise Type_error
			
			|Ast.Eq(e1,e2)             -> prom_log delta e1 e2
			|Ast.Ne(e1,e2)             -> prom_log delta e1 e2	
			|Ast.Lt(e1,e2)             -> prom_log delta e1 e2
			|Ast.Le(e1,e2)             -> prom_log delta e1 e2
			|Ast.Gt(e1,e2)             -> prom_log delta e1 e2
			|Ast.Ge(e1,e2)             -> prom_log delta e1 e2
			
			|Ast.And(e1,e2)            ->
				if (typeof delta e1 = Ast.Tbool && typeof delta e2 = Ast.Tbool) then
					Ast.Tbool
				else
					raise Type_error
		
			|Ast.Or(e1,e2)             ->
				if (typeof delta e1 = Ast.Tbool && typeof delta e2 = Ast.Tbool) then
					Ast.Tbool
				else
					raise Type_error	

and typeof_aexpr delta = 
		function 
			Ast.Id(x)        -> List.assoc x delta
			
			|Ast.Access(a,n) ->
				if typeof delta n = Ast.Tint then
					match typeof_aexpr delta a with
						Ast.Tarray(t) -> t
						|_            -> raise Type_error
				else 
					raise Type_error 

and prom delta e1 e2 =
		if (typeof delta e1 = Ast.Tint && typeof delta e2 = Ast.Tint) then
			Ast.Tint
		else 
			if (typeof delta e1 = Ast.Tfloat && typeof delta e2 = Ast.Tfloat ||
			   (typeof delta e1 = Ast.Tint && typeof delta e2 = Ast.Tfloat)  ||
			   (typeof delta e1 = Ast.Tfloat && typeof delta e2 = Ast.Tint)) then
				Ast.Tfloat
			else
				raise Type_error 

and prom_log delta e1 e2 =
		if ((typeof delta e1 = Ast.Tfloat && typeof delta e2 = Ast.Tfloat) ||
		    (typeof delta e1 = Ast.Tint && typeof delta e2 = Ast.Tint)     ||
		    (typeof delta e1 = Ast.Tint && typeof delta e2 = Ast.Tfloat)   ||
		    (typeof delta e1 = Ast.Tfloat && typeof delta e2 = Ast.Tint))  then
			Ast.Tbool
		else
			raise Type_error 			
						
let rec wfs delta =
		function
			 Ast.Line(e)      ->
				if typeof delta e = Ast.Tvoid then
					()	
				else
					raise Type_error

			|Ast.Assign(ae,e) ->
				if (typeof_aexpr delta ae = typeof delta e) then
					()
				else
					raise Type_error

												
			|Ast.Cond(e,s)    ->
				if (typeof delta e = Ast.Tbool) then
					wfs delta s
				else 
					raise Type_error

			|Ast.Cond_else(e,s1,s2) ->
				if(typeof delta e = Ast.Tbool) then
					(wfs delta s1; wfs delta s2) 
				else 
					raise Type_error

			|Ast.Loop(e,s)    ->
				if (typeof delta e = Ast.Tbool) then
					wfs delta s
				else
					raise Type_error

			|Ast.Ret(e)       -> 
				(match typeof delta e with
					 Ast.Tint      -> ()
					|Ast.Tfloat    -> ()
					|Ast.Tstring   -> ()
					|Ast.Tbool     -> ()
					|Ast.Tarray(_) -> ()  
					|Ast.Tvoid     -> ()
					|_             -> raise Type_error)

			|Ast.For(init,cond,update,s) ->
				wfs delta init; 
				if (typeof delta cond = Ast.Tbool) then
					wfs delta s
				else
					raise Type_error
				wfs delta update;

			|Ast.Do_While(corpo,cond)    ->
				if(typeof delta cond = Ast.Tbool) then
					wfs delta corpo
				else
					raise Type_error

			|Ast.Switch(e,b)             ->
				if typeof delta e = Ast.Tint then
					List.iter (fun b -> match b with
						Ast.TBranch(expr,s) ->
							if typeof delta expr = Ast.Tint then
								()
							else
								raise Type_error;
							wfs delta s;) b
											
			|Ast.Switch_default(e,b1,b2) ->
				if typeof delta e = Ast.Tint then
					List.iter (fun b1 -> match b1 with
						Ast.TBranch(expr,s) ->
							if typeof delta expr = Ast.Tint then
								()
							else
								raise Type_error;
							wfs delta s;) b1;
							wfs delta b2

			|Ast.Block(Ast.TBlock(list_ldecl,list_stmt)) ->
				let delta2 = List.fold_right 
					(fun x env -> match x with
						Ast.Ldecl(tid) -> match tid with
							Ast.Tid(t,s) -> (s,t)::env) list_ldecl delta in
							    	List.iter (wfs delta2) list_stmt 				
(*  Funct of ctype * string * ctid list * stmt    *)						    	
let rec wfs2 delta =
		function			 
			Ast.Funct(t,s,ctid_list,bl) ->
				let type_list = List.fold_right
					(fun x l -> match x with
						Ast.Tid(t,s) -> t::l) ctid_list type_list in
						(*lista tipi funzione es: [Tint;TFloat;Tstring] *)
						let delta'=(s,Ast.Tfunction(type_list, t, ""))::delta in
						(* aggiungo all'ambiente globale la coppia della della funzione.
					        es: (  "somma2", Ast.Tfunction([Tint;Tint],Tint)  )::delta
						Creando così nuovo ambiente delta'  *)

						let delta3 = List.fold_right 
							(fun x env -> match x with
								Ast.Tid(t,s) -> (s,t)::env) ctid_list delta'
								(* accodo i parametri formali all'ambiente del blocco della funzioneche usa i parametri formali e deve vederne tipi e nomi  *)
								in
							   	wfs delta3 bl;
                                                                (* passo il  blocco alla wfs con il nuovo ambiente con
                                                                * in più la coppia (nome,tipo) della funzione
                                                                * (potrebbe  essere ricorsiva ) e i parametri formali *)
								  delta'(*  restituito da questa wfs2 *)
                                                                                                                          
			
			|Ast.Efunct(s,t,s1,ctype_list) -> (s1,Ast.Tfunction(ctype_list, t, s))::delta
													   	
						





			
			


