(* ESEMPIO CODICE C
* int somma2(int a, int b) { int temp; temp=3; return a+b;  }
* void main (string[] args) { int i; int j; int ris;          i=4; i=5;  ris = somma2(i,j);     }
* 
* NOTE:
* delta = [ (  "somma2" , Ast.Tfunction( [Tint;Tint], Tint)   )     ;   ("a",Tint) ; ("a",Tint) ;
*           (  "main",  Ast.Tfunction([Tarray(...)], Tvoid))          ;        ("i"; Tint)   ; ..j..ris ) ]
* nel main saranno accodati in testa i, j e ris...
*  
* NOTA: nel delta tengo i tipi dell'ast, non della JVM. Usare la conversione_tipo per emettere funzioni modulo
*
* indici(in somma2) = [ ("a",0)  ;  ("b",1)  ]
* indici(in main)   = [ ("i",0)  ;  ("j",1)  ;  ("ris",2) ]
*
*)


(* Converte tipo Ast in tipi JVM *)
let rec conversione_tipi =
        function
                Ast.Tvoid        -> J.Type.Void
              | Ast.Tbool        -> J.Type.Bool
              | Ast.Tint         -> J.Type.Int
              | Ast.Tfloat       -> J.Type.Float
              | Ast.Tstring      -> J.Type.Object("java/lang/String")
              | Ast.Tarray(tipo) -> J.Type.Array(conversione_tipi tipo)
              | _                -> J.Type.Void
and

conversione_tipi2 =
        function
              | Ast.Tarray(tipo) -> conversione_tipi tipo
              | _                -> J.Type.Void

and

(**************************************************)
(* Funzione principale di compilazione funzioni. chiamata dal main.ml con delta_globale (delta') e ast *)	   
(* in delta_globale ho già le coppie (nome,tipo) delle funzioni, grazie alla wfs2 di type.ml *)
compila_gdecl file_nome delta_globale cls = 
       function (* match sull'Ast sui due tipi di funzioni *)
		Ast.Efunct(path, tiporest, nomef, lista_tipi) ->  (* creare lista di funzioni extern *)
			()

       
       	      | Ast.Funct(tiporest, nomef, lista_parametri, comandi_fun (*TBlock of ldecl list * stmt list*) ) ->
              (* delta_globale passato dal main.ml contiene già nome e tipo di questa f *)
	      (* es ( "somma2", Tfunction([ Tint ; Tint], Tint)  )  .  *)
	      (* Devo creare la lista di tipiJVM. es: [ J.Type.Int ; J.Type.Int]  [foglio appunti 4] *)
	      let lista_tipi_jvm = List.fold_right (fun x l -> match x with
            		Ast.Tid(t,s) -> (conversione_tipi t)::l) lista_parametri [] in
	                	let tipo_rest_jvm = conversione_tipi(tiporest) in
	                	let h = J.Class.create_method  nomef  cls  lista_tipi_jvm tipo_rest_jvm in
	                	(* h è l'handle alla funzione corrente. Tutti i comandi emessi al suo interno saranno
				riferiti a questo h *)
	                	(* la lista_tipi va nell'ambiente interno (accodato in testa al globale) come nel type.ml*)
	                	(* es: int somma2(int a, int b) {...}  => intesto [("a",Tint);("b",Tint)] all'ambiente.  *)
	    			let delta_dopo_funzione = List.fold_right 
	                        	(fun x env -> match x with  Ast.Tid(t,s) -> (s,t)::env)
			        		lista_parametri delta_globale
            					in
					(* crea nuova lista di indici_var = [ ("a",0) ; ("b",1) ] che il blocco userà  *)
	    
	    			let indici_var = List.rev (List.fold_right
	                        	(fun x indici -> match x with  Ast.Tid(t,s) ->
                                        	(s,List.length(indici))::indici )
			        		(List.rev (lista_parametri)) [] )
	     				in
	     
(* Chiamo la funzione di compilazione del comando (che è un blocco), passando:
-  il nuovo ambiente con i nuovi tipi in testa (par. formali)
-  la lista degli indici delle variabili, da zero in poi (quando verrà chiamata la Call saranno messi sullo stack in questo orine e sarà possibile accedervi con questi indici ??? )
- blocco comandi della funzione
- handle a questa funzione che è stata creata, in modo che tutte le emit al suo interno lo usino  *)
        
    compila_comando file_nome delta_dopo_funzione indici_var  h comandi_fun;
    if (tipo_rest_jvm = J.Type.Void) then
	J.Method.emit h ( J.Code.Return(J.Type.Void))
           
	    
 
(**************************************************)
(* crea codice per i comandi *********************)
(* ogni comando vede l'ambiente e gli indici delle variabili , a seconda di dove di trova  *)
and compila_comando file_nome delta_interno indici_var h =
	function
	Ast.Line(expr) -> (* comando costituito solo da una espressione. Può essere solo una Call a funzione Void
	                     oppure il type Checker restituisce errore.
			     E' sufficiente richiamare la funzione di compilazione dell'espressione passando ambiente e indici *)
		compila_expr file_nome delta_interno indici_var h expr

      | Ast.Assign(ae,e) ->
		begin
           		match ae with
             			Ast.Id(nomevar) ->
		(* Compilo la parte dx dell'assegnamento e mi troverò sullo stack il valore risultante dell'espressione.*)
	                  		compila_expr file_nome  delta_interno indici_var h e; 
			 (* Memorizzo il valore del giusto tipo (richiamo la type_of_aexpr) nella giusta locazione (guardo lista indici) *)
			 		let tipo_var = conversione_tipi(List.assoc nomevar delta_interno)
                         		in
			 		let posizione_var = List.assoc nomevar indici_var
                         		in
	                 		J.Method.emit h (J.Code.Store(tipo_var , posizione_var) )
				| Ast.Access(ae2,indice) ->
		(* Se assegno ad un elemento di array ae2[indice]=e, carico l'array sullo stack*)
		   			let tipo_ae2 = conversione_tipi2(Type.typeof_aexpr delta_interno ae2)
                   			in (* tipo dell'array J.Type.Int|..|String *)
		   			(* load array dalle var *)
                    			compila_aexpr file_nome delta_interno indici_var h ae2;
	           			(* compilo l'espressione dell'indice, che verrà portata sullo stack *)
		   			compila_expr file_nome  delta_interno indici_var h indice;
		   			(* compilo l'espressione a destra dell'assegnamento *)
		   			compila_expr file_nome  delta_interno indici_var h e;
		   			(* adesso posso emettere la ArrayStore. *)
		   			J.Method.emit h (J.Code.ArrayStore( tipo_ae2) )
          	end


      | Ast.Cond (expr_condizione, corpo_if) ->
                (* valuto exp e carico 0|1 della condizione *)
		compila_expr file_nome delta_interno indici_var h expr_condizione; 
                
		(* dichiaro etichette *)
		let l_corpo_if = J.Label.create() in (* crea label *)
		let l_continua =  J.Label.create() in (* crea label *)
		
		(* emetto jumpif con l'etichetta prima del corpo *)
		(* Se trovo sullo stack:  0=>continuo(cond falsa)      1=>salto(cond vera)  *)
		(* JUMPIF l_corpo_if *)
		J.Method.emit h ( J.Code.JumpIfZero(J.Cmp.Ne,l_corpo_if)  ) ;
		(* JUMP l_continua . emetto salto a etichetta dopo il corpo (condizione non verificata e non devo eseguire corpo dell'if) *)
		J.Method.emit h ( J.Code.Jump (l_continua) );
		(* l_corpo_if:  *)
		J.Method.emit h ( J.Code.Label (l_corpo_if) );
		  (* compila il corpo dell'if *)
		  compila_comando file_nome delta_interno indici_var h corpo_if;
		(* l_continua: *)
		J.Method.emit h ( J.Code.Label(l_continua) )
      
      | Ast.Cond_else (expr_condizione, corpo_vero, corpo_falso) -> 
                (* valuto exp e carico 0|1 della condizione *)
		compila_expr  file_nome  delta_interno indici_var h expr_condizione; 
                
		(* dichiaro etichette *)
		let l_corpo_if = J.Label.create() in (* crea label *)
                let l_else     = J.Label.create() in (* crea label *)
		let l_continua = J.Label.create() in (* crea label *)
		
		J.Method.emit h ( J.Code.JumpIfZero(J.Cmp.Ne, l_corpo_if)  ) ;
		J.Method.emit h ( J.Code.Jump (l_else) );
		J.Method.emit h ( J.Code.Label (l_corpo_if) );
		compila_comando file_nome delta_interno indici_var h corpo_vero;
                J.Method.emit h ( J.Code.Jump (l_continua) );
                J.Method.emit h ( J.Code.Label (l_else) );
                compila_comando file_nome delta_interno indici_var h corpo_falso;
                J.Method.emit h ( J.Code.Label (l_continua) )

      | Ast.Loop(expr_while, corpo_while) -> 
		(* dichiaro etichette *)
                let l_inizio      =  J.Label.create() in
		let l_continua    =  J.Label.create() in
		let l_corpo_while =  J.Label.create() in
		
		(* inizio: *)
		J.Method.emit h ( J.Code.Label(l_inizio) );
		(* compilo condizione e porto sullo stack 0|1  *)
		compila_expr file_nome delta_interno indici_var h expr_while; 
		(* JUMPIF l_corpo_while.      se verificata la condizione salta all'inizio del corpo *)  
                J.Method.emit h ( J.Code.JumpIfZero(J.Cmp.Ne, l_corpo_while) );
		(* JUMP l_continua.    condizione non verificata => esco dal while saltando a l_continua *)
		J.Method.emit h ( J.Code.Jump l_continua );
		(* l_corpo_while: *)
                J.Method.emit h ( J.Code.Label(l_corpo_while) );
		(* compila il corpo del while *)
		compila_comando file_nome delta_interno indici_var h corpo_while;
		(* JUMP  l_inizio   *)
		J.Method.emit h ( J.Code.Jump l_inizio );  
		(* l_continua: *)
		J.Method.emit h ( J.Code.Label(l_continua) )
      
      |Ast.For(assign,cond,update,corpo) ->
		(*dichiaro etichette*)
		let l_inizio    = J.Label.create() in
		let l_continua  = J.Label.create() in
		let l_corpo_for = J.Label.create() in
	
		compila_comando file_nome delta_interno indici_var h assign;
		J.Method.emit h(J.Code.Label(l_inizio));
		compila_expr file_nome delta_interno indici_var h cond;
		J.Method.emit h(J.Code.JumpIfZero(J.Cmp.Ne, l_corpo_for));
		J.Method.emit h(J.Code.Jump l_continua);
		J.Method.emit h(J.Code.Label(l_corpo_for));
		compila_comando file_nome delta_interno indici_var h corpo;
		compila_comando file_nome delta_interno indici_var h update;
		J.Method.emit h(J.Code.Jump l_inizio);
		J.Method.emit h(J.Code.Label(l_continua))

      |Ast.Do_While(corpo_do, cond) ->
		let l_inizio   = J.Label.create() in
		let l_continua = J.Label.create() in
		
		J.Method.emit h(J.Code.Label(l_inizio));
		compila_comando file_nome delta_interno indici_var h corpo_do;
		compila_expr file_nome delta_interno indici_var h cond;
		J.Method.emit h(J.Code.JumpIfZero(J.Cmp.Ne, l_inizio));
		J.Method.emit h(J.Code.Jump l_continua);
		J.Method.emit h(J.Code.Label(l_continua))


      |Ast.Switch(variabile, lista_case) ->
		let l_esci =  J.Label.create() in

		List.iter (fun x -> match x with 
			Ast.TBranch(intero, corpo_case) -> 
				begin
					let l_case     = J.Label.create() in 
					let l_continua = J.Label.create() in 
					compila_expr file_nome delta_interno indici_var h variabile;
					compila_expr file_nome delta_interno indici_var h intero;
					J.Method.emit h ( J.Code.JumpIf(J.Cmp.Eq, J.Type.Int, l_case)  ) ;
					J.Method.emit h ( J.Code.Jump (l_continua) );
					J.Method.emit h ( J.Code.Label (l_case) );
					compila_comando file_nome delta_interno indici_var h corpo_case;
					J.Method.emit h ( J.Code.Jump (l_esci) );
					J.Method.emit h ( J.Code.Label (l_continua) )
				end
		) lista_case;

		J.Method.emit h ( J.Code.Label (l_esci) )


      |Ast.Switch_default(variabile, lista_case, blocco_default) ->
		let l_esci =  J.Label.create() in

		List.iter (fun x -> match x with 
			Ast.TBranch(intero, corpo_case) -> 
				begin
					let l_case     = J.Label.create() in 
					let l_continua = J.Label.create() in 
					compila_expr file_nome delta_interno indici_var h variabile;
					compila_expr file_nome delta_interno indici_var h intero;
					J.Method.emit h ( J.Code.JumpIf(J.Cmp.Eq, J.Type.Int, l_case)  ) ;
					J.Method.emit h ( J.Code.Jump (l_continua) );
					J.Method.emit h ( J.Code.Label (l_case) );
					compila_comando file_nome delta_interno indici_var h corpo_case;
					J.Method.emit h ( J.Code.Jump (l_esci) );
					J.Method.emit h ( J.Code.Label (l_continua) )
				end
		) lista_case;

		compila_comando file_nome delta_interno indici_var h blocco_default;
		J.Method.emit h ( J.Code.Label (l_esci) )

      
      | Ast.Ret(expr) -> (* metto sulla pila la espressione dopo la return *)
           	compila_expr file_nome delta_interno indici_var  h expr;  
		(* emetto la giusta J.Code.Return(J.Type.Int|Float|String) *)
		J.Method.emit h ( J.Code.Return(conversione_tipi (Type.typeof delta_interno expr) ))
			
      | Ast.Block( Ast.TBlock(list_ldecl,list_stmt) ) -> (* chiamata anche subito dopo funzione *)
(* Se segue la dichiarazione di funzione, contiene già:
- il delta_interno con le coppie (nome,tipo) relative ai parametri formali della funzione. Le nuove dichiarazioni del blocco
  saranno messe in testa all'ambiente
- gli indici da cui reperire i parametri passati. Le nuove dichiarazioni del blocco saranno messe in coda con indici successivi *)
(* aggiungo al delta_interno (che nel caso funzione aveva già le coppie nome-tipo dei par.formali ) le dichiarazioni locali presenti nella list_ldecl*)
	  	let delta_interno_con_dich_locali = List.fold_right (fun x env -> match x with Ast.Ldecl(tid) -> match tid with Ast.Tid(t,s) -> (s,t)::env) list_ldecl delta_interno
	  	(* es: in somma2 : delta_interno_con_dich_local= [("temp",Tint) ; ("a",Tint) ; ("b",Tint) ...somma2...main... ]*)
          	in
	  	(* memorizzo indici delle nuove variabili dichiarate nel blocco  *)  
          	let indici_var_con_dich_locali = List.fold_right (fun x indici ->
                	match x with Ast.Ldecl(tid) -> match tid with Ast.Tid(t,s) ->
                          (s,List.length indici)::indici ) list_ldecl indici_var
          	in
	  	(* [ ("a",0) ; ("b",1) ; ("temp",2) ]   *)
 	  	(* devo passare a ogni comando le due liste (che  non saranno + modificate da nessun comando, quindi basta la iter) *)
	   	List.iter ( compila_comando file_nome delta_interno_con_dich_locali indici_var_con_dich_locali h) list_stmt;
	   	J.Method.emit h (J.Code.LoadInt(99));
	   	J.Method.emit h (J.Code.Pop(J.Type.Int));
and

(**************************************************)
(* restituisce  il tipo di una coppia mista di Int-Float nel caso di operazioni binarie. Usata dalla compila_expr *)
tipo_coppia delta t1 t2 =
       		if (Type.typeof delta t1=Type.typeof delta t2 && Type.typeof delta t2=Ast.Tint) then
			J.Type.Int
       		else
			J.Type.Float

and

(**************************************************)
(* date 2 espressioni di tipo misto Int-Float, chiama la compila_expr su entrambe con le dovute Convert al momento giusto *)
compila_e_converti file_nome delta indici_var e1 e2 h =
     		if ((Type.typeof delta e1) = (Type.typeof delta e2)) then (* entrambi Tint o Tfloat => niente Convert *)    			    (compila_expr file_nome delta indici_var  h e1; compila_expr file_nome delta indici_var  h e2)
    		else if ((Type.typeof delta e1) = Ast.Tfloat && (Type.typeof delta e2) = Ast.Tint) then
      	   		( compila_expr file_nome delta indici_var  h e1; compila_expr file_nome delta indici_var  h e2; 
           		J.Method.emit h (J.Code.Convert(J.Type.Int, J.Type.Float)) )
    		else (* caso int-float *)
       			(compila_expr file_nome delta indici_var  h e1; J.Method.emit h
       			(J.Code.Convert(J.Type.Int, J.Type.Float)) ; 
       			compila_expr file_nome delta indici_var  h e2)

and 

compila_condizione delta h e1 e2 tipo_confronto = 
		let l_corpo_if = J.Label.create() in 
		let l_else     =  J.Label.create() in 
		let l_continua =  J.Label.create() in
		J.Method.emit h ( J.Code.JumpIf(tipo_confronto ,  tipo_coppia delta e1 e2,  l_corpo_if)  ) ;
		J.Method.emit h ( J.Code.Jump (l_else) );
		J.Method.emit h ( J.Code.Label (l_corpo_if) );
		J.Method.emit h (J.Code.LoadInt(1));
		J.Method.emit h ( J.Code.Jump (l_continua) );
		J.Method.emit h ( J.Code.Label (l_else) );
		J.Method.emit h (J.Code.LoadInt(0)) ;
		J.Method.emit h ( J.Code.Label (l_continua) )

and

(**************************************************)             
(* fatta al livello di "expr" nell'Ast. Crea le giuste emit per ogni espressione negli stmt*)
compila_expr file_nome env indici_var h = (* compila tipi  const e expr*)
	function
		 Ast.Const(Ast.CBool(false)) -> J.Method.emit h (J.Code.LoadInt(0))
		|Ast.Const(Ast.CBool(true))  -> J.Method.emit h (J.Code.LoadInt(1))
		|Ast.Const(Ast.CString(s))   -> J.Method.emit h (J.Code.LoadString(s))
		|Ast.Const(Ast.CInt(x))      -> J.Method.emit h (J.Code.LoadInt(x))
		|Ast.Const(Ast.CFloat(f))    -> J.Method.emit h (J.Code.LoadFloat(f))
    
         	|Ast.Aexpr(ae)               -> compila_aexpr file_nome env indici_var h ae (* lo fa l'altra funzione *)
		|Ast.Call(nomef, lista_expr) -> 
                        (* metto sulla pila tutte le variabili. cioè tutte le expr verranno caricate *)
			List.iter (fun e -> compila_expr file_nome env indici_var  h e) lista_expr;
		     	(* se Call("somma2", [3+4;5]) restituisce  [J.Type.Int;J.Type.Int] *)
		     	let lista_tipi = List.fold_right (fun t lt ->
                             (conversione_tipi(Type.typeof env t))::lt) lista_expr []
                     	in
		     	let tipo_f = List.assoc nomef env in
		   	begin
                     		match tipo_f with
					Ast.Tfunction(ctypelist,tiporest,path) ->
						if (path = "") then
                           				J.Method.emit h (J.Code.Call(J.Path.parse(file_nome^"/"^nomef), lista_tipi, conversione_tipi(tiporest) ))  
						else	  
			   			J.Method.emit h (J.Code.Call(J.Path.parse(path),lista_tipi, conversione_tipi(tiporest)))
					|_ -> ()
                             
                                        
                     	end
   
                |Ast.Create(tipo, dim)    ->  (* es: a=new int[4+5].*)
		   	(* compilo l'espressione del numero di elementi dell'array  *)
		        compila_expr file_nome env indici_var  h dim;
			(* emetto la NewArray *)
			J.Method.emit h (J.Code.NewArray(conversione_tipi tipo))
			
		|Ast.Cast(nuovotipo,expr) ->
		        (* compilo espressione *)
			compila_expr file_nome env indici_var  h expr; 
		        (* es: (int)4.5 *)
			J.Method.emit h (J.Code.Convert(J.Type.Float,J.Type.Int))
		        
		|Ast.Neg(expr)            ->
			compila_expr file_nome env indici_var  h expr;
		        let tipo_expr = conversione_tipi (Type.typeof env expr) in
		        J.Method.emit h (J.Code.Neg(tipo_expr))
		        (* se expr di tipo Ast.Tint, crea J.Code.Neg(J.Type.Int)  *)
			(* toglie dalla pila, nega e rimette sulla pila *)

		|Ast.Pos(expr)            ->
			compila_expr file_nome env indici_var h expr;

                |Ast.Not(expr)            ->
			compila_expr file_nome env indici_var h expr;
                        J.Method.emit h (J.Code.LoadInt(1));
                        J.Method.emit h (J.Code.Sub(J.Type.Int)) 
                                            
	        |Ast.Plus(e1,e2)          ->
			compila_e_converti file_nome env indici_var e1 e2 h;
		        J.Method.emit h (J.Code.Add(tipo_coppia env  e1 e2) ) 

		|Ast.Minus(e1,e2)         ->
			compila_e_converti file_nome env indici_var e1 e2 h;
		        J.Method.emit h (J.Code.Sub(tipo_coppia env e1 e2) )
 
		|Ast.Times(e1,e2)         ->
			compila_e_converti file_nome env indici_var e1 e2 h;
		        J.Method.emit h (J.Code.Mul(tipo_coppia env  e1 e2) )

                |Ast.Divide(e1,e2)        ->
			compila_e_converti file_nome env indici_var e1 e2 h; (* controllare divisore!=0 se dividendo != 0 *)
		        J.Method.emit h (J.Code.Div(tipo_coppia env  e1 e2 ) )

		|Ast.Rest(e1,e2)          ->
			compila_e_converti file_nome env indici_var e1 e2 h; 
		        J.Method.emit h (J.Code.Rem((*J.Type.Int*)tipo_coppia env  e1 e2 ) )

		|Ast.Eq(e1,e2)            ->
                        (* carica sullo stack le due espressioni *)
                        compila_e_converti file_nome env indici_var e1 e2 h; (* valida anche se 2.0 == 3 e bool1==bool2*)
                        compila_condizione env h e1 e2 J.Cmp.Eq;

		|Ast.Ne(e1,e2)            -> 
                        (* carica sullo stack le due espressioni *)
                        compila_e_converti file_nome env indici_var e1 e2 h; (* valida anche se 2.0 == 3 e bool1==bool2*)
	            	compila_condizione env h e1 e2  J.Cmp.Ne 
                               
                |Ast.Lt(e1,e2)            ->
                        (* carica sullo stack le due espressioni *)
                        compila_e_converti file_nome env indici_var e1 e2 h; (* valida anche se 2.0 == 3 e bool1==bool2*)
	            	compila_condizione env h e1 e2  J.Cmp.Lt

                |Ast.Le(e1,e2)            ->
                        (* carica sullo stack le due espressioni *)
                        compila_e_converti file_nome env indici_var e1 e2 h; (* valida anche se 2.0 == 3 e bool1==bool2*)
			compila_condizione env h e1 e2  J.Cmp.Le

                |Ast.Gt(e1,e2)            ->
                        (* carica sullo stack le due espressioni *)
                        compila_e_converti file_nome env indici_var e1 e2 h; (* valida anche se 2.0 == 3 e bool1==bool2*)
		        compila_condizione env h e1 e2  J.Cmp.Gt

                |Ast.Ge(e1,e2)            ->
                        (* carica sullo stack le due espressioni *)
                        compila_e_converti file_nome env indici_var e1 e2 h;
	                compila_condizione env h e1 e2  J.Cmp.Ge

                |Ast.And(e1, e2)          ->
			compila_expr file_nome env indici_var h e1;
                        compila_expr file_nome env indici_var h e2;
                        J.Method.emit h (J.Code.And)

		|Ast.Or(e1, e2)           ->
			compila_expr file_nome env indici_var h e1;
                        compila_expr file_nome env indici_var h e2;
                        J.Method.emit h (J.Code.Or)
 

               
(*  ES:   let delta = [("n", Ast.Tint); ("s1", Ast.Tstring)];; *)
(*  ES:   let indici= [("n",    0    ); ("s1",      1     )];; - da inserire*)
and

(**************************************************)
(* crea codice per espressione atomiche come variabili e array *)
compila_aexpr file_nome env indici_var h =
	function
      
         	Ast.Id(nome_id) -> (* crea la giusta Load(   J.Type.Int|Float|String,     0|1|...|n    )    ***)
	        	let tipo_id =  conversione_tipi(List.assoc nome_id env) in
		  	let indice_id = List.assoc nome_id indici_var in
	          	J.Method.emit h (J.Code.Load(tipo_id, indice_id ))
          
	  	| Ast.Access(ae, e) ->  (* es: a[5+5] *)
			(* porto sullo stack l'array *)
			compila_aexpr file_nome env indici_var h ae;
			(* porto valore espressioen sullo stack
		   	richiamo questa funzione in modo
		   	nota: nel caso array[n], se 'e' è una aexpr la compila_expr richiama compila_aexpr.
		   	In ogni caso mette l'indice dell'array sulla pila *)
			compila_expr file_nome env indici_var  h e;
			(* ArrayLoad che si trova array e indice sullo stack *)
			let tipo_ae = conversione_tipi2( Type.typeof_aexpr env ae)
                	in
			J.Method.emit h (J.Code.ArrayLoad( tipo_ae ))            
