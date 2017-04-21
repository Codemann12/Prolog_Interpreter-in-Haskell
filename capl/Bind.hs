module Bind where
import Parser
import Type
import System.IO
import System.Exit

capl::IO()
capl = do
     putStrLn "\nWelcome to CEDRIC&ALICE-Prolog (Version 1.0)"
     putStrLn "Copyright (c) 2017 University of Kiel."
     putStrLn "CEDRIC&ALICE-Prolog comes with ABSOLUTELY NO WARRANTY.\n"
     putStrLn "For help, use ?-  help.\n"
     capl' (Prog [])

capl' :: Prog -> IO ()
capl' prog = do putStr "?-  "
                input<-getLine
                if input == "help." then do	putStrLn"\nUse :q to exit"
                	                        putStrLn"Your input must be a goal or a program."
                	                        putStrLn"A goal is something like this: p(A,b)."
                	                        putStrLn"A program is a set of rules."
                	                        putStrLn"You can load a program with : [name]. "
                	                        putStrLn"A rule is something like this p(A,B):-q(C,a), p(a,d).\n"
                      	            else  
                      	              if input == "" then capl' prog
            		                  else if input == ":q"then  
            		                  	      do putStrLn"see you soon."
            		                  	         exitFailure
                                           else if head input== '['&&
                                                   last input== '.'&&
                                                   last (init input)==']'
                                                then  do
                 contents <- readFile (removeVSadd input) 
		 let maybeProg = (parseProg contents) in
			case maybeProg of
		 		(Right x) -> do putStrLn "Well done, you typed in a valid program :)"
						putStrLn (pretty x)
						capl' x
				(Left x)  -> do putStrLn "Please type a valid Prog!"
						capl' prog
                  else 	do
			let maybeGoal = (parseGoal input) in 
	                 case maybeGoal of
		          (Right x) ->  do putStrLn "Do You want to see the pretty SLD-Tree? Y/N ?"
					   input <- getChar
					   putStrLn ""
					   if input == 'Y'|| input == 'y' then
						     do putStrLn ""
							putStrLn(pretty (sld prog x))
				                        capl' prog
					   else do putStrLn "Do You want to use dfs or bfs? d/b ?"
						   input <- getChar
						   putStrLn ""
						   if input == 'd' || input == 'D' then
							      let variables = (makeListofVariables x)
					                          subs	    = (solve dfs prog x) in
							       do putStrLn ""
							          resultHandler (match variables subs)
				                                  capl' prog
							    else let variables = (makeListofVariables x) --we are lazy (same as haskell)
					                             subs      = (solve bfs prog x) in
							          do putStrLn ""
							             resultHandler (match variables subs)
				                                     capl' prog
		          (Left x)  -> do putStrLn "Please type a valid goal or program!"
					  capl' prog

-- helps handling the output of results, especially in case of several results
resultHandler :: [String] -> IO()
resultHandler []         = putStrLn "false."
--resultHandler (x:[])     = putStrLn (x ++ ".")
resultHandler [x]	 = putStrLn (x ++ ".")
resultHandler (x:xs)     = do putStr x
			      input <- getChar
			      if input /= ';' then do putStrLn "Unknown action"
				else do if input == '.' then 
					    do putStrLn ""
					       else do
					       	 putStrLn ""
					       	 resultHandler xs
	
     
removeVSadd:: String -> String
removeVSadd (x:xs) = init (init xs)++".pl"  
