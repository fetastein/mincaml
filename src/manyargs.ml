let f = fun x -> fun y -> fun z -> fun o -> fun p -> fun q -> fun r -> fun s -> x in let rec times n = if n = 0 then 0 else times ((((((((f (n-1)) 2) 3) 4) 5) 6) 7) 8) in times 10000
