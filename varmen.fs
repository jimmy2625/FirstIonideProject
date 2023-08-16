module varmen

    (* 1: Grayscale images *)
    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img =
        Quad (Square 255uy,
                Square 128uy,
                Quad(Square 255uy,
                    Square 128uy,
                    Square 192uy,
                    Square 64uy),
                Square 0uy)
        
    (* Question 1.1 *)
    //If it is a Square i return 0, if it is a Quad i will recursively call maxDepth on each quadrant which adds 1 for each level of nesting
    //I then max to find the max level of nesting in the given grayscale
    let rec maxDepth img =
        match img with
        |Square _ -> 0
        |Quad (a,b,c,d) ->
            let depthA = maxDepth a
            let depthB = maxDepth b
            let depthC = maxDepth c
            let depthD = maxDepth d
            1 + max (max depthA depthB) (max depthC depthD)
            
    (* Question 1.2 *)
    //If Square, I just return it
    //If quad i call the mirror recursively on each quad with the mirrored quadrants given in the question
    let rec mirror img =
        match img with
        |Square x -> Square x
        |Quad(a,b,c,d) -> Quad(mirror b, mirror a, mirror d, mirror c)
        
    (* Question 1.3 *)
    //If Square, I just return it
    //If quad, i call operate with the function f given on each quadrant and then apply the function f to the operated quadrants
    let rec operate f img =
        match img with
        |Square v -> Square v
        |Quad(a,b,c,d) -> f (operate f a) (operate f b) (operate f c) (operate f d)
        
    let average a b c d =
        match a, b, c, d with
        | Square v1, Square v2, Square v3, Square v4 ->
          Square (((float v1 + float v2 + float v3 + float v4) / 4.0) |> uint8)
        | _, _, _, _ -> Quad (a, b, c, d)
        
    //I create the mirrored quadrants in a new Quad to satisfy the typing of the operate function
    let mirror2 img =
        let mirrorQuadrants q1 q2 q3 q4 =
            Quad (q2, q1, q4, q3)
        operate mirrorQuadrants img
        
    (* Question 1.4 *)
    //If Square, I just return it
    //If quad then i check if the compressed quadrants all are the same, if yes then I return one of the compressed quadrants.
    //If no, I just return a new Quad with the compressed quadrants
    let rec compress img =
        match img with
        |Square x -> Square x
        |Quad(a,b,c,d) -> if compress a = compress b && compress a = compress c && compress a = compress d then compress a else Quad(compress a, compress b, compress c, compress d)
        
        
    (* 2: Code Comprehension *)
    let rec foo f =
        function
        | [] -> []
        | x :: xs when f x -> x :: (foo f xs)
        | _ :: xs -> foo f xs
    let rec bar fs xs =
        match fs with
        | [] -> xs
        | f :: fs' -> bar fs' (foo f xs)
        
    //helper functions to test foo and bar
    let isEven x = x % 2 = 0
    let isSix x = x = 6
         
    (* Question 2.1 *)
    
    (*
        the type of foo is = ('a -> bool) -> 'a list -> 'a list
        the type of bar is = ('a -> bool) list -> 'a list -> 'a list
        
        foo acts as List.filter by only adding the elements where the predicate is true
        bar uses foo to filter with a list of predicates instead. Only the elements where all the predicates in the list are true will be added
        
        appropriate name for foo = filterList
        appropriate name for bar = filterListWithFunctions
        
        the underscore in foo is good coding practice since the head of the list is not relevant to the logic of the case
        if a placeholder name like "x" or "a" replaced the underscore, it would produce a "value is unused", therefore making the underscore good practice in this specific case
    *)
    
    (* Question 2.2 *)
    //I use List.fold since im going through the whole list of functions
    //Since the question states that I can't use foo, I will use List.filter instead to act as a replacement for foo
    
    let bar2 fs xs = List.fold (fun acc f -> List.filter f acc) xs fs
    
    (* Question 2.3 *)
    //I use List.forall to check if all the predicates in the fs list holds true for the given x
    let baz (fs: ('a -> bool) list) (x: 'a) : bool = List.forall (fun f -> f x) fs
    
    (* Question 2.4 *)
    (*
        bar is the only tail-recursive function sine the "bar fs' (foo f xs)" recursive call is the last operation.
        
        foo is not tail-recursive since in the second pattern, a cons operation of the x is done AFTER the recursive call to (foo f xs), therefore making foo not tail-recursive
        
        Evaluation of foo =
        
        let f x = x < 4
        
        foo f [5;3;2;6]
        = foo f [3;2;6]
        = 3 :: (foo f [2;6]
        = 3 :: 2 :: (foo f [6])
        = 3 :: 2 :: []
        = [3;2]
        
        Here it is clear, that the cons operations have to be done AFTER the recursive call which makes it not tail-recursive
    *)
    
    (* Question 2.5 *)
    //I create an inner aux function that takes the list xs and the continuation c
    //I create the same match cases but run the continuation on each case   
    let fooTail f =
        let rec aux c =
            function
            |[] -> c []
            |x :: xs when f x -> aux (fun result -> c(x::result)) xs
            |_ :: xs -> aux c xs
        aux id
        
    (* 3: Guess a number *)
    type guessResult = Lower | Higher | Equal

    type oracle =
        { max: int
          f: int -> guessResult }
        
    (* Question 3.1 *)
    //
    let validOracle (o: oracle) =
        let max_num = o.max

        // Condition 1: Check that Equal is returned for a single number x between 1 and max inclusive
        let equalCount =
            [1 .. max_num]
            |> List.filter (fun x -> o.f x = Equal)
            |> List.length

        if equalCount <> 1 then
            false
        else
            // Find the target number for later use in conditions 2 and 3
            let targetNum =
                [1 .. max_num]
                |> List.find (fun x -> o.f x = Equal)

            // Condition 2: Check that Higher is returned for all numbers greater than or equal to 1 and strictly smaller than x
            let condition2 =
                [1 .. targetNum - 1]
                |> List.forall (fun x -> o.f x = Higher)

            // Condition 3: Check that Lower is returned for all numbers strictly greater than x and smaller than or equal to max
            let condition3 =
                [targetNum + 1 .. max_num]
                |> List.forall (fun x -> o.f x = Lower)

            // If all conditions are satisfied, return true; otherwise, return false
            condition2 && condition3
            
    let randomOracle (m: int) (oseed: int option) =
    // Create a random number generator with the provided seed or a new seed if none is provided
        let random =
            match oseed with
            | Some seed -> System.Random(seed)
            | None -> System.Random()

        // Generate a random number r between 1 and m inclusive
        let r = random.Next(1, m + 1)

        // Construct the oracle based on the random number r
        let f x =
            if x = r then
                Equal
            elif x < r then
                Higher
            else
                Lower

        { max = m; f = f }
        
    let findNumber (o: oracle) =
        let max_num = o.max

        let rec binarySearch a b =
            if a > b then
                []  // No valid range to search, return an empty list
            else
                let g = a + (b - a) / 2

                match o.f g with
                | Equal -> [g]  // Correct guess, return it as a single-element list
                | Lower -> binarySearch a (g - 1)  // Guess too high, search in the range a to (g - 1)
                | Higher -> binarySearch (g + 1) b  // Guess too low, search in the range (g + 1) to b

        binarySearch 1 max_num