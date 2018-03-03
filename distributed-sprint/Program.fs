// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let countSplitInv (left: 'a array) (right: 'a array) ar_length =
        let result = Array.zeroCreate<'a> ar_length
        let leftlen = left.Length
        let mutable left_ind, right_ind= 0,0
        let mutable inv_count = 0L
        for k = 1 to ar_length-1 do
            if left_ind >= leftlen then 
                result.[k] <- right.[right_ind]; right_ind<-right_ind+1
            elif right_ind > right.Length then 
                result.[k] <- left.[left_ind]; left_ind<-left_ind+1
            elif left.[left_ind] < right.[right_ind] then 
                result.[k] <- left.[left_ind]; left_ind<-left_ind + 1  
            else result.[k] <- right.[right_ind]; right_ind <- right_ind + 1; inv_count <- inv_count + int64(leftlen - left_ind)
        
        result,inv_count

    let rec sortAndCount (arr:'a array) = 
        match arr.Length with
        | 0 | 1 -> arr, 0L
        | n -> 
                let (b,x) = sortAndCount (arr.[0..n/2-1])
                let (c,y) = sortAndCount(arr.[n/2..n-1])
                let (d,z) = countSplitInv b c n

                d, z+x+y

    let testList = [|1;3;5;2;4;6|]
    let result= testList |> sortAndCount
    printfn "Hello World from \n %A!" result
    0 // return an integer exit code
