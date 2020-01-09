module Chess

type Color = white | black
type Position = int * int

[<AbstractClass>]
type chessPiece (color : Color) =
    let mutable _position : Position option = None
    abstract member nameOfType : string
    member this.color = color
    member this.position
        with get() = _position
        and set(pos) = _position <- pos
    override this.ToString() =
        match color with
            White -> (string this.nameOfType.[0]).ToUpper()
            | Black -> (string this.nameOfType.[0]).ToLower()
    abstract member candidateRelativeMoves : Position list list
    member this.availableMoves (board : Board) : (Position list * chessPiece list) =
        board.getVacantNeighbours this
and Board() =
    let _array = Collections.Array2D.create < chessPiece option > 8 8 None
    let validPositionWrap (pos : Position) : Position option =
    let (rank, file) = pos
    if rank < 0 || rank > 7 || file < 0 || file > 7 then
        None
    else
        Some (rank, file)
    let relativeToAbsolute (pos : Position) (lst : Position list) : Position list =
        let addPair (a : int, b : int) (c : int, d : int) : Position =
            (a + c, b + d)
        List.map (addPair pos) lst
        |> List.choose validPositionWrap
    member this.Item
        with get (a : int, b : int) = _array.[a, b]
        and set (a : int, b : int) (p : chessPiece option) =
            if p.IsSome then p.Value.position <- Some(a, b)
            _array.[a, b] <- p
    override this.ToString() =
        let rec boardStr (i : int) (j : int) : string =
            match (i, j) with
                (8 ,0) -> ""
                | _ ->
                    let stripOption (p : chessPiece option) : string =
                        match p with
                            None -> ""
                            | Some p -> p.ToString()
                        let pieceStr = stripOption _array.[7 - i, j]
                        let lineSep = " " + String . replicate (8*4 -1) "-"
                            match (i ,j ) with
                                (0 ,0) ->
                                    let str = sprintf "%s\n| %1s " lineSep pieceStr
                                    str + boardStr 0 1
                                (i ,7) ->
                                    let str = sprintf "| %1s |\n%s\n" pieceStr lineSep
                                    str + boardStr ( i +1) 0
                                | (i, j) ->
                                    let str = sprintf "| %1s " pieceStr
                                    str + boardStr i (j +1)
        boardStr 0 0
    member this.move(source : Position) ( target : Position ) : unit =
        this.[fst target, snd target] <- this.[fst source, snd source]
        this.[fst source, snd source] <- None
        member this.getVacantNOccupied (run : Position list) : Position list * (chessPiece option)) =
            try
                let idx = List.findIndex (fun (i , j) -> this.[i, j].IsSome) run
                let (i , j ) = run .[ idx ]
                let piece = this .[ i , j ] // The first non - vacant neighbour
                if idx = 0 then
                ([] , piece )
                else
88 ( run .[..( idx -1) ] , piece )
89 with
90 _ -> ( run , None ) // outside the board
91 /// find the list of all empty squares and list of neighbours
92 member this . getVacantNNeighbours ( piece : chessPiece ) :
( Position list * chessPiece list ) =
93 match piece . position with
94 None ->
95 ([] ,[])
96 | Some p ->
97 let convertNWrap =
98 ( relativeToAbsolute p ) >> this . getVacantNOccupied
99 let vacantPieceLists = List . map convertNWrap
    piece