open Chess
open Pieces

let printPiece(board : board) (p : chessPiece) : unit = 
    printfn "%A: %A %A" p p.position (p.availableMoves board)


let board = Chess.Board()

let pieces = [|
    king(White) :> chessPiece
    king(black) :> chessPiece
    
|]