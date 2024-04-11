module Abstract.Machine (
    Algorithm (),
    Operation (..),
    assignment,
    easyCode,
) where

import Abstract.Machine.Algorithm (Algorithm (), assignment)
import Abstract.Machine.Operation (Operation (..))


easyCode ∷ Algorithm a
easyCode = assignment 1 $ SFTL (ADD (LIT 42) (VAR 0)) (LIT 1)

{-
prettyPrinter :: (Integral i, IsString s, Semigroup s) => Interpretation i s
prettyPrinter = Interpretation $ \_ ->
    let renderAll :: (Integral i, IsString s, Semigroup s) => Operation i -> s
        renderAll = \case
              ADD  x y -> renderBin "+" x y
              SUB  x y -> renderBin "-" x y
              MUL  x y -> renderBin "*" x y
              AND  x y -> renderBin "&" x y
              OR   x y -> renderBin "|" x y
              XOR  x y -> renderBin "^" x y
              SFTL x y -> renderBin "<<" x y
              SFTR x y -> renderBin ">>" x y
              ROTL x y -> renderBin "<|" x y
              ROTR x y -> renderBin "|>" x y
              otherOp  -> renderSub otherOp

        renderBin :: (Integral i, IsString s, Semigroup s) => s -> Operation i -> Operation i -> s
        renderBin op lhs rhs = renderSub lhs <> " " <> op <> " " <> renderSub rhs

        renderHex :: (Integral i, IsString s) => i -> s
        renderHex = fromString . ("0x" <>) . (`showHex` "")

        renderSub :: (Integral i, IsString s, Semigroup s) => Operation i -> s
        renderSub (LIT val) = renderHex val
        renderSub (NEG val) = "~" <> renderSub val
        renderSub (VAR   0) = "input"
        renderSub (VAR ref) = renderVar ref
        renderSub otherOp   = "( " <> renderAll otherOp <> " )"

        renderVar :: (IsString s, Semigroup s) => Word -> s
        renderVar ref =
            let prefix
                    | ref < 10  = "var0"
                    | otherwise = "var"
            in  prefix <> fromString (show ref)

        run :: (Integral i, IsString s, Semigroup s) => (Word, Operation i) -> s
        run (note, op) = renderVar note <> " = " <> renderAll op
    in  run
-}
{-

Binary Operators: BV -> BV -> BV

( + ) Addition       modulo \(\mathbb{Z}^{d}\)
( - ) Subtraction    modulo \(\mathbb{Z}^{d}\)
( * ) Multiplication modulo \(\mathbb{Z}^{d}\)
( / ) Division       modulo \(\mathbb{Z}^{d}\)
(.&.) Bitwise AND
(.|.) Bitwise OR
-}

{-

Unary Operatiors: BV -> BV

complement Bitwise NEG

-}

{-

Nullary Operators: BV

Hexadecimal literals \(\in \mathbb{Z}^{d}\)

-}
