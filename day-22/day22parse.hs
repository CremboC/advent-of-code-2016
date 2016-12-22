module Day22Parse (parse, Coord, Node(Node, loc, size, used, available)) where

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     91T   71T    20T   78%
type Coord = (Int, Int)
data Node = Node { loc :: Coord, size :: Int, used :: Int, available :: Int} deriving (Show, Eq, Ord)

parse :: String -> Maybe Node
parse = parseMaybe (node)
    where
        node = f <$> (string "/dev/grid/node-x" *> int) <*> (string "-y" *> int) <*> fsspace <*> fsspace <*> (fsspace <* fsspace)
        f x y = Node (x, y)
        fsspace = (skipMany spaceChar) *> int <* (choice [char 'T', char '%'])
        int = fromIntegral <$> L.integer :: ParsecT Dec String a Int