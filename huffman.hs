import Data.List
import Data.Word
import Data.Maybe
import System.Environment
import qualified Data.ByteString
import qualified Data.Map.Strict as Map

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show --Define recursive Priority Queue as Binary Tree
type Bits = [Int]
type Priority = Int
type PQueue a = BTree (Priority, a)
type HTree = BTree String


enqueue :: (Priority,a) -> PQueue a -> PQueue a
enqueue x Empty = Node x Empty Empty
enqueue (p1,x) (Node (p2,y) l r) 
  | p1 <= p2 = Node (p2,y) (enqueue (p1,x) l) r
  | p1 > p2 = Node (p2,y) l (enqueue (p1,x) r)

dequeueMax :: PQueue a -> ((Priority, a),PQueue a)
dequeueMax (Node x l Empty) = (x, l)
dequeueMax (Node x l r) = let (popped, subTree) = dequeueMax r in (popped, Node x l subTree)

dequeueMin :: PQueue a -> ((Priority, a),PQueue a)
dequeueMin (Node x Empty r) = (x, r)
dequeueMin (Node x l r) = let (popped, subTree) = dequeueMin l in (popped, Node x subTree r)

queueFromList :: [(Priority, a)] -> PQueue a
queueFromList = foldr enqueue Empty


countOccurs :: (Eq a, Ord a) => [a] -> [(Int,a)]
countOccurs list = map countObj $ group $ sort list 
  where countObj = \(x:xs) -> (1 + (length xs),x)

makeHuffmanTree :: String -> HTree
makeHuffmanTree str = oneTreeStep . queueFromList $ map (\(occurs,val) -> (occurs, Node [val] Empty Empty)) (countOccurs str)

oneTreeStep :: PQueue (HTree)-> HTree
oneTreeStep (Node (_,x) Empty Empty) = x --If there is only one tree left in the queue that is the final HTree. We don't care about its weight
oneTreeStep queue = let ((p1,min1),q1) = dequeueMin queue
                        ((p2,min2),q2) = dequeueMin q1
                        newQueue  = enqueue ((p1+p2,Node "" min2 min1)) q2 
                    in oneTreeStep newQueue

convToMap :: HTree -> Map.Map Char Bits -> Bits -> Map.Map Char Bits
convToMap Empty mp _ = mp
convToMap (Node x l r) mp ds =  let leftDone = convToMap l mp (0:ds)
                                    rightDone = convToMap r leftDone (1:ds)
                                in case x of
                                    ""    ->  rightDone 
                                    (x:_) ->  Map.insert x ds mp

getHuffmanDict :: HTree -> Map.Map Char Bits
getHuffmanDict tree = fmap (reverse) (convToMap (tree) Map.empty [])

encodeToDirections :: String -> HTree -> Bits
encodeToDirections inp tree =  let treeMap = getHuffmanDict tree in
                                   concat $ map (\a -> fromMaybe [] $ Map.lookup a treeMap) inp

directionToByte :: Bits -> Word8
directionToByte (a:b:c:d:e:f:g:h:rest) = fromIntegral (128*a + 64*b + 32*c + 16*d + 8*e + 4*f + 2*g + 1*h)

encodeToByteString :: Bits -> Data.ByteString.ByteString
encodeToByteString (a:b:c:d:e:f:g:h:rest) = Data.ByteString.cons  (directionToByte [a,b,c,d,e,f,g,h]) (encodeToByteString rest) 
encodeToByteString fewerThan8 = Data.ByteString.singleton $ directionToByte (fewerThan8 ++ (replicate (8 - (length fewerThan8)) 0))

intToBinary :: Int -> Bits
intToBinary 0 = [0]
intToBinary 1 = [1]
intToBinary c = (c `mod` 2):(intToBinary (c `div` 2))

intToPaddedBinary :: Int -> Bits
intToPaddedBinary i = let rev = intToBinary i in (replicate (8 - (length rev)) 0) ++ (reverse rev)

charToPaddedBinary :: Char -> Bits
charToPaddedBinary x = intToPaddedBinary . fromEnum $ x 
                            
treeToBits :: HTree -> Bits
treeToBits (Node (x:_) Empty Empty) = 1:(charToPaddedBinary x)
treeToBits (Node _ l r) = let left  = treeToBits l
                              right = treeToBits r
                          in  [0] ++ left ++ right

decodeFromByteString :: Data.ByteString.ByteString -> Bits
decodeFromByteString bts = if Data.ByteString.null bts then [] else
                            (intToPaddedBinary . fromIntegral $ (Data.ByteString.head bts)) ++ decodeFromByteString (Data.ByteString.tail bts) 

bitsToTree :: Bits -> (HTree,Bits)
bitsToTree (0:bs) = let (left,remainingBits') = bitsToTree bs 
                        (right,remainingBits) = bitsToTree remainingBits' in
                        (Node "" left right,remainingBits)
bitsToTree (1:a:b:c:d:e:f:g:h:rest) = let char = toEnum . fromIntegral $ (directionToByte [a,b,c,d,e,f,g,h]) in
                      (Node [char] Empty Empty,rest)

decodeChar :: HTree -> Bits -> (Char,Bits)
decodeChar (Node "" l r) (0:bs) = decodeChar l bs
decodeChar (Node "" l r) (1:bs) = decodeChar r bs
decodeChar (Node (x:_) Empty Empty) bs = (x,bs)  
decodeChar a b = error ("Error: Unable to fully decode. Make sure input files are newline terminated! See output file for part decoded result.")

decode :: HTree -> Bits -> String
decode tree [] = ""
decode tree bs = let  (char,remainingBits) = decodeChar tree bs in 
                      if char == '\n' && sum remainingBits == 0 then [char] else char:(decode tree remainingBits)

main = do
  args <- getArgs
  case args of 
        ("encode":inputFile:outputFile:_) -> do
          str <- readFile inputFile
          let tree = makeHuffmanTree str
          Data.ByteString.writeFile outputFile (encodeToByteString ((treeToBits tree)++(encodeToDirections str tree)))
                                              
        ("decode":inputFile:outputFile:_) -> do
          fileData <- Data.ByteString.readFile inputFile
          let (tree,msg) = bitsToTree (decodeFromByteString fileData)
          print tree
          let res = decode tree msg
          writeFile outputFile res
        _                               -> putStr "Usage: huffman [encode/decode] inputFile outputFile \nFiles to be compressed should be newline terminated\n"