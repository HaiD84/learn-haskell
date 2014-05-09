import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as S

{-
 - *Main> Lazy.pack [99, 97, 110]
 - Chunk "can" Empty
 - *Main> Lazy.pack [98..120]
 - Chunk "bcdefghijklmnopqrstuvwx" Empty
 -
 - *Main> :t Lazy.pack
 - Lazy.pack :: [GHC.Word.Word8] -> Lazy.ByteString
 -
 - Word8 cuts number to module 256
 - *Main> Lazy.pack [80, 336]
 - Chunk "PP" Empty
 -
 - Convert ByteString to lazy (and back with toChunks):
 - *Main> Lazy.fromChunks [S.pack [100..110], S.pack [43, 44, 45], S.pack [46, 47, 48]]
 - Chunk "defghijklmn" (Chunk "+,-" (Chunk "./0" Empty))
 -}
