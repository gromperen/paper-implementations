-- https://doi.org/10.1016/0304-3975(88)90113-2
-- https://en.wikipedia.org/wiki/Lyndon_word
import Data.List
import Data.Maybe



nextLyndonWord :: Int -> [Char] -> String -> Maybe String
nextLyndonWord n alpha w = case x of [] -> Nothing
                                     otherwise -> Just (init x ++ [nextVal $ last x])
        where x = reverse . dropWhile (== last alpha) . reverse . take n $ cycle w
              nextVal p = fromJust . lookup p $ zip alpha (tail alpha)

allLyndonWords :: Int -> [Char] -> [String]
allLyndonWords n alpha = go (init alpha)
  where go w = case x of Nothing -> []
                         (Just k) -> k : go k
          where x = nextLyndonWord n alpha w
