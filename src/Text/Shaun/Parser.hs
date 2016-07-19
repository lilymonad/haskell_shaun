module Text.Shaun.Parser
  ( parseShaun
  )
where

import Text.Parsec
import Text.Shaun.Lexer
import Text.Shaun.Types

-- those are helper functions
obj = SObject
num = SNumber
str = SString
bool = SBool

-- those are parsing functions which consume a [Token] stream
snAtom :: Parsec [Token] () Atom
snAtom = getPosition >>= \p -> token show (const (incSourceColumn p 1)) getAtom
  where getAtom (TAtom a) = Just a
        getAtom _ = Nothing

snKey :: String -> Parsec [Token] () String
snKey s = getPosition >>= \p -> token show (const (incSourceColumn p 1)) getKey
  where getKey (TKey k) = if s == k then Just s else Nothing
        getKey _ = Nothing

snId :: Parsec [Token] () String
snId = getPosition >>= \p -> token show (const (incSourceColumn p 1)) getId
  where getId (TId i) = Just i
        getId _ = Nothing

snComment :: Parsec [Token] () String
snComment = getPosition >>= \p -> token show (const (incSourceColumn p 1)) getComment
  where getComment (TComment c) = Just c
        getComment _ = Nothing

isComment (TComment _) = True
isComment _ = False

-- | @parseShaun@ doesn't parse directly a @String@ of @ByteString@ value.
-- This function is used to parse a [Token] stream.
-- If you want to parse a @String@ Use @read :: String -> ShaunValue@ which is
-- a combination of @makeLexer@ and @parseShaun@ instead.
parseShaun :: [Token] -> Either ParseError ShaunValue
parseShaun = parse rootP "" . filter (not . isComment)
  where rootP = try objP <|> rawObjP
        objP = between (snKey "{") (snKey "}") rawObjP
        rawObjP = fmap obj $ many $ do
          k <- snId <?> "object attribute key"
          snKey ":"
          v <- valP <?> "object attribute value"

          return (k, v)


        valP = objP <|> listP <|> atomP <?> "shaun value"

        listP = fmap SList $ between (snKey "[") (snKey "]") (many valP)

        atomP = try numberP <|> otherAtomP

        numberP = do
          a <- snAtom
          case a of
            ADouble d -> do
              unit <- option Nothing $ do
                i <- snId
                notFollowedBy (snKey ":")
                return (Just i)

              return (num d unit)
            _ -> parserFail "expected a number"

        otherAtomP = fmap toShaun snAtom

