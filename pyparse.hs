module Main where

import Control.Monad.State.Lazy(State, state, runState)

data ParserCtx = ParserCtx [String]
data Parser s = Parser [s]

type ParseFn s r = Parser s -> State ParserCtx (ParseFnRet s r)
type ParseFnRet s r = Maybe (Parser s, r)

peek :: Parser s -> Maybe s
peek (Parser (x:_)) = Just x
peek (Parser []) = Nothing

advance :: Parser s -> Parser s
advance (Parser stream) = Parser $ drop 1 stream

addError :: ParserCtx -> String -> ParserCtx
addError (ParserCtx errs) err = ParserCtx $ errs ++ [err]

consume :: Eq s => s -> String -> ParseFn s s
consume expected err parser =
    state $ \ ctx ->
    case peek parser of
        Just peeked ->
            if peeked == expected
            then (Just (advance parser, peeked), ctx)
            else nothingRes ctx

        Nothing -> nothingRes ctx

    where
        nothingRes ctx = (Nothing, ctx `addError` err)

empty :: ParseFn s ()
empty parser =
    state $ \ ctx ->
    (Just (parser, ()), ctx)

seqp :: ParseFn s a -> ParseFn s b -> ParseFn s (a, b)
seqp a b parser =
    a parser >>= \ mares ->
    case mares of
        Just (aftera, ares) ->
            b aftera >>= \ mbres ->
            case mbres of
                Just (afterb, bres) -> return $ Just (afterb, (ares, bres))
                Nothing -> return Nothing
        Nothing -> return Nothing

choice :: ParseFn s r -> ParseFn s r -> ParseFn s r
choice a b parser =
    a parser >>= \ mares ->
    case mares of
        jar@(Just _) -> return jar

        Nothing ->
            b parser >>= \ mbres ->
            case mbres of
                jar@(Just _) ->
                    return jar

                Nothing -> return Nothing

zeromore :: ParseFn s r -> ParseFn s [r]
zeromore thing = helper
    where
        helper parser =
            thing parser >>= \ mthingres ->
            case mthingres of
                Just (after, res) ->
                    helper after >>= \ mmore ->
                    case mmore of
                        Just (afterafter, more) ->
                            return $ Just (afterafter, res : more)

                        Nothing -> error "zeromore returned Nothing"

                Nothing -> return $ Just (parser, [])

onemore :: ParseFn s r -> ParseFn s [r]
onemore thing = helper []
    where
        helper acc parser =
            thing parser >>= \ mthingres ->
            case mthingres of
                Just (after, res) ->
                    helper (acc ++ [res]) after

                Nothing ->
                    if null acc
                    then return Nothing
                    else return $ Just (parser, acc)

optional :: ParseFn s r -> ParseFn s (Maybe r)
optional thing =
    choice
    (convert thing Just)
    (convert empty $ const Nothing)

convert :: ParseFn s a -> (a -> b) -> ParseFn s b
convert thing conv parser =
    thing parser >>= \ mres ->
    case mres of
        Just (after, res) ->
            return $ Just (after, conv res)

        Nothing -> return Nothing

must :: ParseFn s a -> ParseFn s ()
must ex parser =
    ex parser >>= \ exres ->
    case exres of
        Just _ -> return $ Just (parser, ())
        Nothing -> return Nothing

mustnot :: ParseFn s a -> String -> ParseFn s ()
mustnot ex onerr parser =
    ex parser >>= \ exres ->
    -- TODO: silence errors if ex does not match because if it does not match then this combinator is successful
    case exres of
        Just _ -> return $ Nothing
        Nothing -> return $ Just (parser, ())

main :: IO ()
main = putStrLn "pyparse"
