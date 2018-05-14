{-# LANGUAGE GADTs, FlexibleInstances #-}

module Tucker.Console where

import Data.List
import Data.Char

import Text.Read

import Control.Monad
import Control.Applicative

import Tucker.Util
import Tucker.Error

{-

POSIX recommends these conventions for command line arguments. getopt (see Getopt) and argp_parse (see Argp) make it easy to implement them.

1. Arguments are options if they begin with a hyphen delimiter (‘-’).
2. Multiple options may follow a hyphen delimiter in a single token if the options do not take arguments. Thus, ‘-abc’ is equivalent to ‘-a -b -c’.
3. Option names are single alphanumeric characters (as for isalnum; see Classification of Characters).
4. Certain options require an argument. For example, the ‘-o’ command of the ld command requires an argument—an output file name.
5. An option and its argument may or may not appear as separate tokens. (In other words, the whitespace separating them is optional.) Thus, ‘-o foo’ and ‘-ofoo’ are equivalent.
6. Options typically precede other non-option arguments.
7. The implementations of getopt and argp_parse in the GNU C Library normally make it appear as if all the option arguments were specified before all the non-option arguments for the purposes of parsing, even if the user of your program intermixed option and non-option arguments. They do this by reordering the elements of the argv array. This behavior is nonstandard; if you want to suppress it, define the _POSIX_OPTION_ORDER environment variable. See Standard Environment.

The argument ‘--’ terminates all options; any following arguments are treated as non-option arguments, even if they begin with a hyphen.
A token consisting of a single hyphen character is interpreted as an ordinary non-option argument. By convention, it is used to specify input from or output to the standard input and output streams.
Options may be supplied in any order, or appear multiple times. The interpretation is left up to the particular application program.
GNU adds long options to these conventions. Long options consist of ‘--’ followed by a name made of alphanumeric characters and dashes. Option names are typically one to three words long, with hyphens to separate words. Users can abbreviate the option names as long as the abbreviations are unique.

To specify an argument for a long option, write ‘--name=value’. This syntax enables a long option to accept an argument that is itself optional.

Eventually, GNU systems will provide completion for long option names in the shell.

-}

{-

three types of options:
1. -[char]\s?[argument]
2. --[string][=argument]
3. everything after -- are non-option arguments

-}

class ArgType t where
    parseArg :: String -> Either String t

instance ArgType String where
    parseArg = Right

instance ArgType Bool where
    parseArg str =
        case map toLower (trim str) of
            "true" -> Right True
            "false" -> Right False
            _ -> Left ("unknown expression for bool type: " ++ str)

instance ArgType Int where
    parseArg str =
        case readMaybe str of
            Just v -> Right v
            Nothing -> Left ("failed to parse the integer argument " ++ str)

-- if the option name is single-char, we are expecting a single hyphen before it
-- data Option f
--     = NoArg [String] f String
--     | WithArg [String] (a -> f) String
data Option f where
    NoArg :: [String] -> f -> String -> Option f
    WithArg :: ArgType a => [String] -> (a -> f) -> String -> Option f

data MatchResult f
    = Match (f, [String])
    | NoMatch
    | MatchError String

optionToNote :: Option f -> String
optionToNote (NoArg _ _ note) = note
optionToNote (WithArg _ _ note) = note

-- generate help messages
genHelp :: [Option f] -> String
genHelp opts =
    let flags' =
            flip map opts $ \opt ->
                case opt of
                    NoArg names _ _ ->
                        intercalate ", " (map (gen False) names)

                    WithArg names _ _ ->
                        intercalate ", " (map (gen True) names)

        flags = "flag syntax" : flags'
        notes = "details" : map optionToNote opts

        gen has_arg name =
            if length name == 1 then
                "-" ++ name ++ (if has_arg then "<value>" else "")
            else
                "--" ++ name ++ (if has_arg then "=<value>" else "")

        align_n = 3 + length (maximumBy (\a b -> compare (length a) (length b)) flags)
    in
        intercalate "\n" $
        flip map (zip flags notes) $ \(flag, note) ->
            flag ++ replicate (align_n - length flag) ' ' ++ note

-- find the first item that's not Left Nothing
-- otherwise return Left Nothing
matchResultFold :: [MatchResult f] -> MatchResult f
matchResultFold [] = NoMatch
matchResultFold (NoMatch:rst) = matchResultFold rst
matchResultFold (a:_) = a

-- assuming the arg begins with '-'
-- return Right _ for a correct match
-- Left Nothing for a non-match
-- Left (Just _) for a error(option matches, but syntax error)
matchOption :: String -> [String] -> Option f -> MatchResult f -- Either (Maybe TCKRError) (f, [String])
matchOption arg rest (NoArg names flag _) =
    matchResultFold $
    flip map names $ \name ->
        case length name of
            0 -> error "empty option name"
            1 ->
                if arg == '-':name then Match (flag, rest)
                else NoMatch
            _ ->
                if arg == "--" ++ name then Match (flag, rest)
                else NoMatch

matchOption arg rest (WithArg names flag_gen _) =
    matchResultFold $
    flip map names $ \name ->
        case length name of
            0 -> error "empty option name"
            1 ->
                if ('-':name) `isPrefixOf` arg then
                    if length arg > 2 then
                        -- the argument is directly appended to the option
                        -- e.g. -fhi.h
                        case parseArg (drop 2 arg) of
                            Right arg -> Match (flag_gen arg, rest)
                            Left err ->
                                MatchError ("argument parsing error for option -" ++ name ++ ": " ++ err)
                    else
                        -- read the next argument
                        if null rest then
                            MatchError ("expecting one argument for option -" ++ name)
                        else
                            case parseArg (head rest) of
                                Right arg -> Match (flag_gen arg, tail rest)
                                Left err ->
                                    MatchError ("argument parsing error for option -" ++ name ++ ": " ++ err)
                else NoMatch

            _ ->
                let pref = "--" ++ name in
                if pref `isPrefixOf` arg then
                    let suf = drop (length pref) arg in
                    if "=" `isPrefixOf` suf then
                        case parseArg (tail suf) of
                            Right arg -> Match (flag_gen arg, rest)
                            Left err ->
                                MatchError ("argument parsing error for long option --" ++ name ++ ": " ++ err)
                    else
                        MatchError ("expecting one argument for long option --" ++ name)
                else
                    NoMatch

isOption arg = not (null arg || head arg /= '-')

parseFlags :: [String] -> [Option f] -> Either TCKRError ([f], [String])
parseFlags args opts =
    case result of
        Right ([], flags, non_opt) -> Right (flags, non_opt)
        Left err -> Left (TCKRError err)

    where
        Just result =
            first pred $
            iterate proc (Right (args, [], []))

        pred (Left err) = True
        pred (Right ([], _, _)) = True
        pred _ = False

        proc (Right (args, flags, non_opt)) =
            if null args then Right (args, flags, non_opt ++ [""])
            else
                let arg = head args in
                if not (isOption arg) then
                    Right (tail args, flags, non_opt ++ [arg])
                else if arg == "--" then
                    -- all arguments after "--" are non-option arguments
                    Right ([], flags, non_opt ++ tail args)
                else
                    let res = matchResultFold $
                              map (matchOption arg (tail args)) opts
                    in case res of
                        Match (flag, rest) -> Right (rest, flags ++ [flag], non_opt)
                        NoMatch -> Left ("unable to parse option " ++ arg)
                        MatchError err -> Left err
