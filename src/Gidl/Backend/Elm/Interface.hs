{-# LANGUAGE OverloadedStrings #-}

-- | Produce an Elm module for an 'Interface'. This provides type
-- definitions and useful helpers for integrating apps written in
-- the Elm Architecture.
-- 
-- Unlike the Ivory, Tower, and Haskell backends, we don't have a
-- Producer/Consumer distinction. Instead, the Elm backend is only
-- intended to be a client of the RPC server. This is reflected in the
-- selection of messages that are exposed in this convenience
-- interface. For example, you can send @set@ requests, but not
-- receive them.
module Gidl.Backend.Elm.Interface where

import Prelude ()
import Prelude.Compat

import Data.Char (toUpper)
import Data.List (intercalate,nub)

import Gidl.Backend.Elm.Common
import Gidl.Interface
    (Interface(..),Method(..),MethodName,interfaceMethods,readable,writable)

import Gidl.Types (Type(..))

import Ivory.Artifact
    (Artifact,artifactPath,artifactText)

import Text.PrettyPrint.Mainland

interfaceModule :: ModulePath -> Interface -> Artifact
interfaceModule mp i@(Interface rawName _ _) =
  artifactPath (intercalate "/" mp) $
  artifactText ((ifModuleName i) ++ ".elm") $
  prettyLazyText 1000 $
  stack $
    [ "module" <+> im (ifModuleName i) <+> "exposing (..)"
    , empty
    , stack $ typeimports ++ extraimports
    , empty
    , clientIfDecl tmp rawName (ifModuleName i) (clientMessages i)
    ]
  where
  im mname = mconcat $ punctuate dot
                     $ map text (mp ++ [mname])
  tm mname = mconcat $ punctuate dot
                     $ map text (tmp ++ [mname])
  tmp = typepath mp ++ ["Types"]
  typepath = reverse . drop 1 . reverse
  utils = mconcat $ punctuate dot $ map text (typepath mp ++ ["Utils"])

  typeimports = map (\a -> importDecl tm a)
              $ nub
              $ map importType
              $ clientMessageTypes i

  extraimports = [ "import" <+> utils <+> "as Utils"
                 , "import Http"
                 , "import Json.Decode"
                 , "import Json.Encode"
                 , "import Task"
                 , "import Time exposing (Time)"
                 ]

data ClientMessage
  = GetMessage String Type
  | SetMessage String Type
  deriving (Eq, Show)

clientMessages :: Interface -> [ClientMessage]
clientMessages i = concatMap f (interfaceMethods i)
  where
  f :: (MethodName, Method) -> [ClientMessage]
  f (_, (StreamMethod _    _)) = [] -- unimplemented
  f (n, (AttrMethod   perm t)) =
    [ SetMessage n t | writable perm ] ++
    [ GetMessage n t | readable perm ]

clientMessageTypes :: Interface -> [Type]
clientMessageTypes i =
     [ t | (GetMessage _ t) <- clientMessages i]
  ++ [ t | (SetMessage _ t) <- clientMessages i]

-- | Produce the body of the module for an 'Interface'
clientIfDecl :: ModulePath -> String -> String -> [ClientMessage] -> Doc
clientIfDecl _ _ interfaceName [] = stack $
    [ "{-| Cannot define client for" <+> text interfaceName <> colon
      <+> "interface is empty -}"
      , "unused = True"
    ]
clientIfDecl tmp rawName interfaceName ms = stack $
    [ "{-| A type containing all of the fields and poll rates of the interface,"
      <+> "useful for keeping state in a model -}"
    , "type alias" <+> text interfaceName <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        concat [ [ text (toCamel n) <+> colon <+> elmTypeQName tmp t
                 , pollRateName n <+> colon <+> "Maybe Time" ]
               | (GetMessage n t) <- ms ]
    , empty
    , "{-|" <+> text interfaceName
      <+> "initialized with (arbitrary) default values -}"
    , "init" <+> colon <+> text interfaceName
    , "init" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        concat [ [ text (toCamel n) <+> equals <+> typeInit tmp t
                 , pollRateName n <+> equals <+> "Nothing" ]
               | (GetMessage n t) <- ms ]
    , empty
      -- client interface
    , "{-| Client interface for" <+> text interfaceName <+> "backed by a"
      <+> "Gidl-generated RPC server -}"
    , "type alias" <+> "Client" <+> "msg" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        concat [ [ getFieldName n <+> colon <+> "Cmd msg"
                 , pollFieldName n <+> colon
                     <+> "Maybe Time" <+> "->" <+> "Cmd msg" ]
               | (GetMessage n _) <- ms ] ++
        [ setFieldName n <+> colon
          <+> elmTypeQName tmp t <+> "->" <+> "Cmd msg"
        | (SetMessage n t) <- ms
        ]
    , empty
      -- Msg variants for interpretation by Handler
    , "{-| Msg variants. Not recommended for use directly, instead"
      <+> "use the `Handler` interface -}"
    , "type" <+> "Msg"
    , indent 2 $ encloseStack equals empty (text "|") $
        concat [ [ getMsgConstr n <+> elmTypeQName tmp t
                 , pollMsgConstr n
                 , pollRateMsgConstr n <+> parens "Maybe Time" ]
               | (GetMessage n t) <- ms ] ++
        [ setMsgConstr n | (SetMessage n _) <- ms ]
      -- helper to determine whether a Msg originated from a network call
    , "{-| Determine whether a `Msg` arose from a network call -}"
    , "networkMsg" <+> colon <+> "Msg" <+> "->" <+> "Bool"
    , "networkMsg" <+> "msg" <+> equals
    , indent 2 $ stack $
        [ "case msg of"
        , indent 2 $ stack $
          [ getMsgConstr n <+> "_" <+> "->" <+> "True"
          | (GetMessage n _) <- ms ] ++
          [ setMsgConstr n <+> "->" <+> "True"
          | (SetMessage n _) <- ms ] ++
          [ "_ -> False" ]
        ]
    , empty
      -- function for building a client
    , "{-| Given an error handler, a builder for your `msg` type, and a"
      <+> "base URL for the RPC server, build a `Client` -}"
    , "client" <+> colon
      <+> "Time.Time" <+> "->"
      <+> "(Http.Error -> msg)" <+> "->"
      <+> parens ("Msg" <+> "->" <+> "msg") <+> "->"
      <+> "String" <+> "->"
      <+> "Client" <+> "msg"
    , "client" <+> "to" <+> "err" <+> "ok" <+> "url" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ getFieldName n <+> equals
          <+> "Utils.send_" <+> "err"
          <+> parens ("ok" <+> "<<" <+> getMsgConstr n)
          <+> parens ("Utils.get_" <+> "to"
            <+> parens ("url" <+> "++"
              <+> dquotes ("/" <> text rawName <> "/" <> text n))
            <+> typeDecoder tmp t)
        | (GetMessage n t) <- ms ] ++
        [ pollFieldName n <+> equals <+> "\\mt ->"
          <+> "Task.perform"
          <+> parens ("always"
            <+> parens ("ok"
              <+> parens (pollRateMsgConstr n <+> "mt")))
          <+> parens ("Task.succeed" <+> "()")
        | (GetMessage n _) <- ms ] ++
        [ setFieldName n <+> equals <+> "\\x ->"
          <+> "Utils.send_" <+> "err"
          <+> parens ("ok" <+> "<<" <+> "always" <+> setMsgConstr n)
          <+> parens ("Utils.post_" <+> "to"
            <+> parens ("url" <+> "++"
              <+> dquotes ("/" <> text rawName <> "/" <> text n))
            <+> parens ("Http.jsonBody"
              <+> parens (typeEncoder tmp t <+> "x")))
        | (SetMessage n t) <- ms
        ]
    , empty
      -- Handler type declaration and default implementation
    , "{-| The `Handler` is how the client should integrate with"
      <+> "your Elm Architecture `update` function -}"
    , "type alias" <+> "Handler"
      <+> "model" <+> "msg" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ handleGet n <+> colon
          <+> elmTypeQName tmp t <+> "-> model -> (model, Cmd msg)"
        | (GetMessage n t) <- ms ] ++
        [ handlePoll n <+> colon
          <+> "(model -> Client msg) -> model -> (model, Cmd msg)"
        | (GetMessage n _) <- ms ] ++
        [ handlePollRate n <+> colon
          <+> "Maybe Time -> model -> (model, Cmd msg)"
        | (GetMessage n _) <- ms ] ++
        [ handleSet n <+> colon <+> "model -> (model, Cmd msg)"
        | (SetMessage n _) <- ms ]
    , empty
    , "{-| The `defaultHandler` does nothing for any messages;"
      <+> "override the operations you need with your logic -}"
    , "defaultHandler" <+> colon <+> "Handler" <+> "model" <+> "msg"
    , "defaultHandler" <+> equals <+> "updatingHandler (\\m _ -> m)"
    , empty
      -- model-updating handler
    , "{-| The `updatingHandler` updates a field in your model when"
      <+> "new data arrives. You must provide a suitable update function"
      <+> "that updates the" <+> text interfaceName <+> "in your model -}"
    , "updatingHandler" <+> colon
      <+> parens
        ("model"
          <+> "->"
          <+> parens (text interfaceName <+> "->" <+> text interfaceName)
          <+> "->"
          <+> "model")
      <+> "->" <+> "Handler" <+> "model" <+> "msg"
    , "updatingHandler" <+> "upd" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ handleGet n <+> equals
          <+> "\\x m ->"
          <+> parens
            ("upd m"
             <+> parens
               ("\\i ->" <+> braces
                  ("i |" <+> text (toCamel n) <+> equals <+> "x"))
             <> comma <+> "Cmd.none")
        | (GetMessage n _) <- ms ] ++
        [ handlePoll n <+> equals
          <+> "\\cl m ->"
          <+> parens ("m" <+> comma
            <+> parens ("cl m") <> dot <> getFieldName n)
        | (GetMessage n _) <- ms ] ++
        [ handlePollRate n <+> equals
          <+> "\\mt m ->"
          <+> parens
            ("upd m"
             <+> parens
               ("\\i ->" <+> braces
                  ("i |" <+> pollRateName n <+> equals <+> "mt"))
               <> comma <+> "Cmd.none")
        | (GetMessage n _) <- ms ] ++
        [ handleSet n <+> equals <+> "\\m -> (m, Cmd.none)"
        | (SetMessage n _) <- ms ]
    , empty
      -- Handler driver
    , "{-| Driver for a `Handler`; use in your `update` function -}"
    , "handle" <+> colon
      <+> "Handler model msg -> (model -> Client msg)"
      <+> "-> Msg -> model -> (model, Cmd msg)"
    , "handle" <+> "h" <+> "cl" <+> "r" <+> "m" <+> equals
    , indent 2 $ stack $
        [ "case r of"
        , indent 2 $ stack $
            [ getMsgConstr n <+> "x" <+> "->"
              <+> "h" <> dot <> handleGet n <+> "x" <+> "m"
            | (GetMessage n _) <- ms ] ++
            [ pollMsgConstr n <+> "->"
              <+> "h" <> dot <> handlePoll n <+> "cl" <+> "m"
            | (GetMessage n _) <- ms ] ++
            [ pollRateMsgConstr n <+> "mt" <+> "->"
              <+> "h" <> dot <> handlePollRate n <+> "mt" <+> "m"
            | (GetMessage n _) <- ms ] ++
            [ setMsgConstr n <+> "->"
              <+> "h" <> dot <> handleSet n <+> "m"
            | (SetMessage n _) <- ms
            ]
        ]
    , empty
      -- Subscription generator
    , "{-| Generate subscriptions from the poll rates in the `"
      <> text interfaceName <> "` -}"
    , "subscriptions" <+> colon
      <+> text interfaceName <+> "->"
      <+> parens "Msg -> msg" <+> "->"
      <+> "List (Sub.Sub msg)"
    , "subscriptions" <+> "i" <+> "toMsg" <+> equals
    , indent 2 $ encloseStack lbracket rbracket comma $
        [ "Maybe.withDefault Sub.none" <+> parens
            ("Maybe.map" <+> parens
              ("\\t -> Time.every t" <+> parens
                ("always" <+> parens ("toMsg" <+> pollMsgConstr n)))
              <+> "i" <> dot <> pollRateName n)
        | (GetMessage n _) <- ms ]
    , empty
    , "{-| Map over the polling rates that are not disabled -}"
    , "mapPollRates" <+> colon <+> parens "Time -> Time" <+> "->"
      <+> text interfaceName <+> "->" <+> text interfaceName
    , "mapPollRates f i ="
    , indent 2 $ braces ("i" <+>
        indent 2 (encloseStack "|" empty comma $
          [ pollRateName n <+> equals
            <+> "Maybe.map f" <+> "i" <> dot <> pollRateName n
          | (GetMessage n _) <- ms ]))
    ]
  where
  handleGet n = "handleGot" <> text (toCapped n)
  handleSet n = "handleSet" <> text (toCapped n)
  handlePoll n = "handlePoll" <> text (toCapped n)
  handlePollRate n = "handlePollRate" <> text (toCapped n)
  getMsgConstr n = "Got" <> text (toCapped n)
  setMsgConstr n = "Set" <> text (toCapped n)
  pollMsgConstr n = "Poll" <> text (toCapped n)
  pollRateMsgConstr n = "PollRate" <> text (toCapped n)
  getFieldName n = "get" <> text (toCapped n)
  setFieldName n = "set" <> text (toCapped n)
  pollFieldName n = "poll" <> text (toCapped n)
  pollRateName n = text (toCamel n) <> "PollRate"


-- | Similar to 'toCapped', but ignores a trailing @_i@
ifModuleName :: Interface -> String
ifModuleName (Interface iname _ _) = aux iname
  where
  aux :: String -> String
  aux = first_cap . u_to_camel
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'i':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []
