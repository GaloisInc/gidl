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
    where typepath = reverse . drop 1 . reverse

  typeimports = map (\a -> importDecl tm a)
              $ nub
              $ map importType
              $ clientMessageTypes i

  extraimports = [ "import Http"
                 , "import Json.Decode"
                 , "import Json.Encode"
                 , "import Task" ]

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
    , "unused = unused"
    ]
clientIfDecl tmp rawName interfaceName ms = stack $
    [ "{-| A type containing all of the fields of the interface,"
      <+> "useful for keeping state in a model -}"
    , "type alias" <+> text interfaceName <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ text (toCamel n) <+> colon <+> elmTypeQName tmp t
        | (GetMessage n t) <- ms
        ]
    , empty
    , "{-|" <+> text interfaceName
      <+> "initialized with (arbitrary) default values -}"
    , "init" <+> colon <+> text interfaceName
    , "init" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ text (toCamel n) <+> equals <+> typeInit tmp t
        | (GetMessage n t) <- ms
        ]
    , empty
      -- client interface
    , "{-| Client interface for" <+> text interfaceName <+> "backed by a"
      <+> "Gidl-generated RPC server -}"
    , "type alias" <+> "Client" <+> "msg" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ getFieldName n <+> colon <+> "Cmd msg"
        | (GetMessage n _) <- ms
        ] ++
        [ setFieldName n <+> colon
          <+> elmTypeQName tmp t <+> "->" <+> "Cmd msg"
        | (SetMessage n t) <- ms
        ]
    , empty
      -- response variants for interpretation by Handler
    , "{-| Response variants. Not recommended for use directly, instead"
      <+> "use the `Handler` interface -}"
    , "type" <+> "Response"
    , indent 2 $ encloseStack equals empty (text "|") $
        [ getResponseConstr n <+> elmTypeQName tmp t
        | (GetMessage n t) <- ms ] ++
        [ setResponseConstr n
        | (SetMessage n _) <- ms ]
      -- function for building a client
    , "{-| Given an error handler, a builder for your `msg` type, and a"
      <+> "base URL for the RPC server, build a `Client` -}"
    , "client" <+> colon
      <+> "(Http.Error -> msg)" <+> "->"
      <+> parens ("Response" <+> "->" <+> "msg") <+> "->"
      <+> "String" <+> "->"
      <+> "Client" <+> "msg"
    , "client" <+> "err" <+> "ok" <+> "url" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ getFieldName n <+> equals
          <+> "Task.perform" <+> "err"
          <+> parens ("ok" <+> "<<" <+> getResponseConstr n)
          <+> parens ("Http.get" <+> typeDecoder tmp t
            <+> parens ("url" <+> "++"
              <+> dquotes ("/" <> text rawName <> "/" <> text n)))
        | (GetMessage n t) <- ms ] ++
        [ setFieldName n <+> equals <+> "\\x ->"
          <+> "Task.perform" <+> "err"
          <+> parens ("ok" <+> "<<" <+> "always" <+> setResponseConstr n)
          <+> parens ("Http.post" <+> parens ("Json.Decode.succeed" <+> "()")
            <+> parens ("url" <+> "++"
              <+> dquotes ("/" <> text rawName <> "/" <> text n))
            <+> parens ("Http.string"
              <+> parens ("Json.Encode.encode 1000"
                <+> parens (typeEncoder tmp t <+> "x"))))
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
        [ handleSet n <+> equals <+> "\\m -> (m, Cmd.none)"
        | (SetMessage n _) <- ms ]
    , empty
      -- Handler driver
    , "{-| Driver for a `Handler`; use in your `update` function -}"
    , "handle : Handler model msg -> Response -> model -> (model, Cmd msg)"
    , "handle" <+> "h" <+> "r" <+> "m" <+> equals
    , indent 2 $ stack $
        [ "case r of"
        , indent 2 $ stack $
            [ getResponseConstr n <+> "x" <+> "->"
              <+> "h" <> dot <> handleGet n <+> "x" <+> "m"
            | (GetMessage n _) <- ms ] ++
            [ setResponseConstr n <+> "->"
              <+> "h" <> dot <> handleSet n <+> "m"
            | (SetMessage n _) <- ms
            ]
        ]
    ]
  where
  handleGet n = "handleGot" <> text (toCapped n)
  handleSet n = "handleSet" <> text (toCapped n)
  getResponseConstr n = "Got" <> text (toCapped n)
  setResponseConstr n = "Set" <> text (toCapped n)
  getFieldName n = "get" <> text (toCapped n)
  setFieldName n = "set" <> text (toCapped n)

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
