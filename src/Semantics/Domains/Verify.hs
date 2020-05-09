module Semantics.Domains.Verify (verifyDomains) where

import Syntax.Ast

import Semantics.Env
import Semantics.General

import Semantics.Domains.Bind
import Semantics.Domains.Sanity
import Semantics.Domains.Wellformed

verifyDomains :: Spec -> Result DomEnv
verifyDomains ast = case bindDom ast of
  Ok (de, nuxs, uxs) ->
    case uniqueTags de uxs of
      Bad msg -> Bad msg
      _ -> case wellformed de nuxs of
        Bad msg -> Bad msg
        _ -> case unionAliasCheck de nuxs of
          Bad msg -> Bad msg
          _ -> Ok de
  Bad msg -> Bad msg
