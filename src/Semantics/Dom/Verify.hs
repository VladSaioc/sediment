module Semantics.Dom.Verify (verifyDomains) where

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.Bind
import Semantics.Dom.General
import Semantics.Dom.Sanity
import Semantics.Dom.Wellformed
import Semantics.Dom.Wellordered

verifyDomains :: Spec -> Err DomEnv
verifyDomains ast = case bindDom ast of
  Ok (de, nuxs, uxs) ->
    case uniqueTags de uxs of
      Bad msg -> Bad msg
      _ -> case wellformed de nuxs of
        Bad msg -> Bad msg
        _ -> case unionAliasCheck de nuxs of
          Bad msg -> Bad msg
          _ -> case wellordered de uxs of
            Bad msg -> Bad msg
            _ -> Ok de
  Bad msg -> Bad msg
