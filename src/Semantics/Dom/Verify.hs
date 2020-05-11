module Semantics.Dom.Verify (verifyDomains) where

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.Bind
import Semantics.Dom.General
import Semantics.Dom.Sanity
import Semantics.Dom.Wellformed
import Semantics.Dom.Wellordered

verifyDomains :: Spec -> Err (DomEnv, TagTable)
verifyDomains ast = case bindDom ast of
  Ok (de, nuxs, uxs) ->
    case domDeclCheck de ast of
      Bad msg -> Bad msg
      Ok _ -> case uniqueTags de uxs of
        Bad msg -> Bad msg
        _ -> let
            tt = makeTagTable de uxs
          in case wellformed de nuxs of
            Bad msg -> Bad msg
            _ -> case unionAliasCheck de nuxs of
              Bad msg -> Bad msg
              _ -> case wellordered de uxs of
                Bad msg -> Bad msg
                _ -> Ok (de, tt)
  Bad msg -> Bad msg
