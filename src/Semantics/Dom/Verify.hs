module Semantics.Dom.Verify (verifyDomains) where

import Syntax.Ast
import Syntax.ErrM

import Semantics.Dom.Bind
import Semantics.Dom.General
import Semantics.Dom.Sanity
import Semantics.Dom.Wellformed
import Semantics.Dom.Wellordered

verifyDomains :: Spec -> Err (DomEnv, TagTable)
verifyDomains ast = do
  (de, nuxs, uxs) <- bindDom ast
  domDeclCheck de ast
  uniqueTags de uxs
  let tt = makeTagTable de uxs
  wellformed de nuxs
  unionAliasCheck de nuxs
  wellordered de uxs
  Ok (de, tt)
