module Malgo.Lens
  ( HasBranches (..),
    HasClauses (..),
    HasCoclauses (..),
    HasConsumers (..),
    HasDefaultBranch (..),
    HasLiteral (..),
    HasProducers (..),
    HasTag (..),
    HasTerm (..),
    HasParams (..),
    HasReturns (..),
    HasPattern (..),
    HasCopattern (..),
    HasStatement (..),
    HasProducer (..),
    HasConsumer (..),
    HasConstructors (..),
    HasTypes (..),
    HasOrigin (..),
    HasXPVar (..),
  )
where

import Control.Lens (Traversal')

class HasBranches s a | s -> a where
  branches :: Traversal' s a

class HasClauses s a | s -> a where
  clauses :: Traversal' s a

class HasCoclauses s a | s -> a where
  coclauses :: Traversal' s a

class HasConsumers s a | s -> a where
  consumers :: Traversal' s a

class HasDefaultBranch s a | s -> a where
  defaultBranch :: Traversal' s a

class HasLiteral s a | s -> a where
  literal :: Traversal' s a

class HasProducers s a | s -> a where
  producers :: Traversal' s a

class HasTag s a | s -> a where
  tag :: Traversal' s a

class HasTerm s a | s -> a where
  term :: Traversal' s a

class HasParams s a | s -> a where
  params :: Traversal' s a

class HasReturns s a | s -> a where
  returns :: Traversal' s a

class HasPattern s a | s -> a where
  pattern :: Traversal' s a

class HasCopattern s a | s -> a where
  copattern :: Traversal' s a

class HasStatement s a | s -> a where
  statement :: Traversal' s a

class HasProducer s a | s -> a where
  producer :: Traversal' s a

class HasConsumer s a | s -> a where
  consumer :: Traversal' s a

class HasConstructors s a | s -> a where
  constructors :: Traversal' s a

class HasTypes s a | s -> a where
  types :: Traversal' s a

class HasOrigin s a | s -> a where
  origin :: Traversal' s a

class HasXPVar s a | s -> a where
  xPVar :: Traversal' s a