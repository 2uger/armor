data SiblingList = Sibling ASTNode SiblingList | NULL deriving (Show, Eq)

data ASTNode = ASTNode { parent :: ASTNode
                       , lefmostChildList :: SiblingList
                       , lefmostChildNum :: Int
                       } deriving (Eq, Show, Read)

-- Need to construct ast node for every 
-- production
--

-- Add ASTNode to list of siblings
makeSiblings :: ASTNode -> SiblingList -> SiblingList

-- Make node depend on type of t 
makeNode t 
    | t == DeclList = make node for decl list
    | t == Decl = make node for decl
  ...

-- Adopt y's children and y itself to parent x 
adoptChildren x y



